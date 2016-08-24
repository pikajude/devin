{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

module Irc where

import           ClientState
import           Control.Category                (Category)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted       hiding (writeChan, yield)
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import qualified Damn
import           Data.ByteString                 as SB (ByteString, elem)
import qualified Data.ByteString                 as SB hiding (splitAt)
import qualified Data.ByteString.Lazy            as LB
import qualified Data.ByteString.UTF8            as SB
import qualified Data.HashMap.Strict             as H
import           Data.IORef
import           Data.List                       (sort)
import           Data.Monoid
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Data.Text.Encoding
import qualified Data.Text.IO                    as T
import           Data.Time
import           Data.Version
import           LogInstances
import           Network
import qualified Network.Damn                    as Damn
import           Network.IRC.Base                hiding (render)
import           Paths_devin
import           System.Exit
import           System.IO
import qualified System.IO.Streams               as S
import qualified System.IO.Streams.Concurrent    as S
import qualified System.IO.Streams.Core          as S
import           Text.PrettyPrint.ANSI.Leijen    hiding ((<$>), (<>))

pattern Action t <- (SB.splitAt 8 -> ("\1ACTION ", SB.init -> t))

prettyIrc m@(Message pre cmd prms) =
    showPre pre (showCmd cmd <+> showParams prms)
    where
        showPre (Just s) = (<+>) (dullmagenta (":" <> string (SB.toString $ showPrefix s)))
        showPre Nothing = id
        showParams [p] | SB.elem 32 p = ":" <> bsToDoc p
                       | otherwise = bsToDoc p
        showParams (p:ps) = bsToDoc p <+> showParams ps
        showParams [] = mempty
        showCmd = dullblue . bsToDoc
        bsToDoc = string . SB.toString

respond :: (MonadIO m, MonadLog Doc m, MonadReader c m, HasClientEnv c
           , GetLogHandler Doc m, MonadCatch m)
        => S.InputStream (Maybe (Either String Message))
        -> S.OutputStream Message
        -> ClientEnv
        -> m ()
respond ircInputs ircOutput ce = (`fix` AuthEnvPre Nothing Nothing ce) $ \ f st -> do
    st'@(AuthEnvPre n a _) <- execStateT (mapM_ (`runStep` setupMap) =<< liftIO (join <$> S.read ircInputs)) st
    retried <- liftIO newEmptyMVar
    case (n, a) of
        (Just n', Just a') -> fix $ \ g -> do
            clientRes <- Damn.initClientInstance
            case clientRes of
                Left ioe -> notifyAndWait retried ioe g
                Right (damnInputs, damnOutput) -> do
                    generalized <- liftIO $ S.map IRCMessage ircInputs
                    switcher <- liftIO $ S.concurrentMerge [damnInputs, generalized]

                    let ae = AuthEnv { clientEnv = ce
                                     , clientNick = n'
                                     , clientAuth = a'
                                     , _joinQueue = []
                                     , _loggedIn = False
                                     , _privclasses = mempty
                                     , _users = mempty
                                     , _serverStream = damnOutput
                                     }

                    clientLoop n' a' ae switcher damnOutput g
        _ -> f st'
    where
        clientLoop n' a' ae switcher damnOutput next = do
            goAgain <- (`evalStateT` ae) $ do
                greet n' a'
                handshake
                fix $ \ h -> do
                    x <- liftIO $ S.read switcher
                    case x of
                        Nothing -> return False
                        Just (IRCMessage Nothing) -> return False
                        Just (IRCMessage (Just i)) -> runStep i respondMap >> h
                        Just (DamnMessage Nothing) -> return True
                        Just (DamnMessage (Just d)) -> Damn.runStep d >> h

            if goAgain
                then next
                else do
                    liftIO $ S.write Nothing damnOutput
                    liftIO $ S.write Nothing ircOutput

        notifyAndWait retriedMV ioe next = do
            liftIO $ do
                S.writeTo ircOutput $ Just $ noticeMessage $
                    "Failed to connect to dAmn! The error was: " <> SB.fromString (show ioe)
                S.writeTo ircOutput $ Just $ noticeMessage $ SB.fromString
                    "Waiting 30 seconds before trying again. "
                firstTry <- isEmptyMVar retriedMV
                when firstTry $ do
                    putMVar retriedMV ()
                    S.writeTo ircOutput $ Just $ noticeMessage $ SB.fromString $
                        "If the problem persists, dAmn may be down; check the deviantART status page"
                        ++ " at https://support.deviantart.com/forums/140408-DeviantArt-Status#recent"
                delay 30000000
            next

runStep (Left x) rMap = sendClient $ noticeMessage (SB.fromString x)
runStep (Right pkt) rMap = H.lookupDefault (const $ return ()) (msg_command pkt) rMap pkt

noop = return ()

setupMap = [ ("NICK", c_nick)
           , ("USER", const noop)
           , ("PASS", c_pass)
           , ("CAP", c_cap)
           -- , ("JOIN", c_prejoin)
           ]

irc2dc channel
    | "#" `SB.isPrefixOf` channel = pure $ "chat:" <> SB.tail channel
    | "&" `SB.isPrefixOf` channel = do
        nick <- gets clientNick
        pure $ "pchat:" <> SB.intercalate ":" (sort [nick, SB.tail channel])

respondMap = H.fromList
             [ ("CAP" , c_cap)
             , ("MODE", c_mode)
             , ("PING", c_ping)
             , ("JOIN", c_join)
             , ("PART", c_part)
             , ("QUIT", \ _ -> return ())

             , ("PRIVMSG", c_privmsg)
             , ("WHO", c_who)
             , ("NAMES", c_names)

             , ("NICK", const noop)
             , ("USER", const noop)
             , ("PASS", const noop)
             ]

c_cap (Message _ _ ["LS"])
    = sendClient $ serverMessage "CAP" ["*", "LS", "account-notify"]
c_cap _ = noop

c_nick (Message _ _ [n]) = nick ?= n

c_pass (Message _ _ [password]) = authtoken ?= password

c_mode (Message _ _ [channel]) = do
    nick <- gets clientNick
    sendClient $ serverMessage "324" [nick, channel, "+i"]

c_mode (Message _ _ [channel, "b"]) = do
    nick <- gets clientNick
    sendClient $ serverMessage "368" [nick, channel, "End of channel ban list."]

c_mode _ = noop

c_ping (Message _ _ args) = sendClient $ serverMessage "PONG" $ args ++ args

c_join (Message _ _ rooms'') = do
    let rooms' = SB.split 44 . SB.intercalate "," $ filter (not . SB.null) rooms''
    lin <- use loggedIn
    rooms <- mapM irc2dc rooms'
    if lin
        then forM_ rooms joinChannel
        else joinQueue <>= Set.fromList rooms

c_part (Message _ _ (room : _)) = do
    d <- irc2dc room
    sendServer $ Damn.Message "part" (Just d) [] Nothing

c_privmsg (Message _ _ [channel, Action msg]) = do
    room <- irc2dc channel
    sendServer $ Damn.Message "send" (Just room) [] (Just $ Damn.toBodyText $ "action main\n\n" <> decodeUtf8 msg)

c_privmsg (Message _ _ [channel, msg]) = do
    room <- irc2dc channel
    sendServer $ Damn.Message "send" (Just room) [] (Just $ Damn.toBodyText $ "msg main\n\n" <> decodeUtf8 msg)

c_who (Message _ _ [channel])
    | "#" `SB.isPrefixOf` channel || "&" `SB.isPrefixOf` channel = do
        room <- irc2dc channel
        Damn.specialWHO room
    | otherwise = noop

c_names (Message _ _ [channel])
    | "#" `SB.isPrefixOf` channel || "&" `SB.isPrefixOf` channel = do
        room <- irc2dc channel
        Damn.specialNAMES room
    | otherwise = noop

greet n p = do
    host <- gets (serverHost . getClientEnv)
    t <- gets (startTime . getClientEnv)
    sendClient $ serverMessage "001" [n, "Hi " <> n <> ", welcome to devin - the dAmn IRC proxy"]
    sendClient $ serverMessage "002" [n, "Your host is "
                                   <> SB.fromString host
                                   <> ", running version "
                                   <> SB.fromString (showVersion version)]
    sendClient $ serverMessage "003" [n, "This server was created " <> SB.fromString (fmtTime t)]
    sendClient $ serverMessage "004" [n, SB.fromString host, "devin-" <> SB.fromString (showVersion version)
                                     , "bqov"
                                     , "ib"
                                     ]
    sendClient $ serverMessage "005" [n, "CHANTYPES=#&", "PREFIX=(qov)~@+", "NETWORK=deviantart", "STATUSMSG=@+", "are supported by this server"]
    sendClient $ serverMessage "375" [n, "- devin Message of the Day -"]
    sendClient $ serverMessage "372" [n, "- Welcome to devin, the dAmn IRC proxy."]
    sendClient $ serverMessage "376" [n, "End of /MOTD command."]
    sendClient $ noticeMessage "Connecting to dAmn..."
    where
        fmtTime = formatTime defaultTimeLocale "%a %b %e %Y at %H:%M:%S UTC"

handshake = sendServer $ Damn.Message "dAmnClient" (Just "0.3") [("agent", "devin")] Nothing

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
