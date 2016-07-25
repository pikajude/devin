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
import           Control.Category                   (Category)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted          hiding (writeChan, yield)
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad
import qualified Control.Monad.Ether.Implicit.State as E
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Damn                               hiding (respondMap)
import           Data.ByteString                    as SB (ByteString, elem)
import qualified Data.ByteString                    as SB hiding (splitAt)
import qualified Data.ByteString.Lazy               as LB
import qualified Data.ByteString.UTF8               as SB
import qualified Data.HashMap.Strict                as H
import           Data.IORef
import           Data.List                          (sort)
import           Data.Machine
import           Data.Monoid
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import           Data.Text.Encoding
import qualified Data.Text.IO                       as T
import           Data.Time
import           Data.Version
import           LogInstances
import           Network
import           Network.IRC.Base                   hiding (render)
import           Paths_devin
import           System.Exit
import           System.IO
import           Text.Damn.Packet
import           Text.PrettyPrint.ANSI.Leijen       hiding ((<>))

pattern Action t <- (SB.splitAt 8 -> ("\1ACTION ", SB.init -> t))

prettyIrc (Message pre cmd prms) =
    showPre pre $ showCmd cmd <+> showParams prms
    where
        showPre (Just s) = (<+>) (dullmagenta (":" <> string (SB.toString $ showPrefix s)))
        showPre Nothing = id
        showParams [p] | SB.elem 32 p = ":" <> bsToDoc p
                       | otherwise = bsToDoc p
        showParams (p:ps) = bsToDoc p <+> showParams ps
        showParams [] = mempty
        showCmd = dullblue . bsToDoc
        bsToDoc = string . SB.toString

respond ircChan = fix (\ f st -> do
    st'@(ClientState n a) <- execStateT (runT_ $ construct nextPacket ~> construct (processOne setupMap)) st
    case (n, a) of
        (Just n', Just a') -> do
            greet n' a'
            c <- asks getClientEnv

            fix $ \ g -> do
                (authEnv, damnThread) <- initClientInstance c n' a'
                myThread <- async $ (`runReaderT` authEnv) $ do
                    handshake
                    runT_ $ repeatedly nextPacket ~> repeatedly (processOne respondMap)

                ended <- waitEitherCancel damnThread myThread
                case ended of
                    Left () -> g
                    Right () -> return ()

        _ -> f st') (ClientState Nothing Nothing)
    where
        nextPacket :: forall m k. MonadIO m => PlanT k (Either (String, LB.ByteString) Message) m ()
        nextPacket = yield =<< liftIO (readChan ircChan)

processOne rMap = do
    pkt' <- await
    case pkt' of
        Left (x, _) -> sendClient $ noticeMessage (SB.fromString x)
        Right pkt -> do
            logMessage $ dullgreen "<<<" <+> prettyIrc pkt
            H.lookupDefault (const stop) (msg_command pkt) rMap pkt

noop = return ()

setupMap = [ ("NICK", c_nick)
           , ("USER", const noop)
           , ("PASS", c_pass)
           , ("CAP", c_cap)
           -- , ("JOIN", c_prejoin)
           ]

irc2dc channel
    | "#" `SB.isPrefixOf` channel = pure $ "chat:" <> decodeUtf8 (SB.tail channel)
    | "&" `SB.isPrefixOf` channel = do
        nick <- asks clientNick
        pure $ decodeUtf8 $ "pchat:" <> SB.intercalate ":" (sort [nick, SB.tail channel])

-- respondMap :: (MonadIO m, HasClientEnv c, MonadReader c m, MonadLog Doc m)
--            => H.HashMap Command (Message -> ReaderT AuthEnv (PlanT (k (Either a Message)) o m) ())
respondMap = [ ("CAP" , c_cap)
             , ("MODE", c_mode)
             , ("PING", c_ping)
             , ("JOIN", c_join)
             , ("PART", c_part)
             , ("QUIT", const stop)

             , ("PRIVMSG", c_privmsg)
             , ("WHO", c_who)

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
    nick <- asks clientNick
    sendClient $ serverMessage "324" [nick, channel, "+i"]

c_mode (Message _ _ [channel, "b"]) = do
    nick <- asks clientNick
    sendClient $ serverMessage "368" [nick, channel, "End of channel ban list."]

c_mode _ = noop

c_ping (Message _ _ args) = sendClient $ serverMessage "PONG" $ args ++ args

c_join (Message _ _ [SB.split 44 -> rooms']) = do
    lin <- asks loggedIn >>= liftIO . readIORef
    rooms <- mapM irc2dc rooms'
    if lin
        then forM_ rooms joinChannel
        else do
            jq <- asks joinQueue
            liftIO $ modifyIORef' jq (<> S.fromList rooms)

c_part (Message _ _ [SB.split 44 -> rooms]) = forM_ rooms $ \ room -> do
    d <- irc2dc room
    sendServer $ Packet "part" (Just d) [] Nothing

c_privmsg (Message _ _ [channel, Action msg]) = do
    room <- irc2dc channel
    sendServer $ Packet "send" (Just room) [] (Just $ "action main\n\n" <> decodeUtf8 msg)

c_privmsg (Message _ _ [channel, msg]) = do
    room <- irc2dc channel
    sendServer $ Packet "send" (Just room) [] (Just $ "msg main\n\n" <> decodeUtf8 msg)

c_who (Message _ _ [channel]) = do
    room <- irc2dc channel
    sendDamnResponder $ WHO room

greet n p = do
    host <- asks (serverHost . getClientEnv)
    t <- asks (startTime . getClientEnv)
    sendClient $ serverMessage "001" [n, "Hi " <> n <> ", welcome to devin - the dAmn IRC proxy"]
    sendClient $ serverMessage "002" [n, "Your host is "
                                   <> SB.fromString host
                                   <> ", running version "
                                   <> SB.fromString (showVersion version)]
    sendClient $ serverMessage "003" [n, "This server was created " <> SB.fromString (fmtTime t)]
    sendClient $ serverMessage "004" [n, SB.fromString host, "devin-" <> SB.fromString (showVersion version), "i", "s", "I"]
    sendClient $ serverMessage "005" [n, "CHANTYPES=#&", "PREFIX=(qov)~@+", "are supported by this server"]
    sendClient $ serverMessage "375" [n, "- devin Message of the Day -"]
    sendClient $ serverMessage "372" [n, "- Welcome to devin, the dAmn IRC proxy."]
    sendClient $ serverMessage "376" [n, "End of /MOTD command."]
    sendClient $ noticeMessage "Connecting to dAmn..."
    where
        fmtTime = formatTime defaultTimeLocale "%a %b %e %Y at %H:%M:%S UTC"

handshake = sendServer $ Packet "dAmnClient" (Just "0.3") [("agent", "devin")] Nothing
