{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ViewPatterns              #-}

module Damn (runStep, initClientInstance, specialWHO, specialNAMES) where

import           ClientState
import qualified Colors
import           Control.Arrow                (second)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.State
import qualified Data.ByteString              as SB
import qualified Data.ByteString.UTF8         as SB (fromString, toString)
import qualified Data.HashMap.Strict          as H
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as Set
import           Data.String
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Read               as R
import           LogInstances
import           Network
import           Network.Damn
import           Network.Damn.Format.IRC
import qualified Network.IRC.Base             as IRC
import           ParserStreams
import           Prelude
import           System.IO
import qualified System.IO.Streams            as S
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))

initClientInstance :: (GetLogHandler Doc m, MonadIO m)
                   => m (S.InputStream Event, S.OutputStream Message)
initClientInstance = do
    logHandler <- getLogHandler

    handle <- liftIO $ connectTo "chat.deviantart.com" (PortNumber 3900)
    liftIO $ hSetBinaryMode handle True

    (i__, o) <- handleStreams handle messageP render
        (logHandler . (<+>) (dullyellow "<<<") . prettyDamn)
        (logHandler . (<+>) (dullmagenta ">>>") . prettyDamn)
    i_ <- makeExplicitEOF i__
    i <- liftIO $ S.map DamnMessage i_
    return (i, o)

runStep :: (MonadLog Doc m, MonadIO m, MonadState AuthEnv m)
        => Either String Message -> m ()
runStep (Left s) = logMessage (dullred "!!!" <+> string s)
runStep (Right d) = case H.lookup (messageName d) respondMap of
    Just f -> f d
    Nothing -> sendClient $ noticeMessage $ "Unknown Message: " <> SB.fromString (show d)

dc2Irc :: MonadState AuthEnv m => SB.ByteString -> m SB.ByteString
dc2Irc chat
    | "chat:" `SB.isPrefixOf` chat = pure $ "#" <> SB.drop 5 chat
    | "pchat:" `SB.isPrefixOf` chat
    , [_, u1, u2] <- SB.split 58 chat = do
        nick' <- gets clientNick
        pure $ "&" <> (if nick' == u1 then u2 else u1)
    | otherwise = error "dc2Irc"

respondMap :: (MonadIO m, MonadState AuthEnv m) => H.HashMap SB.ByteString (Message -> m ())
respondMap = H.fromList
             [ ("dAmnServer", c_dAmnServer)
             , ("login", c_login)
             , ("join", c_join)
             , ("part", c_part)
             , ("recv", c_recv)
             , ("property", c_property)
             , ("ping", \ _ -> sendServer $ Message "pong" Nothing [] Nothing)
             ]

c_dAmnServer :: (MonadIO m, MonadState AuthEnv m) => Message -> m ()
c_dAmnServer _ = do
    n <- gets clientNick
    a <- gets clientAuth
    sendServer $ Message "login" (Just n) [("pk", T.decodeUtf8 a)] Nothing
    sendClient $ noticeMessage "Successful handshake"

c_login :: (MonadIO m, MonadState AuthEnv m) => Message -> m ()
c_login (Message _ _ args _)
    | Just "ok" <- lookup "e" args = do
        loggedIn .= True
        jq <- use joinQueue
        unless (Set.null jq) $ forM_ jq joinChannel
        sendClient $ noticeMessage "Logged in successfully."
    | Just x <- lookup "e" args = sendClient $ noticeMessage $ "Login error: " <> T.encodeUtf8 x
    | otherwise = sendClient $ noticeMessage "Unknown login packet."

c_join :: (MonadIO m, MonadState AuthEnv m) => Message -> m ()
c_join (Message _ (Just param) args _)
    | Just "ok" <- lookup "e" args = do
        nick' <- gets clientNick
        room <- dc2Irc param
        sendClient $ IRC.Message (mkNickName nick') "JOIN" [room]
    | Just err <- lookup "e" args = sendClient $ noticeMessage $
        "Can't join room: " <> T.encodeUtf8 err

c_join _msg = return ()

c_part :: (MonadIO m, MonadState AuthEnv m) => Message -> m ()
c_part (Message _ (Just param) args _)
    | Just "ok" <- lookup "e" args = do
        nick' <- gets clientNick
        room <- dc2Irc param
        sendClient $ IRC.Message (mkNickName nick') "PART" [room]
    | Just err <- lookup "e" args = sendClient $ noticeMessage $
        "Can't join room: " <> T.encodeUtf8 err

c_part _msg = return ()

c_property :: (MonadIO m, MonadState AuthEnv m) => Message -> m ()
c_property (Message _ (Just param) args body)
    | Just "topic" <- lookup "p" args
    , Just ts <- lookup "ts" args
    , Just setter <- lookup "by" args = do
        nick' <- gets clientNick
        room <- dc2Irc param
        let topicLines = unLines $ maybe "" (bodyWithFormat ircFormat) body
        sendClient $ serverMessage "332" [ nick', room, SB.intercalate " - " topicLines ]
        sendClient $ serverMessage "333" [ nick', room
                                         , IRC.showPrefix $ fromJust $ mkNickNameText setter
                                         , T.encodeUtf8 ts
                                         ]

    | Just "members" <- lookup "p" args = do
        Just pcs <- use (privclasses . at param)
        room <- dc2Irc param
        us' <- (users . at param) <?= userListToMap (mapMaybe (toUser pcs) $ subpacketList body)
        showUsers room us'

    | Just "privclasses" <- lookup "p" args
    , Just mb <- body = privclasses . at param ?= H.fromList (parsePrivclasses (bodyBytes mb))

    | otherwise = return ()
    where
        subpacketList (SubM pkt@(SubMessage _ _ _ body')) = pkt : subpacketList body'
        subpacketList _ = []
        toUser pcs (SubMessage _ param' args' _) = do
            uname <- param'
            pcname <- lookup "pc" args'
            rn <- lookup "realname" args'
            pclevel <- H.lookup pcname pcs
            return $ User uname pcname (pcToSymbol pclevel) rn 1
        userListToMap = foldr (\ u -> H.insertWith incJoinCount (userName u) u) mempty
        incJoinCount (User _ _ _ _ j1) (User a b c d j2) = User a b c d (j1 + j2)
        parsePrivclasses = mapMaybe (readInt . second T.tail . T.breakOn ":") . T.lines . T.decodeLatin1
        readInt (R.decimal -> Right (i, ""), x) = Just (x, i)
        readInt _                               = Nothing

c_property _msg = return ()

pcToSymbol :: (IsString t, Ord a, Num a) => a -> t
pcToSymbol n | n >= 99 = "~"
             | n >= 75 = "@"
             | n >= 50 = "+"
             | otherwise = ""

pcToMode :: (IsString t, Ord a, Num a) => a -> t
pcToMode n | n >= 99 = "q"
           | n >= 75 = "o"
           | n >= 50 = "v"
           | otherwise = ""

bodyLines :: MessageBody -> [SB.ByteString]
bodyLines b = unLines $ bodyWithFormat ircFormat b

c_recv :: (MonadIO m, MonadState AuthEnv m)
       => Message -> m ()
c_recv (Message _ (Just param) _ (SubM (SubMessage (Just "msg") _ args (Just body))))
    | Just from' <- lookup "from" args = do
        room <- dc2Irc param
        nick' <- gets clientNick
        when (T.encodeUtf8 from' /= nick') $
            forM_ (bodyLines body) $ \ line' ->
                sendClient $ IRC.Message (mkNickNameText from') "PRIVMSG" [room, line']

c_recv (Message _ (Just param) _ (SubM (SubMessage (Just "action") _ args (Just body))))
    | Just from' <- lookup "from" args = do
        room <- dc2Irc param
        nick' <- gets clientNick
        when (T.encodeUtf8 from' /= nick') $
            forM_ (bodyLines body) $ \ line' ->
                sendClient $ IRC.Message (mkNickNameText from') "PRIVMSG"
                    [room, "\1ACTION " <> line' <> "\1"]

c_recv (Message _ (Just param) _
    (SubM (SubMessage (Just "join") (Just uname) _
        (SubM (SubMessage Nothing Nothing subArgs _)))))
    | Just pc <- lookup "pc" subArgs
    , Just rn <- lookup "realname" subArgs = do
        Just pclevel <- preuse (privclasses . ix param . ix pc)
        existingUser <- preuse (users . ix param . ix uname)
        let newUser = User uname pc (pcToSymbol pclevel) rn newJc
            newJc = maybe 1 (succ . joinCount) existingUser
        room <- dc2Irc param
        users . ix param %= H.insert uname newUser
        when (newJc == 1) $ do
            sendClient $ IRC.Message (mkNickName uname) "JOIN" [room]
            unless (SB.null $ userSymbol newUser) $
                sendClient $ serverMessage "MODE"
                    [room, "+" <> pcToMode pclevel, uname]
        unless (newJc == 1) $
            sendClient $ channelNotice room $
                uname <> " has joined again, now joined "
                      <> SB.fromString (show newJc)
                      <> " time(s)"

c_recv (Message _ (Just param) _ (SubM (SubMessage (Just "part") (Just uname) args _)))
    | reason <- lookup "r" args = do
        Just existingUser <- preuse (users . ix param . ix uname)
        let newJc = pred (joinCount existingUser)
        users . ix param . at uname ?= (existingUser { joinCount = newJc })
        room <- dc2Irc param
        when (newJc == 0) $ do
            sendClient $ IRC.Message (mkNickName uname) "PART" [room, maybe "no reason" T.encodeUtf8 reason]
            users . ix param %= H.delete uname
        unless (newJc == 0) $
            sendClient $ channelNotice room $
                uname <> " has left, now joined "
                      <> SB.fromString (show newJc)
                      <> " time(s)"

c_recv (Message _ (Just param) _
    (SubM (SubMessage (Just "privchg") (Just uname) args _)))
    | Just pc <- lookup "pc" args
    , Just by <- lookup "by" args = do
        Just existingUser <- preuse (users . ix param . ix uname)
        Just pcs <- use (privclasses . at param)

        let oldPrivclass = userPrivclass existingUser
            newPrivclass = pc
            Just oldLevel = H.lookup oldPrivclass pcs
            Just newLevel = H.lookup newPrivclass pcs
            newUser = existingUser { userPrivclass = newPrivclass
                                   , userSymbol = pcToSymbol newLevel
                                   }

            modeline = (case pcToMode oldLevel of "" -> ""; m -> "-" <> m)
                    <> (case pcToMode newLevel of "" -> ""; m -> "+" <> m)

            modeArgCount = SB.length modeline `div` 2

        room <- dc2Irc param
        users . ix param %= H.insert uname newUser

        sendClient $ channelNotice room $
             uname <> " has been made a member of "
                   <> Colors.bold (T.encodeUtf8 newPrivclass)
                   <> " by " <> T.encodeUtf8 by

        when (modeline /= "") $
            sendClient $ serverMessage "MODE" $
                [room, modeline] ++ replicate modeArgCount uname

c_recv _msg = return ()

prettyDamn :: Message -> Doc
prettyDamn (Message cmd prm args body) =
    appendBody body $ appendArgs args $ bsToDoc cmd <> showParam prm
    where
        showParam Nothing  = empty
        showParam (Just s) = space <> dullyellow (bsToDoc s)
        textToDoc = string . T.unpack
        bsToDoc = textToDoc . T.decodeUtf8
        appendArgs as doc
            | null as = doc
            | otherwise = align (vsep $ doc : map argToDoc as)
        argToDoc (k,a) = bsToDoc k <> "=" <> dullblue (textToDoc a)
        appendBody Nothing doc     = doc
        appendBody (Just b) doc = align (vsep [doc, empty, string $ SB.toString $ bodyBytes b])

specialWHO :: (MonadIO m, MonadState AuthEnv m)
           => SB.ByteString -> m ()
specialWHO room = do
    list' <- preuse (users . ix room)
    channel <- dc2Irc room
    nick' <- gets clientNick
    forM_ list' $ \ x -> do
        forM_ (H.toList x) $ \ (uname, user) ->
            sendClient $ serverMessage "352" [nick', channel
                                             , uname
                                             , "dAmn"
                                             , "dAmn"
                                             , uname
                                             , "H"
                                             , "0 " <> T.encodeUtf8 (userRealName user)
                                             ]
        sendClient $ serverMessage "315" [nick', channel, "End of /WHO list."]

specialNAMES :: (MonadIO m, MonadState AuthEnv m)
             => SB.ByteString -> m ()
specialNAMES param = do
    room <- dc2Irc param
    Just us' <- use (users . at param)
    showUsers room us'

showUsers room us' = do
    nick' <- gets clientNick
    sendClient $ serverMessage "353" [ nick'
                                     , "="
                                     , room
                                     , SB.intercalate " " $ map (\ u -> userSymbol u <> userName u) $ H.elems us'
                                     ]
    sendClient $ serverMessage "366" [ nick', room, "End of /NAMES list." ]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
