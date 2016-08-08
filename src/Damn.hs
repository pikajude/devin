{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}

module Damn (damnPackets, damnRespond, prettyDamn, ChatState(..), initClientInstance) where

import           ClientState
import qualified Colors
import           Control.Arrow                   (left, second)
import           Control.Concurrent.Async.Lifted
import qualified Control.Concurrent.Chan         as C
import           Control.Concurrent.Lifted       hiding (yield)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch             (finally)
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Attoparsec                 (parseOnly)
import qualified Data.ByteString                 as SB
import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.UTF8            as SB (fromString)
import qualified Data.HashMap.Strict             as H
import           Data.IORef
import           Data.List
import           Data.Machine
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Text.Internal.Builder      (toLazyText)
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as LT
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Read                  as R
import           LogInstances
import           Network
import           Network.Damn
import           Network.Damn.Format.IRC
import qualified Network.IRC.Base                as IRC
import           Prelude
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen    hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen    as P ((<$>))

data User = User
          { userName      :: SB.ByteString
          , userPrivclass :: T.Text
          , userSymbol    :: SB.ByteString
          , userRealName  :: T.Text
          , joinCount     :: Integer
          } deriving Show

data ChatState = ChatState
               { _privclasses :: H.HashMap SB.ByteString (H.HashMap T.Text Integer)
               , _users       :: H.HashMap SB.ByteString (H.HashMap SB.ByteString User)
               }

instance Monoid ChatState where
    mempty = ChatState mempty mempty
    mappend (ChatState a1 a2) (ChatState b1 b2) = ChatState (a1 <> b1) (a2 <> b2)

makeLenses ''ChatState

initClientInstance c n' a' = do
    logHandler <- fmap (\ hndl -> hndl . (dullgreen "!!!" <+>)) getLogHandler

    (handleKey, handle) <- (`allocate` hClose) $ connectTo "chat.deviantart.com" (PortNumber 3900)
    liftIO $ hSetBinaryMode handle True

    upstreamChan <- liftIO C.newChan
    upstreamTid <- fork $ forever $ do
        pkt <- liftIO $ C.readChan upstreamChan
        liftIO $ SB.hPutStr handle $ render pkt
        logMessage $ dullyellow "<<<" <+> prettyDamn pkt

    downstreamChan <- liftIO C.newChan
    downstreamTid <- fork $ damnPackets handle downstreamChan

    loggedInRef <- liftIO $ newIORef False
    joinListRef <- liftIO $ newIORef mempty

    let aE = AuthEnv { clientEnv = c
                     , clientNick = n'
                     , clientAuth = a'
                     , _sendToDamn = C.writeChan upstreamChan
                     , _sendToResponder = C.writeChan downstreamChan
                     , loggedIn = loggedInRef
                     , joinQueue = joinListRef
                     }

    k1 <- register $ do
        killThread downstreamTid
        logHandler $ "stopped reading from dAmn" <+> parens (string (show downstreamTid))

    k2 <- register $ do
        killThread upstreamTid
        logHandler $ "stopped writing to dAmn" <+> parens (string (show upstreamTid))

    respondThread <- async $ (`evalStateT` mempty) $ (`runReaderT` aE) $
        do
            runT_ $ repeatedly (damnMsgs downstreamChan) ~> repeatedly damnRespond
            logMessage $ dullgreen "!!!" <+> "disconnected from dAmn"
        `finally` mapM_ release (handleKey : [k1, k2])

    return (aE, respondThread)
    where
        damnMsgs ch = liftIO (C.readChan ch) >>= yield

damnPackets h chan = do
    b <- liftIO $ B.hGetContents h
    go b
    where
        go (B.break (== 0) -> ( head'
                              , B.tail -> tail'
                              ))
            | B.null head' = return ()
            | m <- parseOnly messageP (B.toStrict head' <> "\0") = do
                liftIO $ C.writeChan chan (Right (left ((,) head') m))
                go tail'
        stripNewlines b | "\n" `B.isSuffixOf` b = stripNewlines (B.init b)
                        | otherwise = b

damnRespond = do
    p <- await
    case p of
        Right (Left (badInp, s)) -> logMessage (dullred "!!!" <+> string s <> ":" <+> string (show badInp))

        Right (Right d) -> do
            logMessage $ dullmagenta ">>>" <+> prettyDamn d
            case H.lookup (messageName d) respondMap of
                Just f -> f d
                Nothing -> sendClient $ noticeMessage $ "Unknown Message: " <> SB.fromString (show d)

        Left otherMsg -> handleSpecial otherMsg
    where
        handleSpecial (WHO w)   = special_WHO w
        handleSpecial (NAMES n) = special_NAMES n

dc2Irc chat
    | "chat:" `SB.isPrefixOf` chat = pure $ "#" <> SB.drop 5 chat
    | "pchat:" `SB.isPrefixOf` chat
    , [_, u1, u2] <- SB.split 58 chat = do
        nick <- asks clientNick
        pure $ "&" <> (if nick == u1 then u2 else u1)

respondMap = [ ("dAmnServer", c_dAmnServer)
             , ("login", c_login)
             , ("join", c_join)
             , ("part", c_part)
             , ("recv", c_recv)
             , ("property", c_property)
             , ("ping", \ _ -> sendServer $ Message "pong" Nothing [] Nothing)
             ]

c_dAmnServer _ = do
    n <- asks clientNick
    a <- asks clientAuth
    sendServer $ Message "login" (Just n) [("pk", T.decodeUtf8 a)] Nothing
    sendClient $ noticeMessage "Successful handshake"

c_login (Message _ _ args _)
    | Just "ok" <- lookup "e" args = do
        l <- asks loggedIn
        liftIO $ writeIORef l True
        jq <- asks joinQueue >>= liftIO . readIORef
        unless (S.null jq) $ forM_ jq joinChannel
        sendClient $ noticeMessage "Logged in successfully."
    | Just x <- lookup "e" args = sendClient $ noticeMessage $ "Login error: " <> T.encodeUtf8 x
    | otherwise = sendClient $ noticeMessage "Unknown login packet."

c_join (Message _ (Just param) args _)
    | Just "ok" <- lookup "e" args = do
        nick <- asks clientNick
        room <- dc2Irc param
        sendClient $ IRC.Message (mkNickName nick) "JOIN" [room]
    | Just err <- lookup "e" args = sendClient $ noticeMessage $
        "Can't join room: " <> T.encodeUtf8 err

c_part (Message _ (Just param) args _)
    | Just "ok" <- lookup "e" args = do
        nick <- asks clientNick
        room <- dc2Irc param
        sendClient $ IRC.Message (mkNickName nick) "PART" [room]
    | Just err <- lookup "e" args = sendClient $ noticeMessage $
        "Can't join room: " <> T.encodeUtf8 err

c_property (Message _ (Just param) args body)
    | Just "topic" <- lookup "p" args
    , Just ts <- lookup "ts" args
    , Just setter <- lookup "by" args = do
        nick <- asks clientNick
        room <- dc2Irc param
        let topicLines = unLines $ maybe "" (bodyWithFormat ircFormat) body
        sendClient $ serverMessage "332" [ nick, room, SB.intercalate " - " topicLines ]
        sendClient $ serverMessage "333" [ nick, room
                                         , IRC.showPrefix $ fromJust $ mkNickNameText setter
                                         , T.encodeUtf8 ts
                                         ]

    | Just "members" <- lookup "p" args = do
        Just pcs <- use (privclasses . at param)
        room <- dc2Irc param
        us' <- (users . at param) <?= userListToMap (mapMaybe (toUser pcs) $ subpacketList body)
        nick <- asks clientNick
        sendClient $ serverMessage "353" [ nick
                                         , "="
                                         , room
                                         , SB.intercalate " " $ map (\ u -> userSymbol u <> userName u) $ H.elems us'
                                         ]
        sendClient $ serverMessage "366" [ nick, room, "End of /NAMES list." ]

    | Just "privclasses" <- lookup "p" args
    , Just mb <- body = privclasses . at param ?= H.fromList (parsePrivclasses (bodyBytes mb))

    | otherwise = return ()
    where
        subpacketList (SubM pkt@(SubMessage _ _ _ body)) = pkt : subpacketList body
        subpacketList _ = []
        toUser pcs (SubMessage _ param args _) = do
            uname <- param
            pcname <- lookup "pc" args
            rn <- lookup "realname" args
            pclevel <- H.lookup pcname pcs
            return $ User uname pcname (pcToSymbol pclevel) rn 1
        userListToMap = foldr (\ u -> H.insertWith incJoinCount (userName u) u) mempty
        incJoinCount (User _ _ _ _ j1) (User a b c d j2) = User a b c d (j1 + j2)
        parsePrivclasses = mapMaybe (readInt . second T.tail . T.breakOn ":") . T.lines . T.decodeLatin1
        readInt (R.decimal -> Right (i, ""), x) = Just (x, i)
        readInt _                               = Nothing

pcToSymbol n | n >= 99 = "~"
             | n >= 75 = "@"
             | n >= 50 = "+"
             | otherwise = ""

pcToMode n | n >= 99 = "q"
           | n >= 75 = "o"
           | n >= 50 = "v"
           | otherwise = ""

bodyLines b = unLines $ bodyWithFormat ircFormat b

c_recv msg@(Message _ (Just param) _ (SubM (SubMessage (Just "msg") _ args (Just body))))
    | Just from <- lookup "from" args = do
        room <- dc2Irc param
        nick <- asks clientNick
        when (T.encodeUtf8 from /= nick) $
            forM_ (bodyLines body) $ \ line ->
                sendClient $ IRC.Message (mkNickNameText from) "PRIVMSG" [room, line]

c_recv msg@(Message _ (Just param) _ (SubM (SubMessage (Just "action") _ args (Just body))))
    | Just from <- lookup "from" args = do
        room <- dc2Irc param
        nick <- asks clientNick
        when (T.encodeUtf8 from /= nick) $ do
            forM_ (bodyLines body) $ \ line ->
                sendClient $ IRC.Message (mkNickNameText from) "PRIVMSG"
                    [room, "\1ACTION " <> line <> "\1"]

c_recv msg@(Message _ (Just param) _
    (SubM (SubMessage (Just "join") (Just uname) args
        (SubM (SubMessage Nothing Nothing subArgs _)))))
    | Just pc <- lookup "pc" subArgs
    , Just rn <- lookup "realname" subArgs = do
        nick <- asks clientNick
        Just pclevel <- preuse (privclasses . ix param . ix pc)
        existingUser <- preuse (users . ix param . ix uname)
        let newUser = User uname pc (pcToSymbol pclevel) rn newJc
            newJc = maybe 1 (succ . joinCount) existingUser
        room <- dc2Irc param
        users . ix param %= H.insert uname newUser
        when (newJc == 1) $ do
            sendClient $ IRC.Message (mkNickName uname) "JOIN" [room]
            when (not $ SB.null $ userSymbol newUser) $
                sendClient $ serverMessage "MODE"
                    [room, "+" <> pcToMode pclevel, uname]
        unless (newJc == 1) $
            sendClient $ channelNotice room $
                uname <> " has joined again, now joined "
                      <> SB.fromString (show newJc)
                      <> " time(s)"

c_recv msg@(Message _ (Just param) _ (SubM (SubMessage (Just "part") (Just uname) args _)))
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

prettyDamn (Message cmd prm args body) =
    appendBody body $ appendArgs args $ bsToDoc cmd <> showParam prm
    where
        showParam Nothing  = empty
        showParam (Just s) = space <> dullyellow (bsToDoc s)
        textToDoc = string . T.unpack
        bsToDoc = textToDoc . T.decodeUtf8
        appendArgs args doc
            | null args = doc
            | otherwise = align (vsep $ doc : map argToDoc args)
        argToDoc (k,a) = bsToDoc k <> "=" <> dullblue (textToDoc a)
        appendBody Nothing doc     = doc
        appendBody (Just body) doc = align (vsep [doc, empty, string $ show $ bodyBytes body])

special_WHO room = do
    list <- preuse (users . ix room)
    channel <- dc2Irc room
    nick <- asks clientNick
    forM_ list $ \ x -> do
        forM_ (H.toList x) $ \ (uname, user) ->
            sendClient $ serverMessage "352" [nick, channel
                                             , uname
                                             , "dAmn"
                                             , "dAmn"
                                             , uname
                                             , "H"
                                             , "0 " <> T.encodeUtf8 (userRealName user)
                                             ]
        sendClient $ serverMessage "315" [nick, channel, "End of /WHO list."]

special_NAMES param = do
    room <- dc2Irc param
    Just us' <- use (users . at param)
    nick <- asks clientNick
    sendClient $ serverMessage "353" [ nick
                                     , "="
                                     , room
                                     , SB.intercalate " " $ map (\ u -> userSymbol u <> userName u) $ H.elems us'
                                     ]
    sendClient $ serverMessage "366" [ nick, room, "End of /NAMES list." ]
