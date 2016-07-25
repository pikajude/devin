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
import           HTMLEntities.Decoder
import           LogInstances
import           Network
import           Network.IRC.Base                hiding (render)
import           Prelude
import           System.IO
import           Text.Damn.Packet
import           Text.PrettyPrint.ANSI.Leijen    hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen    as P ((<$>))

data User = User
          { userName      :: T.Text
          , userPrivclass :: T.Text
          , userSymbol    :: T.Text
          , userRealName  :: T.Text
          , joinCount     :: Integer
          } deriving Show

data ChatState = ChatState
               { _privclasses :: H.HashMap T.Text (H.HashMap T.Text Integer)
               , _users       :: H.HashMap T.Text (H.HashMap T.Text User)
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
        liftIO $ T.hPutStr handle (render pkt <> "\n\0")
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
            | m <- parse (LT.toStrict $ decodeLatin1 $ stripNewlines head') = do
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
            H.lookupDefault (const stop) (pktCommand d) respondMap d

        Left otherMsg -> handleSpecial otherMsg
    where
        handleSpecial w@WHO{} = special_WHO w

pattern Subpacket pkt <- ((>>= parse') -> Just pkt)

dc2Irc (T.encodeUtf8 -> chat)
    | "chat:" `SB.isPrefixOf` chat = pure $ "#" <> SB.drop 5 chat
    | "pchat:" `SB.isPrefixOf` chat
    , [_, u1, u2] <- SB.split 58 chat = do
        nick <- asks clientNick
        pure $ "&" <> (if nick == u1 then u2 else u1)

respondMap = [ ("dAmnServer", c_dAmnServer)
             , ("login", c_login)
             , ("join", c_join)
             , ("recv", c_recv)
             , ("property", c_property)
             , ("ping", \ _ -> sendServer $ Packet "pong" Nothing [] Nothing)
             ]

c_dAmnServer _ = do
    n <- asks clientNick
    a <- asks clientAuth
    sendServer $ Packet "login" (Just $ T.decodeUtf8 n) [("pk", T.decodeUtf8 a)] Nothing
    sendClient $ noticeMessage "Successful handshake"

c_login (Packet _ _ args _)
    | Just "ok" <- M.lookup "e" args = do
        l <- asks loggedIn
        liftIO $ writeIORef l True
        jq <- asks joinQueue >>= liftIO . readIORef
        unless (S.null jq) $ forM_ jq joinChannel
        sendClient $ noticeMessage "Logged in successfully."
    | Just x <- M.lookup "e" args = sendClient $ noticeMessage $ "Login error: " <> T.encodeUtf8 x
    | otherwise = sendClient $ noticeMessage "Unknown login packet."

c_join (Packet _ (Just param) args _)
    | Just "ok" <- M.lookup "e" args = do
        nick <- asks clientNick
        room <- dc2Irc param
        sendClient $ Message (mkNickName nick) "JOIN" [room]

c_property (Packet _ (Just param) args body)
    | Just "topic" <- M.lookup "p" args
    , Just ts <- M.lookup "ts" args
    , Just setter <- M.lookup "by" args = do
        nick <- asks clientNick
        room <- dc2Irc param
        sendClient $ serverMessage "332" [ nick, room, maybe "" decodeEntities body ]
        sendClient $ serverMessage "333" [ nick, room
                                         , T.encodeUtf8 setter <> "@chat.deviantart.com"
                                         , T.encodeUtf8 ts
                                         ]

    | Just "members" <- M.lookup "p" args = do
        Just pcs <- use (privclasses . at param)
        room <- dc2Irc param
        us' <- (users . at param) <?= userListToMap (mapMaybe (toUser pcs) $ subpacketList body)
        logMessage $ string $ show us'
        nick <- asks clientNick
        sendClient $ serverMessage "353" [ nick
                                         , "="
                                         , room
                                         , T.encodeUtf8 $ T.intercalate " " $ map (\ u -> userSymbol u <> userName u) $ H.elems us'
                                         ]
        sendClient $ serverMessage "366" [ nick, room, "End of /NAMES list." ]

    | Just "privclasses" <- M.lookup "p" args
    , Just body' <- body = privclasses . at param ?= H.fromList (parsePrivclasses body')

    | otherwise = return ()
    where
        subpacketList (Subpacket pkt@(Packet _ _ _ body)) = pkt : subpacketList body
        subpacketList _ = []
        toUser pcs (Packet _ param args _) = do
            uname <- param
            pcname <- M.lookup "pc" args
            rn <- M.lookup "realname" args
            pclevel <- H.lookup pcname pcs
            return $ User uname pcname (pcToSymbol pclevel) rn 1
        userListToMap = foldr (\ u -> H.insertWith incJoinCount (userName u) u) mempty
        incJoinCount (User _ _ _ _ j1) (User a b c d j2) = User a b c d (j1 + j2)
        parsePrivclasses = mapMaybe (readInt . second T.tail . T.breakOn ":") . T.lines
        readInt (R.decimal -> Right (i, ""), x) = Just (x, i)
        readInt _ = Nothing

decodeEntities = T.encodeUtf8 . LT.toStrict . toLazyText . htmlEncodedText

pcToSymbol n | n >= 99 = "~"
             | n >= 75 = "@"
             | n >= 50 = "+"
             | otherwise = ""

c_recv msg@(Packet _ (Just param) _ (Subpacket (Packet "msg" _ args (Just body))))
    | Just from <- M.lookup "from" args = do
        room <- dc2Irc param
        nick <- asks clientNick
        when (T.encodeUtf8 from /= nick) $
            sendClient $ Message (mkNickNameText from) "PRIVMSG" [room, decodeEntities body]

c_recv msg@(Packet _ (Just param) _ (Subpacket (Packet "action" _ args (Just body))))
    | Just from <- M.lookup "from" args = do
        room <- dc2Irc param
        nick <- asks clientNick
        when (T.encodeUtf8 from /= nick) $
            sendClient $ Message (mkNickNameText from) "PRIVMSG" [room, "\1ACTION " <> decodeEntities body <> "\1"]

c_recv msg@(Packet _ (Just param) _ body')
    | Subpacket (Packet "join" (Just uname) args _) <- T.replace "\n\n" "\n" <$> body'
    , Just pc <- M.lookup "pc" args
    , Just rn <- M.lookup "realname" args = do
        Just pclevel <- preuse (privclasses . ix param . ix pc)
        existingUser <- preuse (users . ix param . ix uname)
        let newUser = User uname pc (pcToSymbol pclevel) rn newJc
            newJc = maybe 1 (succ . joinCount) existingUser
        room <- dc2Irc param
        users . ix param %= H.insert uname newUser
        when (newJc == 1) $
            sendClient $ Message (mkNickNameText uname) "JOIN" [room]
        unless (newJc == 1) $
            sendClient $ channelNotice room $
                T.encodeUtf8 uname <> " has joined again, now joined "
                                   <> SB.fromString (show newJc)
                                   <> " time(s)"

c_recv msg@(Packet _ (Just param) _ (Subpacket (Packet "part" (Just uname) args _)))
    | Just reason <- M.lookup "r" args = do
        Just existingUser <- preuse (users . ix param . ix uname)
        let newJc = pred (joinCount existingUser)
        room <- dc2Irc param
        when (newJc == 0) $ do
            sendClient $ Message (mkNickNameText uname) "PART" [room, T.encodeUtf8 reason]
            users . ix param %= H.delete uname
        unless (newJc == 0) $
            sendClient $ channelNotice room $
                T.encodeUtf8 uname <> " has left, now joined "
                                   <> SB.fromString (show newJc)
                                   <> " time(s)"

prettyDamn (Packet cmd prm args body) =
    appendBody body $ appendArgs args $ textToDoc cmd <> showParam prm
    where
        showParam Nothing = empty
        showParam (Just s) = space <> dullyellow (textToDoc s)
        textToDoc = string . T.unpack
        appendArgs args doc
            | M.null args = doc
            | otherwise = align (vsep $ doc : map argToDoc (M.toList args))
        argToDoc (k,a) = textToDoc k <> "=" <> dullblue (textToDoc a)
        appendBody Nothing doc = doc
        appendBody (Just body) doc = align (vsep [doc, empty, textToDoc body])

special_WHO (WHO room) = do
    list <- preuse (users . ix room)
    channel <- dc2Irc room
    nick <- asks clientNick
    forM_ list $ \ x -> do
        forM_ (H.toList x) $ \ (uname, user) ->
            sendClient $ serverMessage "352" [nick, channel
                                             , T.encodeUtf8 uname
                                             , "chat.deviantart.com"
                                             , "chat.deviantart.com"
                                             , T.encodeUtf8 uname
                                             , "H"
                                             , "0 " <> decodeEntities (userRealName user)
                                             ]
        sendClient $ serverMessage "315" [nick, channel, "End of /WHO list."]
