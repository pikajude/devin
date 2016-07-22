{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ViewPatterns              #-}

module Damn (damnPackets, damnRespond, prettyDamn) where

import           ClientState
import           Control.Arrow                (left, second)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Log
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString              as SB
import qualified Data.ByteString.Lazy         as B
import qualified Data.HashMap.Strict          as H
import           Data.IORef
import           Data.List
import           Data.Machine
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                     as S
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Text.Internal.Builder   (toLazyText)
import qualified Data.Text.Lazy               as LT
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Read               as R
import           Debug.Trace
import           HTMLEntities.Decoder
import           Network.IRC.Base
import           Prelude
import           Text.Damn.Packet
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P ((<$>))

damnPackets h = construct $ do
    b <- liftIO $ B.hGetContents h
    go b
    where
        go (B.break (== 0) -> ( head'
                              , B.tail -> tail'
                              ))
            | B.null head' = stop
            | m <- parse (LT.toStrict $ decodeUtf8 $ stripNewlines head') =
                yield (left ((,) head') m) >> go tail'
        stripNewlines b | "\n" `B.isSuffixOf` b = stripNewlines (B.init b)
                        | otherwise = b

damnRespond = forever $ do
    p <- await
    case p of
        Left (badInp, s) -> logMessage (dullred "!!!" <+> string s <> ":" <+> string (show badInp))
        Right d -> do
            logMessage $ dullmagenta ">>>" <+> prettyDamn d
            H.lookupDefault (const stop) (pktCommand d) respondMap d

respondMap = [ ("dAmnServer", c_dAmnServer)
             , ("login", c_login)
             , ("join", c_join)
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
        sendClient $ Message (Just (NickName nick Nothing Nothing)) "JOIN" ["#" <> T.encodeUtf8 (T.drop 5 param)]

c_property (Packet _ (Just param) args body)
    | Just "topic" <- M.lookup "p" args
    , Just ts <- M.lookup "ts" args
    , Just setter <- M.lookup "by" args = do
        nick <- asks clientNick
        sendClient $ serverMessage "332" [ nick
                                         , "#" <> T.encodeUtf8 (T.drop 5 param)
                                         , maybe "" decodeEntities body
                                         ]
        sendClient $ serverMessage "333" [ nick
                                         , "#" <> T.encodeUtf8 (T.drop 5 param)
                                         , T.encodeUtf8 setter <> "@chat.deviantart.com"
                                         , T.encodeUtf8 ts
                                         ]

    | Just "members" <- M.lookup "p" args
    , Just body' <- body = do
        Just pcs <- H.lookup param <$> get
        let users = nub $ mapMaybe (toUser pcs) $ subpacketList body'
        nick <- asks clientNick
        logMessage $ string $ show pcs
        sendClient $ serverMessage "353" [ nick
                                         , "="
                                         , "#" <> T.encodeUtf8 (T.drop 5 param)
                                         , SB.intercalate " " users
                                         ]
        sendClient $ serverMessage "366" [ nick
                                         , "#" <> T.encodeUtf8 (T.drop 5 param)
                                         , "End of /NAMES list."
                                         ]

    | Just "privclasses" <- M.lookup "p" args
    , Just body' <- body = modify (H.insert param $ parsePrivclasses body')

    | otherwise = return ()
    where
        decodeEntities = T.encodeUtf8 . LT.toStrict . toLazyText . htmlEncodedText
        subpacketList body = case parse' body of
            Just pkt@(Packet _ _ _ body)
                | Just body' <- body -> pkt : subpacketList body'
                | otherwise -> [pkt]
            Nothing -> []
        toUser pcs (Packet _ param args _) = do
            uname <- T.encodeUtf8 <$> param
            pcname <- M.lookup "pc" args
            pclevel <- lookup pcname pcs
            return $ pcToSymbol pclevel <> uname
        pcToSymbol n | n >= 90 = "~"
                     | n >= 75 = "@"
                     | n >= 50 = "+"
                     | otherwise = ""
        parsePrivclasses = mapMaybe (readInt . second T.tail . T.breakOn ":") . T.lines
        readInt (R.decimal -> Right (i, ""), x) = Just (x, i)
        readInt _ = Nothing

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
