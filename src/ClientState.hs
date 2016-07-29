{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ClientState where

import           Control.Concurrent.Chan
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString
import qualified Data.ByteString.Lazy    as LB
import qualified Data.ByteString.UTF8    as SB
import           Data.IORef
import           Data.Monoid
import           Data.Set
import           Data.Text               (Text)
import           Data.Text.Encoding
import           Data.Time
import           Network.Damn            as Damn
import           Network.IRC.Base        as IRC

data SpecialRequest = WHO ByteString
                    | NAMES ByteString
                    deriving Show

data ClientEnv = ClientEnv
                 { _sendToClient :: IRC.Message -> IO ()
                 , serverHost    :: String
                 , startTime     :: UTCTime
                 }

data AuthEnv = AuthEnv
             { clientEnv        :: ClientEnv
             , clientNick       :: ByteString
             , clientAuth       :: ByteString
             , _sendToDamn      :: Damn.Message -> IO ()
             , _sendToResponder :: Either SpecialRequest (Either (LB.ByteString, String) Damn.Message) -> IO ()
             , loggedIn         :: IORef Bool
             , joinQueue        :: IORef (Set ByteString)
             }

data ClientState = ClientState
                 { _nick      :: Maybe ByteString
                 , _authtoken :: Maybe ByteString
                 }

makeLenses ''ClientState

class HasClientEnv a where
    getClientEnv :: a -> ClientEnv

instance HasClientEnv ClientEnv where getClientEnv = id
instance HasClientEnv AuthEnv where getClientEnv = clientEnv

sendClient msg = do
    sender <- asks (_sendToClient . getClientEnv)
    liftIO $ sender msg

sendServer msg = do
    sender <- asks _sendToDamn
    liftIO $ sender msg

sendDamnResponder msg = do
    sender <- asks _sendToResponder
    liftIO $ sender (Left msg)

serverMessage :: Command -> [Parameter] -> IRC.Message
serverMessage = IRC.Message (Just (Server "dAmn"))

serverUserMessage = IRC.Message (mkNickName "dAmn")

noticeMessage :: ByteString -> IRC.Message
noticeMessage = channelNotice "*"

channelNotice r x = serverMessage "NOTICE" [r, x]

mkNickName x = Just $ NickName x (Just x) (Just "dAmn")
mkNickNameText = mkNickName . encodeUtf8

-- joinChannel :: (MonadReader AuthEnv m, MonadIO m) => ByteString -> m ()
joinChannel room = sendServer $ Damn.Message "join" (Just room) [] Nothing
