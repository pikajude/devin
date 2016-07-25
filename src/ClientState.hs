{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ClientState where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.ByteString
import qualified Data.ByteString.UTF8   as SB
import           Data.IORef
import           Data.Monoid
import           Data.Set
import           Data.Text              (Text)
import           Data.Text.Encoding
import           Data.Time
import           Network.IRC.Base
import           Text.Damn.Packet

data ClientEnv = ClientEnv
                 { writeChan  :: TChan Message
                 , serverHost :: String
                 , startTime  :: UTCTime
                 }

data AuthEnv = AuthEnv
             { clientEnv  :: ClientEnv
             , clientNick :: ByteString
             , clientAuth :: ByteString
             , damnChan   :: TChan Packet
             , loggedIn   :: IORef Bool
             , joinQueue  :: IORef (Set Text)
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
    chan <- asks (writeChan . getClientEnv)
    liftIO $ atomically $ writeTChan chan msg

sendServer msg = do
    chan <- asks damnChan
    liftIO $ atomically $ writeTChan chan msg

serverMessage :: Command -> [Parameter] -> Message
serverMessage = Message (Just (Server "chat.deviantart.com"))

noticeMessage :: ByteString -> Message
noticeMessage = channelNotice "*"

channelNotice r x = serverMessage "NOTICE" [r, x]

mkNickName x = Just $ NickName x (Just x) (Just "chat.deviantart.com")
mkNickNameText = mkNickName . encodeUtf8

-- joinChannel :: (MonadReader AuthEnv m, MonadIO m) => ByteString -> m ()
joinChannel room = sendServer $ Packet "join" (Just room) [] Nothing
