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
             , joinQueue  :: IORef (Set ByteString)
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
noticeMessage x = serverMessage "NOTICE" ["*", x]

joinChannel :: (MonadReader AuthEnv m, MonadIO m) => ByteString -> m ()
joinChannel room = case SB.uncons room of
    Just ('#', rmname) -> sendServer $
        Packet "join" (Just $ "chat:" <> decodeUtf8 rmname) [] Nothing
    Just ('&', username) -> sendClient $ noticeMessage "Pchats not implemented yet"
    Just _ -> sendClient $ serverMessage "403" [room, "Invalid channel name"]
    Nothing -> error "how did we get here?"
