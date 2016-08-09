{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ClientState where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.ByteString
import qualified Data.ByteString.UTF8   as SB
import qualified Data.HashMap.Lazy      as H
import           Data.Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding
import           Data.Time
import           Network.Damn           as Damn
import           Network.IRC.Base       as IRC
import qualified System.IO.Streams      as S

data SpecialRequest = WHO ByteString
                    | NAMES ByteString
                    deriving Show

data ClientEnv = ClientEnv
                 { _clientStream :: S.OutputStream IRC.Message
                 , serverHost    :: String
                 , startTime     :: UTCTime
                 }

data AuthEnvPre = AuthEnvPre
                { _nick        :: Maybe ByteString
                , _authtoken   :: Maybe ByteString
                , aepClientEnv :: ClientEnv
                }

data AuthEnv = AuthEnv
             { clientEnv    :: ClientEnv
             , clientNick   :: ByteString
             , clientAuth   :: ByteString
             , _joinQueue    :: Set ByteString
             , _loggedIn     :: Bool
             , _privclasses :: H.HashMap SB.ByteString (H.HashMap T.Text Integer)
             , _users       :: H.HashMap SB.ByteString (H.HashMap SB.ByteString User)
             , _serverStream :: S.OutputStream Damn.Message
             }

data User = User
          { userName      :: SB.ByteString
          , userPrivclass :: T.Text
          , userSymbol    :: SB.ByteString
          , userRealName  :: T.Text
          , joinCount     :: Integer
          } deriving Show

data Event = IRCMessage (Maybe (Either String IRC.Message))
           | DamnMessage (Maybe (Either String Damn.Message))
           deriving Show

makeLenses ''AuthEnvPre
makeLenses ''AuthEnv

class HasClientEnv a where
    getClientEnv :: a -> ClientEnv

instance HasClientEnv ClientEnv where getClientEnv = id
instance HasClientEnv AuthEnv where getClientEnv = clientEnv
instance HasClientEnv AuthEnvPre where getClientEnv = aepClientEnv

sendClient :: (HasClientEnv a, MonadIO m, MonadState a m)
           => IRC.Message -> m ()
sendClient msg = do
    sender <- gets (_clientStream . getClientEnv)
    liftIO $ S.write (Just msg) sender

sendServer :: (MonadIO m, MonadState AuthEnv m) => Damn.Message -> m ()
sendServer msg = do
    sender <- gets _serverStream
    liftIO $ S.write (Just msg) sender

serverMessage :: Command -> [Parameter] -> IRC.Message
serverMessage = IRC.Message (Just (Server "dAmn"))

serverUserMessage :: Command -> [Parameter] -> IRC.Message
serverUserMessage = IRC.Message (mkNickName "dAmn")

noticeMessage :: ByteString -> IRC.Message
noticeMessage = channelNotice "*"

channelNotice :: ByteString -> ByteString -> IRC.Message
channelNotice r x = serverMessage "NOTICE" [r, x]

mkNickName :: ByteString -> Maybe Prefix
mkNickName x = Just $ NickName x (Just x) (Just "dAmn")
mkNickNameText :: Text -> Maybe Prefix
mkNickNameText = mkNickName . encodeUtf8

joinChannel :: (MonadState AuthEnv m, MonadIO m) => ByteString -> m ()
joinChannel room = sendServer $ Damn.Message "join" (Just room) [] Nothing
