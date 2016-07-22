{-# OPTIONS_GHC -Werror -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module LogInstances where

import Control.Monad.Log
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Machine

instance MonadLog msg m => MonadLog msg (PlanT k o m) where
    logMessage = lift . logMessage

instance MonadLog msg m => MonadLog msg (ReaderT r m) where
    logMessage = lift . logMessage

instance MonadLog msg m => MonadLog msg (StateT r m) where
    logMessage = lift . logMessage

instance MonadLog msg m => MonadLog msg (ResourceT m) where
    logMessage = lift . logMessage

class GetLogHandler message m where
    getLogHandler :: m (Handler IO message)

instance (Monad m, GetLogHandler message m) => GetLogHandler message (ReaderT r m) where
    getLogHandler = lift getLogHandler

instance (Monad m, GetLogHandler message m) => GetLogHandler message (ResourceT m) where
    getLogHandler = lift getLogHandler

instance GetLogHandler message (LoggingT message IO) where
    getLogHandler = LoggingT ask
