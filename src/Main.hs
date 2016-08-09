{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Main where

import           ClientState
import           Control.Applicative
import           Control.Concurrent.Lifted    hiding (yield)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Log            hiding (withFDHandler)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString
import qualified Data.ByteString              as SB
import qualified Data.ByteString.Lazy         as B
import           Data.Monoid
import           Data.Time
import           Irc
import           LogInstances
import           Network
import           Network.HostName
import           Network.IRC                  hiding (message, showMessage)
import           Network.IRC.Base.Extension
import           ParserStreams
import           System.IO
import qualified System.IO.Streams            as S
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

runOneClient :: (MonadBaseControl IO m, MonadLog Doc m, MonadResource m,
                 GetLogHandler Doc m)
             => String -> UTCTime -> (Handle, t, t1) -> m ()
runOneClient host time (clientH, _, _) = do
    liftIO $ hSetBinaryMode clientH True
    logHandler <- getLogHandler

    (i_, o) <- handleStreams clientH message ((<> "\r\n") . showMessage)
        (logHandler . (<+>) (dullgreen "<<<") . prettyIrc)
        (logHandler . (<+>) (dullred ">>>") . prettyIrc)
    i <- makeExplicitEOF i_

    let cEnv = ClientEnv { serverHost = host
                         , startTime = time
                         , _clientStream = o
                         }

    runReaderT (respond i o cEnv) cEnv
    where
        message = Message
            <$> optionMaybe (tokenize prefix)
            <*> command
            <*> many (spaces *> parameter)
            <* crlf
            <?> "message"
        optionMaybe p = option Nothing (Just <$> p)
        tokenize = (<* spaces)

main :: IO ()
main = do
    server <- listenOn (PortNumber 6667)
    host <- getHostName
    time <- getCurrentTime

    withFDHandler defaultBatchingOptions stdout 0.4 80 $ \ logger ->
        (`runLoggingT` logger) $ forever $ do
            trip <- liftIO $ accept server
            fork $ runResourceT $ runOneClient host time trip
    where
        withFDHandler options fd ribbonFrac width' =
          withBatchedHandler options
                             (PP.displayIO fd . PP.renderPretty ribbonFrac width'
                                              . (<> PP.linebreak) . PP.vsep)
