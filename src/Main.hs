{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ViewPatterns              #-}

module Main where

import           ClientState
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
import qualified Data.ByteString              as SB
import qualified Data.ByteString.Lazy         as B
import           Data.Machine
import           Data.Monoid
import           Data.Time
import           Network
import           Network.HostName
import           Network.IRC
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>))

import           Irc
import           LogInstances

-- packets :: MonadIO m => Handle -> MachineT m k (Either (String, B.ByteString) Message)
packets h ch = do
    b <- liftIO $ B.hGetContents h
    go b
    where
        go (B.break (== 10) -> ( head'
                               , B.tail -> tail' -- chop off the '\n'
                               ))
            | B.null head' = return ()
            | Just m <- decode (B.toStrict head') = writeChan ch (Right m) >> go tail'
            | otherwise = writeChan ch (Left ("Unrecognized message", head')) >> go tail'

runOneClient :: ( MonadBaseControl IO m, MonadIO m, MonadLog Doc m, MonadCatch m, MonadResource m
                , GetLogHandler Doc m, MonadMask m)
             => String -> UTCTime -> (Handle, t, t1) -> m ()
runOneClient host time (clientH, _, _) = do
    liftIO $ hSetBinaryMode clientH True
    chan <- liftIO newChan
    let cEnv = ClientEnv { _sendToClient = writeChan chan
                         , serverHost = host
                         , startTime = time
                         }

    clientWriter <- fork $ forever $ do
        m <- liftIO $ readChan chan
        liftIO $ SB.hPut clientH $ encode m <> "\r\n"
        logMessage $ prettyIrc m

    logHandler <- fmap (\ hndl -> hndl . (dullgreen "!!!" <+>)) getLogHandler

    let initState = ClientState Nothing Nothing

    ch <- liftIO newChan
    otherTid <- fork $ packets clientH ch

    register $ do
        killThread clientWriter
        logHandler $ "stopped writing to handle" <+> parens (string (show clientWriter))
    register $ do
        hClose clientH
        logHandler $ "closed client handle" <+> parens (string (show clientH))
    register $ do
        killThread otherTid
        logHandler $ "stopped reading from client" <+> parens (string (show otherTid))

    (`runReaderT` cEnv) $ respond ch

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
