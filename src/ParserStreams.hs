{-# LANGUAGE OverloadedStrings #-}

module ParserStreams where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString
import           Data.ByteString
import           System.IO
import qualified System.IO.Streams          as S

handleStreams :: MonadIO m
              => Handle
              -> Parser pkt
              -> (pkt -> ByteString)
              -> (pkt -> IO ())
              -> (pkt -> IO ())
              -> m (S.InputStream (Either String pkt), S.OutputStream pkt)
handleStreams h parser render logIn logOut = liftIO $ do
    (inp__, out__) <- S.handleToStreams h
    inp_ <- S.atEndOfInput (hClose h) inp__
    out_ <- S.atEndOfOutput (hClose h) out__
    packets <- parseAndPrint inp_
    writer <- S.contramapM_ logOut =<< S.contramap render out_
    return (packets, writer)
    where
        parseAndPrint s = liftIO $ do
            t <- S.fromGenerator $ attoparsecGen s parser
            S.mapM_ (mapM_ logIn) t

makeExplicitEOF :: MonadIO m
                => S.InputStream a -> m (S.InputStream (Maybe a))
makeExplicitEOF i = liftIO $ S.fromGenerator $ forever $ S.yield =<< liftIO (S.read i)

attoparsecGen :: S.InputStream ByteString
              -> Parser a
              -> S.Generator (Either String a) ()
attoparsecGen s parser = go (parse parser "") where
    go (Done rest thing) = S.yield (Right thing) >> go (parse parser rest)
    go (Partial next) = do
        bytes <- liftIO $ S.read s
        case bytes of
            Nothing -> return ()
            Just b  -> go (next b)
    go (Fail _ _ msg) = S.yield (Left msg)
