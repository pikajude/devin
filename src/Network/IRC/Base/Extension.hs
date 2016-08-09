{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Base.Extension where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Network.IRC.Base      (Message (..), Parameter, Prefix (..))

showMessage :: Message -> ByteString
showMessage (Message p c ps) = showMaybe p `BS.append` c `BS.append` showParameters ps
  where showMaybe Nothing = BS.empty
        showMaybe (Just prefix) = BS.concat [ B8.pack ":"
                                            , showPrefix prefix
                                            , B8.pack " " ]

bsConsAscii :: Char -> ByteString -> ByteString
bsConsAscii c = BS.cons (fromIntegral . ord $ c)

showPrefix :: Prefix -> ByteString
showPrefix (Server s)       = s
showPrefix (NickName n u h) = BS.concat [n, showMaybe '!' u, showMaybe '@' h]
  where showMaybe c = maybe BS.empty (bsConsAscii c)

showParameters :: [Parameter] -> ByteString
showParameters []     = BS.empty
showParameters params = BS.intercalate (B8.pack " ") (BS.empty : showp params)
  where showp [p]
            | B8.elem ' ' p || BS.take 1 p == ":" = [bsConsAscii ':' p]
            | otherwise = [p]
        showp (p:ps) = p : showp ps
        showp []     = []
