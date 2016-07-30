{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Network.Damn.Tablumps where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       hiding (word8)
import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.Monoid
import           Data.Text                        (split)
import           Data.Text.Encoding
import           Data.Text.Internal.Builder       (toLazyText)
import           Data.Text.Lazy                   (toStrict)
import           Debug.Trace
import           HTMLEntities.Decoder

type Lump = (ByteString, [ByteString])
-- data Lump = Lump deriving (Eq, Show)

tablumpP = many $ (Left . bytesToText <$> (C.takeWhile1 (/= '&')))
       <|> lump
       <|> fmap (Left . bytesToText) (C.string "&")

lump = (C.char '&' *>) $
        ary 5 "emote"
    <|> ary 6 "thumb"
    <|> ary 0 "br"
    where ary n s = do
            string s
            C.char '\t'
            fmap (Right . (,) s) $ count n (C.takeWhile (/= '\t') <* C.char '\t')

toLumps x = case parseOnly tablumpP x of
    Right y -> joinLefts y
    Left _  -> [Left $ bytesToText x]

lumpsToTextInline = foldr (\ a b -> lumpToTextWith "" a <> b) ""

lumpsToText = Data.Text.split (== '\n')
            . foldr (\ a b -> lumpToTextWith "\n" a <> b) ""

lumpsToRaw = foldr (\ a b -> lumpSrc a <> b) ""

joinLefts (Left a : Left b : xs) = joinLefts (Left (a <> b) : xs)
joinLefts (x:xs)                 = x : joinLefts xs
joinLefts []                     = []

lumpSrc (Left x)        = encodeUtf8 x
lumpSrc (Right (x, bs)) = "&" <> x <> "\t" <> B.concat (map (<> "\t") bs)

lumpToTextWith _ (Left x)                         = htmlDecode x
lumpToTextWith _ (Right ("emote", [x,_,_,_,_]))   = bytesToText x
lumpToTextWith _ (Right ("thumb", [x,_,_,_,_,_])) = ":thumb" <> bytesToText x <> ":"
lumpToTextWith x (Right ("br", []))               = bytesToText x

bytesToText = htmlDecode . decodeLatin1
htmlDecode = toStrict . toLazyText . htmlEncodedText
