{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Damn (
    -- *** Datatypes
    Message(..),
    SubMessage(..),
    MessageBody,
    -- *** Working with message bodies
    bodyRaw, bodyText, bodyTextInline, subMessage,
    pattern Sub,
    -- *** Parsing
    parseMessage,
    messageP,
    -- *** Rendering
    render, toBody
) where

import           Control.Applicative
import           Control.Arrow                    (left)
import qualified Control.Monad
import           Control.Monad.Fail
import           Data.Attoparsec.ByteString       hiding (word8)
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString
import qualified Data.ByteString                  as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy             as LB (toStrict)
import           Data.Char
import           Data.Ix
import           Data.Monoid
import           Data.Text                        hiding (singleton)
import           Data.Text.Encoding
import           Data.Text.Internal.Builder       (toLazyText)
import           Data.Text.Lazy                   (toStrict)
import           Data.Word
import           HTMLEntities.Decoder
import           Network.Damn.Tablumps
import           Prelude                          hiding (fail)

bytesToTextUtf8 :: ByteString -> Text
bytesToTextUtf8 = toStrict . toLazyText . htmlEncodedText . decodeUtf8

-- | A top-level dAmn message.
--
-- General syntax for a message:
--
-- @
-- name arg
-- attr1=val1
-- attr2=val2
--
-- body
-- @
--
-- As reflected in the field types of 'Message', the 'arg' and 'body' are
-- both optional.
--
-- Attribute values are considered to be textual data and generally consist
-- of part reasons, privclass names, users' "taglines" and so on. The
-- message body can either be treated as text or as a 'SubMessage' (see
-- 'MessageBody').
--
-- In addition, HTML entity decoding is applied to all 'Text' fields of
-- a 'Message', meaning that the following message:
--
-- @
-- user pikajude
-- realname=\&\#9398;\&\#9773;
-- @
--
-- will have the attributes @[("realname","Ⓐ☭")]@.
--
-- The reverse is also true (see 'render').
data Message = Message
             { messageName     :: ByteString
             , messageArgument :: Maybe ByteString
             , messageAttrs    :: [(ByteString, Text)]
             , messageBody     :: Maybe MessageBody
             } deriving (Eq, Show)

-- | A second-level dAmn message. Note that this message can omit the
-- name/argument pair.
data SubMessage = SubMessage
                { subMessageName     :: Maybe ByteString
                , subMessageArgument :: Maybe ByteString
                , subMessageAttrs    :: [(ByteString, Text)]
                , subMessageBody     :: Maybe MessageBody
                } deriving (Eq, Show)

-- | The body of a message, which can be either viewed as Text ('bodyText')
-- or parsed as a 'SubMessage' ('subMessage').
data MessageBody = MessageBody
                 { -- | View the 'Text' of a 'MessageBody'.
                   --
                   -- Some implementation details:
                   --
                   -- * HTML entities will be decoded.
                   -- * Characters outside the ASCII block will be treated
                   -- as ISO-8859-1 (the dAmn standard encoding).
                   _body      :: [Either Text Lump]
                   -- | Try to parse a 'MessageBody' as a 'SubMessage'.
                 , subMessage :: forall m. MonadFail m => m SubMessage
                 }

bodyRaw (MessageBody b _) = lumpsToRaw b
bodyText (MessageBody b _) = lumpsToText b
bodyTextInline (MessageBody b _) = lumpsToTextInline b

instance Show MessageBody where
    show (MessageBody r _) = show r

instance Eq MessageBody where
    MessageBody r _ == MessageBody r1 _ = r == r1

-- | 'subMessage' as a pattern.
--
-- @
-- case messageBody of
--     Sub (SubMessage name args attrs body)) -> ...
--     _ -> error "No parse"
-- @
--
-- Can be nested:
--
-- @
-- isJoinPacket :: Message -> Bool
-- isJoinPacket (Message "recv" room _
--     (Sub (SubMessage (Just "join") (Just uname) _
--         (Sub (SubMessage Nothing Nothing userAttrs _)))))
--     = True
-- isJoinPacket _ = False
-- @
pattern Sub pkt <- ((>>= subMessage) -> Just pkt)

messageP :: Parser Message
messageP = do
    name <- C.takeWhile1 C.isAlpha_iso8859_15
    next <- C.peekChar'
    arg <- if next == ' '
               then C.char ' ' *> (Just <$> C.takeWhile1 (/= '\n'))
               else pure Nothing

    _ <- C.char '\n'
    attrs <- many attr

    body <- parseBody

    return $ Message name arg attrs body

parseBody :: Parser (Maybe MessageBody)
parseBody = do
    next <- C.anyChar
    case next of
        '\n' -> Just . toBodyInternal toLumps
            <$> Data.Attoparsec.ByteString.takeWhile (/= 0) <* A.word8 0
        '\0' -> pure Nothing
        _    -> Control.Monad.fail "Malformed packet"

subMessageP :: Parser SubMessage
subMessageP = do
    firstAttr <- optional attr
    case firstAttr of
        Just a -> do
            otherAttrs <- many attr
            body <- parseBody
            return $ SubMessage Nothing Nothing (a:otherAttrs) body
        Nothing -> do
            Message a b c d <- messageP
            return $ SubMessage (Just a) b c d

attr :: Parser (ByteString, Text)
attr = do
    k <- takeWhile1 nameChars
    C.char '='
    v <- C.takeWhile (/= '\n')
    C.char '\n'
    return (k, bytesToText v)

nameChars :: Word8 -> Bool
nameChars x = inRange (integralOrd 'a', integralOrd 'z') x
           || inRange (integralOrd 'A', integralOrd 'Z') x
           || inRange (integralOrd '0', integralOrd '9') x
    where integralOrd = fromIntegral . ord

toBodyInternal f x = MessageBody (f x)
    (either Control.Monad.fail return $ parseOnly subMessageP (x <> "\0"))

-- | Convert a UTF8-encoded ByteString to a message body.
toBody = toBodyInternal (\ x -> [Left $ bytesToTextUtf8 x])

-- | @'parseOnly' 'messageP'@
parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly messageP

-- | Convert a 'Message' back into a 'ByteString' to send to dAmn. The null
-- byte is appended. In addition, all characters outside the ASCII block
-- are converted to HTML entities, thus
--
-- >>> render (Message "foo" (Just "bar") [("attr1", "☭")] Nothing)
-- "foo bar\nattr1=&#9773;\n\NUL"
render :: Message -> ByteString
render (Message name arg attrs body) = appendArg arg name
    <> "\n"
    <> renderAttrs attrs
    <> renderBody body
    <> "\0"
    where
        appendArg (Just b) = (<> (" " <> b))
        appendArg _        = id
        renderAttrs []         = ""
        renderAttrs ((a,b):bs) = a <> "=" <> textToBytes b <> "\n" <> renderAttrs bs
        renderBody (Just (MessageBody b _)) = "\n" <> B.concat (fmap (textToBytes . fromLeft) b)
        renderBody _                        = ""
        fromLeft (Left t) = t
        fromLeft _ = error "render should never be called on packets with tablumps"
        textToBytes = LB.toStrict . toLazyByteString . Data.Text.foldr (\ c b -> maybeEscape c <> b) ""
        maybeEscape c
            | ord c <= 127 = word8 (fromIntegral $ ord c)
            | otherwise = "&#" <> intDec (ord c) <> ";"
