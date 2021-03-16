{-# LANGUAGE OverloadedStrings #-}

module Data.Adnot.Emit where

import Control.Monad (sequence)
import Data.Adnot.Type
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import qualified Data.Vector as V

encodeValue :: Value -> ByteString
encodeValue = toLazyByteString . buildValue

buildValue :: Value -> Builder
buildValue (Sum n vs)
  | V.null vs = char7 '(' <> buildString n <> char7 ')'
  | otherwise =
    char7 '(' <> buildString n <> char7 ' ' <> spaceSepArr vs <> char7 ')'
buildValue (Product ps) =
  char7 '{' <> buildPairs ps <> char7 '}'
buildValue (List vs) =
  char7 '[' <> spaceSepArr vs <> char7 ']'
buildValue (Integer i) = integerDec i
buildValue (Double d) = doubleDec d
buildValue (String t) = buildString t

buildString t
  | isValidSymbol t = encodeUtf8Builder t
  | otherwise = char7 '"' <> escape t <> char7 '"'

escape :: T.Text -> Builder
escape = T.foldr go mempty
  where
    go '"' r = byteString "\\\"" <> r
    go '\n' r = byteString "\\n" <> r
    go '\\' r = byteString "\\\\" <> r
    go c r = char7 c <> r

spaceSep :: [Builder] -> Builder
spaceSep = mconcat . intersperse (char7 ' ')

spaceSepArr :: Array -> Builder
spaceSepArr = spaceSep . map buildValue . V.toList

buildPairs :: Product -> Builder
buildPairs ps = spaceSep [go k v | (k, v) <- M.toList ps]
  where
    go k v = buildString k <> char7 ' ' <> buildValue v
