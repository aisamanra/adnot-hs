{-# LANGUAGE OverloadedStrings #-}

module Data.Adnot.Emit where

import           Control.Monad (sequence)
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Builder
import           Data.List (intersperse)
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import Data.Adnot.Type

encodeValue :: Value -> ByteString
encodeValue = toLazyByteString . buildValue

buildValue :: Value -> Builder
buildValue (Sum n vs)
  | V.null vs = char7 '(' <> ident n <> char7 ')'
  | otherwise =
    char7 '(' <> ident n <> char7 ' ' <> spaceSepArr vs <> char7 ')'
buildValue (Product ps) =
  char7 '{' <> buildPairs ps <> char7 '}'
buildValue (List vs) =
  char7 '[' <> spaceSepArr vs <> char7 ']'
buildValue (Integer i) = integerDec i
buildValue (Double d) = doubleDec d
buildValue (Symbol t) = ident t
buildValue (String t) =
  char7 '"' <> byteString (encodeUtf8 t) <> char7 '"'

spaceSep :: [Builder] -> Builder
spaceSep = mconcat . intersperse (char7 ' ')

spaceSepArr :: Array -> Builder
spaceSepArr = spaceSep . map buildValue . V.toList

ident :: Text -> Builder
ident = byteString . encodeUtf8

buildPairs :: Product -> Builder
buildPairs ps = spaceSep [ go k v | (k, v) <- M.toList ps ]
  where go k v = ident k <> char7 ' ' <> buildValue v
