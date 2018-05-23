{-# LANGUAGE OverloadedStrings #-}

module Data.Adnot.Parse (decodeValue) where

import           Control.Applicative((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import           Data.Adnot.Type

decodeValue :: ByteString -> Either String Value
decodeValue = parseOnly pVal
  where pVal = ws *> (pSum <|> pProd <|> pList <|> pLit)
        pSum = Sum <$> (char '(' *> ws *> pIdent)
                   <*> (pValueList <* ws <* char ')')
        pProd =  Product . M.fromList
             <$> (char '{' *> pProdBody <* ws <* char '}')
        pProdBody = many' pPair
        pPair = (,) <$> (ws *> pIdent) <*> pVal
        pList = List <$> (char '[' *> pValueList <* ws <* char ']')
        pLit  =  Symbol  <$> pIdent
             <|> String  <$> pString
             <|> Double  <$> double
             <|> Integer <$> decimal
        pValueList = V.fromList <$> many' pVal
        pIdent = T.pack <$>
                 ((:) <$> (letter_ascii <|> char '_')
                      <*> many' (letter_ascii <|> digit <|> char '_'))
        pString = T.decodeUtf8 . BS.pack <$> (char '"' *> manyTill pStrChar (char '"'))
        pStrChar =  '\n' <$ string "\\n"
                <|> '\t' <$ string "\\t"
                <|> '\r' <$ string "\\r"
                <|> '\b' <$ string "\\b"
                <|> '\f' <$ string "\\f"
                <|> '\'' <$ string "\\'"
                <|> '\"' <$ string "\\\""
                <|> '\\' <$ string "\\\\"
                <|> anyChar
        ws = skipSpace *> ((comment *> ws) <|> return ())
        comment = char '#' *> manyTill anyChar (char '\n')
