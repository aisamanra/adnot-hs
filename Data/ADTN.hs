{-# LANGUAGE OverloadedStrings #-}

module Data.ADTN where

import           Control.Applicative((<|>))
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (Vector)
import qualified Data.Vector as V


data Value
  = Sum Text Array
  | Product Object
  | List Array
  | Integer Integer
  | Double Double
  | Symbol Text
  | String Text
    deriving (Eq, Show)

type Array = Vector Value
type Object = Map Text Value

decodeValue :: ByteString -> Either String Value
decodeValue = parseOnly pVal
  where pVal :: Parser Value
        pVal = skipSpace *> (pSum <|> pProd <|> pList <|> pLit)
        pSum = Sum <$> (char '(' *> skipSpace *> pIdent)
                   <*> (pValueList <* char ')')
        pProd =  Product . M.fromList
             <$> (char '{' *> pProdBody <* skipSpace <* char '}')
        pProdBody = many' pPair
        pPair = (,) <$> (skipSpace *> pIdent) <*> pVal
        pList = List <$> (char '[' *> pValueList <* skipSpace <* char ']')
        pLit  =  Symbol  <$> pIdent
             <|> String  <$> pString
             <|> Integer <$> decimal
        pValueList = V.fromList <$> many' pVal
        pIdent = T.pack <$> many1' letter_ascii
        pString = T.pack <$> (char '"' *> manyTill pStrChar (char '"'))
        pStrChar =  '\n' <$ string "\\n"
                <|> '\t' <$ string "\\t"
                <|> '\r' <$ string "\\r"
                <|> '\b' <$ string "\\b"
                <|> '\f' <$ string "\\f"
                <|> '\'' <$ string "\\'"
                <|> '\"' <$ string "\\\""
                <|> '\\' <$ string "\\\\"
                <|> anyChar
