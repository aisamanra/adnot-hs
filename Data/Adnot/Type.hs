{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Adnot.Type (Value (..), Array, Product, isValidSymbol) where

import Control.DeepSeq (NFData (..))
import qualified Data.Char as C
import Data.Data (Data)
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import GHC.Exts (IsString (..))

-- | An Adnot value represented as a Haskell value
data Value
  = Sum !Text !Array
  | Product !Product
  | List !Array
  | Integer !Integer
  | Double !Double
  | String !Text
  deriving (Eq, Show, Read, Typeable, Data)

instance NFData Value where
  rnf (Sum t as) = rnf t `seq` rnf as
  rnf (Product ls) = rnf ls
  rnf (List as) = rnf as
  rnf (Integer i) = rnf i
  rnf (Double d) = rnf d
  rnf (String t) = rnf t

instance IsString Value where
  fromString = String . fromString

type Array = Vector Value

type Product = Map Text Value

isValidSymbol :: Text -> Bool
isValidSymbol t = case T.uncons t of
  Nothing -> False
  Just (x, xs) -> C.isAlpha x && T.all C.isAlphaNum xs
