{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module Data.Adnot.Type (Value(..), Array, Product) where

import           Control.DeepSeq (NFData(..))
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           Data.Map.Strict (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Vector (Vector)
import           GHC.Exts (IsString(..))

-- | An Adnot value represented as a Haskell value
data Value
  = Sum !Text !Array
  | Product !Product
  | List !Array
  | Integer !Integer
  | Double !Double
  | Symbol !Text
  | String !Text
    deriving (Eq, Show, Read, Typeable, Data)

instance NFData Value where
  rnf (Sum t as) = rnf t `seq` rnf as
  rnf (Product ls) = rnf ls
  rnf (List as) = rnf as
  rnf (Integer i) = rnf i
  rnf (Double d) = rnf d
  rnf (Symbol t) = rnf t
  rnf (String t) = rnf t

instance IsString Value where
  fromString = String . fromString

type Array = Vector Value
type Product = Map Text Value
