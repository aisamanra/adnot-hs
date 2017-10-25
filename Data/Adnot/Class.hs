{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Adnot.Class where

import           Data.Adnot.Type
import           Data.Adnot.Emit
import           Data.Adnot.Parse
import           Data.Int
import           Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as MS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import           GHC.Generics
import           GHC.TypeLits (KnownSymbol)

encode :: ToAdnot a => a -> BSL.ByteString
encode = encodeValue . toAdnot

class GenToAdnot f where
  genToAdnot :: f p -> Value

instance (GenToAdnot l, GenToAdnot r) => GenToAdnot (l :+: r) where
  genToAdnot (L1 x) = genToAdnot x
  genToAdnot (R1 y) = genToAdnot y

instance ToAdnot x => GenToAdnot (K1 i x) where
  genToAdnot (K1 x) = toAdnot x

instance (GatherSequence f, Constructor name) => GenToAdnot (C1 name f) where
  genToAdnot (M1 x) = Sum (T.pack (conName c)) (V.fromList (gatherSequence x))
    where c :: M1 c name f ()
          c = undefined

instance (GenToAdnot f) => GenToAdnot (S1 name f) where
  genToAdnot (M1 x) = genToAdnot x

instance (GenToAdnot f) => GenToAdnot (D1 name f) where
  genToAdnot (M1 x) = genToAdnot x

instance (GatherSequence l, GatherSequence r) => GenToAdnot (l :*: r) where
  genToAdnot x = List (V.fromList (gatherSequence x))

class GatherRecord f where
  gatherRecord :: f p -> [(T.Text, Value)]

instance (GatherRecord l, GatherRecord r) => GatherRecord (l :*: r) where
  gatherRecord (x :*: y) = gatherRecord x ++ gatherRecord y

instance (GenToAdnot f, Selector name) => GatherRecord (S1 name f) where
  gatherRecord (M1 x) = [(T.pack (selName s), genToAdnot x)]
    where s :: S1 name f ()
          s = undefined

instance GatherRecord U1 where
  gatherRecord U1 = []

class GatherSequence f where
  gatherSequence :: f p -> [Value]

instance GatherSequence U1 where
  gatherSequence U1 = []

instance (GatherSequence l, GatherSequence r) => GatherSequence (l :*: r) where
  gatherSequence (x :*: y) = gatherSequence x ++ gatherSequence y

instance ToAdnot x => GatherSequence (K1 i x) where
  gatherSequence (K1 x) = [toAdnot x]

instance GenToAdnot f => GatherSequence (S1 name f) where
  gatherSequence (M1 x) = [genToAdnot x]

instance GenToAdnot f => GatherSequence (D1 name f) where
  gatherSequence (M1 x) = [genToAdnot x]

instance GenToAdnot U1 where
  genToAdnot U1 = List []

genericToAdnot :: (GenToAdnot (Rep t), Generic t) => t -> Value
genericToAdnot x = genToAdnot (from x)

class ToAdnot a where
  toAdnot :: a -> Value

  default toAdnot :: (Generic a, GenToAdnot (Rep a)) => a -> Value
  toAdnot = genericToAdnot

-- * Integral types
instance ToAdnot Int where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Integer where
  toAdnot n = Integer n

instance ToAdnot Int8 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Int16 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Int32 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Int64 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Word where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Word8 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Word16 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Word32 where
  toAdnot n = Integer (fromIntegral n)

instance ToAdnot Word64 where
  toAdnot n = Integer (fromIntegral n)

-- * Rational/Floating types
instance ToAdnot Double where
  toAdnot d = Double d

instance ToAdnot Float where
  toAdnot d = Double (fromRational (toRational d))

-- * String types
instance {-# INCOHERENT #-} ToAdnot String where
  toAdnot s = String (T.pack s)

instance ToAdnot T.Text where
  toAdnot s = String s

instance ToAdnot TL.Text where
  toAdnot s = String (TL.toStrict s)

instance ToAdnot Char where
  toAdnot c = String (T.singleton c)

-- * List types
instance ToAdnot a => ToAdnot [a] where
  toAdnot ls = List (fmap toAdnot (V.fromList ls))

instance ToAdnot a => ToAdnot (V.Vector a) where
  toAdnot ls = List (fmap toAdnot ls)

instance ToAdnot a => ToAdnot (Seq.Seq a) where
  toAdnot ls = List (V.fromList (F.toList (fmap toAdnot ls)))

instance ToAdnot a => ToAdnot (NE.NonEmpty a) where
  toAdnot ls = List (V.fromList (F.toList (fmap toAdnot ls)))

-- * Mapping types
instance ToAdnot a => ToAdnot (MS.Map T.Text a) where
  toAdnot ls = Product (fmap toAdnot ls)

product :: [(T.Text, Value)] -> Value
product = Product . MS.fromList

(.=) :: ToAdnot t => T.Text -> t -> (T.Text, Value)
key .= val = (key, toAdnot val)

-- * Tuples
instance ToAdnot () where
  toAdnot () = List []

instance (ToAdnot a, ToAdnot b) => ToAdnot (a, b) where
  toAdnot (a, b) = List [toAdnot a, toAdnot b]

instance (ToAdnot a, ToAdnot b, ToAdnot c) => ToAdnot (a, b, c) where
  toAdnot (a, b, c) = List [toAdnot a, toAdnot b, toAdnot c]

instance (ToAdnot a, ToAdnot b, ToAdnot c, ToAdnot d)
         => ToAdnot (a, b, c, d) where
  toAdnot (a, b, c, d) = List [toAdnot a, toAdnot b, toAdnot c, toAdnot d]

-- Some common Haskell algebraic data types
instance ToAdnot a => ToAdnot (Maybe a) where
  toAdnot Nothing = Sum "Nothing" []
  toAdnot (Just x) = Sum "Just" [toAdnot x]

instance (ToAdnot a, ToAdnot b) => ToAdnot (Either a b) where
  toAdnot (Left x)  = Sum "Left" [toAdnot x]
  toAdnot (Right y) = Sum "Right" [toAdnot y]

instance ToAdnot Bool where
  toAdnot True  = String "True"
  toAdnot False = String "False"

-- * Parsing

type ParseError = String
type Parser a = Either ParseError a

niceType :: Value -> String
niceType Sum {}     = "sum"
niceType Product {} = "product"
niceType List {}    = "list"
niceType Integer {} = "integer"
niceType Double {}  = "double"
niceType String {}  = "string"

withSum :: String -> (T.Text -> Array -> Parser a) -> Value -> Parser a
withSum n k val = case val of
  Sum t as -> k t as
  _        -> Left ("Expected sum in " ++ n)

withProd :: String -> (Product -> Parser a) -> Value -> Parser a
withProd n k val = case val of
  Product as -> k as
  _          -> Left ("Expected product in " ++ n)

withList :: String -> (Array -> Parser a) -> Value -> Parser a
withList n k val = case val of
  List as -> k as
  _       -> Left ("Expected list in " ++ n)

withString :: String -> (T.Text -> Parser a) -> Value -> Parser a
withString n k val = case val of
  String t -> case k t of
    Right x -> Right x
    Left msg -> Left (msg ++ " in " ++ n)
  _        -> Left ("Expected string in " ++ n)

decode :: FromAdnot a => BS.ByteString -> Maybe a
decode x = case decodeValue x of
  Left _ -> Nothing
  Right y -> case parseAdnot y of
    Left _ -> Nothing
    Right z -> Just z

decodeEither :: FromAdnot a => BS.ByteString -> Either String a
decodeEither x = do
  y <- decodeValue x
  parseAdnot y

class FromAdnot a where
  parseAdnot :: Value -> Parser a

instance FromAdnot Value where
  parseAdnot v = return v

instance FromAdnot Int where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Integer where
  parseAdnot (Integer n) = return n
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Int8 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Int16 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Int32 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Int64 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Word8 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Word16 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Word32 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

instance FromAdnot Word64 where
  parseAdnot (Integer n) = return (fromIntegral n)
  parseAdnot n = Left ("Expected integer, found " ++ niceType n)

-- Rational/Floating types

instance FromAdnot Double where
  parseAdnot (Double d) = return d
  parseAdnot n = Left ("Expected double, found " ++ niceType n)

instance FromAdnot Float where
  parseAdnot (Double d) = return (fromRational (toRational d))
  parseAdnot n = Left ("Expected double, found " ++ niceType n)

-- String types

instance FromAdnot T.Text where
  parseAdnot (String t) = return t
  parseAdnot n = Left ("Expected string, found " ++ niceType n)

instance FromAdnot TL.Text where
  parseAdnot (String t) = return (TL.fromStrict t)
  parseAdnot n = Left ("Expected string, found " ++ niceType n)

instance {-# INCOHERENT #-} FromAdnot String where
  parseAdnot (String t) = return (T.unpack t)
  parseAdnot n = Left ("Expected string, found " ++ niceType n)

-- sequence types

instance FromAdnot t => FromAdnot [t] where
  parseAdnot (List ts) =
    fmap (V.toList) (traverse parseAdnot ts)
  parseAdnot n = Left ("Expected list, found " ++ niceType n)

instance FromAdnot t => FromAdnot (V.Vector t) where
  parseAdnot (List ts) = traverse parseAdnot ts
  parseAdnot n = Left ("Expected list, found " ++ niceType n)

instance FromAdnot t => FromAdnot (Seq.Seq t) where
  parseAdnot (List ts) =
    fmap (Seq.fromList . V.toList) (traverse parseAdnot ts)
  parseAdnot n = Left ("Expected list, found " ++ niceType n)

instance FromAdnot t => FromAdnot (NE.NonEmpty t) where
  parseAdnot (List ts) =
    fmap (NE.fromList . V.toList) (traverse parseAdnot ts)
  parseAdnot n = Left ("Expected list, found " ++ niceType n)

-- tuples

instance FromAdnot () where
  parseAdnot (List []) = return ()
  parseAdnot n = Left ("Expected list of length 0, found " ++ niceType n)

instance (FromAdnot a, FromAdnot b) => FromAdnot (a, b) where
  parseAdnot (List [a, b]) = (,) <$> parseAdnot a <*> parseAdnot b
  parseAdnot n = Left ("Expected list of length 2, found " ++ niceType n)

instance (FromAdnot a, FromAdnot b, FromAdnot c) => FromAdnot (a, b, c) where
  parseAdnot (List [a, b, c]) =
    (,,) <$> parseAdnot a
         <*> parseAdnot b
         <*> parseAdnot c
  parseAdnot n = Left ("Expected list of length 2, found " ++ niceType n)

instance (FromAdnot a, FromAdnot b, FromAdnot c, FromAdnot d)
         => FromAdnot (a, b, c, d) where
  parseAdnot (List [a, b, c, d]) =
    (,,,) <$> parseAdnot a
          <*> parseAdnot b
          <*> parseAdnot c
          <*> parseAdnot d
  parseAdnot n = Left ("Expected list of length 2, found " ++ niceType n)

instance FromAdnot a => FromAdnot (Maybe a) where
  parseAdnot (Sum "Nothing" []) = return Nothing
  parseAdnot (Sum "Just" [x]) = Just <$> parseAdnot x
  parseAdnot (Sum "Nothing" xs) =
    Left ("Expected 0 arguments to Maybe, but found " ++ show (F.length xs))
  parseAdnot (Sum "Just" xs) =
    Left ("Expected 1 argument to Just, but found " ++ show (F.length xs))
  parseAdnot (Sum t _) =
    Left ("Expected tag \"Nothing\" or \"Just\", but found " ++ show t)
  parseAdnot n =
    Left ("Expected tagged value, but found " ++ niceType n)

instance (FromAdnot a, FromAdnot b) => FromAdnot (Either a b) where
  parseAdnot (Sum "Left" [l]) = Left <$> parseAdnot l
  parseAdnot (Sum "Right" [r]) = Right <$> parseAdnot r
  parseAdnot (Sum "Left" xs) =
    Left ("Expected 1 arguments to Maybe, but found " ++ show (F.length xs))
  parseAdnot (Sum "Right" xs) =
    Left ("Expected 1 argument to Just, but found " ++ show (F.length xs))
  parseAdnot (Sum t _) =
    Left ("Expected tag \"Left\" or \"Right\", but found " ++ show t)
  parseAdnot n =
    Left ("Expected tagged value, but found " ++ niceType n)

instance FromAdnot Bool where
  parseAdnot (String "True") = return True
  parseAdnot (String "False") = return False
  parseAdnot (String t) =
    Left ("Expected \"True\" or \"False\", but found " ++ show t)
  parseAdnot n =
    Left ("Expected string, but found " ++ niceType n)

-- mapping types

instance FromAdnot t => FromAdnot (MS.Map T.Text t) where
  parseAdnot (Product as) =
    traverse parseAdnot as
  parseAdnot n = Left ("Expected product, found " ++ niceType n)


(.:) :: FromAdnot a => Product -> T.Text -> Parser a
p .: name
  | Just x <- MS.lookup name p = parseAdnot x
  | otherwise = Left ("Unable to look up " ++ show name ++ " in product")
