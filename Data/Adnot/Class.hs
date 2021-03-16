{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Data.Adnot.Class where

import           Control.Monad ((>=>))
import           Data.Adnot.Parse
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

-- Integral types
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

-- Rational/Floating types
instance ToAdnot Double where
  toAdnot d = Double d

instance ToAdnot Float where
  toAdnot d = Double (fromRational (toRational d))

-- String types
instance {-# INCOHERENT #-} ToAdnot String where
  toAdnot s = String (T.pack s)

instance ToAdnot T.Text where
  toAdnot s = String s

instance ToAdnot TL.Text where
  toAdnot s = String (TL.toStrict s)

instance ToAdnot Char where
  toAdnot c = String (T.singleton c)

-- List types
instance ToAdnot a => ToAdnot [a] where
  toAdnot ls = List (fmap toAdnot (V.fromList ls))

instance ToAdnot a => ToAdnot (V.Vector a) where
  toAdnot ls = List (fmap toAdnot ls)

instance ToAdnot a => ToAdnot (Seq.Seq a) where
  toAdnot ls = List (V.fromList (F.toList (fmap toAdnot ls)))

instance ToAdnot a => ToAdnot (NE.NonEmpty a) where
  toAdnot ls = List (V.fromList (F.toList (fmap toAdnot ls)))

-- Mapping types
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

-- Common Haskell algebraic data types
instance ToAdnot a => ToAdnot (Maybe a) where
  toAdnot Nothing = Sum "Nothing" []
  toAdnot (Just x) = Sum "Just" [toAdnot x]

instance (ToAdnot a, ToAdnot b) => ToAdnot (Either a b) where
  toAdnot (Left x)  = Sum "Left" [toAdnot x]
  toAdnot (Right y) = Sum "Right" [toAdnot y]

instance ToAdnot Bool where
  toAdnot True  = String "True"
  toAdnot False = String "False"

-- Parsing

decode :: FromAdnot a => BS.ByteString -> Either String a
decode = decodeValue >=> parseAdnot

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

withSumNamed :: String -> T.Text -> (Array -> Parser a) -> Value -> Parser a
withSumNamed n tag k val = case val of
  Sum t as
    | tag == t -> k as
    | otherwise -> Left $ unwords
        [ "Expected tag", T.unpack tag, "in", n, "but found", T.unpack t ]
  _        -> Left ("Expected sum in " ++ n)

withProduct :: String -> (Product -> Parser a) -> Value -> Parser a
withProduct n k val = case val of
  Product ps -> k ps
  _          -> Left ("Expected product in " ++ n)

withList :: String -> (Array -> Parser a) -> Value -> Parser a
withList n k val = case val of
  List ls -> k ls
  _       -> Left ("Expected list in " ++ n)

withInteger :: String -> (Integer -> Parser a) -> Value -> Parser a
withInteger n k val = case val of
  Integer i -> k i
  _         -> Left ("Expected integer in " ++ n)

withDouble :: String -> (Double -> Parser a) -> Value -> Parser a
withDouble n k val = case val of
  Double d  -> k d
  Integer i -> k (fromIntegral i)
  _        -> Left ("Expected double in " ++ n)

withString :: String -> (T.Text -> Parser a) -> Value -> Parser a
withString n k val = case val of
  String s -> k s
  _        -> Left ("Expected string in " ++ n)

(.:) :: FromAdnot a => Product -> T.Text -> Parser a
map .: key = case MS.lookup key map of
  Just x  -> parseAdnot x
  Nothing -> Left ("Missing key " ++ show key)

(.:?) :: FromAdnot a => Product -> T.Text -> Parser (Maybe a)
map .:? key = case MS.lookup key map of
  Just x  -> Just <$> parseAdnot x
  Nothing -> return Nothing

(.!=) :: Parser (Maybe a) -> a -> Parser a
c .!= r = fmap (maybe r id) c

class FromAdnot a where
  parseAdnot :: Value -> Parser a

instance FromAdnot Value where
  parseAdnot = return

-- Integer Types
instance FromAdnot Int where
  parseAdnot = withInteger "Int" (return . fromIntegral)

instance FromAdnot Integer where
  parseAdnot = withInteger "Int" return

instance FromAdnot Int8 where
  parseAdnot = withInteger "Int8" (return . fromIntegral)

instance FromAdnot Int16 where
  parseAdnot = withInteger "Int16" (return . fromIntegral)

instance FromAdnot Int32 where
  parseAdnot = withInteger "Int32" (return . fromIntegral)

instance FromAdnot Int64 where
  parseAdnot = withInteger "Int64" (return . fromIntegral)

instance FromAdnot Word where
  parseAdnot = withInteger "Word" (return . fromIntegral)

instance FromAdnot Word8 where
  parseAdnot = withInteger "Word8" (return . fromIntegral)

instance FromAdnot Word16 where
  parseAdnot = withInteger "Word16" (return . fromIntegral)

instance FromAdnot Word32 where
  parseAdnot = withInteger "Word32" (return . fromIntegral)

instance FromAdnot Word64 where
  parseAdnot = withInteger "Word64" (return . fromIntegral)

-- Rational/Floating types

instance FromAdnot Double where
  parseAdnot = withDouble "Double" return

instance FromAdnot Float where
  parseAdnot =
    withDouble "Float" (return . fromRational . toRational)

-- String types

instance {-# INCOHERENT #-} FromAdnot String where
  parseAdnot = withString "String" (return . T.unpack)

instance FromAdnot T.Text where
  parseAdnot = withString "Text" return

instance FromAdnot TL.Text where
  parseAdnot = withString "Text"  (return . TL.fromStrict)

instance FromAdnot Char where
  parseAdnot = withString "Char" $ \s -> case T.uncons s of
    Just (c, "") -> return c
    _            -> Left "Expected a single-element string"


-- List types
instance FromAdnot a => FromAdnot [a] where
  parseAdnot = withList "List" $ \ls ->
    F.toList <$> mapM parseAdnot ls

instance FromAdnot a => FromAdnot (V.Vector a) where
  parseAdnot = withList "Vector" $ \ls ->
    mapM parseAdnot ls

instance FromAdnot a => FromAdnot (Seq.Seq a) where
  parseAdnot = withList "Seq" $ \ls ->
    Seq.fromList . F.toList <$> mapM parseAdnot ls

instance FromAdnot a => FromAdnot (NE.NonEmpty a) where
  parseAdnot = withList "NonEmpty" $ \ls -> do
    lst <- mapM parseAdnot ls
    case F.toList lst of
      [] -> Left "Expected non-empty sequence"
      (x:xs) -> Right (x NE.:| xs)

-- Mapping types
instance FromAdnot a => FromAdnot (MS.Map T.Text a) where
  parseAdnot = withProduct "Map" $ \ms -> do
    lst <- mapM parseAdnot ms
    return (MS.fromList (F.toList lst))

-- Tuples
instance FromAdnot () where
  parseAdnot = withList "()" $ \ls ->
    case ls of
      [] -> return ()
      _  -> Left "Expected empty list"

instance (FromAdnot a, FromAdnot b) => FromAdnot (a, b) where
  parseAdnot = withList "(a, b)" $ \ls ->
    case ls of
      [a, b] -> (,) <$> parseAdnot a <*> parseAdnot b
      _      -> Left "Expected two-element list"


instance (FromAdnot a, FromAdnot b, FromAdnot c) => FromAdnot (a, b, c) where
  parseAdnot = withList "(a, b, c)" $ \ls ->
    case ls of
      [a, b, c] -> (,,) <$> parseAdnot a <*> parseAdnot b <*> parseAdnot c
      _         -> Left "Expected three-element list"


instance (FromAdnot a, FromAdnot b, FromAdnot c, FromAdnot d)
         => FromAdnot (a, b, c, d) where
  parseAdnot = withList "(a, b, c, d)" $ \ls ->
    case ls of
      [a, b, c, d] -> (,,,) <$> parseAdnot a <*> parseAdnot b
                            <*> parseAdnot c <*> parseAdnot d
      _         -> Left "Expected four-element list"

-- Common Haskell algebraic data types
instance FromAdnot a => FromAdnot (Maybe a) where
  parseAdnot = withSum "Maybe" go
    where go "Nothing" []  = return Nothing
          go "Just"    [x] = Just <$> parseAdnot x
          go _ _ = Left "Invalid Maybe"

instance (FromAdnot a, FromAdnot b) => FromAdnot (Either a b) where
  parseAdnot = withSum "Either" go
    where go "Left"  [x] = Left <$> parseAdnot x
          go "Right" [x] = Right <$> parseAdnot x
          go _ _ = Left "Invalid Either"

instance FromAdnot Bool where
  parseAdnot = withString "Bool" go
    where go "True"  = return True
          go "False" = return False
          go _ = Left "Invalid Bool"
