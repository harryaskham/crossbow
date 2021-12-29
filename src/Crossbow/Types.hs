module Crossbow.Types where

import Crossbow.Util
import Data.Char
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (both)
import Text.Show (showsPrec)

data Value
  = VInteger Integer
  | VDouble Double
  | VBool Bool
  | VChar Char
  | VList [Value]
  | VFunction Function
  deriving (Show, Eq)

-- TODO: Required for Integral - what does this break?
instance Enum Value where
  toEnum = error "Use of toEnum on Value"
  fromEnum = error "Use of fromEnum on Value"

instance Semigroup Value where
  (VInteger a) <> (VInteger b) = VInteger (a + b)
  (VDouble a) <> (VDouble b) = VDouble (a + b)
  (VInteger a) <> (VDouble b) = VDouble (fromIntegral a + b)
  (VDouble a) <> (VInteger b) = VDouble (a + fromIntegral b)
  (VChar a) <> (VChar b) = VChar (chr $ ord a + ord b)
  a@(VChar _) <> (VList bs) = VList (a : bs)
  (VList as) <> b@(VChar _) = VList (as ++ [b])
  (VChar a) <> b = let VInteger v = castToInt b in VChar (chr $ ord a + fromIntegral v)
  a <> (VChar b) = let VInteger v = castToInt a in VChar (chr $ fromIntegral v + ord b)
  (VList a) <> (VList b) = VList (a ++ b)
  a <> (VList b) = VList ((a <>) <$> b)
  (VList a) <> b = VList (a <&> (<> b))

instance Num Value where
  (+) = (<>)
  (VInteger a) * (VInteger b) = (VInteger $ a * b)
  (VDouble a) * (VDouble b) = (VDouble $ a * b)
  (VInteger a) * (VDouble b) = (VDouble $ fromIntegral a * b)
  (VDouble a) * (VInteger b) = (VDouble $ a * fromIntegral b)
  (VList _) * (VList []) = VList []
  (VList []) * (VList _) = VList []
  (VList (a : as)) * (VList (b : bs)) = vCons (a * b) (VList as * VList bs)
  a * (VList b) = VList ((a *) <$> b)
  (VList a) * b = VList (a <&> (* b))
  abs (VInteger a) = VInteger (abs a)
  abs (VDouble a) = VDouble (abs a)
  signum a
    | a == VInteger 0 = 0
    | a < VInteger 0 = -1
    | otherwise = 1
  fromInteger a = VInteger a
  negate (VInteger a) = VInteger (negate a)
  negate (VDouble a) = VDouble (negate a)
  negate (VBool True) = VBool False
  negate (VBool False) = VBool True

instance Integral Value where
  quotRem a b =
    let (VInteger a') = castToInt a
        (VInteger b') = castToInt b
     in both VInteger (a' `quotRem` b')
  toInteger a = let (VInteger a') = castToInt a in a'

instance Real Value where
  toRational a = let (VDouble a') = castToDouble a in toRational a'

vCons :: Value -> Value -> Value
vCons a (VList bs) = VList (a : bs)
vCons _ _ = error "Invalid cons"

asText :: Value -> Text
asText (VInteger a) = show a
asText (VDouble a) = show a
asText (VBool a) = show a
asText (VChar a) = T.pack [a]
asText (VList as) = mconcat (asText <$> as)
asText (VFunction _) = error "Can't coerce function to text"

isNumeric :: Value -> Bool
isNumeric (VInteger _) = True
isNumeric (VDouble _) = True
isNumeric _ = False

truthy :: Value -> Bool
truthy (VBool b) = b
truthy (VInteger 0) = False
truthy (VList []) = False
truthy (VDouble 0.0) = False
truthy _ = True

castToInt :: Value -> Value
castToInt v@(VInteger _) = v
castToInt (VDouble v) = VInteger (round v)
castToInt (VChar v) = VInteger (fromIntegral $ digitToInt v)
castToInt v@(VList vs)
  | all (`elem` (VChar <$> ("-0123456789" :: String))) vs =
    VInteger (readOne (TR.signed TR.decimal) (asText v))
  | otherwise = VList (castToInt <$> vs)
castToInt f@(VFunction _) = f -- TODO: Compose casting with the given f
castToInt (VBool b) = VInteger $ (fromIntegral $ fromEnum b)

castToDouble :: Value -> Value
castToDouble v@(VDouble _) = v
castToDouble (VInteger v) = VDouble (fromIntegral v)
castToDouble (VChar v) = VDouble (fromIntegral $ digitToInt v)
castToDouble (VList vs) = VList (castToDouble <$> vs)
castToDouble f@(VFunction _) = f -- TODO: Compose casting with the given f
castToDouble (VBool b) = VInteger $ (fromIntegral $ fromEnum b)

castToChar :: Value -> Value
castToChar v@(VChar _) = v
castToChar (VInteger v) = VChar (intToDigit (fromIntegral v))
castToChar (VDouble v) = VChar (intToDigit $ round v)
castToChar (VList vs) = VList (castToChar <$> vs)
castToChar f@(VFunction _) = f -- TODO: Compose casting with the given f
castToChar (VBool b) = VChar (intToDigit $ fromEnum b)

instance Ord Value where
  (VFunction _) <= _ = error "No Ord for functions"
  _ <= (VFunction _) = error "No Ord for functions"
  (VInteger a) <= (VInteger b) = a <= b
  (VDouble a) <= (VDouble b) = a <= b
  (VChar a) <= (VChar b) = a <= b
  a@(VInteger _) <= b@(VDouble _) = castToDouble a <= b
  a@(VDouble _) <= b@(VInteger _) = a <= castToDouble b
  (VList []) <= (VList []) = True
  (VList (a : as)) <= (VList (b : bs))
    | castToDouble a == castToDouble b = VList as <= VList bs
    | otherwise = a <= b
  a <= (VList bs)
    | isNumeric a = all (castToDouble a <=) bs
    | otherwise = error "Invalid Ord on list"
  (VList as) <= b
    | isNumeric b = all (<= castToDouble b) as
    | otherwise = error "Invalid Ord on list"

data Argument = Unbound | Bound Value deriving (Show, Eq)

data Function = Function (Maybe Text) OpImpl [Argument]

instance Show Function where
  showsPrec i (Function (Just n) _ args) = showsPrec i (n, args)
  showsPrec i (Function Nothing _ args) = showsPrec i ("<function>", args)

instance Eq Function where
  (==) = error "Function equality"

data OpType = OpType Text deriving (Show, Eq)

newtype Valence = Valence Int deriving (Show, Eq)

data OpImpl
  = HSImpl ([Value] -> Value)
  | HSImplIO ([Value] -> IO Value)
  | CBImpl Program

data Operator = Operator OpType Valence deriving (Show, Eq)

data Clause = CLValue Value deriving (Show, Eq)

data Program = Program [Clause] deriving (Show, Eq)

isChar :: Value -> Bool
isChar (VChar _) = True
isChar _ = False

isString :: Value -> Bool
isString (VList as) = all isChar as
isString _ = False

class Pretty a where
  pretty :: a -> Text

instance Pretty Value where
  pretty (VInteger a) = show a
  pretty (VDouble a) = show a
  pretty (VChar a) = show a
  pretty (VBool a) = show a
  pretty s@(VList a)
    | isString s && (not (null a)) = "\"" <> (T.pack $ (\(VChar c) -> c) <$> a) <> "\""
    | otherwise = "[" <> T.intercalate "," (pretty <$> a) <> "]"
  pretty (VFunction f) = pretty f

instance Pretty Function where
  pretty (Function name _ args) =
    let n = fromMaybe "<function>" name
     in "(" <> n <> " " <> T.intercalate "," (pretty <$> args) <> ")"

instance Pretty Argument where
  pretty Unbound = "_"
  pretty (Bound v) = pretty v

instance Pretty OpType where
  pretty (OpType t) = t
