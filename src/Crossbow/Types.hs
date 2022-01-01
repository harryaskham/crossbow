module Crossbow.Types where

import Crossbow.Util
import Data.Char
import Data.Either.Extra (fromRight')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (both)
import Text.Parsec.Error (ParseError)
import Text.Show (showsPrec)

data CrossbowError
  = TooManyArgumentsError OpType Int Int
  | UncaughtParseError ParseError
  | EvalError Text

instance Pretty CrossbowError where
  pretty (TooManyArgumentsError o v numArgs) = show o <> " expected " <> show v <> " args, got " <> show numArgs
  pretty (UncaughtParseError e) = "Parsing error: " <> show e
  pretty (EvalError e) = "Evaluation error: " <> show e

data Value
  = VInteger Integer
  | VDouble Double
  | VBool Bool
  | VChar Char
  | VList [Value]
  | VFunction Function
  | VIdentifier Text
  deriving (Show, Eq)

-- TODO: Required for Integral - what does this break?
instance Enum Value where
  toEnum = error "Use of toEnum on Value"
  fromEnum = error "Use of fromEnum on Value"

instance Semigroup Value where
  a <> (VList b) = VList ((a <>) <$> b)
  (VList a) <> b = VList (a <&> (<> b))
  (VInteger a) <> (VInteger b) = VInteger (a + b)
  (VInteger a) <> (VDouble b) = VDouble (fromIntegral a + b)
  a@(VInteger _) <> b = a <> castToInt b
  (VDouble a) <> (VDouble b) = VDouble (a + b)
  (VDouble a) <> (VInteger b) = VDouble (a + fromIntegral b)
  a@(VDouble _) <> b = a <> castToDouble b
  (VChar a) <> (VChar b) = VChar (chr $ ord a + ord b)
  a@(VChar _) <> (VList bs) = VList (a : bs)
  a@(VChar _) <> b = a <> castToChar b
  (VList as) <> b@(VChar _) = VList (as ++ [b])
  (VList a) <> (VList b) = VList (getZipList $ (<>) <$> ZipList a <*> ZipList b)
  a@(VBool _) <> b = a <> castToBool b
  (VFunction f) <> _ = error $ "Cannot + unevaluated function: " <> show f
  (VIdentifier i) <> _ = error $ "Cannot + unbound identifier"

instance Num Value where
  (+) = (<>)
  (VInteger a) * (VInteger b) = VInteger $ a * b
  (VInteger a) * (VDouble b) = VDouble $ fromIntegral a * b
  (VInteger _) * (VChar _) = error "Cannot multiply Integer and Char"
  (VInteger _) * (VBool _) = error "Cannot multiply Integer and Bool"
  (VInteger _) * (VFunction f) = error $ "Cannot multiply Integer and unevaluated function: " <> show f
  (VInteger _) * (VIdentifier i) = error $ "Cannot multiply Integer and unbound identifier: " <> i
  (VDouble a) * (VDouble b) = VDouble $ a * b
  (VDouble a) * (VInteger b) = VDouble $ a * fromIntegral b
  (VDouble _) * (VChar _) = error "Cannot multiply Double and Char"
  (VDouble _) * (VBool _) = error "Cannot multiply Double and Bool"
  (VDouble _) * (VFunction f) = error $ "Cannot multiply Double and unevaluated function: " <> show f
  (VDouble _) * (VIdentifier i) = error $ "Cannot multiply Double and unbound identifier: " <> i
  (VList _) * (VList []) = VList []
  (VList []) * (VList _) = VList []
  (VList (a : as)) * (VList (b : bs)) = vCons (a * b) (VList as * VList bs)
  a * (VList b) = VList ((a *) <$> b)
  (VList a) * b = VList (a <&> (* b))
  (VBool _) * _ = error "Cannot multiply Bool"
  (VChar _) * _ = error "Cannot multiply Char"
  (VFunction f) * _ = error $ "Cannot multiply unevaluated function: " <> show f
  (VIdentifier i) * _ = error $ "Cannot multiply an unbound identifier: " <> i
  abs (VInteger a) = VInteger (abs a)
  abs (VDouble a) = VDouble (abs a)
  abs (VList as) = VList (abs <$> as)
  abs (VChar a) = error "Cannot abs a Char"
  abs (VBool a) = error "Cannot abs a Bool"
  abs (VFunction f) = error $ "Cannot abs an unevaluated function: " <> show f
  abs (VIdentifier i) = error $ "Cannot abs an unbound identifier: " <> i
  signum (VInteger a)
    | a == 0 = 0
    | a < 0 = -1
    | otherwise = 1
  signum a = signum (castToInt a)
  fromInteger a = VInteger a
  negate (VInteger a) = VInteger (negate a)
  negate (VDouble a) = VDouble (negate a)
  negate (VBool True) = VBool False
  negate (VBool False) = VBool True
  negate (VList as) = VList (negate <$> as)
  negate (VChar _) = error "Cannot negate a Char"
  negate f@(VFunction _) = error $ "Cannot negate an unevaluated function: " <> show f
  negate (VIdentifier i) = error $ "Cannot negate an unbound identifier: " <> i

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
    VInteger (fst . fromRight' $ (TR.signed TR.decimal) (asText v))
  | otherwise = VList (castToInt <$> vs)
castToInt (VFunction f) = error $ "Cannot cast function to Integer: " <> show f
castToInt (VBool b) = VInteger (fromIntegral $ fromEnum b)

castToDouble :: Value -> Value
castToDouble v@(VDouble _) = v
castToDouble (VInteger v) = VDouble (fromIntegral v)
castToDouble (VChar v) = VDouble (fromIntegral $ digitToInt v)
castToDouble (VList vs) = VList (castToDouble <$> vs)
castToDouble (VBool b) = VInteger $ (fromIntegral $ fromEnum b)
castToDouble f@(VFunction _) = error $ "Cannot cast function to Double: " <> show f
castToDouble (VIdentifier i) = error $ "Cannot cast unbound identifier to Double: " <> i

castToChar :: Value -> Value
castToChar v@(VChar _) = v
castToChar (VInteger v) = VChar (intToDigit (fromIntegral v))
castToChar (VDouble v) = VChar (intToDigit $ round v)
castToChar (VList vs) = VList (castToChar <$> vs)
castToChar f@(VFunction _) = error $ "Cannot cast function to Char: " <> show f
castToChar (VBool b) = VChar (intToDigit $ fromEnum b)
castToChar (VIdentifier i) = error $ "Cannot cast unbound identifier to Char: " <> i

castToBool :: Value -> Value
castToBool v@(VBool _) = v
castToBool a@(VInteger _) = VBool (truthy a)
castToBool a@(VDouble _) = VBool (truthy a)
castToBool a@(VList _) = VBool (truthy a)
castToBool (VChar _) = error "Cannot cast Char to Bool"
castToBool f@(VFunction _) = error $ "Cannot cast unevaluated function to Bool: " <> show f
castToBool (VIdentifier i) = error $ "Cannot cast unbound identifier to Bool: " <> i

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
  | CBImpl Text

data Operator = Operator OpType Valence deriving (Show, Eq)

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
  pretty (VIdentifier i) = i

instance Pretty Function where
  pretty (Function name _ args) =
    let n = fromMaybe "<function>" name
     in "(" <> n <> " " <> T.intercalate "," (pretty <$> args) <> ")"

instance Pretty Argument where
  pretty Unbound = "_"
  pretty (Bound v) = pretty v

instance Pretty OpType where
  pretty (OpType t) = t
