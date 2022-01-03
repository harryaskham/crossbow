module Crossbow.Types where

import Crossbow.Util
import Data.Char
import Data.Either.Extra (fromRight')
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (both)
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec (GenParser)
import Text.Show (showsPrec)

type P = GenParser Char ()

type ProgramParser = P [Value]

type Builtins = Map Text (Valence, OpImpl)

data ProgramContext = ProgramContext
  { _programParser :: ProgramParser,
    _builtins :: Builtins
  }

type CrossbowEval = State ProgramContext

data CrossbowError
  = TooManyArgumentsError Text Int Int
  | UncaughtParseError ParseError
  | EvalError Text
  | CastToIntError Value
  | CastToDoubleError Value
  | CastToCharError Value
  | CastToBoolError Value
  | InternalError Text
  | EmptyProgramError
  | NonLambdaCompilationError Value
  | ApplyError [CrossbowError]

-- TODO: Remove this when making entire REPL error-safe
withPrettyError :: Either CrossbowError a -> a
withPrettyError = \case
  Left e -> error (pretty e)
  Right v -> v

instance Pretty CrossbowError where
  pretty (TooManyArgumentsError o v numArgs) = o <> " expected " <> show v <> " args, got " <> show numArgs
  pretty (UncaughtParseError e) = "Parsing error: " <> show e
  pretty (EvalError e) = "Evaluation error: " <> show e
  pretty (CastToIntError v) = "Cannot cast to Integer: " <> show v
  pretty (CastToDoubleError v) = "Cannot cast to Double: " <> show v
  pretty (CastToCharError v) = "Cannot cast to Char: " <> show v
  pretty (CastToBoolError v) = "Cannot cast to Bool: " <> show v
  pretty (InternalError e) = "Internal error: " <> e
  pretty (NonLambdaCompilationError v) = "Attempting to compile non-lambda: " <> show v
  pretty EmptyProgramError = "No program supplied"

data Value
  = VInteger Integer
  | VDouble Double
  | VBool Bool
  | VChar Char
  | VList [Value]
  | VFunction Function
  | VLambda Valence [Value]
  | VIdentifier Text
  deriving (Show, Eq)

-- TODO: Required for Integral - what does this break?
instance Enum Value where
  toEnum = error "Use of toEnum on Value"
  fromEnum = error "Use of fromEnum on Value"

instance Semigroup Value where
  (VInteger a) <> (VInteger b) = VInteger (a + b)
  (VInteger a) <> (VDouble b) = VDouble (fromIntegral a + b)
  (VDouble a) <> (VDouble b) = VDouble (a + b)
  (VDouble a) <> (VInteger b) = VDouble (a + fromIntegral b)
  (VChar a) <> (VChar b) = VChar (chr $ ord a + ord b)
  a@(VChar _) <> (VList bs) = VList (a : bs)
  (VList as) <> b@(VChar _) = VList (as ++ [b])
  (VChar a) <> b =
    case castToInt b of
      Right (VInteger v) -> VChar (chr $ ord a + fromIntegral v)
      Left e -> error (pretty e)
  a <> (VChar b) =
    case castToInt a of
      Right (VInteger v) -> VChar (chr $ fromIntegral v + ord b)
      Left e -> error (pretty e)
  (VList a) <> (VList b) = VList (getZipList $ (<>) <$> ZipList a <*> ZipList b)
  a <> (VList b) = VList ((a <>) <$> b)
  (VList a) <> b = VList (a <&> (<> b))
  f@(VFunction _) <> _ = error $ "Cannot + unevaluated function: " <> show f
  (VIdentifier i) <> _ = error $ "Cannot + unbound identifier: " <> i

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
  abs (VChar _) = error "Cannot abs a Char"
  abs (VBool _) = error "Cannot abs a Bool"
  abs (VFunction f) = error $ "Cannot abs an unevaluated function: " <> show f
  abs (VIdentifier i) = error $ "Cannot abs an unbound identifier: " <> i
  signum (VInteger a)
    | a == 0 = 0
    | a < 0 = -1
    | otherwise = 1
  signum a = case castToInt a of
    Right a -> signum a
    Left e -> error (pretty e)
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
  quotRem (VInteger a) (VInteger b) = both VInteger (a `quotRem` b)
  quotRem a b = case ( do
                         a' <- castToInt a
                         b' <- castToInt b
                         return $ a' `quotRem` b'
                     ) of
    Right v -> v
    Left e -> error (pretty e)
  toInteger a = case castToInt a of
    Right (VInteger a) -> a
    Left e -> error (pretty e)

instance Real Value where
  toRational a = case castToDouble a of
    Right (VDouble a) -> toRational a
    Left e -> error (pretty e)

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

castToInt :: Value -> Either CrossbowError Value
castToInt v@(VInteger _) = Right v
castToInt (VDouble v) = Right $ VInteger (round v)
castToInt (VChar v) = Right $ VInteger (fromIntegral $ digitToInt v)
castToInt v@(VList vs)
  | all (`elem` (VChar <$> ("-0123456789" :: String))) vs =
    -- Special case a numeric integer string
    case TR.signed TR.decimal (asText v) of
      Right (a, _) -> Right $ VInteger a
      Left _ -> Left $ CastToIntError v
  | otherwise =
    case traverse castToInt vs of
      Left e -> Left e
      Right vs -> Right $ VList vs
castToInt f@(VFunction _) = Left $ CastToIntError f
castToInt (VBool b) = Right $ VInteger (fromIntegral $ fromEnum b)

castToDouble :: Value -> Either CrossbowError Value
castToDouble v@(VDouble _) = Right v
castToDouble (VInteger v) = Right $ VDouble (fromIntegral v)
castToDouble (VChar v) = Right $ VDouble (fromIntegral $ digitToInt v)
castToDouble (VList vs) =
  case traverse castToDouble vs of
    Left e -> Left e
    Right vs -> Right $ VList vs
castToDouble (VBool b) = Right $ VInteger $ (fromIntegral $ fromEnum b)
castToDouble f@(VFunction _) = Left $ CastToDoubleError f
castToDouble i@(VIdentifier _) = Left $ CastToDoubleError i

castToChar :: Value -> Either CrossbowError Value
castToChar v@(VChar _) = Right v
castToChar (VInteger v) = Right $ VChar (intToDigit (fromIntegral v))
castToChar (VDouble v) = Right $ VChar (intToDigit $ round v)
castToChar (VList vs) =
  case traverse castToChar vs of
    Left e -> Left e
    Right vs -> Right $ VList vs
castToChar (VBool b) = Right $ VChar (intToDigit $ fromEnum b)
castToChar f@(VFunction _) = Left $ CastToCharError f
castToChar i@(VIdentifier _) = Left $ CastToCharError i

castToBool :: Value -> Either CrossbowError Value
castToBool v@(VBool _) = Right v
castToBool a@(VInteger _) = Right $ VBool (truthy a)
castToBool a@(VDouble _) = Right $ VBool (truthy a)
castToBool a@(VList _) = Right $ VBool (truthy a)
castToBool v@(VChar _) = Left $ CastToCharError v
castToBool f@(VFunction _) = Left $ CastToCharError f
castToBool i@(VIdentifier _) = Left $ CastToCharError i

instance Ord Value where
  (VFunction _) <= _ = error "No Ord for functions"
  _ <= (VFunction _) = error "No Ord for functions"
  (VInteger a) <= (VInteger b) = a <= b
  (VDouble a) <= (VDouble b) = a <= b
  (VChar a) <= (VChar b) = a <= b
  a@(VInteger _) <= b@(VDouble _) =
    case castToDouble a of
      Left e -> error (pretty e)
      Right a -> a <= b
  a@(VDouble _) <= b@(VInteger _) =
    case castToDouble b of
      Left e -> error (pretty e)
      Right a -> a <= b
  (VList []) <= (VList []) = True
  (VList (a : as)) <= (VList (b : bs)) = case ( do
                                                  a' <- castToDouble a
                                                  b' <- castToDouble b
                                                  return $ if a' == b' then VList as <= VList bs else a <= b
                                              ) of
    Left e -> error (pretty e)
    Right v -> v
  _ <= (VList _) = error "Invalid Ord on list"
  (VList _) <= _ = error "Invalid Ord on list"
  (VBool a) <= (VBool b) = a <= b

data Function = Function Text [Value]

instance Show Function where
  showsPrec i (Function n args) = showsPrec i (n, args)

instance Eq Function where
  (==) = error "Function equality"

newtype Valence = Valence Int deriving (Show, Eq)

type Eval = ReaderT ProgramContext IO

data OpImpl
  = HSImpl ([Value] -> Eval Value)
  | CBImpl Text

data BindDir = BindFromLeft | BindFromRight

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
  pretty (VLambda _ cs) = "<lambda " <> T.intercalate "|" (pretty <$> cs) <> ">"
  pretty (VIdentifier i) = i

instance Pretty Function where
  pretty (Function name args) = "(" <> name <> " [" <> T.intercalate "," (pretty <$> args) <> "])"
