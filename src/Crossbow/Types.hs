{-# OPTIONS_GHC -Wno-identities #-}

module Crossbow.Types where

import Data.Char
import Data.Map.Strict qualified as M
import Data.Text qualified as T

data Value = VInteger Integer | VDouble Double | VChar Char | VList [Value] | VFunction Function deriving (Show, Eq)

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

isNumeric :: Value -> Bool
isNumeric (VInteger _) = True
isNumeric (VDouble _) = True
isNumeric _ = False

castToInt :: Value -> Value
castToInt v@(VInteger _) = v
castToInt (VDouble v) = VInteger (round v)
castToInt (VChar v) = VInteger (fromIntegral $ ord v)
castToInt (VList vs) = VList (castToInt <$> vs)
castToInt f@(VFunction _) = f -- TODO: Compose casting with the given f

castToDouble :: Value -> Value
castToDouble v@(VDouble _) = v
castToDouble (VInteger v) = VDouble (fromIntegral v)
castToDouble (VChar v) = VDouble (fromIntegral $ ord v)
castToDouble (VList vs) = VList (castToDouble <$> vs)
castToDouble f@(VFunction _) = f -- TODO: Compose casting with the given f

castToChar :: Value -> Value
castToChar v@(VChar _) = v
castToChar (VInteger v) = VChar (chr (fromIntegral v))
castToChar (VDouble v) = VChar (chr $ round v)
castToChar (VList vs) = VList (castToChar <$> vs)
castToChar f@(VFunction _) = f -- TODO: Compose casting with the given f

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

data Function = Function Operator [Argument] deriving (Show, Eq)

data OpType = OpType Text deriving (Show, Eq)

newtype Valence = Valence Int deriving (Show, Eq)

data OpImpl = HSImpl ([Value] -> Value) | CBImpl Program

data Operator = Operator OpType Valence deriving (Show, Eq)

data Clause = CLValue Value deriving (Show, Eq)

data Program = Program [Clause] deriving (Show, Eq)

class Pretty a where
  pretty :: a -> Text

instance Pretty Value where
  pretty (VInteger a) = show a
  pretty (VDouble a) = show a
  pretty (VChar a) = show a
  pretty (VList a) = "[" <> T.intercalate "," (pretty <$> a) <> "]"
  pretty (VFunction f) = pretty f

instance Pretty Function where
  pretty (Function (Operator opType _) args) =
    "(" <> pretty opType <> " " <> T.intercalate "," (pretty <$> args) <> ")"

instance Pretty Argument where
  pretty Unbound = "_"
  pretty (Bound v) = pretty v

instance Pretty OpType where
  pretty (OpType t) = t

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, HSImpl (\[a, b] -> a <> b))),
      ("max", (Valence 2, HSImpl (\[a, b] -> max a b))),
      ("min", (Valence 2, HSImpl (\[a, b] -> min a b))),
      ("int", (Valence 1, HSImpl (\[a] -> castToInt a))),
      ("double", (Valence 1, HSImpl (\[a] -> castToDouble a))),
      ("char", (Valence 1, HSImpl (\[a] -> castToChar a)))
    ]
