module Crossbow.Types where

import Data.Text qualified as T

data Value = VInteger Integer | VDouble Double | VList [Value] | VFunction Function deriving (Show, Eq)

instance Semigroup Value where
  (VInteger a) <> (VInteger b) = VInteger (a + b)
  (VDouble a) <> (VDouble b) = VDouble (a + b)
  (VInteger a) <> (VDouble b) = VDouble (fromIntegral a + b)
  (VDouble a) <> (VInteger b) = VDouble (a + fromIntegral b)
  (VList a) <> (VList b) = VList (a ++ b)
  a <> (VList b) = VList ((a <>) <$> b)
  (VList a) <> b = VList (a <&> (<> b))

data Argument = Unbound | Bound Value deriving (Show, Eq)

data Function = Function Operator [Argument] deriving (Show, Eq)

data OpType = OPAdd deriving (Show, Eq)

newtype Valence = Valence Int deriving (Show, Eq)

data Operator = Operator OpType Valence deriving (Show, Eq)

data Clause = CLValue Value deriving (Show, Eq)

data Program = Program [Clause] deriving (Show, Eq)

class Pretty a where
  pretty :: a -> Text

instance Pretty Value where
  pretty (VInteger a) = show a
  pretty (VDouble a) = show a
  pretty (VList a) = "[" <> T.intercalate "," (pretty <$> a) <> "]"
  pretty (VFunction f) = pretty f

instance Pretty Function where
  pretty (Function (Operator opType _) args) =
    "(" <> pretty opType <> " " <> T.intercalate "," (pretty <$> args) <> ")"

instance Pretty Argument where
  pretty Unbound = "_"
  pretty (Bound v) = pretty v

instance Pretty OpType where
  pretty OPAdd = "(+)"
