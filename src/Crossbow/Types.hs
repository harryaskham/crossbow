module Crossbow.Types where

data Value = VInt Int deriving (Show, Eq)

instance Semigroup Value where
  (VInt a) <> (VInt b) = VInt (a + b)

data Operator = OPAdd deriving (Show)

data Clause = CLConstant Value | CLOperation Operator Value deriving (Show)

data Program = Program [Clause] deriving (Show)
