module Crossbow.Parser (compile) where

import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Data.Text.Read (decimal, signed)
import Text.ParserCombinators.Parsec hiding (many)

type P = GenParser Char ()

-- Parse one of the things given, backtracking on failure
distinct :: [P a] -> P a
distinct = choice . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

compile :: Text -> Either ParseError Program
compile = parse program "" . T.unpack

clauseDivider :: P Char
clauseDivider = char '|'

program :: P Program
program = Program <$> clause `sepBy` clauseDivider

clause :: P Clause
clause = ignoreSpaces $ distinct [clConstant, clOperation]

value :: P Value
value = VInt . readOne (signed decimal) . T.pack <$> ignoreSpaces (many1 (oneOf "-0123456789"))

operator :: P Operator
operator = ignoreSpaces $ char '+' >> return OPAdd

clConstant :: P Clause
clConstant = CLConstant <$> value

clOperation :: P Clause
clOperation = do
  op <- operator
  CLOperation op <$> value
