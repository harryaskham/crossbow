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

compile :: Text -> Either ParseError Program
compile = parse program "" . T.unpack

clauseDivider :: P Char
clauseDivider = spaces *> char '|' <* spaces

program :: P Program
program = Program <$> clause `sepBy` clauseDivider

clause :: P Clause
clause = distinct [clConstant, clOperation]

value :: P Value
value = VInt . readOne (signed decimal) . T.pack <$> many1 (oneOf "-0123456789")

operator :: P Operator
operator = char '+' >> return OPAdd

clConstant :: P Clause
clConstant = CLConstant <$> value

clOperation :: P Clause
clOperation = do
  op <- operator
  spaces
  CLOperation op <$> value
