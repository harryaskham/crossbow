module Crossbow.Parser (compile) where

import Crossbow.Types
import Crossbow.Util
import Data.Foldable (foldl1)
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import Text.ParserCombinators.Parsec hiding (many, (<|>))

type P = GenParser Char ()

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

compile :: Text -> Either ParseError Program
compile = parse program "" . T.unpack

clauseDivider :: P Char
clauseDivider = char '|'

program :: P Program
program = Program <$> (clause `sepBy` clauseDivider)

clause :: P Clause
clause = ignoreSpaces $ firstOf [clOperator, clValue]

value :: P Value
value = firstOf [vList, vNumber]
  where
    vNumber :: P Value
    vNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf "-.0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ readOne (signed double) x
        else return . VInteger $ readOne (signed decimal) x
    vList = VList <$> between (char '[') (char ']') (value `sepBy1` char ',')

operator :: P Operator
operator = ignoreSpaces $ char '+' >> return OPAdd

clValue :: P Clause
clValue = CLValue <$> value

clOperator :: P Clause
clOperator = CLOperation <$> operator <*> value
