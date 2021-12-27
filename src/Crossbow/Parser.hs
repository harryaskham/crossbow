module Crossbow.Parser where

import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Data.Text.Read (decimal, signed)
import Text.ParserCombinators.Parsec hiding (many)

compile :: Text -> Either ParseError Program
compile = parse parseProgram "" . T.unpack

clauseDivider :: GenParser Char () Char
clauseDivider = spaces *> char '|' <* spaces

parseProgram :: GenParser Char () Program
parseProgram = Program <$> parseClause `sepBy` clauseDivider

parseClause :: GenParser Char () Clause
parseClause = choice (try <$> [parseCLConstant, parseCLOperation])

parseValue :: GenParser Char () Value
parseValue = VInt . readOne (signed decimal) . T.pack <$> many1 (oneOf "-0123456789")

parseOperator :: GenParser Char () Operator
parseOperator = char '+' >> return OPAdd

parseCLConstant :: GenParser Char () Clause
parseCLConstant = CLConstant <$> parseValue

parseCLOperation :: GenParser Char () Clause
parseCLOperation = do
  op <- parseOperator
  spaces
  CLOperation op <$> parseValue
