module Crossbow.Parser (compile) where

import Crossbow.Types
import Crossbow.Util
import Data.Either.Extra (fromRight')
import Data.Foldable (foldl1)
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Prelude hiding (optional)

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
clause = clValue

value :: P Value
value = firstOf [vList, vNumber, vFunc]
  where
    vNumber :: P Value
    vNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf "-.0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ readOne (signed double) x
        else return . VInteger $ readOne (signed decimal) x
    vList = VList <$> between (char '[') (char ']') (value `sepBy1` char ',')
    vFunc = VFunction . fromRight' <$> (mkFunc <$> operator <*> many value)

data ArgNumError = ArgNumError

mkFunc :: Operator -> [Value] -> Either ArgNumError Function
mkFunc o@(Operator _ (Valence v)) args
  | length args > v = Left ArgNumError
  | otherwise =
    let unbound = replicate (v - length args) Unbound
        bound = Bound <$> args
     in Right $ Function o (bound ++ unbound)

operator :: P Operator
operator = ignoreSpaces $ char '+' >> return (Operator OPAdd (Valence 2))

clValue :: P Clause
clValue = CLValue <$> value
