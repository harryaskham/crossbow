module Crossbow.Parser (compile) where

import Crossbow.Interpreter (evalF, getUnbound)
import Crossbow.Types
import Crossbow.Util
import Data.Either.Extra (fromRight')
import Data.Foldable (foldl1)
import Data.Map.Strict qualified as M
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
value = firstOf [vFuncL, vFuncR, vFunc, vList, vNumber]
  where
    vNumber :: P Value
    vNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf "-.0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ readOne (signed double) x
        else return . VInteger $ readOne (signed decimal) x
    vList = VList <$> between (char '[') (char ']') (value `sepBy1` char ',')
    -- If this function is already fully bound, reduce it down to a value
    maybeApply f
      | null (getUnbound f) = fromRight' (evalF f)
      | otherwise = VFunction f
    vFuncR = maybeApply . fromRight' <$> (mkFuncR <$> operator <*> many1 value)
    -- Only allow literal partial right application to avoid infinite parsing
    vLiteral = firstOf [vList, vNumber]
    vFuncL = maybeApply . fromRight' <$> (flip mkFuncL <$> many1 vLiteral <*> operator)
    -- Finally, a func with no arguments
    vFunc = maybeApply . mkFunc <$> operator

data ArgNumError = ArgNumError

mkFunc :: Operator -> Function
mkFunc o@(Operator _ (Valence v)) = Function o (replicate v Unbound)

mkFuncL :: Operator -> [Value] -> Either ArgNumError Function
mkFuncL o@(Operator _ (Valence v)) args
  | length args > v = Left ArgNumError
  | otherwise =
    let unbound = replicate (v - length args) Unbound
        bound = Bound <$> args
     in Right $ Function o (bound ++ unbound)

mkFuncR :: Operator -> [Value] -> Either ArgNumError Function
mkFuncR o@(Operator _ (Valence v)) args
  | length args > v = Left ArgNumError
  | otherwise =
    let unbound = replicate (v - length args) Unbound
        bound = Bound <$> args
     in Right $ Function o (unbound ++ bound)

operator :: P Operator
operator = ignoreSpaces $ do
  k <- T.pack <$> firstOf (string . T.unpack <$> M.keys builtins)
  let (v, _) = builtins M.! k
  return $ Operator (OpType k) v

clValue :: P Clause
clValue = CLValue <$> value
