module Crossbow.Parser where

import Control.Exception qualified as E
import Control.Monad (foldM)
import Crossbow.Evaluator
import Crossbow.Types
import Crossbow.Util ((<$$>))
import Data.Either.Extra (fromRight')
import Data.Foldable (foldl1)
import Data.Foldable.Extra (foldrM)
import Data.List ((!!))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.String qualified
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Vector.Fusion.Stream.Monadic (foldl1M')
import GHC.ExecutionStack (Location (functionName))
import Text.ParserCombinators.Parsec
  ( ParseError,
    anyChar,
    between,
    char,
    digit,
    many1,
    noneOf,
    oneOf,
    optionMaybe,
    sepBy,
    sepBy1,
    spaces,
    string,
    try,
  )
import Prelude hiding (optional)

type ParseContext = Builtins

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

nonNewlineSpaces :: P String
nonNewlineSpaces = many (oneOf " \t")

ignoreSpaces :: P a -> P a
ignoreSpaces p = nonNewlineSpaces *> p <* nonNewlineSpaces

ignoreSpacesAndNewlines :: P a -> P a
ignoreSpacesAndNewlines p = spaces *> p <* spaces

clauseDivider :: P Char
clauseDivider = ignoreSpaces (char '|')

clauses :: P [Value]
clauses = value `sepBy1` clauseDivider

program :: P [[Value]]
program = many (clauses <* many (oneOf "\n\r"))

inParens :: P a -> P a
inParens = between (char '(') (char ')')

value :: P Value
value =
  ignoreSpaces $
    firstOf
      [ vComment,
        vIx,
        vBool,
        binding,
        vRange,
        vNumber,
        inParens value,
        vLambdaZeroArgs,
        vLambda,
        vIdentifier,
        vFunction,
        vList,
        vChar,
        vString
      ]
  where
    vComment :: P Value
    vComment = do
      ignoreSpaces (char '#')
      many (noneOf "\n")
      return VNull
    vIdentifier :: P Value
    vIdentifier = do
      char '$'
      n <- many1 digit
      return $ VIdentifier ("$" <> T.pack n)
    vNegativeNumber :: P Value
    vNegativeNumber = do
      char '('
      x <- ignoreSpaces $ char '-' *> vNumber
      char ')'
      return (negate x)
    vPositiveNumber :: P Value
    vPositiveNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf ".0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ (fst . fromRight' . signed double) x
        else return . VInteger $ (fst . fromRight' . signed decimal) x
    vNumber :: P Value
    vNumber = try vNegativeNumber <|> try vPositiveNumber
    vList :: P Value
    vList = VList <$> between (char '[') (char ']') (value `sepBy` ignoreSpaces (char ','))
    vChar :: P Value
    vChar = VChar <$> between (char '\'') (char '\'') anyChar
    vString :: P Value
    vString = VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool :: P Value
    vBool = VBool <$> (string "False" $> False <|> string "True" $> True)
    vRange :: P Value
    vRange = do
      a <- ignoreSpaces vNumber
      ignoreSpaces (string ":")
      b <- ignoreSpaces vNumber
      let VInteger a' = withPrettyError . castToInt $ a
      let VInteger b' = withPrettyError . castToInt $ b
      return $ VList (VInteger <$> [a' .. b'])
    -- An infix binary function bound inside parens
    vFuncBin :: P Value
    vFuncBin =
      do
        char '('
        a1 <- value
        (VFunction (Function name _)) <- vFunc
        a2 <- value
        char ')'
        return (VFunction (Function name [a1, a2]))
    -- A polish notation left-applied func with maybe unbound variables
    vFuncL :: P Value
    vFuncL = do
      VFunction (Function name args) <- vFunc
      args' <- many1 value
      return $ VFunction (Function name (args ++ args'))
    -- Finally, a func with no arguments
    vFunc :: P Value
    vFunc = do
      f <- ignoreSpaces function
      mapBangs <- many (char '!')
      return $ mapWrap (length mapBangs) (VFunction f)
    vFunction :: P Value
    vFunction =
      firstOf
        [ vFuncL,
          vFuncBin,
          vFunc
        ]
    vIx :: P Value
    vIx = do
      char '_'
      ix <- value
      v <- many value
      return $ VFunction (Function "ix" (ix : v))

-- On the fly function with arguments designated $0, $1... within {}
-- If lambda has no argument, we assume it is preceded by $0
vLambda :: P Value
vLambda = do
  cs <- between (char '{') (char '}') clauses
  mapBangs <- many (char '!')
  let numArgs =
        case mapMaybe maxArgIx cs of
          [] -> 0
          ns -> L.maximum ns + 1
  let csWithInitial =
        case numArgs of
          0 -> VIdentifier "$0" : cs
          _ -> cs
  return $ mapWrap (length mapBangs) (VLambda csWithInitial)

-- A zero argument lambda must have at least two clauses otherwise it's just going to be a value
-- TODO: We could just make this what parens always do; but no, they disambiguate
vLambdaZeroArgs :: P Value
vLambdaZeroArgs = do
  cs <- between (char '(') (char ')') clauses
  when (length cs < 2) $ fail "Require 2+ clauses for a zero arg lambda"
  case mapMaybe maxArgIx cs of
    [] -> return $ VLambda cs
    _ -> fail "Zero arg lambda has arguments"

mapWrap :: Int -> Value -> Value
mapWrap 0 f = f
mapWrap n f = mapWrap (n - 1) (VFunction (Function "map" [f]))

name :: P Text
name = T.pack <$> many1 (noneOf "!{}[]()| \t\n,\"\'#`_")

-- Parses arbitrary operator names, which will be looked up later
-- e.g. there's no parse-time guarantee that we're parsing something from builtins
-- A backtick will wrap it in monadic
function :: (P Function)
function = do
  ignoreSpaces $ do
    monadic <- optionMaybe $ char '`'
    fName <- name
    let f = Function fName []
    return $ case monadic of
      Nothing -> f
      Just _ -> Function "monadic" [VFunction f]

binding :: P Value
binding = do
  n <- name
  ignoreSpaces (string "<-")
  v <- value
  return $ VFunction (Function "bind" [VList $ VChar <$> T.unpack n, v])
