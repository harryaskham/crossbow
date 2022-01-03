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

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

clauseDivider :: P Char
clauseDivider = ignoreSpaces (char '|')

clauses :: ProgramParser
clauses = value `sepBy` clauseDivider

program :: ProgramParser
program = clauses

inParens :: P a -> P a
inParens = between (char '(') (char ')')

value :: P Value
value =
  ignoreSpaces $
    firstOf
      [ vRange,
        vNumber,
        inParens value,
        vLambda,
        vIdentifier,
        vFunction,
        vList,
        vChar,
        vString,
        vBool
      ]
  where
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
      (VFunction (Function name _)) <- vFunc
      spaces
      args <- many1 value
      return (VFunction (Function name args))
    -- Finally, a func with no arguments
    vFunc :: P Value
    vFunc = VFunction <$> function
    vFunction :: P Value
    vFunction =
      firstOf
        [ vFuncL,
          vFuncBin,
          vFunc
        ]

-- On the fly function with arguments designated $0, $1... within {}
-- If lambda has no argument, we assume it is preceded by $0
vLambda :: P Value
vLambda = do
  cs <- between (char '{') (char '}') clauses
  let numArgs =
        case mapMaybe maxArgIx cs of
          [] -> 0
          ns -> L.maximum ns + 1
  let csWithInitial =
        case numArgs of
          0 -> VIdentifier "$0" : cs
          _ -> cs
  return $ VLambda (Valence (max numArgs 1)) csWithInitial

maxArgIx :: Value -> Maybe Int
maxArgIx i@(VIdentifier _) = Just $ identifierIx i
maxArgIx (VFunction (Function _ args)) =
  case mapMaybe maxArgIx args of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx (VList as) =
  case mapMaybe maxArgIx as of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx _ = Nothing

mapWrap :: Int -> Function -> Function
mapWrap 0 f = f
mapWrap n f = mapWrap (n - 1) (Function "map" [VFunction f])

-- Parses arbitrary operator names, which will be looked up later
-- e.g. there's no parse-time guarantee that we're parsing something from builtins
function :: (P Function)
function = do
  let disallowedChars = "!{}[]()|_ \t\n,\"\'"
  ignoreSpaces $ do
    -- We parse in favour of longer names first to avoid max/maximum clashes
    fName <- many1 (noneOf disallowedChars)
    -- mapBangs <- many (char '!')
    return (Function (T.pack fName) [])
