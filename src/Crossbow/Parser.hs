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

value :: P (IO Value)
value =
  ignoreSpaces $
    firstOf
      [ vRange,
        vNumber,
        inParens value,
        --vLambda,
        vIdentifier,
        vFunction,
        vList,
        vChar,
        vString,
        vBool
      ]
  where
    vIdentifier :: P (IO Value)
    vIdentifier = do
      char '$'
      n <- many1 digit
      return . return $ VIdentifier ("$" <> T.pack n)
    vNegativeNumber :: P (IO Value)
    vNegativeNumber = do
      char '('
      x <- ignoreSpaces $ char '-' *> vNumber
      char ')'
      return (negate <$> x)
    vPositiveNumber :: P (IO Value)
    vPositiveNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf ".0123456789"))
      if "." `T.isInfixOf` x
        then return . return . VDouble $ (fst . fromRight' . signed double) x
        else return . return . VInteger $ (fst . fromRight' . signed decimal) x
    vNumber :: P (IO Value)
    vNumber = try vNegativeNumber <|> try vPositiveNumber
    vList :: P (IO Value)
    vList = do
      do
        vsIO <- between (char '[') (char ']') (value `sepBy` ignoreSpaces (char ','))
        return $ VList <$> sequence vsIO
    vChar :: P (IO Value)
    vChar = return . VChar <$> between (char '\'') (char '\'') anyChar
    vString :: P (IO Value)
    vString = return . VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool :: P (IO Value)
    vBool = return . VBool <$> (string "False" $> False <|> string "True" $> True)
    vRange :: P (IO Value)
    vRange = do
      aIO <- ignoreSpaces vNumber
      ignoreSpaces (string ":")
      bIO <- ignoreSpaces vNumber
      return do
        VInteger a <- withPrettyError . castToInt <$> aIO
        VInteger b <- withPrettyError . castToInt <$> bIO
        return $ VList (VInteger <$> [a .. b])
    -- An infix binary function bound inside parens
    vFuncBin :: P (IO Value)
    vFuncBin =
      do
        char '('
        a1IO <- value
        vfIO <- vFunc
        a2IO <- value
        char ')'
        return do
          a1 <- a1IO
          (VFunction (Function name _)) <- vfIO
          a2 <- a2IO
          return (VFunction (Function name [a1, a2]))
    -- A polish notation left-applied func with maybe unbound variables
    vFuncL :: P (IO Value)
    vFuncL = do
      vfIO <- vFunc
      spaces
      argsIO <- many1 value
      return do
        VFunction (Function name _) <- vfIO
        args <- sequence argsIO
        return $ VFunction (Function name args)
    -- Finally, a func with no arguments
    vFunc :: P (IO Value)
    vFunc = return . VFunction <$> function
    vFunction :: P (IO Value)
    vFunction =
      firstOf
        [ vFuncL,
          vFuncBin,
          vFunc
        ]

-- On the fly function with arguments designated $0, $1... within {}
-- If lambda has no argument, we assume it is preceded by $0
{- -- TODO: Reenable in the new scheme
vLambda :: P (IO Value)
vLambda = do
  clauses' <- clauses
  return do
    char '{'
    csIO <- clauses'
    char '}'
    -- TODO: Use these
    return do
      numArgs <- do
        cs <- sequence csIO
        case mapMaybe maxArgIx cs of
          [] -> return 0
          ns -> return $ L.maximum ns + 1
      let csIOWithInitial =
            case numArgs of
              0 -> pure (VIdentifier "$0") : csIO
              _ -> csIO
      return
        ( VFunction
            ( Function
                Nothing
                (Valence (max 1 numArgs))
                ( HSImplIO
                    ( \pp args -> do
                        let csIO' = substituteArgs args <$> csIOWithInitial
                        vE <- runClauses pp csIO'
                        -- TODO: Return to here once we enforce that OpImpl returns an Either
                        case vE of
                          Left e -> error (pretty e)
                          Right v -> do
                            deepVE <- deepEval pp v
                            case deepVE of
                              Left e -> error (pretty e)
                              Right deepV -> return deepV
                    )
                )
                (replicate (max 1 numArgs) Unbound)
            )
        )
  -}
{-
maxArgIx :: Value -> Maybe Int
maxArgIx i@(VIdentifier _) = Just $ identifierIx i
maxArgIx (VFunction f@Function {}) =
  case mapMaybe maxArgIx $ unbind <$> getBound f of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx _ = Nothing
-}

mapWrap :: Int -> Function -> Reader ParseContext Function
mapWrap 0 f = return f
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
