module Crossbow.Parser where

import Control.Exception qualified as E
import Control.Monad (foldM)
import Crossbow.Evaluator
  ( deepEval,
    getBound,
    identifierIx,
    runClauses,
    substituteArgs,
    unbind,
  )
import Crossbow.Types
  ( Argument (..),
    Builtins,
    CrossbowError (TooManyArgumentsError),
    Function (..),
    OpImpl (HSImplIO),
    OpType (OpType),
    Operator (..),
    P,
    Pretty (pretty),
    ProgramParser,
    Valence (Valence),
    Value (..),
    castToInt,
    withPrettyError,
  )
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

type ValueParser = State ParseContext (P (IO Value))

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

clauseDivider :: P Char
clauseDivider = ignoreSpaces (char '|')

clauses :: State ParseContext ProgramParser
clauses = do
  value' <- value
  return $ value' `sepBy` clauseDivider

program :: State ParseContext ProgramParser
program = clauses

inParens :: P a -> P a
inParens = between (char '(') (char ')')

value :: State ParseContext (P (IO Value))
value =
  firstOf
    <$> sequence
      [ vRange,
        vNumber,
        inParens <$> value,
        vLambda,
        vIdentifier,
        vFunction,
        vList,
        vChar,
        vString,
        vBool
      ]
  where
    vIdentifier :: ValueParser
    vIdentifier = return do
      char '$'
      n <- many1 digit
      return . return $ VIdentifier ("$" <> T.pack n)
    vNegativeNumber :: ValueParser
    vNegativeNumber = do
      vNumber' <- vNumber
      return $ do
        char '('
        x <- ignoreSpaces $ char '-' *> vNumber'
        char ')'
        return (negate <$> x)
    vPositiveNumber :: ValueParser
    vPositiveNumber = return do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf ".0123456789"))
      if "." `T.isInfixOf` x
        then return . return . VDouble $ (fst . fromRight' . signed double) x
        else return . return . VInteger $ (fst . fromRight' . signed decimal) x
    vNumber :: ValueParser
    vNumber = do
      vNegativeNumber' <- vNegativeNumber
      vPositiveNumber' <- vPositiveNumber
      return $ try vNegativeNumber' <|> try vPositiveNumber'
    vList :: ValueParser
    vList = do
      value' <- value
      return do
        vsIO <- between (char '[') (char ']') (value' `sepBy` ignoreSpaces (char ','))
        return $ VList <$> sequence vsIO
    vChar :: ValueParser
    vChar = return $ return . VChar <$> between (char '\'') (char '\'') anyChar
    vString :: ValueParser
    vString = return $ return . VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool :: ValueParser
    vBool = return $ return . VBool <$> (string "False" $> False <|> string "True" $> True)
    vRange :: ValueParser
    vRange = do
      vNumber' <- vNumber
      return do
        aIO <- ignoreSpaces vNumber'
        ignoreSpaces (string ":")
        bIO <- ignoreSpaces vNumber'
        return do
          VInteger a <- withPrettyError . castToInt <$> aIO
          VInteger b <- withPrettyError . castToInt <$> bIO
          return $ VList (VInteger <$> [a .. b])
    arg :: State ParseContext (P (IO Argument))
    arg = do
      value' <- value
      return $ ignoreSpaces ((Bound <$$> value') <|> char '_' $> return Unbound)
    -- An infix binary function bound inside parens
    vFuncBin :: ValueParser
    vFuncBin =
      do
        arg' <- arg
        vFunc' <- vFunc
        return do
          char '('
          a1IO <- arg'
          vfIO <- vFunc'
          a2IO <- arg'
          char ')'
          return do
            a1 <- a1IO
            (VFunction (Function n v i _)) <- vfIO
            a2 <- a2IO
            return (VFunction (Function n v i [a1, a2]))
    -- A polish notation left-applied func with maybe unbound variables
    vFuncL :: ValueParser
    vFuncL = do
      arg' <- arg
      vFunc' <- vFunc
      return do
        vfIO <- vFunc'
        argsIO <- many1 arg'
        return do
          VFunction (Function n valence@(Valence v) i _) <- vfIO
          args <- sequence argsIO
          return (VFunction (Function n valence i (args ++ replicate (v - length args) Unbound)))
    -- Finally, a func with no arguments
    vFunc :: ValueParser
    vFunc = do
      operator' <- operator
      pc <- get
      return do
        (op, mapDepth) <- operator'
        -- NOTE: safe not to update PC here because mkFunc only reads from builtins
        let f = evalState (mkFunc op mapDepth) pc
        return . return $ VFunction f
    vFunction :: ValueParser
    vFunction =
      firstOf
        <$> sequence
          [ vFuncL,
            vFuncBin,
            vFunc
          ]
    -- On the fly function with arguments designated $0, $1... within {}
    -- If lambda has no argument, we assume it is preceded by $0
    vLambda :: ValueParser
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

maxArgIx :: Value -> Maybe Int
maxArgIx i@(VIdentifier _) = Just $ identifierIx i
maxArgIx (VFunction f@Function {}) =
  case mapMaybe maxArgIx $ unbind <$> getBound f of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx _ = Nothing

mapWrap :: Int -> Function -> State ParseContext Function
mapWrap 0 f = return f
mapWrap n f = do
  builtins <- get
  let (valence, mapImpl) = builtins M.! "map"
  mapWrap (n - 1) (Function (Just "map") valence mapImpl [Bound (VFunction f), Unbound])

mkFunc :: Operator -> Int -> State ParseContext Function
mkFunc (Operator (OpType t) (Valence v)) mapDepth = do
  builtins <- get
  let (valence, impl) = builtins M.! t
  mapWrap mapDepth $ Function (Just t) valence impl (replicate v Unbound)

operator :: State ParseContext (P (Operator, Int))
operator = do
  builtins <- get
  return $
    ignoreSpaces $ do
      -- We parse in favour of longer names first to avoid max/maximum clashes
      k <- T.pack <$> firstOf (string . T.unpack <$> sortOn (Down . T.length) (M.keys builtins))
      mapBangs <- many (char '!')
      let (v, _) = builtins M.! k
      return $ (Operator (OpType k) v, length mapBangs)
