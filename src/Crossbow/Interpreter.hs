module Crossbow.Interpreter where

import Control.Exception qualified as E
import Control.Monad (foldM)
import Crossbow.Types
import Crossbow.Util
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
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Prelude hiding (optional)

type P = GenParser Char ()

compile :: Text -> IO (Either CrossbowError Value)
compile t = case parse program "" . T.unpack $ t of
  Left e -> return . Left $ UncaughtParseError e
  Right pIO -> pIO

compileUnsafe :: Text -> IO Value
compileUnsafe p = do
  pE <- compile p
  return $ fromRight' pE

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

clauseDivider :: P Char
clauseDivider = ignoreSpaces (char '|')

clauses :: P [IO Value]
clauses = value `sepBy1` clauseDivider

-- TODO: Perhaps need deepEval here to enforce strictness
runClauses :: [IO Value] -> IO (Either CrossbowError Value)
runClauses (cIO : cIOs) = go cIO cIOs
  where
    go :: IO Value -> [IO Value] -> IO (Either CrossbowError Value)
    go vIO [] = Right <$> vIO
    go vIO (cIO : cIOs) = do
      v <- vIO
      c <- cIO
      exE <- E.try (apply v c) :: (IO (Either SomeException Value))
      case exE of
        Left e -> return . Left $ InternalError (show e)
        Right v' -> go (return v') cIOs

program :: P (IO (Either CrossbowError Value))
program =
  runClauses <$> clauses

inParens :: P a -> P a
inParens = between (char '(') (char ')')

-- If a function is fully bound, deeply eval, otherwise id
maybeApply :: Function -> IO Value
maybeApply f
  | null (getUnbound f) = do
    r <- deepEval (VFunction f)
    case r of
      Left e -> fail (T.unpack $ pretty e)
      Right v -> return v
  | otherwise = return $ VFunction f

value :: P (IO Value)
value =
  firstOf
    [ return <$> vRange,
      return <$> vNumber,
      inParens value,
      vLambda,
      return <$> vIdentifier,
      vFunction,
      vList,
      return <$> vChar,
      return <$> vString,
      return <$> vBool
    ]
  where
    vIdentifier = do
      char '$'
      n <- many1 digit
      return $ VIdentifier ("$" <> T.pack n)
    vNegativeNumber = do
      char '('
      x <- ignoreSpaces $ char '-' *> vNumber
      char ')'
      return (negate x)
    vPositiveNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf ".0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ (fst . fromRight' . signed double) x
        else return . VInteger $ (fst . fromRight' . signed decimal) x
    vNumber = try vNegativeNumber <|> try vPositiveNumber
    vList :: P (IO Value)
    vList = do
      vsIO <- between (char '[') (char ']') (value `sepBy` ignoreSpaces (char ','))
      return $ VList <$> sequence vsIO
    vChar :: P Value
    vChar = VChar <$> between (char '\'') (char '\'') anyChar
    vString :: P Value
    vString = VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool = VBool <$> (string "False" $> False <|> string "True" $> True)
    vRange :: P Value
    vRange = do
      VInteger a <- ignoreSpaces (fromRight' . castToInt <$> vNumber)
      ignoreSpaces (string ":")
      VInteger b <- ignoreSpaces (fromRight' . castToInt <$> vNumber)
      return $ VList (VInteger <$> [a .. b])
    arg = ignoreSpaces ((Bound <$$> value) <|> char '_' $> return Unbound)
    -- An infix binary function bound inside parens
    vFuncBin :: P (IO Value)
    vFuncBin =
      do
        char '('
        a1IO <- arg
        op <- operator
        a2IO <- arg
        char ')'
        return do
          a1 <- a1IO
          a2 <- a2IO
          case mkFuncL op [a1, a2] of
            Left e -> fail (T.unpack $ pretty e)
            Right f -> maybeApply f
    -- A polish notation left-applied func with maybe unbound variables
    vFuncL :: P (IO Value)
    vFuncL =
      do
        op <- operator
        argsIO <- many1 arg
        return do
          args <- sequence argsIO
          case mkFuncL op args of
            Left e -> fail (T.unpack $ pretty e)
            Right f -> maybeApply f
    -- Finally, a func with no arguments
    vFunc :: P (IO Value)
    vFunc = do
      op <- operator
      let f = mkFunc op
      return $ maybeApply f
    vFunction :: P (IO Value)
    vFunction =
      firstOf
        [ vFuncL,
          vFuncBin,
          vFunc
        ]
    -- On the fly function with arguments designated $0, $1... within {}
    -- If lambda has no argument, we assume it is preceded by $0
    vLambda :: P (IO Value)
    vLambda = do
      char '{'
      csIO <- clauses
      char '}'
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
        return $
          VFunction
            ( Function
                Nothing
                ( HSImplIO
                    ( \args -> do
                        let csIO' = substituteArgs args <$> csIOWithInitial
                        v <- runClauses csIO'
                        -- If our lambda resulted in a fully bound function
                        -- Or e.g. a list of fully bound things
                        -- We need to evaluate it down here
                        -- TODO: This is the wrong place to be enforcing strictness
                        -- Maybe if the last thing that happens in a lambda is a binding we don't eval?
                        fromRight' <$> deepEval (fromRight' v)
                    )
                )
                (replicate (max 1 numArgs) Unbound)
            )

maxArgIx :: Value -> Maybe Int
maxArgIx i@(VIdentifier _) = Just $ identifierIx i
maxArgIx (VFunction f@Function {}) =
  case mapMaybe maxArgIx $ unbind <$> getBound f of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx _ = Nothing

mkFunc :: Operator -> Function
mkFunc (Operator (OpType t) (Valence v)) =
  let (_, impl) = builtins M.! t
   in Function (Just t) impl (replicate v Unbound)

mkFuncL :: Operator -> [Argument] -> Either CrossbowError Function
mkFuncL (Operator o@(OpType t) (Valence v)) args
  | length args > v = Left $ TooManyArgumentsError o v (length args)
  | length args < v = Right $ Function (Just t) impl (args ++ replicate (v - length args) Unbound)
  | otherwise = Right $ Function (Just t) impl args
  where
    (_, impl) = builtins M.! t

operator :: P Operator
operator = ignoreSpaces $ do
  -- We parse in favour of longer names first to avoid max/maximum clashes
  k <- T.pack <$> firstOf (string . T.unpack <$> sortOn (Down . T.length) (M.keys builtins))
  let (v, _) = builtins M.! k
  return $ Operator (OpType k) v

data BindDir = BindFromLeft | BindFromRight

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> IO Value
-- Two functions compose together, if possible
-- TODO: Disabled to enable map; functions are just objects too...
-- apply (VFunction _) (VFunction _) = error "todo: compose functions"
-- If we have a function in program state, apply to the right
apply (VFunction f) v = do
  r <- applyF f v BindFromLeft
  case r of
    Left e -> error (pretty e)
    Right r -> return r
-- If we have a value in program state and encounter a function, apply it
apply v (VFunction f) = fromRight' <$> applyF f v BindFromRight
-- Application of lists tries to ziplist
apply (VList as@((VFunction _) : _)) (VList bs) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF <$> fs <*> bz <*> pure BindFromLeft
    return . VList . fmap fromRight' . getZipList $ rs
-- Fork values with post-application of applicatives should work the same way
apply (VList bs) (VList as@((VFunction _) : _)) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF <$> fs <*> bz <*> pure BindFromRight
    return . VList . fmap fromRight' . getZipList $ rs

-- If we have a value with a value, just override it
apply _ v = return v

-- Binds the next unbound value to that given
bindNext :: Function -> Value -> BindDir -> Function
bindNext f@(Function t impl args) v bindDir =
  case bindDir of
    BindFromLeft -> Function t impl (reverse . fst $ foldl' bindArg ([], False) args)
    BindFromRight -> Function t impl (fst $ foldr (flip bindArg) ([], False) args)
  where
    bindArg (args, True) a = (a : args, True)
    bindArg (args, False) a@(Bound _) = (a : args, False)
    bindArg (args, False) Unbound = (Bound v : args, True)

getUnbound :: Function -> [Argument]
getUnbound (Function _ _ args) = filter (== Unbound) args

getBound :: Function -> [Argument]
getBound (Function _ _ args) = filter (/= Unbound) args

-- Either partially bind this value, or if it's fully applied, evaluate it down
applyF :: Function -> Value -> BindDir -> IO (Either CrossbowError Value)
applyF f value bindDir
  | length unbound == 1 = deepEval (VFunction (bindNext f value bindDir))
  | length unbound > 1 = return . Right $ VFunction (bindNext f value bindDir)
  | null unbound = return $ Left (EvalError "none unbound for the final binding")
  where
    unbound = getUnbound f

unbind :: Argument -> Value
unbind (Bound v) = v
unbind Unbound = error "Unbinding unbound"

isIdentifier :: Value -> Bool
isIdentifier (VIdentifier _) = True
isIdentifier _ = False

runCBImpl :: Text -> [Value] -> IO (Either CrossbowError Value)
runCBImpl cbF argVals = do
  pE <- compile cbF
  case pE of
    Left e -> return $ Left e
    Right f -> do
      result <- foldM apply f argVals
      return $ Right result

evalF :: Value -> IO (Either CrossbowError Value)
evalF vf@(VFunction f@(Function _ impl args))
  -- If we're not fully bound, this is as far as we can go
  | not (null $ getUnbound f) = return $ Right vf
  -- If we have any bound identifier variables variables we can't eval down any further either
  | any isIdentifier argVals = return $ Right vf
  | otherwise =
    case impl of
      HSImpl hsF -> return . Right $ hsF argVals
      HSImplIO hsF -> Right <$> hsF argVals
      CBImpl cbF -> runCBImpl cbF argVals
  where
    argVals = unbind <$> args
evalF v = return $ Right v

-- Turn e.g. $4 into 4
identifierIx :: Value -> Int
identifierIx (VIdentifier i) = fst . fromRight' $ decimal (T.drop 1 i)

-- Sub any Identifier placeholders with their actual values
substituteArgs :: [Value] -> IO Value -> IO Value
substituteArgs subs vIO = do
  v <- vIO
  case v of
    i@(VIdentifier _) -> return $ subs !! identifierIx i
    (VList as) -> VList <$> sequence (substituteArgs subs . pure <$> as)
    (VFunction (Function n impl args)) -> do
      args' <- traverse (substituteBoundArg subs) args
      return $ VFunction (Function n impl args')
    _ -> return v
  where
    substituteBoundArg _ Unbound = return Unbound
    substituteBoundArg subs (Bound v) = Bound <$> substituteArgs subs (pure v)

deepEval :: Value -> IO (Either CrossbowError Value)
deepEval f@(VFunction _) = evalF f
deepEval (VList as) = do
  as <- traverse deepEval as
  return . fmap VList $ sequence as
deepEval v = return $ Right v

-- Helper to pass through a Haskell function to the builtins
passthrough2 :: Text -> (Value -> Value -> Value) -> (Text, (Valence, OpImpl))
passthrough2 name f = (name, (Valence 2, HSImpl (\[a, b] -> f a b)))

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, HSImpl (\[a, b] -> a + b))),
      ("++", (Valence 2, HSImpl (\[VList a, VList b] -> VList $ a ++ b))),
      ("*", (Valence 2, HSImpl (\[a, b] -> a * b))),
      ("^", (Valence 2, HSImpl (\[a, b] -> a ^ b))),
      ("-", (Valence 2, HSImpl (\[a, b] -> a - b))),
      ("negate", (Valence 1, HSImpl (\[a] -> negate a))),
      ("mod", (Valence 2, HSImpl (\[a, b] -> a `mod` b))),
      ("div", (Valence 2, HSImpl (\[a, b] -> a `div` b))),
      ("==", (Valence 2, HSImpl (\[a, b] -> VBool $ a == b))),
      ("<=", (Valence 2, HSImpl (\[a, b] -> VBool $ a <= b))),
      ("<", (Valence 2, HSImpl (\[a, b] -> VBool $ a < b))),
      (">=", (Valence 2, HSImpl (\[a, b] -> VBool $ a >= b))),
      (">", (Valence 2, HSImpl (\[a, b] -> VBool $ a > b))),
      (":", (Valence 2, HSImpl (\[a, b] -> vCons a b))),
      passthrough2 "max" max,
      passthrough2 "min" min,
      -- TODO: Redefine all the below using crossbow folds, maps, filters
      ("id", (Valence 1, HSImpl (\[a] -> a))),
      ("const", (Valence 2, HSImpl (\[a, _] -> a))),
      ("cons", (Valence 2, HSImpl (\[a, b] -> vCons a b))),
      ("ix", (Valence 2, HSImpl (\[VInteger a, VList b] -> b !! fromInteger a))),
      ("drop", (Valence 2, HSImpl (\[VInteger n, VList as] -> VList (drop (fromIntegral n) as)))),
      ("take", (Valence 2, HSImpl (\[VInteger n, VList as] -> VList (take (fromIntegral n) as)))),
      ("head", (Valence 1, HSImpl (\[VList as] -> L.head as))),
      ("tail", (Valence 1, HSImpl (\[VList as] -> VList $ L.tail as))),
      ("zip", (Valence 2, HSImpl (\[VList as, VList bs] -> VList ((\(a, b) -> VList [a, b]) <$> zip as bs)))),
      ("pairs", (Valence 1, CBImpl "{$0|fork 2|[id, drop 1]|monadic zip}")),
      ("square", (Valence 1, CBImpl "{$0|length|flip fork|$0}")),
      ("enum", (Valence 1, CBImpl "{$0|fork 2|[length,id]|[range 0, id]|monadic zip}")),
      ("lengthy", (Valence 2, CBImpl "{$1|length|($0==_)}")),
      ("windows", (Valence 2, CBImpl "{$1|square|enum|map (monadic drop)|map (take $0)|filter (lengthy $0)}")),
      ( "nap",
        ( Valence 3,
          HSImplIO
            ( \[VInteger n, VFunction f, VList as] -> do
                let vas = V.fromList as
                a' <- fromRight' <$> applyF f (vas V.! fromInteger n) BindFromLeft
                let vas' = vas V.// [(fromInteger n, a')]
                return (VList $ V.toList vas')
            )
        )
      ),
      ("first", (Valence 2, CBImpl "nap 0")),
      ("second", (Valence 2, CBImpl "nap 1")),
      ("third", (Valence 2, CBImpl "nap 2")),
      ("fst", (Valence 1, CBImpl "ix 0")),
      ("snd", (Valence 1, CBImpl "ix 1")),
      ("thd", (Valence 1, CBImpl "ix 2")),
      -- TODO variadic
      ("range", (Valence 2, HSImpl (\[VInteger a, VInteger b] -> VList $ VInteger <$> [a .. b]))),
      ( "map",
        ( Valence 2,
          HSImplIO
            ( let map [_, VList []] = return $ VList []
                  map [VFunction f, VList (x : xs)] = do
                    x' <- applyF f x BindFromLeft
                    vCons (fromRight' x') <$> map [VFunction f, VList xs]
               in map
            )
        )
      ),
      ("count", (Valence 2, CBImpl "{$1|filter $0|length}")),
      ( "filter",
        ( Valence 2,
          HSImplIO
            ( let filter [_, VList []] = return $ VList []
                  filter [VFunction f, VList (x : xs)] = do
                    x' <- fromRight' <$> applyF f x BindFromLeft
                    if truthy x'
                      then vCons x <$> filter [VFunction f, VList xs]
                      else filter [VFunction f, VList xs]
               in filter
            )
        )
      ),
      ( "case",
        ( Valence 2,
          HSImpl
            ( \[a, VList xs] ->
                let Just v = foldl' (\acc (VList [k, v]) -> if k == a then Just v else acc) Nothing xs
                 in v
            )
        )
      ),
      ("if", (Valence 3, HSImpl (\[VBool p, a, b] -> if p then a else b))),
      ("aoc", (Valence 1, CBImpl "{$0|string|(\"test/aoc_input/\"++_)|(_++\".txt\")|read}")),
      ("sum", (Valence 1, CBImpl "foldl|+|0")),
      ("odd", (Valence 1, CBImpl "{$0|mod _ 2|bool}")),
      ("even", (Valence 1, CBImpl "{$0|odd|not}")),
      ("not", (Valence 1, CBImpl "if _ False True")),
      -- TODO:
      -- fold1 using lambda with head, tail
      -- then redefine maximum and minimum in terms of fold1
      ("maximum", (Valence 1, CBImpl "foldl|max|(-1)")),
      ("length", (Valence 1, CBImpl "foldl (flip const (+1) _) 0")),
      ( "foldl",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, acc, VList xs] ->
                foldlM
                  ( \acc x -> do
                      (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                      fromRight' <$> applyF f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      --("foldl1" (Valence 2, CBImpl ( "[f,xs]|[id,fork 2]|[id,[head,tail]]|
      ( "foldr",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, VList xs, acc] ->
                foldrM
                  ( \acc x -> do
                      (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                      fromRight' <$> applyF f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      ( "scanl",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, acc, VList xs] ->
                fmap VList . sequence $
                  scanl'
                    ( \accM x -> do
                        acc <- accM
                        (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                        fromRight' <$> applyF f' x BindFromLeft
                    )
                    (pure acc)
                    xs
            )
        )
      ),
      ( "scanr",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, acc, VList xs] ->
                fmap VList . sequence $
                  scanr
                    ( \x accM -> do
                        acc <- accM
                        (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                        fromRight' <$> applyF f' x BindFromLeft
                    )
                    (pure acc)
                    xs
            )
        )
      ),
      -- TODO: Make flip work with other valences
      ( "flip",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, a, b] ->
                do
                  (VFunction f') <- fromRight' <$> applyF f b BindFromLeft
                  fromRight' <$> applyF f' a BindFromLeft
            )
        )
      ),
      ("reverse", (Valence 1, CBImpl "foldl (flip cons) [] _")),
      ( "ap",
        ( Valence 2,
          HSImplIO
            ( \[VFunction f, a] ->
                fromRight' <$> applyF f a BindFromLeft
            )
        )
      ),
      ("fork", (Valence 2, HSImpl (\[n, a] -> let VInteger n' = fromRight' . castToInt $ n in VList (replicate (fromInteger n') a)))),
      ( "monadic",
        ( Valence 2,
          HSImplIO
            ( \[VFunction f, VList args] ->
                foldlM
                  ( \v a -> do
                      case v of
                        VFunction f ->
                          fromRight' <$> applyF f a BindFromLeft
                        _ -> return v
                  )
                  (VFunction f)
                  args
            )
        )
      ),
      ("lines", (Valence 1, HSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> Data.String.lines (unchar <$> t)))))),
      ("words", (Valence 1, HSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> Data.String.words (unchar <$> t)))))),
      ("ints", (Valence 1, CBImpl "{lines|int}")),
      ("int", (Valence 1, HSImpl (\[a] -> fromRight' . castToInt $ a))),
      ("double", (Valence 1, HSImpl (\[a] -> fromRight' . castToDouble $ a))),
      ("char", (Valence 1, HSImpl (\[a] -> fromRight' . castToChar $ a))),
      ("bool", (Valence 1, HSImpl (\[a] -> fromRight' . castToBool $ a))),
      ("string", (Valence 1, HSImpl (\[a] -> VList $ VChar <$> T.unpack (asText a)))),
      ( "read",
        ( Valence 1,
          HSImplIO
            ( \[a] -> do
                t <- readFile (T.unpack $ asText a)
                return (VList $ VChar <$> t)
            )
        )
      ),
      ( "input",
        ( Valence 0,
          HSImplIO
            ( \_ -> do
                t <- T.unpack <$> getLine
                return (VList $ VChar <$> t)
            )
        )
      )
    ]
