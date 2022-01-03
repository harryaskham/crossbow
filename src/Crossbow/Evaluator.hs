module Crossbow.Evaluator where

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
import Data.String qualified as ST
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec (ParseError, parse, parseTest)

debugParser :: Bool
debugParser = True

parseProgram :: Text -> Eval (Either ParseError [Value])
parseProgram t = do
  programParser <- asks _programParser
  return $ parse programParser "" . T.unpack $ t

debugParseProgram :: Text -> Eval ()
debugParseProgram t = do
  programParser <- asks _programParser
  liftIO do
    print "Running parse test"
    parseTest programParser (T.unpack t)
    print "Parse test complete"

compile :: Text -> Eval (Either CrossbowError (IO Value))
compile t = do
  when debugParser (debugParseProgram t)
  pE <- parseProgram t
  case pE of
    Left e -> return $ Left (UncaughtParseError e)
    Right cs -> runClauses cs

compileUnsafe :: Text -> Eval (IO Value)
compileUnsafe t = do
  pE <- compile t
  return $ withPrettyError pE

runClauses :: [Value] -> Eval (Either CrossbowError (IO Value))
runClauses [] = return (Left EmptyProgramError)
-- We might first start with a fully bound clause, so ensure that one is deeply eval'd before moving on
runClauses (c : cs) = do
  cDeepE <- deepEval c
  case cDeepE of
    Left e -> return $ Left e
    Right cDeep -> go (return cDeep) cs
  where
    go :: IO Value -> [Value] -> Eval (Either CrossbowError (IO Value))
    go vIO [] = return $ Right vIO
    go vIO (c : cs) = do
      v <- liftIO vIO
      apE <- apply v c
      case apE of
        Left e -> return $ Left e
        Right v' -> do
          -- TODO: Can we do away with this deepEval? Doesn't apply handle it?
          deepVE <- deepEval v'
          case deepVE of
            Left e -> return $ Left e
            Right deepV -> go (return deepV) cs

-- Build a vFunction that will take arguments, substitute, and then run clauses
-- Does so by creating a hashed name for this lambda and storing it with our program context
-- TODO: Still need to add this to the State, so we do need Eval to become StateT
compileLambda :: Value -> Eval (Either CrossbowError Value)
compileLambda (VLambda (Valence v) clauses) =
  do
    let lambdaName = "onelambda"
        impl =
          ( HSImpl
              ( \args -> do
                  let cs' = substituteArgs args <$> clauses
                  vE <- runClauses cs'
                  case vE of
                    Left e -> error (pretty e)
                    Right vIO -> do
                      v <- liftIO vIO
                      deepVE <- deepEval v
                      case deepVE of
                        Left e -> error (pretty e)
                        Right deepV -> return deepV
              )
          )
    return . Right $ VFunction (Function lambdaName [])
compileLambda v = return . Left $ NonLambdaCompilationError v

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> Eval (Either CrossbowError Value)
-- Lambdas get JIT compiled here
apply l@(VLambda _ _) v = do
  lE <- compileLambda l
  case lE of
    Left e -> return $ Left e
    Right f -> apply f v
-- Lambdas get JIT compiled here
apply v l@(VLambda _ _) = do
  lE <- compileLambda l
  case lE of
    Left e -> return $ Left e
    Right f -> apply v f
-- If we have a function in program state, apply to the right
apply (VFunction f) v = applyF f v BindFromRight
-- If we have a value in program state and encounter a function, apply it
apply v (VFunction f) = applyF f v BindFromRight
-- Application of lists tries to ziplist
apply (VList as@((VFunction _) : _)) (VList bs) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF <$> fs <*> bz <*> pure BindFromRight
    case partitionEithers (getZipList rs) of
      ([], rs) -> return $ Right $ VList rs
      (es, _) -> return $ Left $ ApplyError es
-- Fork values with post-application of applicatives should work the same way
apply (VList bs) (VList as@((VFunction _) : _)) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF <$> fs <*> bz <*> pure BindFromLeft
    case partitionEithers (getZipList rs) of
      ([], rs) -> return $ Right $ VList rs
      (es, _) -> return $ Left $ ApplyError es
-- If we have a value with a value, just override it
apply _ v = return $ Right v

-- Binds the next unbound value to that given
bindNext :: Function -> Value -> BindDir -> Function
bindNext (Function name args) v bindDir =
  case bindDir of
    BindFromLeft -> Function name (v : args)
    BindFromRight -> Function name (args ++ [v])

applyF :: Function -> Value -> BindDir -> Eval (Either CrossbowError Value)
applyF f v bindDir = do
  strictVE <- deepEval v
  case strictVE of
    Left e -> return $ Left e
    Right strictV -> return $ Right (VFunction (bindNext f strictV bindDir))

isIdentifier :: Value -> Bool
isIdentifier (VIdentifier _) = True
isIdentifier _ = False

runCBImpl :: Text -> [Value] -> Eval (Either CrossbowError Value)
runCBImpl cbF args = do
  pE <- compile cbF
  case pE of
    Left e -> return $ Left e
    Right fIO -> do
      f <- liftIO fIO
      result <- foldM (\acc x -> withPrettyError <$> apply acc x) f args
      deepEval result

runHSImpl :: ([Value] -> Eval Value) -> [Value] -> Eval (Either CrossbowError Value)
runHSImpl hsF args = do
  result <- hsF args
  deepEval result

evalF :: Value -> Eval (Either CrossbowError Value)
evalF vf@(VFunction (Function name args)) = do
  builtins <- asks _builtins
  case M.lookup name builtins of
    Nothing -> return . Left . EvalError $ "No value named: " <> name
    Just (Valence v, impl) ->
      if
          | length args == v ->
            case impl of
              HSImpl hsF -> runHSImpl hsF args
              CBImpl cbF -> runCBImpl cbF args
          | length args > v -> return $ Left (TooManyArgumentsError name v (length args))
          | otherwise -> return $ Right vf
evalF v = return $ Right v

-- Turn e.g. $4 into 4
identifierIx :: Value -> Int
identifierIx (VIdentifier i) = fst . fromRight' $ decimal (T.drop 1 i)
identifierIx v = error $ "Cannot get identifier index for value: " <> show v

-- Sub any Identifier placeholders with their actual values
substituteArgs :: [Value] -> Value -> Value
substituteArgs subs i@(VIdentifier _) = subs !! identifierIx i
substituteArgs subs (VList as) = VList (substituteArgs subs <$> as)
substituteArgs subs (VFunction (Function name args)) =
  VFunction (Function name (substituteArgs subs <$> args))
-- In particular we skip lambdas here, which should allow for nesting
substituteArgs _ v = v

-- Deeply evaluate the given value.
-- Ensure if this is a function that all arguments are strictly evaluated.
deepEval :: Value -> Eval (Either CrossbowError Value)
deepEval (VFunction (Function name args)) =
  do
    (errors, argsStrict) <- partitionEithers <$> traverse deepEval args
    if not (null errors)
      then return . Left $ L.head errors
      else evalF (VFunction (Function name argsStrict))
deepEval (VList as) = do
  as <- traverse deepEval as
  return . fmap VList $ sequence as
deepEval v = return $ Right v

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, HSImpl (\[a, b] -> return $ a + b))),
      ("++", (Valence 2, HSImpl (\[VList a, VList b] -> return $ VList $ a ++ b))),
      ("*", (Valence 2, HSImpl (\[a, b] -> return $ a * b))),
      ("^", (Valence 2, HSImpl (\[a, b] -> return $ a ^ b))),
      ("-", (Valence 2, HSImpl (\[a, b] -> return $ a - b))),
      ("abs", (Valence 1, HSImpl (\[a] -> return $ abs a))),
      ("negate", (Valence 1, HSImpl (\[a] -> return $ negate a))),
      ("mod", (Valence 2, HSImpl (\[a, b] -> return $ a `mod` b))),
      ("div", (Valence 2, HSImpl (\[a, b] -> return $ a `div` b))),
      ("==", (Valence 2, HSImpl (\[a, b] -> return $ VBool $ a == b))),
      ("<=", (Valence 2, HSImpl (\[a, b] -> return $ VBool $ a <= b))),
      ("<", (Valence 2, HSImpl (\[a, b] -> return $ VBool $ a < b))),
      (">=", (Valence 2, HSImpl (\[a, b] -> return $ VBool $ a >= b))),
      (">", (Valence 2, HSImpl (\[a, b] -> return $ VBool $ a > b))),
      (":", (Valence 2, HSImpl (\[a, b] -> return $ vCons a b))),
      ("min", (Valence 2, HSImpl (\[a, b] -> return $ min a b))),
      ("max", (Valence 2, HSImpl (\[a, b] -> return $ max a b))),
      ("minOn", (Valence 3, CBImpl "{map $0 [$1,$2]| monadic <= |if _ $1 $2}")),
      ("maxOn", (Valence 3, CBImpl "{map $0 [$1,$2]| monadic > |if _ $1 $2}")),
      -- TODO: Redefine all the below using crossbow folds, maps, filters
      ("id", (Valence 1, HSImpl (\[a] -> return a))),
      ("const", (Valence 2, HSImpl (\[a, _] -> return a))),
      ("cons", (Valence 2, HSImpl (\[a, b] -> return $ vCons a b))),
      ("ix", (Valence 2, HSImpl (\[VInteger a, VList b] -> return $ b !! fromInteger a))),
      ("drop", (Valence 2, HSImpl (\[VInteger n, VList as] -> return $ VList (drop (fromIntegral n) as)))),
      ("take", (Valence 2, HSImpl (\[VInteger n, VList as] -> return $ VList (take (fromIntegral n) as)))),
      ("head", (Valence 1, HSImpl (\[VList as] -> return $ L.head as))),
      ("tail", (Valence 1, HSImpl (\[VList as] -> return $ VList $ L.tail as))),
      -- TODO: Make zip variadic
      ("zip", (Valence 2, HSImpl (\[VList as, VList bs] -> return $ VList ((\(a, b) -> VList [a, b]) <$> zip as bs)))),
      ("zip3", (Valence 3, HSImpl (\[VList as, VList bs, VList cs] -> return $ VList ((\(a, b, c) -> VList [a, b, c]) <$> zip3 as bs cs)))),
      ("pairs", (Valence 1, CBImpl "{$0|fork 2|[id, drop 1]|monadic zip}")),
      ("square", (Valence 1, CBImpl "{$0|length|flip fork|$0}")),
      ("enum", (Valence 1, CBImpl "{$0|fork 2|[length,id]|[range 0, id]|monadic zip}")),
      ("lengthy", (Valence 2, CBImpl "{$1|length|($0==_)}")),
      ("windows", (Valence 2, CBImpl "{$1|square|enum|map (monadic drop)|map (take $0)|filter (lengthy $0)}")),
      ( "nap",
        ( Valence 3,
          HSImpl
            ( \[VInteger n, VFunction f, VList as] -> do
                let vas = V.fromList as
                a' <- withPrettyError <$> applyF f (vas V.! fromInteger n) BindFromLeft
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
      ("range", (Valence 2, HSImpl (\[VInteger a, VInteger b] -> return $ VList $ VInteger <$> [a .. b]))),
      ( "map",
        ( Valence 2,
          HSImpl
            ( let map [_, VList []] = return $ VList []
                  map [VFunction f, VList (x : xs)] = do
                    x' <- applyF f x BindFromLeft
                    vCons (withPrettyError x') <$> map [VFunction f, VList xs]
               in map
            )
        )
      ),
      ("count", (Valence 2, CBImpl "{$1|filter $0|length}")),
      ( "filter",
        ( Valence 2,
          HSImpl
            ( let filter [_, VList []] = return $ VList []
                  filter [VFunction f, VList (x : xs)] = do
                    x' <- withPrettyError <$> applyF f x BindFromLeft
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
                return $
                  let Just v = foldl' (\acc (VList [k, v]) -> if k == a then Just v else acc) Nothing xs
                   in v
            )
        )
      ),
      ("if", (Valence 3, HSImpl (\[p, a, b] -> return $ let (VBool p') = withPrettyError $ castToBool p in if p' then a else b))),
      ("aoc", (Valence 1, CBImpl "{$0|string|(\"test/aoc_input/\"++_)|(_++\".txt\")|read}")),
      ("sum", (Valence 1, CBImpl "foldl|+|0")),
      ("odd", (Valence 1, CBImpl "{$0|mod _ 2|bool}")),
      ("even", (Valence 1, CBImpl "{$0|odd|not}")),
      ("not", (Valence 1, CBImpl "if _ False True")),
      ("maximum", (Valence 1, CBImpl "foldl1 max")),
      ("minimum", (Valence 1, CBImpl "foldl1 min")),
      ("maximumOn", (Valence 2, CBImpl "{foldl1 (maxOn $0) $1}")),
      ("minimumOn", (Valence 2, CBImpl "{foldl1 (minOn $0) $1}")),
      ("mode", (Valence 1, CBImpl "{counts|maximumOn snd|fst}")),
      ("antimode", (Valence 1, CBImpl "{counts|minimumOn snd|fst}")),
      ("length", (Valence 1, CBImpl "foldl (flip const (+1) _) 0")),
      ( "foldl",
        ( Valence 3,
          HSImpl
            ( \[VFunction f, acc, VList xs] -> do
                foldlM
                  ( \acc x -> do
                      (VFunction f') <- withPrettyError <$> applyF f acc BindFromLeft
                      withPrettyError <$> applyF f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      ( "foldr",
        ( Valence 3,
          HSImpl
            ( \[VFunction f, VList xs, acc] -> do
                foldrM
                  ( \acc x -> do
                      (VFunction f') <- withPrettyError <$> applyF f acc BindFromLeft
                      withPrettyError <$> applyF f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      ( "scanl",
        ( Valence 3,
          HSImpl
            ( \[VFunction f, acc, VList xs] -> do
                fmap VList . sequence $
                  scanl'
                    ( \accM x -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF f acc BindFromLeft
                        withPrettyError <$> applyF f' x BindFromLeft
                    )
                    (pure acc)
                    xs
            )
        )
      ),
      ( "scanr",
        ( Valence 3,
          HSImpl
            ( \[VFunction f, acc, VList xs] -> do
                fmap VList . sequence $
                  scanr
                    ( \x accM -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF f acc BindFromLeft
                        withPrettyError <$> applyF f' x BindFromLeft
                    )
                    (pure acc)
                    xs
            )
        )
      ),
      ("foldl1", (Valence 2, CBImpl "{$1|fork 2|[head, tail]|monadic (foldl $0)}")),
      ("scanl1", (Valence 2, CBImpl "{$1|fork 2|[head, tail]|monadic (scanl $0)}")),
      ("fold", (Valence 3, CBImpl "foldl")),
      ("scan", (Valence 3, CBImpl "scanl")),
      ("transpose", (Valence 1, HSImpl (\[VList as] -> return $ let unlist (VList l) = l in VList $ VList <$> transpose (unlist <$> as)))),
      -- TODO: Make flip work with other valences
      ( "flip",
        ( Valence 3,
          HSImpl
            ( \[VFunction f, a, b] ->
                do
                  (VFunction f') <- withPrettyError <$> applyF f b BindFromLeft
                  withPrettyError <$> applyF f' a BindFromLeft
            )
        )
      ),
      ("reverse", (Valence 1, CBImpl "foldl (flip cons) [] _")),
      ( "ap",
        ( Valence 2,
          HSImpl
            ( \[VFunction f, a] -> do
                pE <- applyF f a BindFromLeft
                return $ withPrettyError pE
            )
        )
      ),
      ("fork", (Valence 2, HSImpl (\[n, a] -> return $ let VInteger n' = withPrettyError . castToInt $ n in VList (replicate (fromInteger n') a)))),
      ( "monadic",
        ( Valence 2,
          HSImpl
            ( \[VFunction f, VList args] -> do
                foldlM
                  ( \v a -> do
                      case v of
                        VFunction f ->
                          withPrettyError <$> applyF f a BindFromLeft
                        _ -> return v
                  )
                  (VFunction f)
                  args
            )
        )
      ),
      ("lines", (Valence 1, HSImpl (\[VList t] -> return $ let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.lines (unchar <$> t)))))),
      ("words", (Valence 1, HSImpl (\[VList t] -> return $ let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.words (unchar <$> t)))))),
      ("ints", (Valence 1, CBImpl "{lines|int}")),
      ("int", (Valence 1, HSImpl (\[a] -> return $ withPrettyError . castToInt $ a))),
      ("double", (Valence 1, HSImpl (\[a] -> return $ withPrettyError . castToDouble $ a))),
      ("char", (Valence 1, HSImpl (\[a] -> return $ withPrettyError . castToChar $ a))),
      ("bool", (Valence 1, HSImpl (\[a] -> return $ withPrettyError . castToBool $ a))),
      ("string", (Valence 1, HSImpl (\[a] -> return $ VList $ VChar <$> T.unpack (asText a)))),
      ( "counts",
        ( Valence 1,
          HSImpl
            ( \[VList as] ->
                return $
                  VList
                    . fmap (\(a, b) -> VList [a, b])
                    . fmap (second VInteger)
                    . M.toList
                    . M.fromListWith (+)
                    $ [(a, 1) | a <- as]
            )
        )
      ),
      ( "bits",
        ( Valence 1,
          HSImpl
            ( \[VList as] -> return $ VInteger $ sum [2 ^ i | (i, b) <- zip [0 ..] (reverse as), truthy b]
            )
        )
      ),
      ( "read",
        ( Valence 1,
          HSImpl
            ( \[a] -> do
                t <- liftIO $ readFile (T.unpack $ asText a)
                return (VList $ VChar <$> t)
            )
        )
      ),
      ( "input",
        ( Valence 0,
          HSImpl
            ( \_ -> do
                t <- liftIO $ T.unpack <$> getLine
                return (VList $ VChar <$> t)
            )
        )
      )
    ]
