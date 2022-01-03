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
import System.Random (newStdGen, randomRs)
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec (ParseError, parse, parseTest)

debugParser :: Bool
debugParser = False

parseProgram :: Text -> Eval (Either ParseError [Value])
parseProgram t = do
  programParser <- gets _programParser
  return $ parse programParser "" . T.unpack $ t

debugParseProgram :: Text -> Eval ()
debugParseProgram t = do
  programParser <- gets _programParser
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
      -- TODO: Not clear that this E.try is actually going to catch anything
      apE' <- apply v c
      apE <- liftIO (E.try (pure apE') :: IO (Either SomeException (Either CrossbowError Value)))
      case apE of
        Left e -> return $ Left $ InternalError (show e)
        Right vE ->
          case vE of
            Left e -> return $ Left e
            Right v' -> do
              -- TODO: Can we do away with this deepEval? Doesn't apply handle it?
              deepVE <- deepEval v'
              case deepVE of
                Left e -> return $ Left e
                Right deepV -> go (return deepV) cs

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

numArgs :: Value -> Int
numArgs (VLambda clauses) =
  case mapMaybe maxArgIx clauses of
    [] -> 1
    ns -> L.maximum ns + 1
numArgs _ = error "Can't call numArgs on a non lambda"

-- Build a vFunction that will take arguments, substitute, and then run clauses
-- Does so by creating a hashed name for this lambda and storing it with our program context
-- TODO: Still need to add this to the State, so we do need Eval to become StateT
compileLambda :: Value -> Eval (Either CrossbowError Value)
compileLambda lambda@(VLambda clauses) =
  do
    g <- newStdGen
    let nArgs = numArgs lambda
    let lambdaName = T.pack ("lambda_" <> take 16 (randomRs ('a', 'z') g))
        impl =
          HSImpl
            ( \args -> do
                if length args /= nArgs
                  then return . Left $ ValenceError (length args)
                  else do
                    let cs' = substituteArgs args <$> clauses
                    vE <- runClauses cs'
                    case vE of
                      Left e -> return $ Left e
                      Right vIO -> do
                        v <- liftIO vIO
                        deepVE <- deepEval v
                        case deepVE of
                          Left e -> return $ Left e
                          Right deepV -> return $ Right deepV
            )
    -- Register the lambda with the namespace
    (ProgramContext pp builtins) <- get
    put (ProgramContext pp (M.insert lambdaName impl builtins))
    return . Right $ VFunction (Function lambdaName [])
compileLambda v = return . Left $ NonLambdaCompilationError v

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> Eval (Either CrossbowError Value)
-- Lambdas get JIT compiled here
apply l@(VLambda _) v = do
  lE <- compileLambda l
  case lE of
    Left e -> return $ Left e
    Right f -> apply f v
-- Lambdas get JIT compiled here
apply v l@(VLambda _) = do
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
    rs <- sequence $ applyF <$> fs <*> bz <*> pure BindFromRight
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

runHSImpl :: ([Value] -> Eval (Either CrossbowError Value)) -> [Value] -> Eval (Either CrossbowError Value)
runHSImpl hsF args = do
  resultE <- hsF args
  case resultE of
    Left e -> return $ Left e
    Right result -> deepEval result

evalF :: Value -> Eval (Either CrossbowError Value)
evalF vf@(VFunction (Function name args)) = do
  builtins <- gets _builtins
  case M.lookup name builtins of
    Nothing -> return . Left . EvalError $ "No value named: " <> name
    Just impl -> do
      resultE <-
        case impl of
          HSImpl hsF -> runHSImpl hsF args
          CBImpl cbF -> runCBImpl cbF args
          ConstImpl constV -> return $ Right constV
      case resultE of
        Left _ -> return $ Right vf -- If we could not apply, don't
        Right result -> return $ Right result -- We managed to bind without error
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

-- Coerce lambas into functions for the builtins
-- TODO: Messy, try remove
fromCallable :: Value -> Eval Value
fromCallable l@(VLambda _) = withPrettyError <$> compileLambda l
fromCallable v = return v

wrapImpl :: Int -> OpImpl -> OpImpl
wrapImpl n (HSImpl f) =
  HSImpl
    ( \args ->
        if n == length args
          then f args
          else return . Left $ ValenceError (length args)
    )

builtins :: Map Text OpImpl
builtins =
  M.fromList
    [ ("+", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a + b)),
      ("++", wrapImpl 2 $ HSImpl (\[VList a, VList b] -> return . Right $ VList $ a ++ b)),
      ("*", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a * b)),
      ("^", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a ^ b)),
      ("-", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a - b)),
      ("abs", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ abs a)),
      ("negate", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ negate a)),
      ("mod", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a `mod` b)),
      ("div", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a `div` b)),
      ("==", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ VBool $ a == b)),
      ("<=", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ VBool $ a <= b)),
      ("<", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ VBool $ a < b)),
      (">=", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ VBool $ a >= b)),
      (">", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ VBool $ a > b)),
      (":", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ vCons a b)),
      ("min", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ min a b)),
      ("max", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ max a b)),
      ("minOn", CBImpl "{map $0 [$1, $2]|monadic <= |if|$1|$2}"),
      ("maxOn", CBImpl "{map $0 [$1, $2]|monadic > |if|$1|$2}"),
      -- TODO: Redefine all the below using crossbow folds, maps, filters
      ("id", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ a)),
      ("const", wrapImpl 2 $ HSImpl (\[a, _] -> return . Right $ a)),
      ("cons", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ vCons a b)),
      ("ix", wrapImpl 2 $ HSImpl (\[VInteger a, VList b] -> return . Right $ b !! fromInteger a)),
      ("drop", wrapImpl 2 $ HSImpl (\[VInteger n, VList as] -> return . Right $ VList (drop (fromIntegral n) as))),
      ("take", wrapImpl 2 $ HSImpl (\[VInteger n, VList as] -> return . Right $ VList (take (fromIntegral n) as))),
      ("head", wrapImpl 1 $ HSImpl (\[VList as] -> return . Right $ L.head as)),
      ("tail", wrapImpl 1 $ HSImpl (\[VList as] -> return . Right $ VList $ L.tail as)),
      -- TODO: Make zip variadic
      ("zip", wrapImpl 2 $ HSImpl (\[VList as, VList bs] -> return . Right $ VList ((\(a, b) -> VList [a, b]) <$> zip as bs))),
      ("zip3", wrapImpl 3 $ HSImpl (\[VList as, VList bs, VList cs] -> return . Right $ VList ((\(a, b, c) -> VList [a, b, c]) <$> zip3 as bs cs))),
      ("pairs", CBImpl "{$0|fork 2|[id, drop 1]|monadic zip}"),
      ("square", CBImpl "{fork (length $0) $0}"),
      ("enum", CBImpl "{$0|fork 2|[length,id]|[range 0, id]|monadic zip}"),
      ("lengthy", CBImpl "{$1|length|(== $0)}"),
      ("windows", CBImpl "{$1|square|enum|map (monadic drop)|map (take $0)|filter (lengthy $0)}"),
      ( "nap",
        wrapImpl 3 $
          HSImpl
            ( \[VInteger n, VFunction f, VList as] -> do
                let vas = V.fromList as
                a' <- withPrettyError <$> applyF f (vas V.! fromInteger n) BindFromRight
                let vas' = vas V.// [(fromInteger n, a')]
                return . Right $ VList $ V.toList vas'
            )
      ),
      ("first", CBImpl "nap 0"),
      ("second", CBImpl "nap 1"),
      ("third", CBImpl "nap 2"),
      ("fst", CBImpl "ix 0"),
      ("snd", CBImpl "ix 1"),
      ("thd", CBImpl "ix 2"),
      -- TODO variadic
      ("range", wrapImpl 2 $ HSImpl (\[VInteger a, VInteger b] -> return . Right $ VList $ VInteger <$> [a .. b])),
      ( "map",
        wrapImpl 2 $
          HSImpl
            ( let map [_, VList []] = return . Right $ VList []
                  map [callable, VList (x : xs)] =
                    Right <$> do
                      VFunction f <- fromCallable callable
                      x' <- applyF f x BindFromRight
                      vCons (withPrettyError x') <$> (withPrettyError <$> map [VFunction f, VList xs])
               in map
            )
      ),
      ("count", CBImpl "{filter $0 $1 | length}"),
      ( "filter",
        wrapImpl 2 $
          HSImpl
            ( let filter [_, VList []] = return . Right $ VList []
                  filter [callable, VList (x : xs)] =
                    Right <$> do
                      (VFunction f) <- fromCallable callable
                      x' <- withPrettyError <$> applyF f x BindFromRight
                      deepX' <- withPrettyError <$> deepEval x'
                      if truthy deepX'
                        then vCons x . withPrettyError <$> filter [VFunction f, VList xs]
                        else withPrettyError <$> filter [VFunction f, VList xs]
               in filter
            )
      ),
      ( "case",
        wrapImpl 2 $
          HSImpl
            ( \[a, VList xs] ->
                return . Right $
                  let Just v = foldl' (\acc (VList [k, v]) -> if k == a then Just v else acc) Nothing xs
                   in v
            )
      ),
      ("if", wrapImpl 3 $ HSImpl (\[p, a, b] -> return . Right $ let (VBool p') = withPrettyError $ castToBool p in if p' then a else b)),
      ("aoc", CBImpl "{read ((\"test/aoc_input/\" ++ (string $0)) ++ \".txt\")}"),
      ("sum", CBImpl "foldl|+|0"),
      ("odd", CBImpl "{mod $0 2|bool}"),
      ("even", CBImpl "{odd $0|not}"),
      ("not", CBImpl "{if $0 False True}"),
      ("maximum", CBImpl "foldl1 max"),
      ("minimum", CBImpl "foldl1 min"),
      ("maximumOn", CBImpl "{foldl1 (maxOn $0) $1}"),
      ("minimumOn", CBImpl "{foldl1 (minOn $0) $1}"),
      ("mode", CBImpl "{counts|maximumOn snd|fst}"),
      ("antimode", CBImpl "{counts|minimumOn snd|fst}"),
      ("length", CBImpl "{map (const 1) | sum}"),
      ( "foldl",
        wrapImpl 3 $
          HSImpl
            ( \[callable, acc, VList xs] -> do
                VFunction f <- fromCallable callable
                Right
                  <$> ( foldlM
                          ( \acc x -> do
                              (VFunction f') <- withPrettyError <$> applyF f acc BindFromRight
                              withPrettyError <$> applyF f' x BindFromRight
                          )
                          acc
                          xs
                      )
            )
      ),
      ( "foldr",
        wrapImpl 3 $
          HSImpl
            ( \[callable, VList xs, acc] -> do
                VFunction f <- fromCallable callable
                Right
                  <$> ( foldrM
                          ( \acc x -> do
                              (VFunction f') <- withPrettyError <$> applyF f acc BindFromRight
                              withPrettyError <$> applyF f' x BindFromRight
                          )
                          acc
                          xs
                      )
            )
      ),
      ( "scanl",
        wrapImpl 3 $
          HSImpl
            ( \[callable, acc, VList xs] -> do
                VFunction f <- fromCallable callable
                fmap (Right . VList) . sequence $
                  scanl'
                    ( \accM x -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF f acc BindFromRight
                        withPrettyError <$> applyF f' x BindFromRight
                    )
                    (pure acc)
                    xs
            )
      ),
      ( "scanr",
        wrapImpl 3 $
          HSImpl
            ( \[callable, acc, VList xs] -> do
                VFunction f <- fromCallable callable
                fmap (Right . VList) . sequence $
                  scanr
                    ( \x accM -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF f acc BindFromRight
                        withPrettyError <$> applyF f' x BindFromRight
                    )
                    (pure acc)
                    xs
            )
      ),
      ("foldl1", CBImpl "{foldl $0 (head $1) (tail $1)}"),
      ("scanl1", CBImpl "{scanl $0 (head $1) (tail $1)}"),
      ("fold", CBImpl "foldl"),
      ("scan", CBImpl "scanl"),
      ("transpose", wrapImpl 1 $ HSImpl (\[VList as] -> return . Right $ let unlist (VList l) = l in VList $ VList <$> transpose (unlist <$> as))),
      -- TODO: Make flip work with other valences
      ( "flip",
        wrapImpl 3 $
          HSImpl
            ( \[callable, a, b] ->
                do
                  VFunction f <- fromCallable callable
                  VFunction f' <- withPrettyError <$> applyF f b BindFromRight
                  applyF f' a BindFromRight
            )
      ),
      ("reverse", CBImpl "foldl (flip cons) [] _"),
      ( "ap",
        wrapImpl 2 $
          HSImpl
            ( \[VFunction f, a] -> do
                pE <- applyF f a BindFromRight
                return . Right $ withPrettyError pE
            )
      ),
      ("fork", wrapImpl 2 $ HSImpl (\[n, a] -> return . Right $ let VInteger n' = withPrettyError . castToInt $ n in VList (replicate (fromInteger n') a))),
      ( "monadic",
        wrapImpl 2 $
          HSImpl
            ( \[VFunction f, VList args] -> do
                Right
                  <$> ( foldlM
                          ( \v a -> do
                              case v of
                                VFunction f -> withPrettyError <$> applyF f a BindFromRight
                                _ -> return v
                          )
                          (VFunction f)
                          args
                      )
            )
      ),
      ("lines", wrapImpl 1 $ HSImpl (\[VList t] -> return . Right $ let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.lines (unchar <$> t))))),
      ("words", wrapImpl 1 $ HSImpl (\[VList t] -> return . Right $ let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.words (unchar <$> t))))),
      ("ints", CBImpl "{lines|int}"),
      ("int", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ withPrettyError . castToInt $ a)),
      ("double", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ withPrettyError . castToDouble $ a)),
      ("char", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ withPrettyError . castToChar $ a)),
      ("bool", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ withPrettyError . castToBool $ a)),
      ("string", wrapImpl 1 $ HSImpl (\[a] -> return . Right $ VList $ VChar <$> T.unpack (asText a))),
      ( "counts",
        wrapImpl 1 $
          HSImpl
            ( \[VList as] ->
                return . Right $
                  VList
                    . fmap (\(a, b) -> VList [a, b])
                    . fmap (second VInteger)
                    . M.toList
                    . M.fromListWith (+)
                    $ [(a, 1) | a <- as]
            )
      ),
      ( "bits",
        wrapImpl 1 $
          HSImpl
            ( \[VList as] -> return . Right $ VInteger $ sum [2 ^ i | (i, b) <- zip [0 ..] (reverse as), truthy b]
            )
      ),
      ( "read",
        wrapImpl 1 $
          HSImpl
            ( \[a] -> do
                t <- liftIO $ readFile (T.unpack $ asText a)
                return . Right $ (VList $ VChar <$> t)
            )
      ),
      ( "input",
        wrapImpl 0 $
          HSImpl
            ( \[] -> do
                t <- liftIO $ T.unpack <$> getLine
                return . Right $ (VList $ VChar <$> t)
            )
      ),
      ( "bind",
        wrapImpl 2 $
          HSImpl
            ( \[VList cs, v] -> do
                let unchar (VChar c) = c
                    k = T.pack $ unchar <$> cs
                ProgramContext pp builtins <- get
                put (ProgramContext pp (M.insert k (ConstImpl v) builtins))
                -- TODO: Return Null or something
                return . Right $ v
            )
      )
    ]
