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
import Data.List.Extra (chunksOf)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.String qualified as ST
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Language.Haskell.TH.Ppr (parensIf)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (newStdGen, randomRs)
import Text.Parsec (parserTrace)
import Text.ParserCombinators.Parsec (ParseError, parse, parseTest)

debugParser :: Bool
debugParser = False

debugEvaluation :: Bool
debugEvaluation = False

stdLibPath :: FilePath
stdLibPath = "lib.cb"

loadStdLib :: Eval ()
loadStdLib = do
  stdLibLoaded <- gets _stdLibLoaded
  unless stdLibLoaded do
    (ProgramContext b pp _) <- get
    put $ ProgramContext b pp True
    e <- runFile stdLibPath
    case e of
      Left e -> error (pretty e)
      Right _ -> return ()

runFile :: FilePath -> Eval (Either CrossbowError [Value])
runFile path = do
  t <- T.pack <$> readFile path
  compilePrinted t

parseProgram :: Text -> Eval (Either ParseError [[Value]])
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

compileInternal :: Bool -> Text -> Eval (Either CrossbowError [Value])
compileInternal doPrint t = do
  loadStdLib
  when debugParser (debugParseProgram t)
  pE <- parseProgram t
  case pE of
    Left e -> return $ Left (UncaughtParseError e)
    Right css -> runProgram doPrint css

compile :: Text -> Eval (Either CrossbowError [Value])
compile = compileInternal False

compilePrinted :: Text -> Eval (Either CrossbowError [Value])
compilePrinted = compileInternal True

compileUnsafe :: Text -> Eval [Value]
compileUnsafe t = do
  pE <- compile t
  return $ withPrettyError pE

runProgram :: Bool -> [[Value]] -> Eval (Either CrossbowError [Value])
runProgram doPrint css = go css []
  where
    go [] vs = return $ Right (reverse vs)
    go (cs : css) vs = do
      vE <- runClauses cs
      case vE of
        Left e -> do
          when doPrint $ liftIO $ putTextLn (pretty e)
          return $ Left e
        Right v -> do
          when (doPrint && v /= VNull) $ putTextLn (pretty v)
          go css (v : vs)

runClauses :: [Value] -> Eval (Either CrossbowError Value)
runClauses [] = return (Left EmptyProgramError)
runClauses (c : cs) = do
  cDeepE <- deepEval c
  case cDeepE of
    Left e -> return $ Left e
    Right cDeep -> go cDeep cs
  where
    go :: Value -> [Value] -> Eval (Either CrossbowError Value)
    go v [] = return $ Right v
    go v (c : cs) = do
      when debugEvaluation do
        print $ "State: " <> pretty v
        print $ "Applying to: " <> pretty c
      apE <- apply v c
      case apE of
        Left e -> return $ Left e
        Right v' -> do
          deepVE <- deepEval v'
          case deepVE of
            Left e -> return $ Left e
            Right deepV -> go deepV cs

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
    [] -> 0
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
                when debugEvaluation $
                  liftIO (print $ "Inside lambda " <> lambdaName <> " with args: " <> pretty args)
                if length args /= nArgs
                  then return $ Left $ ValenceError (length args) nArgs
                  else do
                    let cs' = substituteArgs args <$> clauses
                    runClauses cs'
            )
    -- Register the lambda with the namespace
    when debugEvaluation (print $ "Compiled lambda with # args: " <> show (lambdaName, nArgs))
    (ProgramContext pp builtins stdLibLoaded) <- get
    put (ProgramContext pp (M.insert lambdaName impl builtins) stdLibLoaded)
    return . Right $ VFunction (Function lambdaName [])
compileLambda v = return . Left $ NonLambdaCompilationError v

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> Eval (Either CrossbowError Value)
apply v VNull = return . Right $ v
apply VNull v = return . Right $ v
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
  when debugEvaluation do
    print $ "Applying " <> pretty f
  strictVE <- deepEval v
  case strictVE of
    Left e -> return $ Left e
    Right strictV -> return $ Right (VFunction (bindNext f strictV bindDir))

isIdentifier :: Value -> Bool
isIdentifier (VIdentifier _) = True
isIdentifier _ = False

runCBImpl :: Text -> [Value] -> Eval (Either CrossbowError Value)
runCBImpl cbF args = do
  when debugEvaluation (print $ "Running CBImpl with args: " <> cbF <> ", " <> pretty args)
  pE <- compile cbF
  case pE of
    Left e -> return $ Left e
    Right [vf] -> applyCBFunction vf args
    Right _ -> error "Can't handle CBImpl with more than one return value yet"

applyCBFunction :: Value -> [Value] -> Eval (Either CrossbowError Value)
applyCBFunction vf args = do
  result <- foldM (\acc x -> withPrettyError <$> apply acc x) vf args
  deepEval result

runHSImpl :: ([Value] -> Eval (Either CrossbowError Value)) -> [Value] -> Eval (Either CrossbowError Value)
runHSImpl hsF args = do
  when debugEvaluation (print $ "Running HSImpl with args: " <> pretty args)
  resultE <- hsF args
  case resultE of
    Left e -> return $ Left e
    Right result -> do
      when debugEvaluation (print $ "HSImpl succeeded with result: " <> pretty result)
      deepEval result

evalF :: Value -> Eval (Either CrossbowError Value)
evalF vf@(VFunction (Function name args)) = do
  when debugEvaluation (print $ "Evaluating VFunction: " <> pretty vf)
  builtins <- gets _builtins
  case M.lookup name builtins of
    Nothing -> return . Left . EvalError $ "No value named: " <> name
    Just impl -> do
      resultE <-
        case impl of
          HSImpl hsF -> runHSImpl hsF args
          CBImpl cbF -> runCBImpl cbF args
          ConstImpl constV ->
            case constV of
              -- For constant (bound) implementations, we need to check if this is a stored compileLambda
              -- if it is, we need to continue evaluation.
              -- (VFunction (Function name' _)) -> evalF (VFunction (Function name' args))
              vf@(VFunction _) -> applyCBFunction vf args
              v -> return $ Right v
      case resultE of
        Left e -> do
          when debugEvaluation (print $ "Could not apply: " <> pretty e)
          return $ Right vf -- If we could not apply, don't; just return the partially applied function
        Right result -> do
          when debugEvaluation (print $ "Bound successfully with result: " <> pretty result)
          return $ Right result
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
-- If deeply evaluating a function returns another function, we deep-eval that as well
deepEval :: Value -> Eval (Either CrossbowError Value)
deepEval (VFunction f@(Function name args)) =
  do
    when debugEvaluation (print $ "Deeply evaluating function: " <> pretty f)
    (errors, argsStrict) <- partitionEithers <$> traverse deepEval args
    if not (null errors)
      then return . Left $ L.head errors
      else do
        vE <- evalF (VFunction (Function name argsStrict))
        case vE of
          Left e -> return $ Left e
          Right vf@(VFunction (Function name' _)) ->
            if name == name'
              then return $ Right vf
              else deepEval vf
          Right v -> return $ Right v
deepEval (VList as) = do
  as <- traverse deepEval as
  return . fmap VList $ sequence as
deepEval l@(VLambda _) = do
  fE <- compileLambda l
  case fE of
    Left e -> return $ Left e
    Right f -> deepEval f
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
          else return . Left $ ValenceError (length args) n
    )

-- TODO: Redefine as much of the below as possible inside lib.cs
builtins :: Map Text OpImpl
builtins =
  M.fromList
    [ ("null", ConstImpl VNull),
      ("+", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ a + b)),
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
      ("cons", wrapImpl 2 $ HSImpl (\[a, b] -> return . Right $ vCons a b)),
      ("ix", wrapImpl 2 $ HSImpl (\[VInteger a, VList b] -> return . Right $ b !! fromInteger a)),
      ("drop", wrapImpl 2 $ HSImpl (\[VInteger n, VList as] -> return . Right $ VList (drop (fromIntegral n) as))),
      ("take", wrapImpl 2 $ HSImpl (\[VInteger n, VList as] -> return . Right $ VList (take (fromIntegral n) as))),
      ("tails", wrapImpl 1 $ HSImpl (\[VList as] -> return . Right $ VList $ VList <$> L.tails as)),
      -- TODO: Make zip variadic
      ("zip", wrapImpl 2 $ HSImpl (\[VList as, VList bs] -> return . Right $ VList ((\(a, b) -> VList [a, b]) <$> zip as bs))),
      ("zip3", wrapImpl 3 $ HSImpl (\[VList as, VList bs, VList cs] -> return . Right $ VList ((\(a, b, c) -> VList [a, b, c]) <$> zip3 as bs cs))),
      ( "chunks",
        wrapImpl 2 $
          HSImpl (\[VInteger a, VList as] -> return . Right $ VList (VList <$> chunksOf (fromInteger a) as))
      ),
      ( "splitOn",
        wrapImpl 2 $
          HSImpl (\[VList a, VList as] -> return . Right $ VList (VList <$> splitOn a as))
      ),
      -- TODO: need to handle escaped characters
      ( "paragraphs",
        wrapImpl 1 $
          HSImpl (\[VList as] -> return . Right $ VList (VList <$> splitOn [VChar '\n', VChar '\n'] as))
      ),
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
                ProgramContext pp builtins stdLibLoaded <- get
                put (ProgramContext pp (M.insert k (ConstImpl v) builtins) stdLibLoaded)
                return $ Right VNull
            )
      ),
      ( "import",
        wrapImpl 1 $
          HSImpl
            ( \[path] -> do
                let path' = T.unpack $ asText path
                vE <- runFile path'
                case vE of
                  Left e -> return $ Left e
                  -- TODO: Could actually use the imported values here
                  Right _ -> return $ Right VNull
            )
      ),
      ( "trace",
        wrapImpl 2 $
          HSImpl
            ( \[msg, v] -> do
                putTextLn $ "[trace] " <> pretty msg
                return $ Right v
            )
      ),
      ( "traceId",
        wrapImpl 1 $
          HSImpl
            ( \[v] -> do
                putTextLn $ "[trace] " <> pretty v
                return $ Right v
            )
      ),
      ( "union",
        wrapImpl 2 $
          HSImpl
            ( \case
                [VList a, VList b] -> return $ Right . VList $ a `L.union` b
                [VSet a, VSet b] -> return $ Right . VSet $ a `S.union` b
                [VMap a, VMap b] -> return $ Right . VMap $ a `M.union` b
            )
      ),
      ( "intersection",
        wrapImpl 2 $
          HSImpl
            ( \case
                [VList a, VList b] -> return $ Right . VList $ a `L.intersect` b
                [VSet a, VSet b] -> return $ Right . VSet $ a `S.intersection` b
                [VMap a, VMap b] -> return $ Right . VMap $ a `M.intersection` b
            )
      ),
      ( "difference",
        wrapImpl 2 $
          HSImpl
            ( \case
                [VList a, VList b] -> return $ Right . VList $ a L.\\ b
                [VSet a, VSet b] -> return $ Right . VSet $ a `S.difference` b
                [VMap a, VMap b] -> return $ Right . VMap $ a `M.difference` b
            )
      ),
      ( "insert",
        wrapImpl 3 $
          HSImpl
            ( \case
                [k, v, VMap m] -> return $ Right . VMap $ M.insert k v m
            )
      ),
      ( "delete",
        wrapImpl 2 $
          HSImpl
            ( \case
                [a, VList b] -> return $ Right . VList $ a `L.delete` b
                [a, VSet b] -> return $ Right . VSet $ a `S.delete` b
                [a, VMap b] -> return $ Right . VMap $ a `M.delete` b
            )
      ),
      ( "list",
        wrapImpl 1 $
          HSImpl
            ( \case
                [VList a] -> return $ Right . VList $ a
                [VSet a] -> return $ Right . VList $ S.toList a
                [VMap a] -> return $ Right . VList $ (\(a, b) -> VList [a, b]) <$> M.toList a
                [v] -> return $ Right . VList $ [v]
            )
      ),
      ( "assoc",
        wrapImpl 1 $
          HSImpl
            ( \case
                [VList as] -> return $ Right . VMap $ M.fromList ((\(VList [k, v]) -> (k, v)) <$> as)
            )
      ),
      ( "set",
        wrapImpl 1 $
          HSImpl
            ( \case
                [VSet a] -> return $ Right . VSet $ a
                [VList a] -> return $ Right . VSet $ S.fromList a
                [v] -> return $ Right . VSet $ S.singleton v
            )
      ),
      ( "sort",
        wrapImpl 1 $
          HSImpl
            ( \case
                [VList a] -> return . Right . VList $ sort a
            )
      )
    ]
