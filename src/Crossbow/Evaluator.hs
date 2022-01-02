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
import Text.ParserCombinators.Parsec (parse)

compile :: P [IO Value] -> Text -> IO (Either CrossbowError Value)
compile programParser t = case parse programParser "" . T.unpack $ t of
  Left e -> return . Left $ UncaughtParseError e
  Right cs -> runClauses programParser cs

compileUnsafe :: P [IO Value] -> Text -> IO Value
compileUnsafe programParser p = do
  pE <- compile programParser p
  return $ withPrettyError pE

runClauses :: P [IO Value] -> [IO Value] -> IO (Either CrossbowError Value)
runClauses _ [] = return (Left EmptyProgramError)
-- We might first start with a fully bound clause, so ensure that one is deeply eval'd before moving on
runClauses programParser (cIO : cIOs) = do
  c <- cIO
  exE <- deepEval programParser c
  case exE of
    Left e -> return $ Left e
    Right cDeep -> go (return cDeep) cIOs
  where
    go :: IO Value -> [IO Value] -> IO (Either CrossbowError Value)
    go vIO [] = Right <$> vIO
    go vIO (cIO : cIOs) = do
      v <- vIO
      c <- cIO
      exE <- E.try (apply programParser v c) :: (IO (Either SomeException Value))
      case exE of
        Left e -> return . Left $ InternalError (show e)
        Right v' -> go (return v') cIOs

-- Apply the second value to the first in left-to-right fashion.
apply :: P [IO Value] -> Value -> Value -> IO Value
-- If we have a function in program state, apply to the right
apply programParser (VFunction f) v = withPrettyError <$> applyF programParser f v BindFromLeft
-- If we have a value in program state and encounter a function, apply it
apply programParser v (VFunction f) = withPrettyError <$> applyF programParser f v BindFromRight
-- Application of lists tries to ziplist
apply programParser (VList as@((VFunction _) : _)) (VList bs) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF programParser <$> fs <*> bz <*> pure BindFromLeft
    return . VList . fmap withPrettyError . getZipList $ rs
-- Fork values with post-application of applicatives should work the same way
apply programParser (VList bs) (VList as@((VFunction _) : _)) =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
    rs <- sequence $ applyF programParser <$> fs <*> bz <*> pure BindFromRight
    return . VList . fmap withPrettyError . getZipList $ rs

-- If we have a value with a value, just override it
apply _ _ v = return v

-- Binds the next unbound value to that given
bindNext :: Function -> Value -> BindDir -> Function
bindNext f@(Function t valence impl args) v bindDir =
  case bindDir of
    BindFromLeft -> Function t valence impl (reverse . fst $ foldl' bindArg ([], False) args)
    BindFromRight -> Function t valence impl (fst $ foldr (flip bindArg) ([], False) args)
  where
    bindArg (args, True) a = (a : args, True)
    bindArg (args, False) a@(Bound _) = (a : args, False)
    bindArg (args, False) Unbound = (Bound v : args, True)

getUnbound :: Function -> [Argument]
getUnbound (Function _ _ _ args) = filter (== Unbound) args

getBound :: Function -> [Argument]
getBound (Function _ _ _ args) = filter (/= Unbound) args

-- Either partially bind this value, or if it's fully applied, evaluate it down
applyF :: P [IO Value] -> Function -> Value -> BindDir -> IO (Either CrossbowError Value)
applyF programParser f v bindDir = do
  let unbound = getUnbound f
  strictV <- deepEval programParser v
  case strictV of
    Left e -> return $ Left e
    Right strictV ->
      if
          | length unbound == 1 -> deepEval programParser (VFunction (bindNext f strictV bindDir))
          | length unbound > 1 -> return . Right $ VFunction (bindNext f strictV bindDir)
          | otherwise -> return . Left $ EvalError $ "Attempting to bind a fully bound function: " <> show (f, strictV)

unbind :: Argument -> Value
unbind (Bound v) = v

isIdentifier :: Value -> Bool
isIdentifier (VIdentifier _) = True
isIdentifier _ = False

runCBImpl :: P [IO Value] -> Text -> [Value] -> IO (Either CrossbowError Value)
runCBImpl programParser cbF argVals = do
  pE <- compile programParser cbF
  case pE of
    Left e -> return $ Left e
    Right f -> do
      result <- foldM (apply programParser) f argVals
      return $ Right result

evalF :: P [IO Value] -> Value -> IO (Either CrossbowError Value)
evalF programParser vf@(VFunction f@(Function _ _ impl args))
  -- If we're not fully bound, this is as far as we can go
  | not (null $ getUnbound f) = return $ Right vf
  -- If we have any bound identifier variables variables we can't eval down any further either
  | any isIdentifier argVals = return $ Right vf
  | otherwise =
    case impl of
      HSImpl hsF -> return . Right $ hsF programParser argVals
      HSImplIO hsF -> Right <$> hsF programParser argVals
      CBImpl cbF -> runCBImpl programParser cbF argVals
  where
    argVals = unbind <$> args
evalF _ v = return $ Right v

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
    (VFunction (Function n valence impl args)) -> do
      args' <- traverse (substituteBoundArg subs) args
      return $ VFunction (Function n valence impl args')
    _ -> return v
  where
    substituteBoundArg _ Unbound = return Unbound
    substituteBoundArg subs (Bound v) = Bound <$> substituteArgs subs (pure v)

deepEvalArg :: P [IO Value] -> Argument -> IO (Either CrossbowError Argument)
deepEvalArg programParser (Bound v) = Bound <$$> deepEval programParser v
deepEvalArg _ Unbound = return . return $ Unbound

-- Deeply evaluate the given value.
-- Ensure if this is a function that all arguments are strictly evaluated.
deepEval :: P [IO Value] -> Value -> IO (Either CrossbowError Value)
deepEval programParser (VFunction (Function name valence impl args)) =
  do
    (errors, argsStrict) <- partitionEithers <$> traverse (deepEvalArg programParser) args
    if not (null errors)
      then return . Left $ L.head errors
      else evalF programParser (VFunction (Function name valence impl argsStrict))
deepEval programParser (VList as) = do
  as <- traverse (deepEval programParser) as
  return . fmap VList $ sequence as
deepEval _ v = return $ Right v

mkHSImpl :: ([Value] -> Value) -> OpImpl
mkHSImpl f = HSImpl (\_ args -> f args)

mkHSImplIO :: ([Value] -> IO Value) -> OpImpl
mkHSImplIO f = HSImplIO (\_ args -> f args)

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, mkHSImpl (\[a, b] -> a + b))),
      ("++", (Valence 2, mkHSImpl (\[VList a, VList b] -> VList $ a ++ b))),
      ("*", (Valence 2, mkHSImpl (\[a, b] -> a * b))),
      ("^", (Valence 2, mkHSImpl (\[a, b] -> a ^ b))),
      ("-", (Valence 2, mkHSImpl (\[a, b] -> a - b))),
      ("abs", (Valence 1, mkHSImpl (\[a] -> abs a))),
      ("negate", (Valence 1, mkHSImpl (\[a] -> negate a))),
      ("mod", (Valence 2, mkHSImpl (\[a, b] -> a `mod` b))),
      ("div", (Valence 2, mkHSImpl (\[a, b] -> a `div` b))),
      ("==", (Valence 2, mkHSImpl (\[a, b] -> VBool $ a == b))),
      ("<=", (Valence 2, mkHSImpl (\[a, b] -> VBool $ a <= b))),
      ("<", (Valence 2, mkHSImpl (\[a, b] -> VBool $ a < b))),
      (">=", (Valence 2, mkHSImpl (\[a, b] -> VBool $ a >= b))),
      (">", (Valence 2, mkHSImpl (\[a, b] -> VBool $ a > b))),
      (":", (Valence 2, mkHSImpl (\[a, b] -> vCons a b))),
      ("min", (Valence 2, mkHSImpl (\[a, b] -> min a b))),
      ("max", (Valence 2, mkHSImpl (\[a, b] -> max a b))),
      ("minOn", (Valence 3, CBImpl "{map $0 [$1,$2]| monadic <= |if _ $1 $2}")),
      ("maxOn", (Valence 3, CBImpl "{map $0 [$1,$2]| monadic > |if _ $1 $2}")),
      -- TODO: Redefine all the below using crossbow folds, maps, filters
      ("id", (Valence 1, mkHSImpl (\[a] -> a))),
      ("const", (Valence 2, mkHSImpl (\[a, _] -> a))),
      ("cons", (Valence 2, mkHSImpl (\[a, b] -> vCons a b))),
      ("ix", (Valence 2, mkHSImpl (\[VInteger a, VList b] -> b !! fromInteger a))),
      ("drop", (Valence 2, mkHSImpl (\[VInteger n, VList as] -> VList (drop (fromIntegral n) as)))),
      ("take", (Valence 2, mkHSImpl (\[VInteger n, VList as] -> VList (take (fromIntegral n) as)))),
      ("head", (Valence 1, mkHSImpl (\[VList as] -> L.head as))),
      ("tail", (Valence 1, mkHSImpl (\[VList as] -> VList $ L.tail as))),
      -- TODO: Make zip variadic
      ("zip", (Valence 2, mkHSImpl (\[VList as, VList bs] -> VList ((\(a, b) -> VList [a, b]) <$> zip as bs)))),
      ("zip3", (Valence 3, mkHSImpl (\[VList as, VList bs, VList cs] -> VList ((\(a, b, c) -> VList [a, b, c]) <$> zip3 as bs cs)))),
      ("pairs", (Valence 1, CBImpl "{$0|fork 2|[id, drop 1]|monadic zip}")),
      ("square", (Valence 1, CBImpl "{$0|length|flip fork|$0}")),
      ("enum", (Valence 1, CBImpl "{$0|fork 2|[length,id]|[range 0, id]|monadic zip}")),
      ("lengthy", (Valence 2, CBImpl "{$1|length|($0==_)}")),
      ("windows", (Valence 2, CBImpl "{$1|square|enum|map (monadic drop)|map (take $0)|filter (lengthy $0)}")),
      ( "nap",
        ( Valence 3,
          HSImplIO
            ( \pp [VInteger n, VFunction f, VList as] -> do
                let vas = V.fromList as
                a' <- withPrettyError <$> applyF pp f (vas V.! fromInteger n) BindFromLeft
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
      ("range", (Valence 2, mkHSImpl (\[VInteger a, VInteger b] -> VList $ VInteger <$> [a .. b]))),
      ( "map",
        ( Valence 2,
          HSImplIO
            ( let map _ [_, VList []] = return $ VList []
                  map pp [VFunction f, VList (x : xs)] = do
                    x' <- applyF pp f x BindFromLeft
                    vCons (withPrettyError x') <$> map pp [VFunction f, VList xs]
               in map
            )
        )
      ),
      ("count", (Valence 2, CBImpl "{$1|filter $0|length}")),
      ( "filter",
        ( Valence 2,
          HSImplIO
            ( let filter _ [_, VList []] = return $ VList []
                  filter pp [VFunction f, VList (x : xs)] = do
                    x' <- withPrettyError <$> applyF pp f x BindFromLeft
                    if truthy x'
                      then vCons x <$> filter pp [VFunction f, VList xs]
                      else filter pp [VFunction f, VList xs]
               in filter
            )
        )
      ),
      ( "case",
        ( Valence 2,
          mkHSImpl
            ( \[a, VList xs] ->
                let Just v = foldl' (\acc (VList [k, v]) -> if k == a then Just v else acc) Nothing xs
                 in v
            )
        )
      ),
      ("if", (Valence 3, mkHSImpl (\[p, a, b] -> let (VBool p') = withPrettyError $ castToBool p in if p' then a else b))),
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
          HSImplIO
            ( \pp [VFunction f, acc, VList xs] ->
                foldlM
                  ( \acc x -> do
                      (VFunction f') <- withPrettyError <$> applyF pp f acc BindFromLeft
                      withPrettyError <$> applyF pp f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      ( "foldr",
        ( Valence 3,
          HSImplIO
            ( \pp [VFunction f, VList xs, acc] ->
                foldrM
                  ( \acc x -> do
                      (VFunction f') <- withPrettyError <$> applyF pp f acc BindFromLeft
                      withPrettyError <$> applyF pp f' x BindFromLeft
                  )
                  acc
                  xs
            )
        )
      ),
      ( "scanl",
        ( Valence 3,
          HSImplIO
            ( \pp [VFunction f, acc, VList xs] ->
                fmap VList . sequence $
                  scanl'
                    ( \accM x -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF pp f acc BindFromLeft
                        withPrettyError <$> applyF pp f' x BindFromLeft
                    )
                    (pure acc)
                    xs
            )
        )
      ),
      ( "scanr",
        ( Valence 3,
          HSImplIO
            ( \pp [VFunction f, acc, VList xs] ->
                fmap VList . sequence $
                  scanr
                    ( \x accM -> do
                        acc <- accM
                        (VFunction f') <- withPrettyError <$> applyF pp f acc BindFromLeft
                        withPrettyError <$> applyF pp f' x BindFromLeft
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
      ("transpose", (Valence 1, mkHSImpl (\[VList as] -> let unlist (VList l) = l in VList $ VList <$> transpose (unlist <$> as)))),
      -- TODO: Make flip work with other valences
      ( "flip",
        ( Valence 3,
          HSImplIO
            ( \pp [VFunction f, a, b] ->
                do
                  (VFunction f') <- withPrettyError <$> applyF pp f b BindFromLeft
                  withPrettyError <$> applyF pp f' a BindFromLeft
            )
        )
      ),
      ("reverse", (Valence 1, CBImpl "foldl (flip cons) [] _")),
      ( "ap",
        ( Valence 2,
          HSImplIO
            ( \pp [VFunction f, a] ->
                withPrettyError <$> applyF pp f a BindFromLeft
            )
        )
      ),
      ("fork", (Valence 2, mkHSImpl (\[n, a] -> let VInteger n' = withPrettyError . castToInt $ n in VList (replicate (fromInteger n') a)))),
      ( "monadic",
        ( Valence 2,
          HSImplIO
            ( \pp [VFunction f, VList args] ->
                foldlM
                  ( \v a -> do
                      case v of
                        VFunction f ->
                          withPrettyError <$> applyF pp f a BindFromLeft
                        _ -> return v
                  )
                  (VFunction f)
                  args
            )
        )
      ),
      ("lines", (Valence 1, mkHSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.lines (unchar <$> t)))))),
      ("words", (Valence 1, mkHSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> ST.words (unchar <$> t)))))),
      ("ints", (Valence 1, CBImpl "{lines|int}")),
      ("int", (Valence 1, mkHSImpl (\[a] -> withPrettyError . castToInt $ a))),
      ("double", (Valence 1, mkHSImpl (\[a] -> withPrettyError . castToDouble $ a))),
      ("char", (Valence 1, mkHSImpl (\[a] -> withPrettyError . castToChar $ a))),
      ("bool", (Valence 1, mkHSImpl (\[a] -> withPrettyError . castToBool $ a))),
      ("string", (Valence 1, mkHSImpl (\[a] -> VList $ VChar <$> T.unpack (asText a)))),
      ( "counts",
        ( Valence 1,
          mkHSImpl
            ( \[VList as] ->
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
          mkHSImpl
            ( \[VList as] -> VInteger $ sum [2 ^ i | (i, b) <- zip [0 ..] (reverse as), truthy b]
            )
        )
      ),
      ( "read",
        ( Valence 1,
          mkHSImplIO
            ( \[a] -> do
                t <- readFile (T.unpack $ asText a)
                return (VList $ VChar <$> t)
            )
        )
      ),
      ( "input",
        ( Valence 0,
          mkHSImplIO
            ( \_ -> do
                t <- T.unpack <$> getLine
                return (VList $ VChar <$> t)
            )
        )
      )
    ]
