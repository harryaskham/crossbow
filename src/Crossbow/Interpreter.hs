module Crossbow.Interpreter where

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
import Data.Vector.Fusion.Stream.Monadic (foldl1M')
import GHC.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Prelude hiding (optional)

data CrossbowParseError
  = ArgNumError
  | FullApplicationError CrossbowEvalError
  | UncaughtParseError ParseError
  deriving (Show)

type P = GenParser Char ()

compile :: Text -> Either CrossbowParseError (IO Value)
compile t = case parse program "" . T.unpack $ t of
  Left e -> Left $ UncaughtParseError e
  Right p -> Right p

compileUnsafe :: Text -> IO Value
compileUnsafe = fromRight' . compile

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

clauseDivider :: P Char
clauseDivider = ignoreSpaces (char '|')

clauses :: P [IO Value]
clauses = (pure <$> value) `sepBy1` clauseDivider

runClauses :: [IO Value] -> IO Value
runClauses (c : cs) = do
  foldl'
    ( \vIO cIO -> do
        v <- vIO
        c <- cIO
        apply v c
    )
    c
    cs

program :: P (IO Value)
program = do
  cs <- clauses
  return $ runClauses cs

inParens :: P a -> P a
inParens = between (char '(') (char ')')

value :: P Value
value =
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
        then return . VDouble $ readOne (signed double) x
        else return . VInteger $ readOne (signed decimal) x
    vNumber = (try vNegativeNumber) <|> (try vPositiveNumber)
    vList = VList <$> between (char '[') (char ']') (value `sepBy` char ',')
    vChar = VChar <$> between (char '\'') (char '\'') anyChar
    vString = VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool = VBool <$> ((string "False" $> False) <|> (string "True" $> True))
    vRange = do
      VInteger a <- ignoreSpaces (castToInt <$> vNumber)
      ignoreSpaces (string ":")
      VInteger b <- ignoreSpaces (castToInt <$> vNumber)
      return $ VList (VInteger <$> [a .. b])
    -- TODO: Uses unsafePerformIO to reduce functions as we parse
    -- This will break e.g. fully applied getline
    -- TODO: Change this to a P (IO Value)
    maybeApply f
      | null (getUnbound f) =
        case unsafePerformIO $ evalF (VFunction f) of
          Left e -> fail (show e)
          Right v -> return v
      | otherwise = return $ VFunction f
    arg = ignoreSpaces $ ((Bound <$> value) <|> (char '_' $> Unbound))
    vFuncBin =
      do
        char '('
        a1 <- arg
        op <- operator
        a2 <- arg
        char ')'
        case mkFuncL op [a1, a2] of
          Left e -> fail (show e)
          Right f -> maybeApply f
    -- A polish notation left-applied func with maybe unbound variables
    vFuncL =
      do
        op <- operator
        args <- many1 arg
        case mkFuncL op args of
          Left e -> fail (show e)
          Right f -> maybeApply f
    -- Finally, a func with no arguments
    vFunc = do
      op <- operator
      let f = mkFunc op
      maybeApply f
    vFunction =
      firstOf
        [ vFuncL,
          vFuncBin,
          vFunc
        ]
    -- TODO: Lambdas of more than one variable
    vLambda = do
      char '{'
      cs <- clauses
      char '}'
      return $
        VFunction
          ( Function
              Nothing
              ( HSImplIO
                  ( \args -> do
                      print "running lambda"
                      let cs' = substituteArgs args <$> cs
                      runClauses cs'
                  )
              )
              [Unbound] -- TODO: THIS ONLY ALLOWS 1-LAMBDAS
          )

maxArgIx :: Value -> Maybe Int
maxArgIx i@(VIdentifier _) = Just $ identifierIx i
maxArgIx (VFunction f@(Function _ _ _)) =
  case mapMaybe maxArgIx $ unbind <$> getBound f of
    [] -> Nothing
    ixs -> Just $ L.maximum ixs
maxArgIx _ = Nothing

mkFunc :: Operator -> Function
mkFunc (Operator (OpType t) (Valence v)) =
  let (_, impl) = builtins M.! t
   in Function (Just t) impl (replicate v Unbound)

mkFuncL :: Operator -> [Argument] -> Either CrossbowParseError Function
mkFuncL (Operator (OpType t) (Valence v)) args
  | length args > v = Left ArgNumError
  | length args < v = Right $ Function (Just t) impl (args ++ replicate (v - length args) Unbound)
  | otherwise = Right $ Function (Just t) impl args
  where
    (_, impl) = builtins M.! t

operator :: P Operator
operator = ignoreSpaces $ do
  -- We parse in favour of longer names first to avoid max/maximum clashes
  k <- T.pack <$> firstOf (string . T.unpack <$> (sortOn (Down . T.length) (M.keys builtins)))
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
    Left e -> error (show e)
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

data ApplyValenceError = ApplyValenceError deriving (Show)

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
applyF :: Function -> Value -> BindDir -> IO (Either ApplyValenceError Value)
applyF f value bindDir
  | length unbound == 1 = do
    resultE <- evalF (VFunction (bindNext f value bindDir))
    return $ Right (fromRight' resultE)
  | length unbound > 1 = return . Right $ VFunction (bindNext f value bindDir)
  | null unbound = return $ Left ApplyValenceError
  where
    unbound = traceShow (f, value) $ getUnbound f

data CrossbowEvalError = EvalError Text deriving (Show)

unbind :: Argument -> Value
unbind (Bound v) = v
unbind Unbound = error "Unbinding unbound"

isIdentifier :: Value -> Bool
isIdentifier (VIdentifier _) = True
isIdentifier _ = False

evalF :: Value -> IO (Either CrossbowEvalError Value)
evalF vf@(VFunction f@(Function _ impl args))
  | not (null $ getUnbound f) = return . Left $ EvalError "Can't evaluate with unbound variables"
  -- If we have any unbound variables we can't eval down any further
  | any isIdentifier argVals = return $ Right vf
  | otherwise =
    case impl of
      HSImpl hsF -> return . Right $ hsF argVals
      HSImplIO hsF -> Right <$> hsF argVals
      CBImpl cbF ->
        case argVals of
          [arg] -> do
            f <- cbF
            result <- apply f arg
            return $ Right result
          _ -> return . Left $ EvalError "Can only run CB impl monadically"
  where
    argVals = unbind <$> args
evalF v = return $ Right v

identifierIx :: Value -> Int
identifierIx (VIdentifier i) = readOne decimal (T.drop 1 i)

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

-- Evaluate the given value (probably a lambda) after substituting identifiers
-- TODO: Only subs, needs to eval still
evalWithContext :: [Value] -> Value -> IO (Either CrossbowEvalError Value)
evalWithContext args v = evalF =<< substituteArgs args (pure v)

-- Helper to pass through a Haskell function to the builtins
passthrough2 :: Text -> (Value -> Value -> Value) -> (Text, (Valence, OpImpl))
passthrough2 name f = (name, (Valence 2, HSImpl (\[a, b] -> f a b)))

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, HSImpl (\[a, b] -> a <> b))),
      ("*", (Valence 2, HSImpl (\[a, b] -> a * b))),
      ("-", (Valence 2, HSImpl (\[a, b] -> a - b))),
      ("mod", (Valence 2, HSImpl (\[a, b] -> a `mod` b))),
      ("div", (Valence 2, HSImpl (\[a, b] -> a `div` b))),
      ("<=", (Valence 2, HSImpl (\[a, b] -> VBool $ a <= b))),
      ("<", (Valence 2, HSImpl (\[a, b] -> VBool $ a < b))),
      (">=", (Valence 2, HSImpl (\[a, b] -> VBool $ a >= b))),
      (">", (Valence 2, HSImpl (\[a, b] -> VBool $ a > b))),
      passthrough2 "max" max,
      passthrough2 "min" min,
      -- TODO: Redefine all the below using crossbow folds, maps, filters
      ("id", (Valence 1, HSImpl (\[a] -> a))),
      ("const", (Valence 2, HSImpl (\[a, _] -> a))),
      ("drop", (Valence 2, HSImpl (\[VInteger n, VList as] -> VList (drop (fromIntegral n) as)))),
      ("take", (Valence 2, HSImpl (\[VInteger n, VList as] -> VList (take (fromIntegral n) as)))),
      ("head", (Valence 1, HSImpl (\[VList as] -> L.head as))),
      ("tail", (Valence 1, HSImpl (\[VList as] -> VList $ L.tail as))),
      ("zip", (Valence 2, HSImpl (\[VList as, VList bs] -> VList ((\(a, b) -> VList [a, b]) <$> zip as bs)))),
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
      ("if", (Valence 3, HSImpl (\[VBool p, a, b] -> if p then a else b))),
      ("aoc", (Valence 1, CBImpl (compileUnsafe "{$0|string|(\"test/aoc_input/\"+_)|(_+\".txt\")|read}"))),
      ("sum", (Valence 1, CBImpl (compileUnsafe "foldl|+|0"))),
      ("odd", (Valence 1, CBImpl (compileUnsafe "{$0|mod _ 2|bool}"))),
      ("even", (Valence 1, CBImpl (compileUnsafe "{$0|odd|not}"))),
      ("not", (Valence 1, CBImpl (compileUnsafe "if _ False True"))),
      -- TODO:
      -- define head, tail
      -- need applicative style for lists of functions
      -- then fold1 using fork on input to do fold|f|head|tail
      -- then redefine maximum and minimum in terms of fold1
      ("maximum", (Valence 1, CBImpl (compileUnsafe "foldl|max|(-1)"))),
      ("length", (Valence 1, CBImpl (compileUnsafe "foldl (flip const (+1) _) 0"))),
      ( "foldl",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, acc, VList xs] ->
                foldlM
                  ( \acc x -> do
                      (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                      acc' <- fromRight' <$> applyF f' x BindFromLeft
                      return acc'
                  )
                  acc
                  xs
            )
        )
      ),
      --("foldl1" (Valence 2, CBImpl (compileUnsafe "[f,xs]|[id,fork 2]|[id,[head,tail]]|
      ( "foldr",
        ( Valence 3,
          HSImplIO
            ( \[VFunction f, VList xs, acc] ->
                foldrM
                  ( \acc x -> do
                      (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                      acc' <- fromRight' <$> applyF f' x BindFromLeft
                      return acc'
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
      ("cons", (Valence 2, HSImpl (\[a, as] -> vCons a as))),
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
      ("reverse", (Valence 1, CBImpl (compileUnsafe "foldl (flip cons) [] _"))),
      ( "ap",
        ( Valence 2,
          HSImplIO
            ( \[VFunction f, a] ->
                fromRight' <$> applyF f a BindFromLeft
            )
        )
      ),
      ("fork", (Valence 2, HSImpl (\[n, a] -> let VInteger n' = castToInt n in VList (replicate (fromInteger n') a)))),
      ( "monadic",
        ( Valence 2,
          HSImplIO
            ( \[VFunction f, VList args] ->
                foldlM
                  ( \v a -> do
                      case v of
                        VFunction f -> do
                          v <- fromRight' <$> applyF f a BindFromLeft
                          return v
                        _ -> return v
                  )
                  (VFunction f)
                  args
            )
        )
      ),
      ("lines", (Valence 1, HSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> Data.String.lines (unchar <$> t)))))),
      -- TODO: Flip; needs notion of a lambda first
      ("int", (Valence 1, HSImpl (\[a] -> castToInt a))),
      ("double", (Valence 1, HSImpl (\[a] -> castToDouble a))),
      ("char", (Valence 1, HSImpl (\[a] -> castToChar a))),
      ("bool", (Valence 1, HSImpl (\[a] -> VBool $ truthy a))),
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
