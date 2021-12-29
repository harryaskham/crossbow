module Crossbow.Interpreter where

import Crossbow.Types
import Crossbow.Util
import Data.Either.Extra (fromRight')
import Data.Foldable (foldl1)
import Data.List ((!!))
import Data.Map.Strict qualified as M
import Data.String qualified
import Data.Text qualified as T
import Data.Text.Read (decimal, double, signed)
import Data.Text.Read qualified as TR
import GHC.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec hiding (many, (<|>))
import Prelude hiding (optional)

data CrossbowParseError
  = ArgNumError
  | FullApplicationError CrossbowEvalError
  | UncaughtParseError ParseError
  deriving (Show)

type P = GenParser Char ()

compile :: Text -> Either CrossbowParseError Program
compile t = case parse program "" . T.unpack $ t of
  Left e -> Left $ UncaughtParseError e
  Right p -> Right p

compileUnsafe :: Text -> Program
compileUnsafe = fromRight' . compile

-- Parse one of the things given, backtracking on failure
firstOf :: [P a] -> P a
firstOf = foldl1 (<|>) . fmap try

ignoreSpaces :: P a -> P a
ignoreSpaces p = spaces *> p <* spaces

clauseDivider :: P Divider
clauseDivider =
  ignoreSpaces $
    (char '|' >> return ForwardDiv)
      <|> (string "<|" >> return BackwardDiv)
      <|> return NoDiv

program :: P Program
program = Program <$> many clause

clause :: P Clause
clause = CLValue <$> value <*> clauseDivider

value :: P Value
value =
  firstOf
    [ vRange,
      vFuncL,
      --vFuncR,
      vFunc,
      vList,
      vNumber,
      vChar,
      vString,
      vBool
    ]
  where
    vNumber = do
      x <- T.pack <$> ignoreSpaces (many1 (oneOf "-.0123456789"))
      if "." `T.isInfixOf` x
        then return . VDouble $ readOne (signed double) x
        else return . VInteger $ readOne (signed decimal) x
    vList = VList <$> between (char '[') (char ']') (value `sepBy` char ',')
    vChar = VChar <$> between (char '\'') (char '\'') anyChar
    vString = VList <$> between (char '"') (char '"') (many (VChar <$> noneOf "\""))
    vBool = VBool <$> ((string "False" *> return False) <|> (string "True" *> return True))
    vRange = do
      VInteger a <- ignoreSpaces (castToInt <$> vNumber)
      ignoreSpaces (string ":")
      VInteger b <- ignoreSpaces (castToInt <$> vNumber)
      return $ VList (VInteger <$> [a .. b])
    -- TODO: Uses unsafePerformIO to reduce functions as we parse
    -- This will break e.g. fully applied getline
    maybeApply :: Function -> Either CrossbowParseError Value
    maybeApply f
      | null (getUnbound f) =
        case unsafePerformIO $ evalF f of
          Left e -> Left $ FullApplicationError e
          Right v -> Right v
      | otherwise = Right $ VFunction f
    vFuncL =
      do
        op <- operator
        args <- many1 ((Bound <$> value) <|> (char '_' $> Unbound))
        case mkFuncL op args of
          Left e -> fail (show e)
          Right f -> case maybeApply f of
            Left e -> fail (show e)
            Right v -> return v
    -- Only allow literal partial right application to avoid infinite parsing
    -- TODO: Disabled in favour of consistent left application
    -- vLiteral = firstOf [vList, vNumber]
    -- vFuncR = maybeApply . fromRight' <$> (flip mkFuncR <$> many1 vLiteral <*> operator)
    -- Finally, a func with no arguments
    vFunc =
      do
        op <- operator
        case maybeApply $ mkFunc op of
          Left e -> fail (show e)
          Right v -> return v

mkFunc :: Operator -> Function
mkFunc o@(Operator _ (Valence v)) = Function o (replicate v Unbound)

mkFuncL :: Operator -> [Argument] -> Either CrossbowParseError Function
mkFuncL o@(Operator _ (Valence v)) args
  | length args /= v = Left ArgNumError
  | otherwise = Right $ Function o args

operator :: P Operator
operator = ignoreSpaces $ do
  -- We parse in favour of longer names first to avoid max/maximum clashes
  k <- T.pack <$> firstOf (string . T.unpack <$> (sortOn (Down . T.length) (M.keys builtins)))
  let (v, _) = builtins M.! k
  return $ Operator (OpType k) v

-- TODO: Re-split parser and interpreter; below here is interpreter, above is parser

-- TODO: Expand to include all kinds of things like arrow branching, etc
data ProgramState = ProgramState (Maybe Value) deriving (Show)

run :: Program -> IO (Maybe Value)
run program = runWith program (ProgramState Nothing)

runWith :: Program -> ProgramState -> IO (Maybe Value)
runWith program ps = do
  (ProgramState v) <- go ps program
  return v
  where
    go ps (Program []) = return ps
    go ps (Program (c : cs)) = do
      ps' <- runClause ps c
      go ps' (Program cs)

runClause :: ProgramState -> Clause -> IO ProgramState
-- If we're running a function / defining a constant on no state, whatever this is becomes the state
runClause (ProgramState Nothing) (CLValue v _) = return . ProgramState $ Just v
-- Running a function on state applies it
runClause (ProgramState (Just ps)) (CLValue v div) = ProgramState . Just <$> apply ps v div

data BindDir = BindFromLeft | BindFromRight

divToDir :: Divider -> BindDir
divToDir ForwardDiv = BindFromLeft
divToDir NoDiv = BindFromLeft
divToDir BackwardDiv = BindFromRight

reverseDir :: BindDir -> BindDir
reverseDir BindFromLeft = BindFromRight
reverseDir BindFromRight = BindFromLeft

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> Divider -> IO Value
-- Two functions compose together, if possible
-- TODO: Disabled to enable map; functions are just objects too...
-- apply (VFunction _) (VFunction _) = error "todo: compose functions"
-- If we have a function in program state, apply to the right
apply (VFunction f) v div = fromRight' <$> applyF f v (divToDir div)
-- If we have a value in program state and encounter a function, apply it
apply v (VFunction f) div = fromRight' <$> applyF f v (reverseDir $ divToDir div)
-- Application of lists tries to ziplist
apply (VList as@((VFunction _) : _)) (VList bs) div =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
        dir = divToDir div
    rs <- sequence $ applyF <$> fs <*> bz <*> pure dir
    return . VList . fmap fromRight' . getZipList $ rs
-- Fanout values with post-application of applicatives should work the same way
apply (VList bs) (VList as@((VFunction _) : _)) div =
  do
    let unwrap (VFunction f) = f
        fs = ZipList (unwrap <$> as)
        bz = ZipList bs
        dir = reverseDir (divToDir div)
    rs <- sequence $ applyF <$> fs <*> bz <*> pure dir
    return . VList . fmap fromRight' . getZipList $ rs

-- If we have a value with a value, just override it
apply _ v _ = return v

data ApplyValenceError = ApplyValenceError

-- Binds the next unbound value to that given
bindNext :: Function -> Value -> BindDir -> Function
bindNext f@(Function op args) v bindDir =
  case bindDir of
    BindFromLeft -> Function op (reverse . fst $ foldl' bindArg ([], False) args)
    BindFromRight -> Function op (fst $ foldr (flip bindArg) ([], False) args)
  where
    bindArg (args, True) a = (a : args, True)
    bindArg (args, False) a@(Bound _) = (a : args, False)
    bindArg (args, False) Unbound = (Bound v : args, True)

getUnbound :: Function -> [Argument]
getUnbound (Function _ args) = filter (== Unbound) args

getBound :: Function -> [Argument]
getBound (Function _ args) = filter (/= Unbound) args

-- Either partially bind this value, or if it's fully applied, evaluate it down
applyF :: Function -> Value -> BindDir -> IO (Either ApplyValenceError Value)
applyF f value bindDir
  | null unbound = return $ Left ApplyValenceError
  | length unbound == 1 = do
    resultE <- evalF (bindNext f value bindDir)
    return $ Right (fromRight' resultE)
  | length unbound > 1 = return . Right $ VFunction (bindNext f value bindDir)
  where
    unbound = getUnbound f

data CrossbowEvalError = EvalError Text deriving (Show)

unbind :: Argument -> Value
unbind (Bound v) = v
unbind Unbound = error "Unbinding unbound"

evalF :: Function -> IO (Either CrossbowEvalError Value)
evalF f@(Function (Operator (OpType t) (Valence v)) args)
  | not (null $ getUnbound f) = return . Left $ EvalError "Can't evaluate with unbound variables"
  | otherwise =
    case M.lookup t builtins of
      Nothing -> return . Left $ EvalError ("Unsupported opType: " <> t)
      Just (_, HSImpl hsF) ->
        if length argVals == v
          then return . Right $ hsF argVals
          else return . Left $ EvalError "Can't run HS impl without correct valence"
      Just (_, HSImplIO hsF) ->
        if length argVals == v
          then Right <$> hsF argVals
          else return . Left $ EvalError "Can't run HS impl without correct valence"
      Just (_, CBImpl cbF) ->
        case argVals of
          [arg] -> do
            resultM <- runWith cbF (ProgramState (Just arg))
            case resultM of
              Nothing -> return . Left $ EvalError "Unknown failure running CBImpl"
              Just result -> return . Right $ result
          _ -> return . Left $ EvalError "Can only run CB impl monadically"
  where
    argVals = unbind <$> args

-- Helper to pass through a Haskell function to the builtins
passthrough2 :: Text -> (Value -> Value -> Value) -> (Text, (Valence, OpImpl))
passthrough2 name f = (name, (Valence 2, HSImpl (\[a, b] -> f a b)))

builtins :: Map Text (Valence, OpImpl)
builtins =
  M.fromList
    [ ("+", (Valence 2, HSImpl (\[a, b] -> a <> b))),
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
      ("head", (Valence 1, HSImpl (\[VList as] -> as !! 0))),
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
      ("sum", (Valence 1, CBImpl (compileUnsafe "fold|+|0"))),
      -- TODO:
      -- define head, tail
      -- need applicative style for lists of functions
      -- then fold1 using fanout on input to do fold|f|head|tail
      -- then redefine maximum and minimum in terms of fold1
      ("maximum", (Valence 1, CBImpl (compileUnsafe "fold|max|-1"))),
      ( "fold",
        ( Valence 3,
          HSImplIO
            ( let fold [_, acc, VList []] = return acc
                  fold [VFunction f, acc, VList (x : xs)] = do
                    (VFunction f') <- fromRight' <$> applyF f acc BindFromLeft
                    acc' <- fromRight' <$> applyF f' x BindFromLeft
                    fold [VFunction f, acc', VList xs]
               in fold
            )
        )
      ),
      ("fanout", (Valence 2, HSImpl (\[n, a] -> let VInteger n' = castToInt n in VList (replicate (fromInteger n') a)))),
      ( "uncurry2",
        ( Valence 2,
          HSImplIO
            ( \[VFunction f, VList args] -> do
                (VFunction f') <- fromRight' <$> applyF f (args !! 0) BindFromLeft
                f'' <- fromRight' <$> applyF f' (args !! 1) BindFromLeft
                return f''
            )
        )
      ),
      ("lines", (Valence 1, HSImpl (\[VList t] -> let unchar (VChar c) = c in VList (VList <$> (VChar <$$> Data.String.lines (unchar <$> t)))))),
      -- TODO: Flip; needs notion of a lambda first
      ("int", (Valence 1, HSImpl (\[a] -> castToInt a))),
      ("double", (Valence 1, HSImpl (\[a] -> castToDouble a))),
      ("char", (Valence 1, HSImpl (\[a] -> castToChar a))),
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
