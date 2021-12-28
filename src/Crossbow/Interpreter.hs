module Crossbow.Interpreter where

import Crossbow.Types
import Data.Either.Extra (fromRight')
import Data.Map.Strict qualified as M

-- TODO: Expand to include all kinds of things like arrow branching, etc
data ProgramState = ProgramState (Maybe Value)

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
runClause (ProgramState Nothing) (CLValue v) = return . ProgramState $ Just v
-- Running a function on state applies it
runClause (ProgramState (Just ps)) (CLValue v) = ProgramState . Just <$> apply ps v

data BindDir = BindFromLeft | BindFromRight

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> IO Value
-- Two functions compose together, if possible
apply (VFunction _) (VFunction _) = error "todo: compose functions"
-- If we have a function in program state, apply to the right
apply (VFunction f) v = fromRight' <$> applyF f v BindFromLeft
-- If we have a value in program state and encounter a function, apply it
apply v (VFunction f) = fromRight' <$> applyF f v BindFromRight
-- If we have a value with a value, just override it
apply _ v = return v

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

data EvalError = EvalError Text

unbind :: Argument -> Value
unbind (Bound v) = v
unbind Unbound = error "Unbinding unbound"

evalF :: Function -> IO (Either EvalError Value)
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
