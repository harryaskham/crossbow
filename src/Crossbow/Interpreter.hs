module Crossbow.Interpreter where

import Crossbow.Types
import Data.Either.Extra (fromRight')

-- TODO: Expand to include all kinds of things like arrow branching, etc
data ProgramState = ProgramState (Maybe Value)

run :: Program -> IO (Maybe Value)
run program =
  let (ProgramState v) = go (ProgramState Nothing) program
   in return v
  where
    go ps (Program []) = ps
    go ps (Program (c : cs)) = go (runClause ps c) (Program cs)

runClause :: ProgramState -> Clause -> ProgramState
-- If we're running a function / defining a constant on no state, whatever this is becomes the state
runClause (ProgramState Nothing) (CLValue v) = ProgramState $ Just v
-- Running a function on state applies it
runClause (ProgramState (Just ps)) (CLValue v) = ProgramState $ Just (apply ps v)

-- Apply the second value to the first in left-to-right fashion.
apply :: Value -> Value -> Value
-- Two functions compose together, if possible
apply (VFunction _) (VFunction _) = error "todo: compose functions"
-- If we have a function in program state, apply to the right
apply (VFunction f) v = fromRight' $ applyF f v
-- If we have a value in program state and encounter a function, apply it
apply v (VFunction f) = fromRight' $ applyF f v
-- If we have a value with a value, just override it
apply _ v = v

data ApplyValenceError = ApplyValenceError

-- Binds the next unbound value to that given
bindNext :: Function -> Value -> Function
bindNext (Function op args) v = Function op (bound ++ ((Bound v) : replicate numUnbound Unbound))
  where
    bound = takeWhile (/= Unbound) args
    numUnbound = length args - length bound - 1

getUnbound :: Function -> [Argument]
getUnbound (Function _ args) = filter (== Unbound) args

-- Either partially bind this value, or if it's fully applied, evaluate it down
applyF :: Function -> Value -> Either ApplyValenceError Value
applyF f value
  | null unbound = Left ApplyValenceError
  | length unbound == 1 = Right (fromRight' $ evalF (bindNext f value))
  | length unbound > 1 = Right (VFunction $ bindNext f value)
  where
    unbound = getUnbound f

data EvalPartialError = EvalPartialError

unbind :: Argument -> Value
unbind (Bound v) = v
unbind Unbound = error "Unbinding unbound"

evalF :: Function -> Either EvalPartialError Value
evalF f@(Function (Operator opType (Valence valence)) args)
  | not (null $ getUnbound f) = Left EvalPartialError
  | valence == 2 =
    let [a, b] = argVals
     in case opType of
          OPAdd -> Right $ a <> b
  | otherwise = error "Invalid valence / op combo"
  where
    argVals = unbind <$> args
