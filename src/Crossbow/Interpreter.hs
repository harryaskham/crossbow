module Crossbow.Interpreter where

import Crossbow.Types

-- TODO: Expand to include all kinds of things like arrow branching, etc
data ProgramState = ProgramState (Maybe Value)

run :: Program -> IO (Maybe Value)
run program =
  let (ProgramState v) = go (ProgramState Nothing) program
   in return v
  where
    go ps (Program []) = ps
    go ps (Program (c : cs)) = go (runClause ps c) (Program cs)

-- The default behaviour when doing this operation on no state
defOp :: Operator -> Value -> Value
defOp OPAdd v = v

-- TODO: add IO here
runClause :: ProgramState -> Clause -> ProgramState
runClause _ (CLValue v) = ProgramState (Just v)
runClause (ProgramState Nothing) (CLOperation op v) = ProgramState $ Just (defOp op v)
runClause (ProgramState (Just a)) (CLOperation OPAdd b) = ProgramState $ Just (a <> b)
