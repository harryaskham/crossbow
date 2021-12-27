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

-- TODO: add IO here
runClause :: ProgramState -> Clause -> ProgramState
runClause _ (CLConstant v) = ProgramState (Just v)
runClause (ProgramState Nothing) (CLOperation op v) = error "TODO: an op on nothing should have default behaviour"
runClause (ProgramState (Just b)) (CLOperation OPAdd a) = ProgramState $ Just (a <> b)
