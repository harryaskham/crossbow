module Main where

import Crossbow.Evaluator
import Crossbow.Execute
import Crossbow.Parser
import Crossbow.Types (ProgramContext (ProgramContext))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> repl
    [path] -> do
      _ <- runStateT (runFile path) (ProgramContext program builtins False)
      return ()
    _ -> putTextLn "'crossbow <path>' to execute a file; 'crossbow repl' to enter the REPL"
