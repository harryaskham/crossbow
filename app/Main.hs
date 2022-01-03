module Main where

import Crossbow.Execute

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> repl
    [path] -> do
      _ <- runFile path
      return ()
    _ -> putTextLn "'crossbow <path>' to execute a file; 'crossbow repl' to enter the REPL"
