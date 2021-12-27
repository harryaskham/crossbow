module Main where

import Crossbow.Interpreter
import Crossbow.Parser
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      inputM <- getInputLine "|-> "
      case inputM of
        Nothing -> return ()
        Just input -> do
          case compile (T.pack input) of
            Right program -> do
              result <- liftIO $ run program
              print result
            Left e -> liftIO $ print (show e)
          loop
