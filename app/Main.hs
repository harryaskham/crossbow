module Main where

import Control.Exception
import Crossbow.Evaluator
import Crossbow.Parser
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

main :: IO ()
main = do
  _ <-
    flip evalStateT programContext
      . runInputT (defaultSettings {historyFile = Just ".crossbow_history", autoAddHistory = True})
      $ loop
  print "Exiting"
  where
    programContext = ProgramContext clauses builtins
    loop :: (InputT (StateT ProgramContext IO) ())
    loop = do
      inputM <- getInputLine "|-> "
      _ <- case inputM of
        Nothing -> return ()
        Just input -> do
          pE <- lift $ compile (T.pack input)
          case pE of
            Right result -> liftIO $ putTextLn (pretty result)
            Left e -> liftIO $ putTextLn (pretty e)
      loop
