module Main where

import Control.Exception
import Crossbow.Evaluator
import Crossbow.Parser
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

main :: IO ()
main = runInputT (defaultSettings {historyFile = Just ".crossbow_history", autoAddHistory = True}) loop
  where
    programParser = runReader program builtins
    loop = do
      inputM <- getInputLine "|-> "
      case inputM of
        Nothing -> return ()
        Just input -> do
          pE <- liftIO (compile programParser (T.pack input))
          case pE of
            Right result -> putTextLn (pretty result)
            Left e -> liftIO $ putTextLn (pretty e)
          loop
