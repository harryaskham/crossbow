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
    programContext = ProgramContext program builtins
    -- TODO: Yep, run the below with StateT
    loop = do
      inputM <- getInputLine "|-> "
      case inputM of
        Nothing -> return ()
        Just input -> do
          pE <- liftIO (runReaderT (compile (T.pack input)) programContext)
          case pE of
            Right resultIO -> do
              result <- liftIO resultIO
              putTextLn (pretty result)
            Left e -> liftIO $ putTextLn (pretty e)
          loop
