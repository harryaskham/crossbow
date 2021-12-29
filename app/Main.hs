module Main where

import Crossbow.Interpreter
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

debug :: Bool
debug = False

main :: IO ()
main = runInputT (defaultSettings {historyFile = Just ".crossbow_history", autoAddHistory = True}) loop
  where
    loop = do
      inputM <- getInputLine "|-> "
      case inputM of
        Nothing -> return ()
        Just input -> do
          case compile (T.pack input) of
            Right resultIO -> do
              result <- liftIO resultIO
              when debug (liftIO $ print result)
              putTextLn (pretty result)
            Left e -> liftIO $ print (show e)
          loop
