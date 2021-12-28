module Main where

import Crossbow.Interpreter
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

debug :: Bool
debug = True

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
              when debug (liftIO $ print program)
              result <- liftIO $ run program
              case result of
                Nothing -> print "Unknown error"
                Just r -> putTextLn (pretty r)
            Left e -> liftIO $ print (show e)
          loop
