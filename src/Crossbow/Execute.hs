module Crossbow.Execute where

import Control.Exception
import Crossbow.Evaluator
import Crossbow.Parser
import Crossbow.Types
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import System.Console.Haskeline

searchFunc :: String -> Eval [Completion]
searchFunc str = do
  builtins <- gets _builtins
  return $ simpleCompletion <$> filter (str `isPrefixOf`) (T.unpack <$> M.keys builtins)

settings :: Settings Eval
settings =
  Settings
    { historyFile = Just ".crossbow_history",
      complete = completeWord Nothing " \t" searchFunc,
      autoAddHistory = True
    }

repl :: IO ()
repl = do
  _ <-
    flip evalStateT programContext
      . runInputT settings
      $ loop
  print "Exiting"
  where
    programContext = ProgramContext program builtins
    loop :: (InputT (StateT ProgramContext IO) ())
    loop = do
      inputM <- getInputLine "|-> "
      _ <- case inputM of
        Nothing -> return ()
        Just input -> do
          pE <- lift $ compile (T.pack input)
          case pE of
            Right result -> liftIO $ putTextLn (T.intercalate "\n" (pretty <$> result))
            Left e -> liftIO $ putTextLn (pretty e)
      loop
