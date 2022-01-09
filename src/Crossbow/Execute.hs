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

--replPretty :: PrettyTruncated a => a -> Text
--replPretty = prettyTruncated

replPretty :: Pretty a => a -> Text
replPretty = pretty

repl :: IO ()
repl = do
  prefs <- readPrefs ".haskeline"
  _ <-
    flip evalStateT programContext
      . runInputTWithPrefs prefs settings
      $ runRepl
  print "Exiting"
  where
    programContext = ProgramContext program builtins False
    runRepl :: (InputT (StateT ProgramContext IO) ())
    runRepl = do
      lift loadStdLib
      loop
    loop :: (InputT (StateT ProgramContext IO) ())
    loop = do
      inputM <- getInputLine "|-> "
      _ <- case inputM of
        Nothing -> return ()
        Just input -> do
          pE <- lift $ compile (T.pack input)
          case pE of
            Right result -> liftIO $ putTextLn (T.intercalate "\n" (replPretty <$> result))
            Left e -> liftIO $ putTextLn (pretty e)
      loop
