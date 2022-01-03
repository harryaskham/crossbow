module Main where

import Control.Exception
import Crossbow.Evaluator
import Crossbow.Parser
import Crossbow.Types
import Data.Text qualified as T
import System.Console.Haskeline

runFile :: FilePath -> IO (Either CrossbowError [Value])
runFile path = do
  t <- T.pack <$> readFile path
  let pc = ProgramContext program builtins
  flip evalStateT pc do
    vsE <- compile t
    case vsE of
      Left e -> do
        liftIO $ putTextLn (pretty e)
        return $ Left e
      Right vs -> do
        liftIO $ putTextLn $ T.intercalate "\n" (pretty <$> vs)
        return $ Right vs

repl :: IO ()
repl = do
  _ <-
    flip evalStateT programContext
      . runInputT (defaultSettings {historyFile = Just ".crossbow_history", autoAddHistory = True})
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> repl
    [path] -> do
      _ <- runFile path
      return ()
    _ -> putTextLn "'crossbow <path>' to execute a file; 'crossbow repl' to enter the REPL"
