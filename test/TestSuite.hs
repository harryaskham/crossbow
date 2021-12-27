module Main where

import Crossbow.Interpreter
import Crossbow.Parser
import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Test.HUnit (assertEqual, assertFailure)

assertEvaluatesTo :: Text -> Text -> Value -> IO ()
assertEvaluatesTo msg program expected = do
  case compile program of
    Left e -> assertFailure (T.unpack msg <> show e)
    Right p -> do
      resultM <- run p
      assertEqual (T.unpack msg) resultM (Just expected)

main :: IO ()
main = do
  assertEvaluatesTo "zero" "0" (VInt 0)
  assertEvaluatesTo "positive constant" "123" (VInt 123)
  assertEvaluatesTo "negative constant" "-123" (VInt (-123))
  assertEvaluatesTo "addition" "1|+2" (VInt 3)
  assertEvaluatesTo "add a negative" "1|+-2" (VInt (-1))
