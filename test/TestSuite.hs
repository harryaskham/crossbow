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
  assertEvaluatesTo "zero" "0" (VInteger 0)
  assertEvaluatesTo "zero double" "0.0" (VDouble 0.0)
  assertEvaluatesTo "positive int" "123" (VInteger 123)
  assertEvaluatesTo "negative int" "-123" (VInteger (-123))
  assertEvaluatesTo "positive double" "1.4" (VDouble 1.4)
  assertEvaluatesTo "negative double" "-1.4" (VDouble (-1.4))
  assertEvaluatesTo "addition" "1|+2" (VInteger 3)
  assertEvaluatesTo "double addition" "1.0|+2.0" (VDouble 3.0)
  assertEvaluatesTo "int/double addition" "1|+2.0" (VDouble 3.0)
  assertEvaluatesTo "addition with spaces" "    1 |   +     2  " (VInteger 3)
  assertEvaluatesTo "add a negative" "1|+-2" (VInteger (-1))
  assertEvaluatesTo "braced list literal" "[1,2,3]" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "nested list literal" "[1,[2,3],4]" (VList [VInteger 1, VList [VInteger 2, VInteger 3], VInteger 4])
  assertEvaluatesTo "list concatenation" "[1,2,3]|+[4,5]" (VList $ VInteger <$> [1, 2, 3, 4, 5])
  assertEvaluatesTo "list addition" "[1,2,3]|+10" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "reverse list addition" "10|+[1,2,3]" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "full function application" "+ 1 2" (VInteger 3)
  assertEvaluatesTo "min" "min 10 20" (VInteger 10)
  assertEvaluatesTo "max" "max 10 20" (VInteger 20)
