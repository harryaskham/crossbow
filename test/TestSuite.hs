module Main where

import Crossbow.Interpreter
import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Test.HUnit (assertEqual, assertFailure)

assertEvaluatesTo :: Text -> Text -> Value -> IO ()
assertEvaluatesTo msg program expected = do
  print msg
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
  assertEvaluatesTo "addition" "1|+_2" (VInteger 3)
  assertEvaluatesTo "double addition" "1.0|+_2.0" (VDouble 3.0)
  assertEvaluatesTo "int/double addition" "1|+_2.0" (VDouble 3.0)
  assertEvaluatesTo "addition with spaces" "    1 |   +     2  " (VInteger 3)
  assertEvaluatesTo "add a negative" "1|+_-2" (VInteger (-1))
  assertEvaluatesTo "braced list literal" "[1,2,3]" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "nested list literal" "[1,[2,3],4]" (VList [VInteger 1, VList [VInteger 2, VInteger 3], VInteger 4])
  assertEvaluatesTo "list concatenation" "[1,2,3]|+_[4,5]" (VList $ VInteger <$> [1, 2, 3, 4, 5])
  assertEvaluatesTo "list addition" "[1,2,3]|+_10" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "reverse list addition" "10|+_[1,2,3]" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "full function application" "+ 1 2" (VInteger 3)
  assertEvaluatesTo "min" "min 10 20" (VInteger 10)
  assertEvaluatesTo "max" "max 10 20" (VInteger 20)
  assertEvaluatesTo "cast to int" "2.2|int" (VInteger 2)
  assertEvaluatesTo "cast list to int" "[2.2, 3.4]|int" (VList $ VInteger <$> [2, 3])
  assertEvaluatesTo "cast to double" "2|double" (VDouble 2.0)
  assertEvaluatesTo "cast list to double" "[2, 3]|double" (VList $ VDouble <$> [2.0, 3.0])
  assertEvaluatesTo "string aliases to list" "\"hi\"" (VList $ VChar <$> ['h', 'i'])
  assertEvaluatesTo "read file" "read \"test/test_input.txt\"" (VList $ VChar <$> T.unpack "test\ninput\n")
  assertEvaluatesTo "map over empty list" "map|+_1|[]" (VList [])
  assertEvaluatesTo "map over list" "map|+_1|[1,2,3]" (VList $ VInteger <$> [2, 3, 4])
  assertEvaluatesTo "map forward over list" "[1,2,3] | map | +_1" (VList $ VInteger <$> [2, 3, 4])
  assertEvaluatesTo "fold over empty list" "fold | + | 0 | []" (VInteger 0)
  assertEvaluatesTo "fold over list" "fold | + | 0 | [1,2,3]" (VInteger 6)
  assertEvaluatesTo "ranges" "1:100|sum" (VInteger 5050)
  assertEvaluatesTo "maximum" "[1,3,2]|maximum" (VInteger 3)
  assertEvaluatesTo "fans out" "1 | fanout 4 _" (VList $ VInteger <$> [1, 1, 1, 1])
  assertEvaluatesTo "converts string to int" "\"123\"|int" (VInteger 123)
  assertEvaluatesTo "drops" "1:10 | drop 6 _" (VList $ VInteger <$> [7, 8, 9, 10])
  assertEvaluatesTo "takes" "1:10 | take 3 _" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "filter" "filter | <_10 | 8:12" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "filter without pipes" "filter (_<10) 8:12" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "filter forward" "8:12 | filter (_<10) _" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "binary functions" "(1+2)" (VInteger 3)
