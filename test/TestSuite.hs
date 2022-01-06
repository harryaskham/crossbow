module Main where

import Crossbow.Evaluator
import Crossbow.Execute
import Crossbow.Parser
import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Test.HUnit (assertEqual, assertFailure)

assertEvaluatesTo :: Text -> Text -> Value -> IO ()
assertEvaluatesTo msg source expected = do
  print msg
  let programContext = ProgramContext program builtins
  pE <- evalStateT (compile source) programContext
  case pE of
    Left e -> assertFailure $ T.unpack (msg <> "\nFailed with:\n" <> pretty e)
    Right v -> assertEqual (T.unpack msg) v [expected]

assertFileEvaluatesTo :: FilePath -> [Value] -> IO ()
assertFileEvaluatesTo path expected = do
  print path
  let programContext = ProgramContext program builtins
  pE <- evalStateT (runFile path) programContext
  case pE of
    Left e -> assertFailure $ T.unpack (T.pack path <> "\nFailed with:\n" <> pretty e)
    Right v -> assertEqual path (filter (/= VNull) v) expected

main :: IO ()
main = do
  assertEvaluatesTo "zero" "0" (VInteger 0)
  assertEvaluatesTo "zero double" "0.0" (VDouble 0.0)
  assertEvaluatesTo "positive int" "123" (VInteger 123)
  assertEvaluatesTo "negative int" "(-123)" (VInteger (-123))
  assertEvaluatesTo "positive double" "1.4" (VDouble 1.4)
  assertEvaluatesTo "negative double" "(-1.4)" (VDouble (-1.4))
  assertEvaluatesTo "addition" "1|+ 2" (VInteger 3)
  assertEvaluatesTo "multiplication" "2|* 4)" (VInteger 8)
  assertEvaluatesTo "binary addition" "(1 + 2)" (VInteger 3)
  assertEvaluatesTo "double addition" "1.0|+ 2.0" (VDouble 3.0)
  assertEvaluatesTo "int/double addition" "1|+ 2.0" (VDouble 3.0)
  assertEvaluatesTo "addition with spaces" "    1 |   +     2  " (VInteger 3)
  assertEvaluatesTo "add a negative" "1|+ (-2)" (VInteger (-1))
  assertEvaluatesTo "braced list literal" "[1,2,3]" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "nested list literal" "[1,[2,3],4]" (VList [VInteger 1, VList [VInteger 2, VInteger 3], VInteger 4])
  assertEvaluatesTo "list concatenation" "++ [1,2,3]|[4,5]" (VList $ VInteger <$> [1, 2, 3, 4, 5])
  assertEvaluatesTo "list addition" "[1,2,3]|+ 10" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "reverse list addition" "10|+ [1,2,3]" (VList $ VInteger <$> [11, 12, 13])
  assertEvaluatesTo "full function application" "+ 1 2" (VInteger 3)
  assertEvaluatesTo "min" "min 10 20" (VInteger 10)
  assertEvaluatesTo "max" "max 10 20" (VInteger 20)
  assertEvaluatesTo "cast to int" "2.2|int" (VInteger 2)
  assertEvaluatesTo "cast list to int" "[2.2, 3.4]|int" (VList $ VInteger <$> [2, 3])
  assertEvaluatesTo "cast to double" "2|double" (VDouble 2.0)
  assertEvaluatesTo "cast list to double" "[2, 3]|double" (VList $ VDouble <$> [2.0, 3.0])
  assertEvaluatesTo "string aliases to list" "\"hi\"" (VList $ VChar <$> ['h', 'i'])
  assertEvaluatesTo "read file" "read \"test/test_input.txt\"" (VList $ VChar <$> T.unpack "test\ninput\n")
  assertEvaluatesTo "map over empty list" "map|+ 1|[]" (VList [])
  assertEvaluatesTo "map over list" "map|+ 1|[1,2,3]" (VList $ VInteger <$> [2, 3, 4])
  assertEvaluatesTo "map forward over list" "[1,2,3] | map (+ 1)" (VList $ VInteger <$> [2, 3, 4])
  assertEvaluatesTo "fold over empty list" "foldl | + | 0 | []" (VInteger 0)
  assertEvaluatesTo "fold over list" "foldl | + | 0 | [1,2,3]" (VInteger 6)
  assertEvaluatesTo "foldl1" "foldl1 (+) [1,2,3]" (VInteger 6)
  assertEvaluatesTo "scanl1" "scanl1 (+) [1,2,3]" (VList $ VInteger <$> [1, 3, 6])
  assertEvaluatesTo "ranges" "1:100|sum" (VInteger 5050)
  assertEvaluatesTo "maximum" "[1,3,2]|maximum" (VInteger 3)
  assertEvaluatesTo "minimum" "[1,3,2]|minimum" (VInteger 1)
  assertEvaluatesTo "fans out" "1 | fork 4" (VList $ VInteger <$> [1, 1, 1, 1])
  assertEvaluatesTo "converts string to int" "\"123\"|int" (VInteger 123)
  assertEvaluatesTo "drops" "1:10 | drop 6" (VList $ VInteger <$> [7, 8, 9, 10])
  assertEvaluatesTo "takes" "1:10 | take 3" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "filter" "filter (> 10) 8:12" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "filter forward" "8:12 | filter (> 10)" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo
    "pairwise sums"
    "1:4 | fork 2 | [id, drop 1] | `zip | sum!"
    (VList $ VInteger <$> [3, 5, 7])
  assertEvaluatesTo "list length" "length [1,2,3,4]" (VInteger 4)
  assertEvaluatesTo "empty list length" "length []" (VInteger 0)
  assertEvaluatesTo "scanl" "scanl (+) 0 [1,2,3]" (VList $ VInteger <$> [0, 1, 3, 6])
  assertEvaluatesTo "scanr" "scanr (+) 0 [1,2,3]" (VList $ VInteger <$> [6, 5, 3, 0])
  assertEvaluatesTo "fork and ap with monadic backtick" "6 | fork 2 | [+,+ 1] | `ap" (VInteger 13)
  assertEvaluatesTo "fork and flap" "6 | fork 2 | [+ 1, +] | monadic (flip ap)" (VInteger 13)
  assertEvaluatesTo "not true" "not True" (VBool False)
  assertEvaluatesTo "evens" "1:10|filter even" (VList $ VInteger <$> [2, 4, 6, 8, 10])
  assertEvaluatesTo "multivariable lambda" "{$0|(+ $1)|(* $2)}|1|2|3" (VInteger 9)
  assertEvaluatesTo "zero argument lambdas" "(1 | + 2) | * 3" (VInteger 9)
  assertEvaluatesTo "enumeration" "enum 1:3" (VList [VList [VInteger 0, VInteger 1], VList [VInteger 1, VInteger 2], VList [VInteger 2, VInteger 3]])
  assertEvaluatesTo "lengthy" "lengthy 3 1:3" (VBool True)
  assertEvaluatesTo "lengthy" "lengthy 3 1:4" (VBool False)
  assertEvaluatesTo "windows" "windows 2 1:3" (VList [VList [VInteger 1, VInteger 2], VList [VInteger 2, VInteger 3]])
  assertEvaluatesTo "post-applied mapbangs" "[[[1,2,3]]]|sum!!" (VList [VList [VInteger 6]])
  assertEvaluatesTo "pre-applied mapbangs" "sum!! [[[1,2,3]]]" (VList [VList [VInteger 6]])
  assertEvaluatesTo "lambda mapbangs" "[[[1,2,3]]] | {foldl1 (+) $0}!!" (VList [VList [VInteger 6]])
  assertEvaluatesTo "ix underscores" "0:10 | _3" (VInteger 3)

  assertEvaluatesTo "import statements" "import \"test/aoc.cb\" | d2 | sum | `*" (VInteger 1690020)

  assertFileEvaluatesTo "test/file_test.cb" [VInteger 3]

  assertFileEvaluatesTo
    "test/aoc.cb"
    [ VInteger 1316,
      VInteger 1344,
      VInteger 1690020,
      VInteger 1408487760,
      VInteger 3320834,
      VInteger 4481199
      --VInteger 35670
      --VInteger 22704
      --VInteger 6225
      --VInteger 22116
      --VInteger 377263
      --VInteger 1695929023803
      --VInteger 356992
      --VInteger 101268110
      --VInteger 488
      --VInteger 1040429
      --VInteger 465
      --VInteger 1269555
      --VInteger 392139
      --VInteger 4001832844
      --VInteger 1705
      --VInteger 265
      --VInteger 3485
      --VInteger 85062
      --VInteger 720
      --VText " ##  #  # ###  ###  ###   ##  #  # ####\n#  # #  # #  # #  # #  # #  # #  #    #\n#  # #### #  # #  # #  # #  # #  #   # \n#### #  # ###  ###  ###  #### #  #  #  \n#  # #  # #    # #  #    #  # #  # #   \n#  # #  # #    #  # #    #  #  ##  ####"
      --VInteger 2621
      --VInteger 2843834241366
      --VInteger 673
      --VInteger 2893
      --VInteger 971
      --VInteger 831996589851
      --VInteger 23005
      --VInteger 2040
      --VInteger 4173
      --VInteger 4706
      --VInteger 436
      --VInteger 10918
      --VInteger 5619
      --VInteger 20122
      --VInteger 1073709
      --VInteger 148747830493442
      --VInteger 623748
      --VInteger 1227345351869476
      --VInteger 10411
      --VInteger 46721
      --VInteger 98998519596997
      --VInteger 31521119151421
      --VInteger 458
      --Merry Christmas!\"" VText "Merry Christmas!"
    ]
