module Main where

import Crossbow.Evaluator
import Crossbow.Parser
import Crossbow.Types
import Crossbow.Util
import Data.Text qualified as T
import Test.HUnit (assertEqual, assertFailure)

assertEvaluatesTo :: Text -> Text -> Value -> IO ()
assertEvaluatesTo msg source expected = do
  print msg
  let programParser = runReader program builtins
  pE <- compile programParser source
  case pE of
    Left e -> assertFailure $ T.unpack (msg <> "\nFailed with:\n" <> pretty e)
    Right v -> assertEqual (T.unpack msg) v expected

main :: IO ()
main = do
  assertEvaluatesTo "zero" "0" (VInteger 0)
  assertEvaluatesTo "zero double" "0.0" (VDouble 0.0)
  assertEvaluatesTo "positive int" "123" (VInteger 123)
  assertEvaluatesTo "negative int" "(-123)" (VInteger (-123))
  assertEvaluatesTo "positive double" "1.4" (VDouble 1.4)
  assertEvaluatesTo "negative double" "(-1.4)" (VDouble (-1.4))
  assertEvaluatesTo "addition" "1|+_2" (VInteger 3)
  assertEvaluatesTo "multiplication" "2|(_*4)" (VInteger 8)
  assertEvaluatesTo "double addition" "1.0|+_2.0" (VDouble 3.0)
  assertEvaluatesTo "int/double addition" "1|+_2.0" (VDouble 3.0)
  assertEvaluatesTo "addition with spaces" "    1 |   +     2  " (VInteger 3)
  assertEvaluatesTo "add a negative" "1|+_(-2)" (VInteger (-1))
  assertEvaluatesTo "braced list literal" "[1,2,3]" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "nested list literal" "[1,[2,3],4]" (VList [VInteger 1, VList [VInteger 2, VInteger 3], VInteger 4])
  assertEvaluatesTo "list concatenation" "[1,2,3]|++_[4,5]" (VList $ VInteger <$> [1, 2, 3, 4, 5])
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
  assertEvaluatesTo "fold over empty list" "foldl | + | 0 | []" (VInteger 0)
  assertEvaluatesTo "fold over list" "foldl | + | 0 | [1,2,3]" (VInteger 6)
  assertEvaluatesTo "ranges" "1:100|sum" (VInteger 5050)
  assertEvaluatesTo "maximum" "[1,3,2]|maximum" (VInteger 3)
  assertEvaluatesTo "minimum" "[1,3,2]|minimum" (VInteger 1)
  assertEvaluatesTo "fans out" "1 | fork 4 _" (VList $ VInteger <$> [1, 1, 1, 1])
  assertEvaluatesTo "converts string to int" "\"123\"|int" (VInteger 123)
  assertEvaluatesTo "drops" "1:10 | drop 6 _" (VList $ VInteger <$> [7, 8, 9, 10])
  assertEvaluatesTo "takes" "1:10 | take 3 _" (VList $ VInteger <$> [1, 2, 3])
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "heads" "3:10 | head" (VInteger 3)
  assertEvaluatesTo "filter" "filter | <_10 | 8:12" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "filter without pipes" "filter (_<10) 8:12" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "filter forward" "8:12 | filter (_<10) _" (VList $ VInteger <$> [8, 9])
  assertEvaluatesTo "binary functions" "(1+2)" (VInteger 3)
  assertEvaluatesTo
    "pairwise sums"
    "1:4 | fork 2 _ | [id, drop 1 _] | monadic zip | map sum"
    (VList $ VInteger <$> [3, 5, 7])
  assertEvaluatesTo "list length" "length [1,2,3,4]" (VInteger 4)
  assertEvaluatesTo "empty list length" "length []" (VInteger 0)
  assertEvaluatesTo "scanl" "scanl (+) 0 [1,2,3]" (VList $ VInteger <$> [0, 1, 3, 6])
  assertEvaluatesTo "scanr" "scanr (+) 0 [1,2,3]" (VList $ VInteger <$> [6, 5, 3, 0])
  assertEvaluatesTo "fork and ap" "6 | fork 2 | [+,+1] | monadic ap" (VInteger 13)
  assertEvaluatesTo "fork and flap" "6 | fork 2 | [+1,+] | monadic (flip ap)" (VInteger 13)
  assertEvaluatesTo "not true" "not True" (VBool False)
  assertEvaluatesTo "evens" "1:10|filter even" (VList $ VInteger <$> [2, 4, 6, 8, 10])
  assertEvaluatesTo "multivariable lambda" "{$0|(_+$1)|(_*$2)}|1|2|3" (VInteger 9)
  assertEvaluatesTo "enumeration" "enum 1:3" (VList [VList [VInteger 0, VInteger 1], VList [VInteger 1, VInteger 2], VList [VInteger 2, VInteger 3]])
  assertEvaluatesTo "lengthy" "lengthy 3 1:3" (VBool True)
  assertEvaluatesTo "lengthy" "lengthy 3 1:4" (VBool False)
  assertEvaluatesTo "windows" "windows 2 1:3" (VList [VList [VInteger 1, VInteger 2], VList [VInteger 2, VInteger 3]])
  assertEvaluatesTo "foldl1" "[1,2,3]|foldl1|+" (VInteger 6)
  assertEvaluatesTo "scanl1" "[1,2,3]|scanl1|+" (VList $ VInteger <$> [1, 3, 6])
  assertEvaluatesTo "post-applied mapbangs" "[[[1,2,3]]]|sum!!" (VList [VList [VInteger 6]])
  assertEvaluatesTo "pre-applied mapbangs" "sum!! [[[1,2,3]]]" (VList [VList [VInteger 6]])
  -- TODO: Mapbang lambdas

  assertEvaluatesTo
    "Day 1 (Part 1)"
    "aoc 1 | ints | pairs | count (monadic <)"
    (VInteger 1316)

  assertEvaluatesTo
    "Day 1 (Part 2)"
    "aoc 1 | ints | windows 3 | map sum | pairs | count (monadic <)"
    (VInteger 1344)

  assertEvaluatesTo
    "Day 1 (Both Parts)"
    "aoc 1 | ints | fork 2 | second {windows 3 | map sum} | map {pairs | count (monadic <)}"
    (VList [VInteger 1316, VInteger 1344])

  assertEvaluatesTo
    "Day 2 (Part 1) 1"
    "aoc 2 | lines | map {words|second int|first fst} | fork 3 | [filter {fst|=='f'}, filter {fst|=='d'}, filter {fst|=='u'}] | map {map snd|sum} | fork 2 | [head,tail] | second (monadic -) | monadic *"
    (VInteger 1690020)

  assertEvaluatesTo
    "Day 2 (Part 1) 2"
    "aoc 2 | lines | map {words|second int|first fst} | map {$0|ix 0|case _ [['u',[0,0,ix 1 $0]],['d',[0,ix 1 $0,0]],['f',[ix 1 $0,0,0]]]} | sum | fork 2 | [head, tail] | second (monadic -) | monadic *"
    (VInteger 1690020)

  assertEvaluatesTo
    "Day 2 (Part 1) 3"
    "aoc 2 | lines | map {words|second int|first fst} | map {$0|ix 0|case _ [['u', negate [0,ix 1 $0]],['d', [0,ix 1 $0]],['f',[ix 1 $0,0]]]} | sum | monadic *"
    (VInteger 1690020)

  assertEvaluatesTo
    "Day 2 (Part 2)"
    "aoc 2 | lines | map {words|second int|first fst} | map {$0|ix 0|case _ [['u', negate [0,ix 1 $0]],['d', [0,ix 1 $0]],['f',[ix 1 $0,0]]]} | fold {$0|first (+ (fst $1))|second (+ (* (fst $1) (thd $0)))|third (+ (snd $1))} [0,0,0] | take 2 | monadic *"
    (VInteger 1408487760)

  assertEvaluatesTo
    "Day 2 (Both Parts)"
    "aoc 2 | lines | map {words|second int|first fst} | map {$0|ix 0|case _ [['u', negate [0,ix 1 $0]],['d', [0,ix 1 $0]],['f',[ix 1 $0,0]]]} | fork 2 | [{sum | monadic *}, fold {$0|first (+ (fst $1))|second (+ (* (fst $1) (thd $0)))|third (+ (snd $1))} [0,0,0]] | second {take 2 | monadic *}"
    (VList [VInteger 1690020, VInteger 1408487760])

  assertEvaluatesTo
    "Day 3 (Part 1) 1"
    "aoc 3 | lines | transpose | map (map int) | fork 2 | [map mode, map antimode] | map bits | monadic *"
    (VInteger 3320834)

  assertEvaluatesTo
    "Day 3 (Part 1) 2"
    "aoc 3 | lines | transpose | map (int!) | fork 2 | [mode!, antimode!] | bits! | monadic *"
    (VInteger 3320834)

--assertEvaluatesTo "Day 3 (Part 2)" "" (VInteger 4481199)
--assertEvaluatesTo "Day 4 (Part 1)" "" (VInteger 35670)
--assertEvaluatesTo "Day 4 (Part 2)" "" (VInteger 22704)
--assertEvaluatesTo "Day 5 (Part 1)" "" (VInteger 6225)
--assertEvaluatesTo "Day 5 (Part 2)" "" (VInteger 22116)
--assertEvaluatesTo "Day 6 (Part 1)" "" (VInteger 377263)
--assertEvaluatesTo "Day 6 (Part 2)" "" (VInteger 1695929023803)
--assertEvaluatesTo "Day 7 (Part 1)" "" (VInteger 356992)
--assertEvaluatesTo "Day 7 (Part 2)" "" (VInteger 101268110)
--assertEvaluatesTo "Day 8 (Part 1)" "" (VInteger 488)
--assertEvaluatesTo "Day 8 (Part 2)" "" (VInteger 1040429)
--assertEvaluatesTo "Day 9 (Part 1)" "" (VInteger 465)
--assertEvaluatesTo "Day 9 (Part 2)" "" (VInteger 1269555)
--assertEvaluatesTo "Day 10 (Part 1)" "" (VInteger 392139)
--assertEvaluatesTo "Day 10 (Part 2)" "" (VInteger 4001832844)
--assertEvaluatesTo "Day 11 (Part 1)" "" (VInteger 1705)
--assertEvaluatesTo "Day 11 (Part 2)" "" (VInteger 265)
--assertEvaluatesTo "Day 12 (Part 1)" "" (VInteger 3485)
--assertEvaluatesTo "Day 12 (Part 2)" "" (VInteger 85062)
--assertEvaluatesTo "Day 13 (Part 1)" "" (VInteger 720)
--assertEvaluatesTo "Day 13 (Part 2)" "" VText " ##  #  # ###  ###  ###   ##  #  # ####\n#  # #  # #  # #  # #  # #  # #  #    #\n#  # #### #  # #  # #  # #  # #  #   # \n#### #  # ###  ###  ###  #### #  #  #  \n#  # #  # #    # #  #    #  # #  # #   \n#  # #  # #    #  # #    #  #  ##  ####"
--assertEvaluatesTo "Day 14 (Part 1)" "" (VInteger 2621)
--assertEvaluatesTo "Day 14 (Part 2)" "" (VInteger 2843834241366)
--assertEvaluatesTo "Day 15 (Part 1)" "" (VInteger 673)
--assertEvaluatesTo "Day 15 (Part 2)" "" (VInteger 2893)
--assertEvaluatesTo "Day 16 (Part 1)" "" (VInteger 971)
--assertEvaluatesTo "Day 16 (Part 2)" "" (VInteger 831996589851)
--assertEvaluatesTo "Day 17 (Part 1)" "" (VInteger 23005)
--assertEvaluatesTo "Day 17 (Part 2)" "" (VInteger 2040)
--assertEvaluatesTo "Day 18 (Part 1)" "" (VInteger 4173)
--assertEvaluatesTo "Day 18 (Part 2)" "" (VInteger 4706)
--assertEvaluatesTo "Day 19 (Part 1)" "" (VInteger 436)
--assertEvaluatesTo "Day 19 (Part 2)" "" (VInteger 10918)
--assertEvaluatesTo "Day 20 (Part 1)" "" (VInteger 5619)
--assertEvaluatesTo "Day 20 (Part 2)" "" (VInteger 20122)
--assertEvaluatesTo "Day 21 (Part 1)" "" (VInteger 1073709)
--assertEvaluatesTo "Day 21 (Part 2)" "" (VInteger 148747830493442)
--assertEvaluatesTo "Day 22 (Part 1)" "" (VInteger 623748)
--assertEvaluatesTo "Day 22 (Part 2)" "" (VInteger 1227345351869476)
--assertEvaluatesTo "Day 23 (Part 1)" "" (VInteger 10411)
--assertEvaluatesTo "Day 23 (Part 2)" "" (VInteger 46721)
--assertEvaluatesTo "Day 24 (Part 1)" "" (VInteger 98998519596997)
--assertEvaluatesTo "Day 24 (Part 2)" "" (VInteger 31521119151421)
--assertEvaluatesTo "Day 25 (Part 1)" "" (VInteger 458)
--assertEvaluatesTo "Day 25 (Part 2)" "\"Merry Christmas!\"" (VText "Merry Christmas!")
