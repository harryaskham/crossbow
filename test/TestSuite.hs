module Main where

import Crossbow.Lib
import Test.HUnit (assertEqual)

main :: IO ()
main = do
  assertEqual "Test" 123 123
