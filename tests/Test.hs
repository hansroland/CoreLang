-- ----------------------------------------------------------------
-- Main Program for Tests
-- ----------------------------------------------------------------
module Main where

import Test.HUnit
import TestParserBase

main :: IO Counts
main = runTestTT testsParserBase
