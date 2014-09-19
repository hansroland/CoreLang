-- ------------------------------------------------------------------
-- Tests to the ParserBase moule
-- ------------------------------------------------------------------
module TestParserBase where


import Test.HUnit
import ParserBase


testsParserBase = TestList [TestLabel "pLit01" pLit01]


pLit01 = TestCase (assertEqual "pLit01"
              [("hello",["John","!"])]
              (pLit "hello" ["hello", "John", "!"]))







