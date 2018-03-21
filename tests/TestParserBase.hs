-- ------------------------------------------------------------------
-- Tests to the ParserBase module
-- ------------------------------------------------------------------
module TestParserBase where

import Test.HUnit
import ParserBase as P
import Lex as L

testsParserBase :: Test
testsParserBase = TestList
    [ TestLabel "pLit01" pLit01
    , TestLabel "pOneOrMore" pOneOrMore01
    , TestLabel "pOneOreMoreWithSep01" pOneOrMoreWithSep01
    , TestLabel "pVar01" pVar01
    , TestLabel "pInt01" pInt01
    , TestLabel "pZeroOrMore01" pZeroOrMore01
    , TestLabel "pZeroOrMore02" pZeroOrMore02
    , TestLabel "isInteger01" isInteger01
    , TestLabel "isInteger02" isInteger02
    , TestLabel "isInteger03" isInteger03
    , TestLabel "pThen01" pThen01
    , TestLabel "pThen02" pThen02
    , TestLabel "pThen03" pThen03
    , TestLabel "pThen04" pThen04
    ]

pLit01 :: Test
pLit01 = TestCase (assertEqual "pLit01"
    [("hello",["John","!"])]
    (pLit "hello" (L.lex "hello John!")))

pOneOrMore01 :: Test
pOneOrMore01 = TestCase (assertEqual "pOneOrMore01"
    [(["x","x","x","x","x","x"],["y"])]
    (pOneOrMore (pLit "x") (L.lex "x x x x x x y")))

pOneOrMoreWithSep01 :: Test
pOneOrMoreWithSep01 = TestCase (assertEqual "pOneOrMoreWithSep01"
    [(["aa","bbb","cccc","dddd","eeee"],[])]
    (pOneOrMoreWithSep pVar (pLit ",") (L.lex "aa,bbb,cccc,dddd,eeee")))

pVar01 :: Test
pVar01 = TestCase (assertEqual "pVar01"
    [("auto",["bahn"])]
    (pVar (L.lex "auto bahn")))

pInt01 :: Test
pInt01 = TestCase (assertEqual "pInt01"
    [(2345,["autobahn"])]
    (pInt (L.lex "2345 autobahn")))

pZeroOrMore01 :: Test
pZeroOrMore01 = TestCase (assertEqual "pZeroOrMore01"
    [(["xx"],["yy","yy","yy"])]
    (pZeroOrMore (pLit "xx") (L.lex "xx yy yy yy")))

pZeroOrMore02 :: Test
pZeroOrMore02 = TestCase (assertEqual "pZeroOrMore02"
    [([],["xx","yy","yy","yy"])]
    (pZeroOrMore (pLit "yy") (L.lex "xx yy yy yy")))

isInteger01 :: Test
isInteger01 = TestCase (assertEqual "isInteger01"
    True
    (isInteger "78654"))

isInteger02 :: Test
isInteger02 = TestCase (assertEqual "isInteger02"
    False
    (isInteger "78 54"))

isInteger03 :: Test
isInteger03 = TestCase (assertEqual "isInteger02"
    False
    (isInteger "78.54"))

pThen01 :: Test
pThen01 = TestCase (assertEqual "pThen01"
    [(579,[])]
    (pThen (+) pInt pInt $ L.lex "123 456"))

pThen02 :: Test 
pThen02 = TestCase (assertEqual "pThen02"
    [( 60, [])]
    (pThen3 add3 pInt pInt pInt $ L.lex "10 20 30"))
      where 
        add3 a b c = a + b + c

pThen03 :: Test 
pThen03 = TestCase (assertEqual "pThen03"
    [( 100, [])]
    (pThen4 add4 pInt pInt pInt pInt $ L.lex "10 20 30 40"))
      where 
        add4 a b c d = a + b + c + d

pThen04 :: Test
pThen04 = TestCase (assertEqual "pThen04"
    [(("name",["p1","p2"],10),[])]
    (pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pInt  $ L.lex "name p1 p2 = 10"))
      where
        mkSc func parms _ n  = (func, parms, n)