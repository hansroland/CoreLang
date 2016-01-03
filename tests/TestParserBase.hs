-- ------------------------------------------------------------------
-- Tests to the ParserBase moule
-- ------------------------------------------------------------------
module TestParserBase where

import Test.HUnit
import ParserBase as P
import Lex as L

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
    ]


pLit01 = TestCase (assertEqual "pLit01"
    [("hello",["John","!"])]
    (pLit "hello" (L.lex "hello John!")))

pOneOrMore01 = TestCase (assertEqual "pOneOrMore01"
    [(["x","x","x","x","x","x"],["y"])]
    (pOneOrMore (pLit "x") (L.lex "x x x x x x y")))

pOneOrMoreWithSep01 = TestCase (assertEqual "pOneOrMoreWithSep01"
    [(["aa","bbb","cccc","dddd","eeee"],[])]
    (pOneOrMoreWithSep pVar (pLit ",") (L.lex "aa,bbb,cccc,dddd,eeee")))

pVar01 = TestCase (assertEqual "pVar01"
    [("auto",["bahn"])]
    (pVar (L.lex "auto bahn")))

pInt01 = TestCase (assertEqual "pInt01"
    [(2345,["autobahn"])]
    (pInt (L.lex "2345 autobahn")))

pZeroOrMore01 = TestCase (assertEqual "pZeroOrMore01"
    [(["xx"],["yy","yy","yy"])]
    (pZeroOrMore (pLit "xx") (L.lex "xx yy yy yy")))

pZeroOrMore02 = TestCase (assertEqual "pZeroOrMore02"
    [([],["xx","yy","yy","yy"])]
    (pZeroOrMore (pLit "yy") (L.lex "xx yy yy yy")))

isInteger01 = TestCase (assertEqual "isInteger01"
    True
    (isInteger "78654"))

isInteger02 = TestCase (assertEqual "isInteger02"
    False
    (isInteger "78 54"))

isInteger03 = TestCase (assertEqual "isInteger02"
    False
    (isInteger "78.54"))

pThen01 = TestCase (assertEqual "pThen01"
    [(579,[])]
    (pThen (+) pInt pInt $ L.lex "123 456"))
