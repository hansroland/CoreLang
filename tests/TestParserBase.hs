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
    ]


pLit01 = TestCase (assertEqual "pLit01"
    [("hello",["John","!"])]
    (pLit "hello" (L.lex "hello John!")))

pOneOrMore01 = TestCase (assertEqual "pOneOrMore01"
    [(["x","x","x","x","x","x"],[])]
    (pOneOrMore (pLit "x") ["x", "x", "x", "x", "x", "x"]))

pOneOrMoreWithSep01 = TestCase (assertEqual "pOneOrMoreWithSep01"
    [(["aa","bbb","cccc","dddd","eeee"],[])]
    (pOneOrMoreWithSep pVar (pLit ",") (L.lex "aa,bbb,cccc,dddd,eeee")))

pVar01 = TestCase (assertEqual "pVar01"
    [("auto",["bahn"])]
    (pVar (L.lex "auto bahn")))

pInt01 = TestCase (assertEqual "pInt01"
    [(2345,["autobahn"])]
    (pInt (L.lex "2345 autobahn")))




