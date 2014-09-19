-- -----------------------------------------------------------
-- ParserBase.hs - Parser Combinators  for the Core language
--                 Should be replaced with parsec library
-- -----------------------------------------------------------

module ParserBase(Parser,
    pLit, pVar, pInt,
    pAlt,
    pApply,
    pThen, pThen3, pThen4,
    pEmpty,
    pOneOrMore, pZeroOrMore,
    pOneOrMoreWithSep,
    isInteger
    )

where

import Lex
import Data.Char

type Parser a = [Token] -> [(a, [Token])]

-- | Support Function to allow more modulare definitions of pLit and pVar
pSat :: (String -> Bool) -> Parser String
pSat pred  (tok : toks)
    | pred tok  = [(tok, toks)]
    |otherwise  = []
pSat pre []     = []

-- | pLit - a parser for literals
pLit :: String -> Parser String
pLit s = pSat (== s)

-- | pVar - a parser for variables
pVar :: Parser String
pVar  = pSat isVariable
    where
      isVariable x = isNotKeyword x && startWithLetter x
      isNotKeyword x  = x `notElem` ["let", "letrec", "case", "in", "of", "Pack"]
      startWithLetter = isLetter . head

-- | pAlt - a parser for alternative selection
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks
    | null (p1 toks)   = p2 toks
    | otherwise        = p1 toks

-- | pThen - a parser for parsing 2 elements in sequence
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [(combine v1 v2 , toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1]

-- | pThen3 - a parser for parsing 3 elements in sequence
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks = [(combine v1 v2 v3, toks3) |
          (v1, toks1) <- p1 toks,
          (v2, toks2) <- p2 toks1,
          (v3, toks3) <- p3 toks2]

-- pThen4 - a parser for parsing 4 elements in sequence
pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks = [(combine v1 v2 v3 v4, toks4) |
          (v1, toks1) <- p1 toks,
          (v2, toks2) <- p2 toks1,
          (v3, toks3) <- p3 toks2,
          (v4, toks4) <- p4 toks3]


pEmpty :: a -> Parser a
pEmpty x toks = [(x,toks)]


pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p tokarg = [(x1:xs,tokend) | (x1,tok1) <- p tokarg, (xs,tokend) <- pZeroOrMore p tok1]


pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 pSep = pThen (:) p1 (pZeroOrMore pSepAndItem)
    where
      pSepAndItem = pThen retSecond pSep p1
      retSecond x y  = y

-- | Parser Manipulation function. It takes a parser and a function and applies
--   the function to the values returned by the parser
-- ----------------------------------------------------------------------------
pApply :: Parser a -> (a -> b) -> Parser b
pApply  p f toks = [ (f x , tok1 ) |  (x, tok1)  <- p toks]


-- Little support predicate to check whether a string can be converted to an Integer
-- (should be replaced by a standart function)
isInteger :: String -> Bool
isInteger (d: ds)
   | isDigit d && null ds = True
   | isDigit d = isInteger ds
   | not (isDigit d) = False
isInteger []   = False

-- Little support Function to convert string to Integer
-- (should be replaced by a standart function)
string2int :: String -> Int
string2int = read

-- | pInt a Parser to parse an integer
pInt :: Parser Int
pInt =  pApply (pSat isInteger) string2int

