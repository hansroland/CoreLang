-- -----------------------------------------------------------
-- A lexer for the Core language
-- -----------------------------------------------------------

module Lex
    (lex,
    Token)
  where

import Prelude hiding(lex)
import Data.Char

-- Tokens are non-empty strings
type Token = String

-- lex breaks input into a sequence of small chunks
-- eg (identifiers, numbers, symbols etc)
lex :: String -> [Token]
lex (c:cs)
  | isWhiteSpace c = lex cs
  | isDigit  c = num_tok : lex restnum_cs
  | isLetter c = var_tok : lex restvar_cs
     where
     num_tok = c : takeWhile isDigit cs
     restnum_cs = dropWhile isDigit cs
     var_tok = c : takeWhile isIdChar cs
     restvar_cs = dropWhile isIdChar cs
lex (a : b : cs)
   | member twoCharOps op = op : lex cs
     where op = [a,b]
lex (c:cs) = [c] : lex cs
lex [] = []

isWhiteSpace = member " \t\n"

isIdChar c = isLetter c || isDigit c || c == '_'

twoCharOps = ["==", "~=", ">=", "<=", "->"]

-- member : use elem

member :: Eq a => [a] -> a -> Bool
member (c:cs) x
   | c == x  = True
   |otherwise = member cs x
member [] x = False



