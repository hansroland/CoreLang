module PrettyPrinter
  (pprint)
  where

import Syntax

-- | Define an "abstract" data type for the Pretty Printer
data IseqRep = INil
             | IStr String
             | IAppend IseqRep IseqRep
             | IIndent IseqRep
             | INewline

type  Iseq = IseqRep

-- | return empty Iseq
iNil  :: Iseq
iNil  =  INil

-- | Turn a string into an Iseq
iStr :: String -> Iseq
iStr = IStr

-- | Turn a number into an Iseq
iNum :: Int -> Iseq
iNum n = iStr (show n)

-- | Append two Iseqs
iAppend :: Iseq -> Iseq -> Iseq
iAppend =  IAppend

-- | Newline with indentatiion
iNewline :: Iseq
iNewline = INewline

-- | Indent an Iseq
iIndent :: Iseq -> Iseq
iIndent = IIndent

-- | Turn an Iseq into a String
iDisplay :: Iseq -> String
iDisplay iseq = flatten 0 [(iseq,0)]

-- | spaces : return a string with a numer of spaces
genSpaces :: Int -> String
genSpaces n = replicate n ' '

-- "private" support function  for Iseq
flatten :: Int -> 
           [(IseqRep,Int)] ->
           String
flatten _    []  = ""
flatten _   ((INewline,indent) : seqs) = "\n" ++ genSpaces indent ++ flatten indent seqs
flatten col ((INil, _) : seqs) = flatten col seqs
flatten col ((IIndent s, _) : seqs) = flatten col ((s,col) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)


{-  These 2 functions will be used later
-- FixedWith Number	: Display a number with a fixed width
iFWNum :: Int -> Int -> Iseq
iFWNum width n = iStr (genSpaces (width - length digits) ++ digits)
  where
    digits = show n

-- Layout numbered: Layout a list of Iseqs numbered with 1) 2) 3) etc
iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (zipWith lay_item  [1..] seqs)
  where
    lay_item n iseq = iConcat [iFWNum 4 n, iStr ") ", iIndent iseq, iNewline]

-}

-- Define a Pretty Printer for the Core Language
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n)    = iNum n
pprExpr (EVar v)    = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1 `iAppend` iStr " ") `iAppend` pprAExpr e2
pprExpr (ELet isrec defns expr)
  = iConcat [ iStr keyword, iNewline,
              iStr " ", iIndent (pprDefns defns), iNewline,
              iStr "in ", pprExpr expr]
 where
  keyword
     | isrec     = "letrec"
     | otherwise = "let"
pprExpr (EConstr _ _) = iStr "PrettyPrinter.hs - EConstr not yet implemented"
pprExpr (ECase _ _ )  = iStr "PrettyPrinter.hs - ECase not yet implemented"
pprExpr (ELam _ _ )   = iStr "PrettyPrinter.hs - ELam not yet implemented"


pprAExpr :: CoreExpr -> Iseq
pprAExpr e
   | isAtomicExpr e  = pprExpr e
   | otherwise       = (iStr "(" `iAppend` pprExpr e) `iAppend` iStr ")"

pprDefns :: [(String,CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
   where
   sep = iConcat [iStr ";" , iNewline]

pprDefn :: (String, CoreExpr) -> Iseq
pprDefn (name, expr) = 
   iConcat [iStr name, iStr " ", iStr " = ",  iIndent(pprExpr expr) ]


pprProgram :: [CoreScDefn]  -> Iseq
pprProgram  scDefs  = iInterleave sep (map pprScDef scDefs)
   where
   sep = iConcat [iStr ";" , iNewline]

pprScDef :: (String, [String], CoreExpr) -> Iseq
pprScDef (name, vars ,expr) =
   iConcat [iStr name, iStr " ",
   iInterleave (iStr " ") (map iStr vars), iStr " = ", iIndent(pprExpr expr)]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- support functions to for the pretty printe
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []  = INil
iInterleave _   [i] = i      -- no 'ins' at end of list
iInterleave ins (i:is) = (i `iAppend`  ins) `iAppend` iInterleave ins is