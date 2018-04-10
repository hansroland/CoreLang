module PrettyPrintBase
  where

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
flatten col ((IIndent s, _) : seqs) = flatten col ((s,col + 2) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2,indent) : seqs) = flatten col ((seq1,indent) : (seq2,indent) : seqs)

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

-- support functions to for the pretty printer
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _   []  = INil
iInterleave _   [i] = i      -- no 'ins' at end of list
iInterleave ins (i:is) = (i `iAppend`  ins) `iAppend` iInterleave ins is