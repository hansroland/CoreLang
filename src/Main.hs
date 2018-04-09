-- -----------------------------------------------------------
-- Main.hs
-- -----------------------------------------------------------

-- A preliminary main program

import Prelude hiding (lex)
import Lex
import Parser
import PrettyPrinter

main :: IO()
main = do
    putStrLn $ pprint $ syntax $ lex coreprog02


coreprog01 :: String
coreprog01 = "f = 3"

coreprog02 :: String
coreprog02 = "main = S K K 3"

coreprog10 :: String 
coreprog10 = "main = letrec f = f x; in f"

coreprog11 :: String 
coreprog11 = "main = let id1 = I I I; in id1 id1 3"

coreprog20 :: String 
coreprog20 = "main = double 21; double a = a + a"

coreprog21 :: String 
coreprog21 = "main = double_list 6; double_list xs = map (\\ x. 2*x) xs"

coreprog22 :: String 
coreprog22 = "main = Pack{7,2} (Pack{6,1} 3) (Pack{6,1} 4)"

coreprog23 :: String 
coreprog23 = "main = isRed 1; isRed c = case c of <1> -> True; <2> -> False; <3> -> False"