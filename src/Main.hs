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
