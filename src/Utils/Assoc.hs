-- ----------------------------------------------------------------------------
-- Assoc.hs - An implementation of association lists
--            Shuould be replaced later by Data.Map
-- ----------------------------------------------------------------------------


module Utils.Assoc

    where

type Assoc a b = [(a, b)]

-- | aLookup: Lookup a key, if not found return the given default
aLookup :: (Eq k) => Assoc k v -> k -> v -> v
aLookup [] k def = def
aLookup ((k',v) : as) k def 
   | k == k'   = v
   | otherwise = aLookup as k def
