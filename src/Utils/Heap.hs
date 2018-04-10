-- ----------------------------------------------------------------------------
-- Utils.Heap.hs - An implementation of a heap structure
--                 Should be replaced later by a Hackage library
-- ----------------------------------------------------------------------------

module Utils.Heap 
   (Heap
   , Addr
   , hInitial
   , hAlloc
   , hUpdate
   , hSize
   , hFree
   , hAddresses
   , hLookup)
   where

import Utils.Assoc

type Addr = Int

-- | Data type Heap
data Heap a = Heap Addr [Addr] [(Addr, a)]
    -- Note: Do not automatically derive Show, The free list in infinite...
 
-- | Create a Heap
hInitial :: Heap a   
hInitial = Heap 0 [1..] []

-- | Add a new element to the heap
-- Add the new element at the beginning of the list, and remove the address
hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap size (next : free) xs) x =  (Heap (size + 1) free  ((next, x) : xs), next)
hAlloc (Heap _ [] _) _ = error "Heap.hs:hAlloc - Empty free list"
 
-- | Update an element added earlier to the heap
hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap size free xs) a x  = Heap size free ((a,x) : remove xs a) 

-- | Remove an address from the Heap
hFree :: Heap a -> Addr -> Heap a
hFree (Heap size free xs) a = Heap (size - 1) (a:free) (remove xs a)

-- | Helper function remove
remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] adr = error ("Heap.remove - Attemot to update or free nonexistent address"
              ++ show adr)
remove ((a, x) : xs) adr  
    | a == adr  = xs
    | otherwise = (a,x) : remove xs adr

-- Additional Functions

-- | Return the number of elements in our heap
hSize :: Heap a -> Int
hSize (Heap size _ _) = size

-- | Return all addresses with data stored in our heap
hAddresses :: Heap a -> [Addr]
hAddresses (Heap _ _ xs) = [addr | (addr, _) <- xs]

-- | Loopkup an element with its address
hLookup :: Heap a -> Addr -> a 
hLookup (Heap _ _ xs) a = aLookup xs a (error ("Heap.hLokup - can't find address " ++ show a))
