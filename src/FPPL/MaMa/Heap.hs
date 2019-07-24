{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa heap

module FPPL.MaMa.Heap
  ( Heap
  , emptyHeap
  , alloc
  , free
  , theHeap
  , theUnused
  , theAlcnt
  , theHeapCnt
  )
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Value

import qualified Data.Map as M

-- ----------------------------------------
--
-- the heap with a map storing the heap values
-- and a the set of unused addresses

data Heap v = H { _heap   :: ! (Map Addr (Value v))
                , _unused :: [Addr]
                , _alcnt  :: Int             -- allocation count
                }
            deriving (Show)

-- initial heap

instance Empty (Heap v) where
  empty' = emptyHeap
  null'  = M.null . _heap

emptyHeap :: Heap v
emptyHeap = H { _heap   = M.empty
              , _unused = map Addr [1 .. maxBound]
              , _alcnt  = 0
              }

-- allocation

alloc :: Value v -> Heap v -> (Addr, Heap v)
alloc val (H heap unused cnt) = (addr', H heap' unused' (cnt + 1))
  where
    heap'   = M.insert addr' val heap
    addr'   = head unused
    unused' = tail unused

-- deallocation
-- basic op for GC
-- remove an address-value-pair from the heap
-- no check whether address is legal

free :: Addr -> Heap v -> Heap v
free addr (H heap unused cnt) = H heap' unused' cnt
  where
    heap' = M.delete addr heap
    unused' = addr : unused

-- lookup and modify implemented with lenses

instance Ixed (Heap v)

instance At (Heap v) where
  at i = theHeap . at i

type instance Index   (Heap v) = Addr
type instance IxValue (Heap v) = Value v

-- basic field accessors

theHeap :: Lens' (Heap v) (Map Addr (Value v))
theHeap k h = (\ n -> h { _heap = n}) <$> k (_heap h)

theUnused :: Lens' (Heap v) [Addr]
theUnused k h = (\ n -> h { _unused = n}) <$> k (_unused h)

theAlcnt :: Lens' (Heap v) Int
theAlcnt k h = (\ n -> h { _alcnt = n}) <$> k (_alcnt h)

theHeapCnt :: Getter (Heap v) Int
theHeapCnt = theHeap . to M.size

-- ----------------------------------------
