-- simple machine types for virtual MaMa machine

module FPPL.MaMa.SimpleTypes
where

import FPPL.Prelude

-- --------------------
--
-- address arithmetic

class AddrArithm a where
  incr' :: a -> Offset -> a
  disp' :: a -> a -> Offset

-- --------------------
--
-- offsets within stacks and vectors
-- maybe negative

type Offset = Int

instance AddrArithm Offset where
  incr' = (+)
  disp' = (-)

-- --------------------
--
-- addresses of heap objects

newtype Addr = Addr Word
  deriving (Eq, Ord, Show)

instance Empty Addr where
  empty' = Addr 0

-- --------------------
--
-- code positions

newtype CodeAddr = CA Word
  deriving (Eq, Show)

instance Empty CodeAddr where
  empty' = CA maxBound

instance AddrArithm CodeAddr where
  incr' (CA x) o      = CA . toEnum $ fromEnum x + o
  disp' (CA x) (CA y) = fromEnum x - fromEnum y

codeAddr2Offset :: Iso' CodeAddr Offset
codeAddr2Offset = iso (\ (CA w) -> fromEnum w) (CA . toEnum)

-- ----------------------------------------
