-- simple machine types for virtual MaMa machine

module FPPL.MaMa.SimpleTypes
where

import FPPL.Prelude

-- --------------------
--
-- address arithmetic

class AddrArithm a where
  incr'     :: Offset -> a -> a
  disp'     :: a -> a -> Offset
  isoOffset :: Iso' a Offset

-- --------------------
--
-- offsets within stacks and vectors
-- maybe negative

type Offset = Int

instance AddrArithm Offset where
  incr'     = (+)
  disp'     = (-)
  isoOffset = id

-- --------------------
--
-- addresses of heap objects

newtype Addr = Addr Word
  deriving (Eq, Ord, Show)

instance Empty Addr where
  empty' = Addr 0

-- no address arithmetic here

-- --------------------
--
-- addresses into contiguous store

newtype Addr' a = AD Word
  deriving (Eq, Show)

instance Empty (Addr' a) where
  empty' = AD maxBound

instance AddrArithm (Addr' a) where
  incr' o (AD x)      = AD . toEnum $ fromEnum x + o
  disp' (AD x) (AD y) = fromEnum x - fromEnum y
  isoOffset           = iso (\ (AD w) -> fromEnum w) (AD . toEnum)

-- --------------------
--
-- code positions

data IntoCode

type CodeAddr = Addr' IntoCode

-- --------------------
--
-- stack adresses

data IntoStack

type StackAddr = Addr' IntoStack

-- --------------------
--
-- overloaded lenses

class CodePointer v where
  cp :: Lens' v CodeAddr

class GlobalPointer v where
  gp :: Lens' v Addr

class ArgsPointer v where
  ap :: Lens' v Addr

class StackPointer v where
  sp :: Lens' v StackAddr

-- ----------------------------------------
