-- simple machine types for virtual MaMa machine

module FPPL.MaMa.SimpleTypes
where

import FPPL.Prelude

-- --------------------
--
-- addresses of heap objects

newtype Addr = Addr Word
  deriving (Eq, Ord, Show)

instance Empty Addr where
  empty' = Addr 0

-- --------------------
--
-- offsets within stacks and vectors

type Offset = Int

-- --------------------
--
-- code positions

newtype CodeAddr = CA Word
  deriving (Eq, Show)

instance Empty CodeAddr where
  empty' = CA maxBound

-- ----------------------------------------
