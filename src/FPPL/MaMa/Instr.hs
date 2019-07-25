{-# LANGUAGE TypeFamilies #-}

module FPPL.MaMa.Instr
where

import FPPL.Prelude ()
import FPPL.MaMa.SimpleTypes

-- ----------------------------------------

data Instr' d op = Halt
                 | MkInt  Int
                 | MkBool Bool
                 | Jump d
                 | Comp op   -- 0-ary ops
                 | Noop
  deriving (Show)

type Instr op = Instr' Offset op

-- --------------------
