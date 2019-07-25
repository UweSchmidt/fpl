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
                 | Op0 op   -- 0-ary ops
                 | Op1 op   -- unary ops
                 | Op2 op   -- binary ops
  deriving (Show)

type Instr op = Instr' CodeAddr op

-- --------------------
