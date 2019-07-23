{-# LANGUAGE TypeFamilies #-}

module FPPL.MaMa.Instr
where

import FPPL.Prelude ()
import FPPL.MaMa.SimpleTypes

-- ----------------------------------------

data Instr' d = NOOP
              | Jump d
  deriving (Show)

type Instr = Instr' CodeAddr

-- --------------------
