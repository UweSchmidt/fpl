{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module FPPL.MaMa.Instr
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes

-- ----------------------------------------

data Instr' d op = MkInt  Int
                 | MkBool Bool
                 | Jump d
                 | Comp op   -- 0-ary ops
                 | Halt
                 | Noop
                 | NotInUse
  deriving (Show)

type Instr op = Instr' Offset op

-- --------------------

instance (Pretty d, Pretty op) => Pretty (Instr' d op) where
  pretty = prettyInstr

prettyInstr :: (Pretty d, Pretty op) => Instr' d op -> String
prettyInstr = \ case
  MkInt  i -> printf "mkInt   " ++ pretty' i
  MkBool b -> printf "mkBool  " ++ pretty' b
  Jump   d -> printf "jump    " ++ pretty' d
  Comp op' -> pretty' op'
  Halt     -> "halt"
  Noop     -> "noop"
  NotInUse -> "unused"

instance (Pretty op) => Pretty (CodeAddr, Instr op) where
  pretty (pc, instr) = pretty pc ++ ": " ++ pretty instr ++ target instr
    where
      target = \case
        Jump d -> " --> " ++ pretty' (incr' (1 + d) pc)
        _      -> ""

-- ----------------------------------------
