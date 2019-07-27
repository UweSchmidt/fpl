{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module FPPL.MaMa.Instr
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes

-- ----------------------------------------

data Instr' d op = LoadInt  Int
                 | LoadBool Bool
                 | LoadLit  op String
                 | MkBasic
                 | GetBasic
                 | PushLoc  Offset
                 | PushGlb  Offset
                 | Jump d
                 | Branch Bool d
                 | Comp op   -- 0-ary ops
                 | Halt
                 | Noop
  deriving (Show)

type Instr op = Instr' Offset op

-- --------------------

instance (Pretty d, Pretty op) => Pretty (Instr' d op) where
  pretty = prettyInstr

prettyInstr :: (Pretty d, Pretty op) => Instr' d op -> String
prettyInstr = \ case
  LoadInt  i     -> "loadc"   `app8` pretty' i
  LoadBool b     -> "loadc"   `app8` pretty' b
  LoadLit op' xs -> "loadc"   `app8` (pretty op' `app4` show xs)
  MkBasic        -> "mkbasic"
  GetBasic       -> "getbasic"
  PushLoc     d  -> "pushloc" `app8` pretty' d
  PushGlb     d  -> "pushglb" `app8` pretty' d
  Branch    b d  -> ( if b
                      then "brtrue"
                      else "brfalse"
                    )
                    `app8` pretty' d

  Jump         d -> "jump" `app8` pretty' d
  Comp       op' -> pretty' op'
  Halt           -> "halt"
  Noop           -> "noop"

instance (Pretty op) => Pretty (CodeAddr, Instr op) where
  pretty (pc, instr) = pretty pc ++ ": " ++ pretty instr ++ target instr
    where
      target = \case
        Jump d -> "    --> " ++ pretty' (incr' (1 + d) pc)
        _      -> ""

-- ----------------------------------------
