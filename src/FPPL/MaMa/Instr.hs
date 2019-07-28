{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module FPPL.MaMa.Instr
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes

import Text.Pretty

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
  LoadInt  i     -> fmt ["loadc", pretty i]
  LoadBool b     -> fmt ["loadc", pretty b]
  LoadLit op' xs -> fmt ["loadc", pretty op', show xs]
  MkBasic        -> fmt ["mkbasic"]
  GetBasic       -> fmt ["getbasic"]
  PushLoc     d  -> fmt ["pushloc", pretty d]
  PushGlb     d  -> fmt ["pushglb", pretty d]
  Branch    b d  -> fmt [ if b
                          then "brtrue"
                          else "brfalse"
                        , pretty d
                        ]

  Jump         d -> fmt ["jump", pretty d]
  Comp       op' -> fmt [pretty op']
  Halt           -> fmt ["halt"]
  Noop           -> fmt ["noop"]

  where
    fmt = fmtRow [("", alignL 8), (" ", alignR 8), (" ", id)]

instance (Pretty op) => Pretty (CodeAddr, Instr op) where
  pretty (pc, instr) = case instr of
    Jump     d -> prettyJ d
    Branch _ d -> prettyJ d
    _          -> prettyS
    where
        fmt
          = fmtRow [("", alignR 6), (": ", alignL 18), (" --> ", id)]
        prettyS
          =  fmt [prettyPc, pretty instr]

        prettyJ d
          = fmt [prettyPc, pretty instr, pretty (incr' (1 + d) pc)]

        prettyPc
          = show $ pc ^. isoOffset

-- ----------------------------------------
