{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module FPL.MaMa.Instr
  ( Instr'(..)
  , Instr
  )
where

import FPL.Prelude
import FPL.MaMa.SimpleTypes

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

instance Bifunctor Instr' where
  bimap :: (d -> d1)   -> (op -> op1)
        -> Instr' d op -> Instr' d1 op1
  bimap f g = \case
    LoadInt i       -> LoadInt i
    LoadBool b      -> LoadBool b
    LoadLit  op' xs -> LoadLit (g op') xs
    MkBasic         -> MkBasic
    GetBasic        -> GetBasic
    PushLoc o       -> PushLoc o
    PushGlb o       -> PushGlb o
    Jump d          -> Jump (f d)
    Branch c d      -> Branch c (f d)
    Comp op'        -> Comp (g op')
    Halt            -> Halt
    Noop            -> Noop

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
