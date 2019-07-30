{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module FPL.Core.Code
  ( ACode
  , AInstr
  , Label
  , assemble
  )
where

import FPL.Prelude
import FPL.MaMa.Code
import FPL.MaMa.Instr
import FPL.MaMa.SimpleTypes

import qualified Data.Sequence as L
import qualified Data.Map as M

import Text.Pretty

-- ----------------------------------------

type AInstr = Instr' Label

newtype Label = L String
  deriving (Eq, Ord, Show)

data ACode op
  = AC { _instrSeq :: ! (Seq (AInstr op))
       , _labMap   :: ! (Map Label Offset)
       }
    deriving (Show)

instance Semigroup (ACode op) where
  AC is1 lm1 <> AC is2 lm2
    = AC (is1 <> is2)
         (lm1 `M.union` fmap (+ disp) lm2)
    where
      disp = L.length is1

instance Monoid (ACode op) where
  mempty = AC mempty mempty

-- --------------------
--
-- resolve all labels by code address or distances
-- and deliver final code

assemble :: ACode op -> Code op
assemble (AC instrSeq labMap)
  = mkCode $ L.foldrWithIndex ass [] instrSeq
  where
    ass ix' instr rs = f instr : rs
      where
        f :: AInstr op1 -> Instr op1
        f (Branch c l1) = Branch c (displ l1)   -- local jumps use jump distances
        f (Jump     l1) = Jump     (displ l1)
        f instr'        = bimap (const ix') id instr'

        displ lab =
          maybe maxBound (\ t -> t - (ix' + 1)) $ M.lookup lab labMap

-- ----------------------------------------
--
-- the boring pretty printing

instance (Pretty op) => Pretty (ACode op) where
  pretty (AC iseq lm)
    = unlines . map pretty $ zip ls' cs'
    where
      cs' = iseq ^. isoSeqList
      ls' :: [[Label]]
      ls' = ls lm

      pr :: [(Offset, Label)] -> (Offset, [Label])
      pr xs
        = (fst . head $ xs, map snd xs)

      ls :: Map Label Offset -> [[Label]]
      ls lm'
        = f
          [0::Offset ..]
          (map pr . partBy fst . sort . map (\(x, y) -> (y, x)) . M.toList $ lm')

      f (i : is') lss@((j, ls1) : lss')
        | i <  j = []  : f is' lss
        | i == j = ls1 : f is' lss'
      f _is _lss
                 = repeat []


type LabeledInstr op = ([Label], AInstr op)

instance (Pretty op) => Pretty (LabeledInstr op) where
  pretty (ls, instr)
    = ( if null ls
        then ""
        else unlines (map prettyL ls)
      )
      ++
      prettyI instr
    where
      prettyL l = fmtCell ("", alignR 7 . (++ ":")) (pretty l)
      prettyI i = fmtRow [("", alignR 8), ("", alignL 1)] ["", pretty i]



instance Pretty Label where
  pretty (L xs) = xs

-- ----------------------------------------
{-
type Op1 = Bool
type AInstr1 = AInstr Op1

a1 :: ACode Op1
a1 = AC { _instrSeq = L.fromList
                      [ LoadInt 1
                      , LoadInt 2
                      , MkBasic
                      , Branch False (L "l7")
                      , LoadInt 21
                      , Jump (L "l0")
                      , LoadInt 42
                      , Halt
                      ]
        , _labMap   = M.fromList
                      [ (L "start", 0)
                      , (L "l0", 0)
                      , (L "l7", 7)
                      , (L "l5", 5)
                      ]
        }
-- -}
-- ----------------------------------------
