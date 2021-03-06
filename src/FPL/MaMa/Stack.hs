{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module FPL.MaMa.Stack
  ( Stack
  , StackValue
  , stackSize
  , push
  , pop
  , pushSB
  , pushSA
  , asSB   -- basic value on stack
  , asSA   -- address on stack
  )
where

import FPL.Prelude
import FPL.MaMa.SimpleTypes

import Text.Pretty

import qualified Data.Sequence as S

-- ----------------------------------------

-- executable code
-- implemented as vector of instructions
-- due to static length of code segment
--
-- top of stack is at the end of the sequence

newtype Stack v = SK { _sk :: Seq (StackValue v) }
  deriving (Show)

instance Empty (Stack v) where
  empty' = SK S.empty
  null'  = S.null . _sk

instance Ixed (Stack v) where
  ix i f s = (values . ix i') f s
    where
      i' = offset2ix i s

type instance Index   (Stack v) = Offset
type instance IxValue (Stack v) = StackValue v

-- let fp = s ^. sp  -> p is a StackAddr of the last element in the sequence
-- let s1 = push ??? s
-- s1 & sp .~ fp  -> reset the stack to size when fp was set

instance StackPointer (Stack v) where
  sp k (SK v) = adjust <$> k toAdr
    where
      adjust n
        = SK $ S.take (n ^. isoOffset + 1) v
      toAdr
        = (S.length v - 1) ^. from isoOffset

-- convert offset to ix,
-- -1 indicates illegal index

offset2ix :: Offset -> Stack v -> Int
offset2ix i (SK v)
  | 0 <= i' && i' <= l' = i'
  | otherwise           = -1
  where
    l' = S.length v - 1
    i' = l' + i

values :: Iso' (Stack v) (Seq (StackValue v))
values = iso _sk SK

stackSize :: Stack v -> Int
stackSize s = S.length $ s ^. values

-- --------------------

push :: StackValue v -> Stack v -> Stack v
push v s = s & values %~ (S.|> v)

-- removes values from the stack
-- pop 0:  remove top of stack
-- pop -1: remove value below top opf stack
-- index out of bounds: return stack unchanged

pushSB :: v -> Stack v -> Stack v
pushSB v = push (asSB # v)

pushSA :: Addr -> Stack v -> Stack v
pushSA v = push (asSA # v)

pop :: Offset -> Stack v -> Stack v
pop i s = s & values %~ S.deleteAt (offset2ix i s)

-- ----------------------------------------

data StackValue v = SB v
                  | SA Addr
  deriving (Show)

asSB :: Prism' (StackValue v) v
asSB = prism
      SB
      (\ case
          SB y -> Right y
          x    -> Left  x
      )

asSA :: Prism' (StackValue v) Addr
asSA = prism
      SA
      (\ case
          SA y -> Right y
          x    -> Left  x
      )

-- ----------------------------------------
--
-- pretty printing

instance (Pretty v) => Pretty (Stack v) where
  pretty s = unlines $
    zipWith pretty' offsets $ s ^. values . isoSeqList . to reverse
    where
      offsets =
        map negate [(0 :: Offset)..]

      pretty' d v =
        fmtRow [("", alignR 6), (": ", alignR 8)] [pretty d, pretty v]

instance (Pretty v) => Pretty (StackValue v) where
  pretty (SB v) = pretty v
  pretty (SA a) = "<" ++ pretty a ++ ">"

-- ----------------------------------------
