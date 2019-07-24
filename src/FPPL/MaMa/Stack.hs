{-# LANGUAGE TypeFamilies #-}

module FPPL.MaMa.Stack
  ( Stack
  , StackValue
  , push
  , pop
  , pushBasic
  , pushAddr
  , asSB   -- basic value on stack
  , asSA   -- address on stack
  )
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes

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

values :: Lens' (Stack v) (Seq (StackValue v))
values k (SK v) = (\ n -> SK n) <$> k v

-- --------------------

push :: StackValue v -> Stack v -> Stack v
push v s = s & values %~ (S.|> v)

-- removes values from the stack
-- pop 0:  remove top of stack
-- pop -1: remove value below top opf stack
-- index out of bounds: return stack unchanged

pushBasic :: v -> Stack v -> Stack v
pushBasic v = push (asSB # v)

pushAddr :: Addr -> Stack v -> Stack v
pushAddr v = push (asSA # v)

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
