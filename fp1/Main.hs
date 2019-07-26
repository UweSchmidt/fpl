module Main
  -- (main)
where

import FPPL
import FP1.Value

main :: IO ()
main = test0

-- ----------------------------------------

-- it runs (2019-07-26)

test0, test1 :: IO ()

test0 = exec1 p0 >>= print
test1 = exec1 p1 >>= print
test2 = exec1 p1 >>= print

-- ----------------------------------------
--
-- tests

-- the smallest prog
p0 :: Code1
p0 = mkCode [Halt]

-- the answer
p1 :: Code1
p1 = mkCode
  [ MkInt 7
  , MkInt 3
  , MkInt 4
  , Comp Addi
  , Comp Muli
  , MkInt 7
  , Comp Subi
  , Halt
  ]

-- div by 0
p2 :: Code1
p2 = mkCode
  [ MkInt 1
  , MkInt 0
  , Comp Divi
  , Halt
  ]

-- ----------------------------------------
