module Main
  -- (main)
where

import FPL
import FP1.Value

main :: IO ()
main = test0

-- ----------------------------------------

-- it runs (2019-07-26)

test0, test1, test2, test3 :: IO ()

test0 = exec1 p0 >>= print
test1 = exec1 p1 >>= print
test2 = exec1 p2 >>= print
test3 = exec1 p3 >>= print

-- ----------------------------------------
--
-- tests

-- the smallest prog
p0 :: Code1
p0 = mkCode [Halt]

-- the answer
p1 :: Code1
p1 = mkCode
  [ LoadInt 7
  , LoadInt 3
  , LoadInt 4
  , Comp Addi
  , Comp Muli
  , LoadInt 7
  , Comp Subi
  , Halt
  ]

-- div by 0
p2 :: Code1
p2 = mkCode
  [ LoadInt 1
  , LoadInt 0
  , Comp Divi
  , Halt
  ]

-- compare and or
p3 :: Code1
p3 = mkCode
  [ LoadBool False
  , Comp Not
  , LoadInt 1
  , LoadInt 0
  , Comp NEi
  , Comp Or
  , Halt
  ]

-- jump test
p4 :: Code1
p4 = mkCode
  [ Jump 6      -- jump to Halt
  , LoadBool False
  , Comp Not
  , LoadInt 1
  , LoadInt 0
  , Comp NEi
  , Comp Or
  , Halt
  ]

p5 :: Code1
p5 = mkCode
  [ Jump 0  --     jump l1
  , Jump 1  -- l1: jump l2
  , Noop
  , Halt    -- l2: halt
  ]

-- runs pretty long
nudel :: Code1
nudel = mkCode
  [ Jump (-1)  -- l1: jump l1
  , Halt
  ]

-- branch and jump
p6 :: Code1
p6 = mkCode
  [ LoadInt 1
  , LoadInt 2
  , Comp EQi
  , Branch False 2
  , LoadInt 21
  , Jump 1
  , LoadInt 42
  , Halt    -- l2: halt
  ]

-- build heap object and deref object
p7 :: Code1
p7 = mkCode
  [ LoadInt 42
  , MkBasic
  , GetBasic
  , Halt
  ]

-- duplicate value on stack (1)
p8 :: Code1
p8 = mkCode
  [ LoadInt 42
  , PushLoc 0
  , Comp EQi
  , Halt
  ]

-- duplicate value on stack (2)
p9 :: Code1
p9 = mkCode
  [ LoadInt 42
  , MkBasic
  , PushLoc 0
  , GetBasic
  , Halt
  ]


-- conversion of string literals to basic values
p10 :: Code1
p10 = mkCode
  [ LoadLit ToInt "7"
  , LoadLit ToInt "3"
  , LoadLit ToInt "3"
  , LoadLit ToBool "true"
  , Comp B2I
  , Comp Addi
  , Comp Addi
  , Comp Muli
  , LoadInt 7
  , Comp Subi
  , Halt
  ]

-- ----------------------------------------
