{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

-- ----------------------------------------

module Main
where

import FPL

import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT tlist1
  return ()

tlist1 :: Test
tlist1
  = TestList
    [ aTest "p0: halt" p0
      ( hasTerminated
        <&&>
        stackLenIs (== 0)
      )
    , aTest "p1: the answer" p1
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    , aTest "p2: div by 0" p2
      ( (\ s -> s ^. interrupt == DivBy0)
        <&&>
        stackLenIs (== 0)
      )
    , aTest "p3: compare and or" p3
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsBool 0 id
      )
    , aTest "p4: jump to halt" p4
      ( hasTerminated
        <&&>
        stackLenIs (== 0)
      )
    , aTest "p5: jump, jump, halt" p5
      ( hasTerminated
        <&&>
        stackLenIs (== 0)
      )
    , aTest "nudel: jump -1" nudel
      ( (\ s -> s ^. interrupt == LoopInv)
        <&&>
        stackLenIs (== 0)
      )
    , aTest "p6: branch and jump" p6
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    , aTest "p7: build heap object" p7
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    , aTest "p8: duplicate value on stack (1)" p8
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsBool 0 id
      )
    , aTest "p9: duplicate value on stack (2)" p9
      ( hasTerminated
        <&&>
        stackLenIs (== 2)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    , aTest "p10: conversion of string literals" p10
      ( hasTerminated
        <&&>
        stackLenIs (== 1)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    , aTest "p11: 4 values on stack" p11
      ( hasTerminated
        <&&>
        stackLenIs (== 4)
        <&&>
        stackAtIsInt 0 (== 42)
      )
    ]

infixr 3 <&&>

-- ----------------------------------------

-- manual test

testMaMa :: Code1 -> IO ()
testMaMa p = do
  putStr $ unlines (header '=' "program trace")
  exec1 p >>= putStrLn . pretty

-- ----------------------------------------
--
-- HUnit test case

aTest :: String -> Code1 -> Pred1 -> Test
aTest name' prog predicate
  = TestLabel name' $
    TestCase $
    do finalState <- exec1 prog
       assertBool "wrong result" (predicate finalState)

-- ----------------------------------------

type Pred1 = MState1 -> Bool

(<&&>) :: Pred1 -> Pred1 -> Pred1
(<&&>) = liftA2 (&&)

hasTerminated :: MState1 -> Bool
hasTerminated s
  = s ^. interrupt == Terminated

stackLenIs :: (Int -> Bool) -> MState1 -> Bool
stackLenIs p s
  = p (stackSize $ s ^. stack)

stackAtIsInt :: Offset -> (Int -> Bool) -> MState1 -> Bool
stackAtIsInt offset p s
  = maybe False p (s ^? stack . ix offset . asSB . asInt)

stackAtIsBool :: Offset -> (Bool -> Bool) -> MState1 -> Bool
stackAtIsBool offset p s
  = maybe False p (s ^? stack . ix offset . asSB . asBool)

-- ----------------------------------------
--
-- testMaMas

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

-- stack dump
p11 :: Code1
p11= mkCode
  [ LoadInt 42
  , MkBasic
  , LoadInt 23
  , MkBasic
  , PushLoc 0
  , GetBasic
  , PushLoc (-2)
  , GetBasic
  , Halt
  ]

-- ----------------------------------------

-- Value for MaMa with Int and Bool as basic values
--
-- 2. simplest basic value: an Int or a Bool

-- ----------------------------------------

type MaMa1   = MaMa   Op1 Val1
type Instr1  = Instr  Op1
type Code1   = Code   Op1
type MState1 = MState Op1 Val1

-- ----------------------------------------

data Val1 = VInt  ! Int
          | VBool ! Bool

instance BasicValue Val1 where
  asInt = prism
          VInt
          (\ case
              VInt y -> Right y
              x      -> Left  x
          )

  asBool = prism
           VBool
           (\ case
               VBool y -> Right y
               x       -> Left  x
           )

-- ----------------------------------------

data Op1 = --binary ops
           Addi | Subi | Muli | Divi
         | EQi  | NEi
         | LSi  | LEi  | GEi  | GRi
         | And  | Or
           -- unary ops
         | Neg
         | Not
         | B2I  | I2B
           -- conversion of literals
         | ToInt | ToBool

instance ALU Op1 where
  alu = \case
    -- binary ops
    Addi -> exAddInt
    Subi -> exSubInt
    Muli -> exMulInt
    Divi -> exDivInt
    EQi  -> exEqInt
    NEi  -> exNeInt
    LSi  -> exLtInt
    LEi  -> exLeInt
    GEi  -> exGeInt
    GRi  -> exGtInt
    And  -> ex2Bool (&&)
    Or   -> ex2Bool (||)

    -- unary ops
    Neg  -> ex1  asInt negate
    Not  -> ex1  asBool not
    B2I  -> ex1' asBool asInt  fromEnum
    I2B  -> ex1' asInt  asBool (== 0)

    -- default
    _    -> exAbort

--  fromLiteral :: Op1 -> MaMa1 ()
  fromLiteral = \case
    ToInt  -> pushLitInt
    ToBool -> pushLitBool
    _      -> pushLitAbort

-- ----------------------------------------

exec1 :: Code1 -> IO MState1
exec1 = flip execMaMaProg opts
  where
    opts = defaultOptions & mamaTrace .~ True

-- ----------------------------------------
--
-- pretty printing and test output

instance Pretty Val1 where
  pretty = \case
    VInt  i -> show i
    VBool b -> pretty b

instance Pretty Op1 where
  pretty  =  map toLower . show

deriving instance Show Op1
deriving instance Show Val1

-- ----------------------------------------
