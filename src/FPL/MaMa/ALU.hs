{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa ALU (Arithmetic Logic Unit)

module FPL.MaMa.ALU
 ( module FPL.MaMa.ALU )
where

import FPL.Prelude
import FPL.MaMa.Interrupts
import FPL.MaMa.MicroInstructions
import FPL.MaMa.Monad
import FPL.MaMa.SimpleTypes ()
import FPL.MaMa.Value

-- ----------------------------------------
--
-- micro instructions for arithm.-logical ops

class ALU op where

  -- the compute operator map
  -- each operator is associated with an action
  -- getting the arguments from the stack,
  -- apply the operation and push the result back
  -- onto the stack
  --
  -- the alu function is used in the implementation
  -- Comp op

  alu :: BasicValue v => op -> MaMa op v ()

  -- for pushing arbitrary basic values built from
  -- literals onto the stack, we need representation
  -- as literal, here as string, and a conversion
  -- from string to a basic value, for every variant
  -- in the basic value v there must be a conversion
  -- operator from string.
  -- The conversion may fail, so it may raise an
  -- IllegalArgument interrupt
  --
  -- Int and Bool literals are handled by the
  -- builtin LoadInt and LoadBool machine instructions
  --
  -- fromLiteral is used in the implementation of
  -- the instruction LoadVal op

  fromLiteral :: BasicValue v => op -> String -> MaMa op v ()

-- ----------------------------------------
--
-- 0-ary operations
-- the operations run in the MaMa monad,
-- operations like getChar or random are candidates

ex0'M :: Prism' v res
      -> MaMa op v res
      -> MaMa op v ()
ex0'M asr f = do
  res <- f
  pushBasic (asr # res)

-- constants
ex0 :: Prism' v res
    -> res
    -> MaMa op v ()
ex0 asr c = do
  pushBasic (asr # c)

exAbort :: MaMa op v ()
exAbort = abort (NotImplemented "compute op")

-- ----------------------------------------
--
-- most general execution of unary ops
--
-- the operation runs in the MaMa monad

ex1'M :: Prism' v a1
      -> Prism' v res
      -> (a1 -> MaMa op v res)
      -> MaMa op v ()
ex1'M as1 asr f = do
  x1  <- popBV as1
  res <- f x1
  pushBasic (asr # res)

-- pure unary operation

ex1' :: Prism' v a1
     -> Prism' v res
     -> (a1 -> res)
     -> MaMa op v ()
ex1' as1 asr f = ex1'M as1 asr f'
  where
    f' x = return $ f x

-- unary inner ops

ex1 :: Prism' v a
    -> (a -> a)
    -> MaMa op v ()
ex1 as = ex1' as as

-- unary Int ops

ex1Int :: BasicValue v
       => (Int -> Int)
       -> MaMa op v ()
ex1Int = ex1 asInt

-- ----------------------------------------
--
-- most general execution of binary ops
--
-- the operation runs in the MaMa monad
-- useful for e.g. division by zero error detection

ex2'M :: Prism' v a1
      -> Prism' v a2
      -> Prism' v res
      -> (a1 -> a2 -> MaMa op v res)
      -> MaMa op v ()
ex2'M as1 as2 asr f = do
  x2  <- popBV as2
  x1  <- popBV as1
  res <- f x1 x2
  pushBasic (asr # res)

-- a pure binary operation
-- the most frequent case

ex2' :: Prism' v a1
     -> Prism' v a2
     -> Prism' v res
     -> (a1 -> a2 -> res)
     -> MaMa op v ()
ex2' as1 as2 asr f = ex2'M as1 as2 asr f'
  where
    f' x y = return $ f x y

-- binary inner ops

ex2 :: Prism' v a
    -> (a -> a -> a)
    -> MaMa op v ()
ex2 as = ex2' as as as

-- binary Int ops

ex2Int :: BasicValue v
       => (Int -> Int -> Int)
       -> MaMa op v ()
ex2Int = ex2 asInt

-- binary Bool ops

ex2Bool :: BasicValue v
       => (Bool -> Bool -> Bool)
       -> MaMa op v ()
ex2Bool = ex2 asBool

-- rel ops

ex2Rel :: BasicValue v
       => Prism' v a
       -> (a -> a -> Bool)
       -> MaMa op v ()
ex2Rel as = ex2' as as asBool

ex2RelInt :: BasicValue v
          => (Int -> Int -> Bool)
          -> MaMa op v ()
ex2RelInt = ex2Rel asInt

{-# INLINE ex2' #-}
{-# INLINE ex2Int #-}
{-# INLINE ex2Bool #-}
{-# INLINE ex2Rel #-}
{-# INLINE ex2RelInt #-}

-- --------------------
--
-- operations for Int and Bool values
--
-- operations for other basic machine values
-- can be defined simlarly

exAddInt,
  exSubInt,
  exMulInt,
  exEqInt,
  exNeInt,
  exLtInt,
  exLeInt,
  exGeInt,
  exGtInt :: BasicValue v => MaMa op v ()

exAddInt = ex2Int (+)
exSubInt = ex2Int (-)
exMulInt = ex2Int (*)

exEqInt  = ex2RelInt (==)
exNeInt  = ex2RelInt (/=)
exLtInt  = ex2RelInt (<)
exLeInt  = ex2RelInt (<=)
exGeInt  = ex2RelInt (>=)
exGtInt  = ex2RelInt (>)

exDivInt :: BasicValue v => MaMa op v ()
exDivInt = ex2'M asInt asInt asInt div'
  where
    div' x y
      | y == 0    = abort DivBy0
      | otherwise = return $ x `div` y

-- ----------------------------------------
--
-- the configurable conversion

pushLit :: (String -> Maybe a) -> Prism' v a -> String -> MaMa op v ()
pushLit cv asVal xs = do
  x <- checkPrim (cv xs)
  pushBasic (asVal # x)

-- ----------------------------------------

pushLitInt :: BasicValue v => String -> MaMa op v ()
pushLitInt = pushLit readMaybe asInt

pushLitBool :: BasicValue v => String -> MaMa op v ()
pushLitBool = pushLit readMaybe' asBool
  where
    readMaybe' "false" = Just False
    readMaybe' "true"  = Just True
    readMaybe' _       = Nothing

-- the default case: abort exec

pushLitAbort :: String -> MaMa op v ()
pushLitAbort _ = abort (NotImplemented "litteral conversion op")

-- ----------------------------------------
