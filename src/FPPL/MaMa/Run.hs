{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.Run
where

import FPPL.Prelude
import FPPL.MaMa.Code
import FPPL.MaMa.Heap
import FPPL.MaMa.Instr
import FPPL.MaMa.Interrupts
import FPPL.MaMa.MachineState
import FPPL.MaMa.Options
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Stack
import FPPL.MaMa.Value

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Monoid (First)

-- ----------------------------------------

type MaMa op v
    = ExceptT () (ReaderT Options (StateT (MState op v) IO))

runMaMa :: MaMa op v r
        -> Options
        -> MState op v
        -> IO (Either () r, MState op v)
runMaMa cmd opts state0
  = runStateT (runReaderT (runExceptT cmd) opts) state0

execMaMaProg :: BasicValue v => Code op -> Options -> IO (MState op v)
execMaMaProg prog opts
  = snd <$> runMaMa execCode opts state0
  where
    state0 = empty' & code .~ prog

-- ----------------------------------------
--
-- the MaMa control unit

-- the main execution loop

execCode :: BasicValue v => MaMa op v ()
execCode = do
  cont <- uses interrupt null'
  when cont $
    execInstr >> execCode

-- execute a single instruction

execInstr :: BasicValue v => MaMa op v ()
execInstr = do
  i <- getInstr
  pc %= incr' 1
  evalInstr i

-- read current instruction

getInstr :: MaMa op v (Instr op)
getInstr = do
  pc'  <- use pc
  getCode (ix pc')

-- eval an instruction

evalInstr :: BasicValue v => Instr op -> MaMa op v ()
evalInstr Halt
  = abort Terminated

evalInstr (MkInt i)
  = pushBasic (asInt # i)

evalInstr (MkBool b)
  = pushBasic (asBool # b)

evalInstr i
  = abort (NotImplemented "Instuction")

-- --------------------
--
-- micro instructions for arithm.-logical ops

type ALU op v = op -> MaMa op v ()

-- most general execution of binary ops
--
-- the operation runs in the MaMa monad
-- useful for e.g. division by zero error detection

ex2'M :: Prism' v a1
      -> Prism' v a2
      -> Prism' v res
      -> (a1 -> a2 -> MaMa op v res)
      -> MaMa op v ()
ex2'M as1 as2 asr f2 = do
  x2  <- popBV as2
  x1  <- popBV as1
  res <- f2 x1 x2
  pushBasic (asr # res)

-- a pure binary operation
-- the most frequent case

ex2' :: Prism' v a1
     -> Prism' v a2
     -> Prism' v res
     -> (a1 -> a2 -> res)
     -> MaMa op v ()
ex2' as1 as2 asr f2 = ex2'M as1 as2 asr f2'
  where
    f2' x y = return $ f2 x y

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

-- --------------------
--
-- micro instructions

-- take value from top of stack

popS :: MaMa op v (StackValue v)
popS = do
  v <- getStack (ix 0)
  stack %= pop 1
  return v

popBasic :: MaMa op v v
popBasic = do
  v <- popS
  checkBasic (v ^? asSB)

popBV :: Prism' v b -> MaMa op v b
popBV as = do
  v <- popS
  checkPrim (v ^? asSB . as)

popAddr :: MaMa op v Addr
popAddr = do
  v <- popS
  checkAddr (v ^? asSA)

pushBasic :: v -> MaMa op v ()
pushBasic v = do
  stack %= pushSB v

pushAddr :: Addr -> MaMa op v ()
pushAddr a = do
  stack %= pushSA a

-- --------------------
--
-- primitive monadic operations

-- access a state component
-- if access not defined abort execution

getM :: (Maybe b -> MaMa op v b)
     -> Getting s (MState op v) s
     -> Getting (Data.Monoid.First b) s b
     -> MaMa op v b
getM check' var part = do
  x <- use var
  check' (x ^? part)


getCode :: Getting (Data.Monoid.First b) (Code op) b -> MaMa op v b
getCode  = getM checkCode code

getStack :: Getting (Data.Monoid.First b) (Stack v) b -> MaMa op v b
getStack = getM checkStack stack

-- --------------------
--
-- check a value and abort if not there

check :: Interrupt -> Maybe a -> MaMa op v a
check _  (Just x) = return x
check ir _        = abort ir

checkBasic :: Maybe a -> MaMa op v a
checkBasic = check (IllegalArgument "Basic value expected")

checkAddr :: Maybe a -> MaMa op v a
checkAddr  = check (IllegalArgument "Heap address expected")

checkCode :: Maybe a -> MaMa op v a
checkCode  = check IllegalCodeAddr

checkStack :: Maybe a -> MaMa op v a
checkStack = check IllegalStackAddr

checkPrim :: Maybe a -> MaMa op v a
checkPrim = check (IllegalArgument "Illegal primitive value")

checkInt :: Maybe a -> MaMa op v a
checkInt = check  (IllegalArgument "Int value expected")

checkBool :: Maybe a -> MaMa op v a
checkBool = check  (IllegalArgument "Bool value expected")

-- --------------------
--
-- abort execution

abort :: Interrupt -> MaMa op v a
abort ir = do
  interrupt .= ir
  throwError ()

{-# INLINE abort #-}
{-# INLINE check #-}
{-# INLINE getM  #-}

-- ----------------------------------------
