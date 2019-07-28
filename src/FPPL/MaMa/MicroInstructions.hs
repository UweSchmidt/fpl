{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa micro instrucctions

module FPPL.MaMa.MicroInstructions
where

import FPPL.Prelude
import FPPL.MaMa.Code
import FPPL.MaMa.Heap
import FPPL.MaMa.Instr
import FPPL.MaMa.Interrupts
import FPPL.MaMa.MachineState
import FPPL.MaMa.Monad
import FPPL.MaMa.Options
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Stack
import FPPL.MaMa.Value

import Text.Pretty

import Control.Monad.Except
import System.IO (hPutStrLn, stderr)

import qualified Data.Monoid (First)


-- ----------------------------------------
--
-- basic monadic actions for
-- running the MaMa machine

-- --------------------
--
-- read current instruction

getInstr :: MaMa op v (Instr op)
getInstr = do
  pc'  <- use pc
  getCode (ix pc')

-- --------------------
--
-- stack micro instrucctions

-- indexed acces into stack

ixS :: Offset -> MaMa op v (StackValue v)
ixS i = getStack (ix i)

-- take value from top of stack

popS :: MaMa op v (StackValue v)
popS = do
  v <- getStack (ix 0)
  stack %= pop 0
  return v

-- take basic value

popBasic :: MaMa op v v
popBasic = do
  v <- popS
  checkBasic (v ^? asSB)

-- take basic value from the stack
-- and unwrap it into a raw value

popBV :: Prism' v b -> MaMa op v b
popBV as = do
  v <- popS
  checkPrim (v ^? asSB . as)

-- tkae an address from the stack

popAddr :: MaMa op v Addr
popAddr = do
  v <- popS
  checkAddr (v ^? asSA)

pushS :: StackValue v -> MaMa op v ()
pushS v = stack %= push v

-- push a basic value onto the stack

pushBasic :: v -> MaMa op v ()
pushBasic v = stack %= pushSB v

-- push a heap address onto the stack

pushAddr :: Addr -> MaMa op v ()
pushAddr a = stack %= pushSA a

{-# INLINE pushS #-}
{-# INLINE pushBasic #-}
{-# INLINE pushAddr #-}

-- --------------------
--
-- heap micro instrucctions

allocHeap :: Prism' (Value v) v -> v -> MaMa op v Addr
allocHeap toval v = do
  h <- use heap
  let (a, h') = alloc (toval # v) h
  heap .= h'
  return a

allocB :: v -> MaMa op v Addr
allocB = allocHeap asB

allocC :: Closure -> MaMa op Closure Addr
allocC = allocHeap asC

allocF :: Function -> MaMa op Function Addr
allocF = allocHeap asF

allocV :: Vec -> MaMa op Vec Addr
allocV = allocHeap asV

deref :: Addr -> MaMa op v (Value v)
deref a
  | null' a   = abort NullPointer
  | otherwise = do
      x1 <- use (heap . at a)
      checkHeapAccess x1

derefBasic :: Addr -> MaMa op v v
derefBasic a
  | null' a   = abort NullPointer
  | otherwise = do
      x1 <- use (heap . at a)
      x2 <- checkHeapAccess x1
      x3 <- checkBasic (x2 ^? asB)
      return x3

-- --------------------
--
-- access global vector

ixG :: Offset -> MaMa op v Addr
ixG offset = do
  val <- use gp >>= deref
  vec <- checkVec (val ^? asV)
  adr <- checkVecIx (vec ^? ix offset)
  return adr

-- --------------------
--
-- access a state component
-- if access undefined abort execution

getM :: (Maybe b -> MaMa op v b)
     -> Getting s (MState op v) s
     -> Getting (Data.Monoid.First b) s b
     -> MaMa op v b
getM check' var part = do
  x <- use var
  check' (x ^? part)

{-# INLINE getM #-}


-- indexed access into code: get an instructio

getCode :: Getting (Data.Monoid.First b) (Code op) b -> MaMa op v b
getCode  = getM checkCode code

-- indexed access into the stack: get a basic value or an address

getStack :: Getting (Data.Monoid.First b) (Stack v) b -> MaMa op v b
getStack = getM checkStack stack

-- --------------------
--
-- check a value and abort if not there

check :: Interrupt -> Maybe a -> MaMa op v a
check _  (Just x) = return x
check ir _        = abort ir

{-# INLINE check #-}

checkBasic :: Maybe a -> MaMa op v a
checkBasic = check (IllegalArgument "Basic value expected")

checkAddr :: Maybe a -> MaMa op v a
checkAddr  = check (IllegalArgument "Heap address expected")

checkVec :: Maybe a -> MaMa op v a
checkVec  = check (IllegalArgument "Vector expected")

checkVecIx :: Maybe a -> MaMa op v a
checkVecIx = check IllegalVecIndex

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

checkHeapAccess :: Maybe a -> MaMa op v a
checkHeapAccess = check IllegalHeapAddr



{-# INLINE checkBasic #-}
{-# INLINE checkAddr #-}
{-# INLINE checkCode #-}
{-# INLINE checkStack #-}
{-# INLINE checkPrim #-}
{-# INLINE checkInt #-}
{-# INLINE checkBool #-}

-- --------------------
--
-- abort execution

abort :: Interrupt -> MaMa op v a
abort ir = do
  interrupt .= ir
  throwError ()

{-# INLINE abort #-}

-- ----------------------------------------

traceInstr :: (Pretty op) => MaMa op v ()
traceInstr =
  whenM (view mamaTrace) $ do
    is' <- getInstr
    pc' <- use pc
    putStrLnStderr $ pretty (pc', is')

-- ----------------------------------------
--
-- ausiliary IO actions

putStrLnStderr :: String -> MaMa op v ()
putStrLnStderr = liftIO . hPutStrLn stderr

-- ----------------------------------------
