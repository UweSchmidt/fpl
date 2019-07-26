{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.Run
where

import FPPL.Prelude
import FPPL.MaMa.ALU
import FPPL.MaMa.Code
import FPPL.MaMa.Heap ()
import FPPL.MaMa.Instr
import FPPL.MaMa.Interrupts
import FPPL.MaMa.MachineState
import FPPL.MaMa.MicroInstructions
import FPPL.MaMa.Monad
import FPPL.MaMa.Options
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Stack ()
import FPPL.MaMa.Value

-- ----------------------------------------

execMaMaProg :: (BasicValue v, ALU op, Pretty op)
             => Code op
             -> Options
             -> IO (MState op v)
execMaMaProg prog opts
  = snd <$> runMaMa execCode opts state0
  where
    state0 = empty' & code           .~ prog
                    & pc . isoOffset .~ 0     -- start address is 0

-- ----------------------------------------
--
-- the MaMa control unit

-- the main execution loop

execCode :: (BasicValue v, ALU op, Pretty op) => MaMa op v ()
execCode = do
  cont <- uses interrupt null'
  when cont $
    execInstr >> execCode

-- execute a single instruction

execInstr :: (BasicValue v, ALU op, Pretty op) => MaMa op v ()
execInstr = do
  traceInstr

  i <- getInstr
  pc %= incr' 1
  evalInstr i


-- get args, eval and store results for a single instr

evalInstr :: (BasicValue v, ALU op, Pretty op) => Instr op -> MaMa op v ()
evalInstr = \ case

  -- build basic values
  MkInt  i  -> pushBasic (asInt # i)
  MkBool b  -> pushBasic (asBool # b)

  -- exec an operation
  -- arity may vary 0, 1, 2, ...
  Comp op'  -> alu op'

  -- program flow
  Jump d    -> pc %= incr' d

  -- cpu control ops
  Halt      -> abort Terminated
  Noop      -> return ()
  i         -> abort (NotImplemented $ pretty' i)

-- ----------------------------------------
