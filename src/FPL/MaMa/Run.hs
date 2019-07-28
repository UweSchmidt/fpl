{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPL.MaMa.Run
where

import FPL.Prelude
import FPL.MaMa.ALU
import FPL.MaMa.Code
import FPL.MaMa.Heap ()
import FPL.MaMa.Instr
import FPL.MaMa.Interrupts
import FPL.MaMa.MachineState
import FPL.MaMa.MicroInstructions
import FPL.MaMa.Monad
import FPL.MaMa.Options
import FPL.MaMa.SimpleTypes
import FPL.MaMa.Stack ()
import FPL.MaMa.Value

import Text.Pretty

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

evalInstr :: (BasicValue v, ALU op) => Instr op -> MaMa op v ()
evalInstr = \case

                -- push basic values onto stack
  LoadInt  i -> pushBasic (asInt # i)
  LoadBool b -> pushBasic (asBool # b)

  LoadLit op' xs -- load an arbitrary basic value represented as string literal
             -> fromLiteral op' xs

                -- wrap a basic value into a new heap opject
  MkBasic    -> do v <- popBasic
                   a <- allocB v
                   pushAddr a

                -- unwrap a basic value out of a heap object
  GetBasic   -> do a <- popAddr
                   v <- deref a
                   b <- checkBasic (v ^? asB)
                   pushBasic b

                -- push the value of a local var onto stack
  PushLoc  d -> do v <- ixS d
                   pushS v

                -- push a value from the global vector onto stack
  PushGlb  d -> do a <- ixG d
                   pushAddr a

  Comp op'   -> -- apply an ALU operator,  arity may be 0, 1, 2, ...
                alu op'

                -- unconditional jump with displaceent
  Jump d     -> pc %= incr' d

                -- conditional jump with displaceent
  Branch b d -> do b1 <- popBV asBool
                   when (b == b1) $
                     pc %= incr' d

                -- cpu control ops
  Halt       -> abort Terminated
  Noop       -> return ()

-- ----------------------------------------
