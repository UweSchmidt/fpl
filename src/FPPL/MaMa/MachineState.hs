{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.MachineState
  ( MState
  , code
  , stack
  , heap
  , pc
  , fp
  , interrupt
  )
where

import FPPL.Prelude
import FPPL.MaMa.Code
import FPPL.MaMa.Heap
import FPPL.MaMa.Interrupts
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Stack
-- import FPPL.MaMa.Value

-- ----------------------------------------
--
-- the machine state


data MState v = ST { _code           :: ! Code
                   , _stack          :: ! (Stack v)
                   , _heap           :: ! (Heap v)
                   , _programCounter :: ! CodeAddr       -- program counter
                   , _framePointer   :: ! StackAddr      -- frame pointer
                   , _globalPointer  :: ! Addr           -- global pointer
                   , _interrupt      :: ! Interrupt
                   }
  deriving (Show)

instance Empty (MState v) where
  empty' = ST  { _code           = empty'
               , _stack          = empty'
               , _heap           = empty'
               , _programCounter = empty'
               , _framePointer   = empty'
               , _globalPointer  = empty'
               , _interrupt      = empty'
               }
  null' s =  (null' $ _code  s)
          && (null' $ _stack s)
          && (null' $ _heap  s)

code :: Lens' (MState v) Code
code k s = (\ n -> s { _code = n}) <$> k (_code s)

stack :: Lens' (MState v) (Stack v)
stack k s = (\ n -> s { _stack = n}) <$> k (_stack s)

heap :: Lens' (MState v) (Heap v)
heap k s = (\ n -> s { _heap = n}) <$> k (_heap s)

pc :: Lens' (MState v) CodeAddr
pc k s = (\ n -> s { _programCounter = n}) <$> k (_programCounter s)

fp :: Lens' (MState v) StackAddr
fp k s = (\ n -> s { _framePointer = n}) <$> k (_framePointer s)

instance GlobalPointer (MState v) where
  gp :: Lens' (MState v) Addr
  gp k s = (\ n -> s { _globalPointer = n}) <$> k (_globalPointer s)

instance StackPointer (MState v) where
  sp :: Lens' (MState v) StackAddr
  sp = stack . sp

interrupt :: Lens' (MState v) Interrupt
interrupt k s = (\ n -> s { _interrupt = n}) <$> k (_interrupt s)

-- ----------------------------------------
