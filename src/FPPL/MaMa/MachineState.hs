{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.MachineState
  ( Stack
  , code
  , stack
  , heap
  , pc
  , fp
  )
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes
-- import FPPL.MaMa.Value
import FPPL.MaMa.Heap
import FPPL.MaMa.Code
import FPPL.MaMa.Stack

-- ----------------------------------------
--
-- the machine state


data State v = ST { _code           :: ! Code
                  , _stack          :: ! (Stack v)
                  , _heap           :: ! (Heap v)
                  , _programCounter :: ! CodeAddr       -- program counter
                  , _framePointer   :: ! StackAddr      -- frame pointer
                  , _globalPointer  :: ! Addr           -- global pointer
                  }
  deriving (Show)


code :: Lens' (State v) Code
code k s = (\ n -> s { _code = n}) <$> k (_code s)

stack :: Lens' (State v) (Stack v)
stack k s = (\ n -> s { _stack = n}) <$> k (_stack s)

heap :: Lens' (State v) (Heap v)
heap k s = (\ n -> s { _heap = n}) <$> k (_heap s)

pc :: Lens' (State v) CodeAddr
pc k s = (\ n -> s { _programCounter = n}) <$> k (_programCounter s)

fp :: Lens' (State v) StackAddr
fp k s = (\ n -> s { _framePointer = n}) <$> k (_framePointer s)

instance GlobalPointer (State v) where
  gp :: Lens' (State v) Addr
  gp k s = (\ n -> s { _globalPointer = n}) <$> k (_globalPointer s)

instance StackPointer (State v) where
  sp :: Lens' (State v) StackAddr
  sp = stack . sp

-- ----------------------------------------
