{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.MachineState
  ( Stack
  , code
  , stack
  , heap
  , pc
  , frp
  , globp
  )
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Value
import FPPL.MaMa.Heap
import FPPL.MaMa.Code
import FPPL.MaMa.Stack

-- ----------------------------------------
--
-- the machine state


data State v = ST { _code   :: ! Code
                  , _stack  :: ! (Stack v)
                  , _heap   :: ! (Heap v)
                  , _pc     :: ! CodeAddr       -- program counter
                  , _frp    :: ! Offset         -- frame pointer
                  , _globp  :: ! Vec            -- global pointer
                  }
  deriving (Show)


code :: Lens' (State v) Code
code k s = (\ n -> s { _code = n}) <$> k (_code s)

stack :: Lens' (State v) (Stack v)
stack k s = (\ n -> s { _stack = n}) <$> k (_stack s)

heap :: Lens' (State v) (Heap v)
heap k s = (\ n -> s { _heap = n}) <$> k (_heap s)

pc :: Lens' (State v) CodeAddr
pc k s = (\ n -> s { _pc = n}) <$> k (_pc s)

frp :: Lens' (State v) Offset
frp k s = (\ n -> s { _frp = n}) <$> k (_frp s)

globp :: Lens' (State v) Vec
globp k s = (\ n -> s { _globp = n}) <$> k (_globp s)

-- ----------------------------------------
