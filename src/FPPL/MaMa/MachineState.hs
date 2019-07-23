{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.MachineState
where

import FPPL.Prelude ()
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Value
import FPPL.MaMa.Heap
import FPPL.MaMa.Code

-- ----------------------------------------
--
-- the machine state


data State v = ST { _code  :: ! Code
                  -- , _stack :: ! Stack
                  , _heap  :: ! (Heap v)
                  , _pc    :: ! CodeAddr
                  , _fp    :: ! Offset
                  , _sp    :: ! Offset
                  , _gp    :: ! Vec
                  }
  deriving (Show)


{-
theUnused :: Lens' (Heap v) [Addr]
theUnused k h = (\ n -> h { _unused = n}) <$> k (_unused h)

theAlcnt :: Lens' (Heap v) Int
theAlcnt k h = (\ n -> h { _alcnt = n}) <$> k (_alcnt h)

theHeapCnt :: Getter (Heap v) Int
theHeapCnt = theHeap . to M.size
-}

-- ----------------------------------------
