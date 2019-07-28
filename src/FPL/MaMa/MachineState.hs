{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPL.MaMa.MachineState
  ( MState
  , code
  , stack
  , heap
  , pc
  , fp
  , interrupt
  )
where

import FPL.Prelude
import FPL.MaMa.Code
import FPL.MaMa.Heap
import FPL.MaMa.Interrupts
import FPL.MaMa.SimpleTypes
import FPL.MaMa.Stack
-- import FPL.MaMa.Value

import Text.Pretty

-- ----------------------------------------
--
-- the machine state


data MState op v
  = ST { _code           :: ! (Code op)
       , _stack          :: ! (Stack v)
       , _heap           :: ! (Heap v)
       , _programCounter :: ! CodeAddr       -- program counter
       , _framePointer   :: ! StackAddr      -- frame pointer
       , _globalPointer  :: ! Addr           -- global pointer
       , _interrupt      :: ! Interrupt
       }
  deriving (Show)

instance Empty (MState op v) where
  empty'
    = ST  { _code           = empty'
          , _stack          = empty'
          , _heap           = empty'
          , _programCounter = empty'
          , _framePointer   = empty'
          , _globalPointer  = empty'
          , _interrupt      = empty'
          }
  null' s
    = (   null' $ _code  s)
      && (null' $ _stack s)
      && (null' $ _heap  s)

code :: Lens' (MState op v) (Code op)
code k s = (\ n -> s { _code = n}) <$> k (_code s)

stack :: Lens' (MState op v) (Stack v)
stack k s = (\ n -> s { _stack = n}) <$> k (_stack s)

heap :: Lens' (MState op v) (Heap v)
heap k s = (\ n -> s { _heap = n}) <$> k (_heap s)

pc :: Lens' (MState op v) CodeAddr
pc k s = (\ n -> s { _programCounter = n}) <$> k (_programCounter s)

fp :: Lens' (MState op v) StackAddr
fp k s = (\ n -> s { _framePointer = n}) <$> k (_framePointer s)

instance GlobalPointer (MState op v) where
  gp :: Lens' (MState op v) Addr
  gp k s = (\ n -> s { _globalPointer = n}) <$> k (_globalPointer s)

instance StackPointer (MState op v) where
  sp :: Lens' (MState op v) StackAddr
  sp = stack . sp

interrupt :: Lens' (MState op v) Interrupt
interrupt k s = (\ n -> s { _interrupt = n}) <$> k (_interrupt s)

-- ----------------------------------------

instance (Pretty op, Pretty v) => Pretty (MState op v) where
  pretty = prettyMState

prettyMState :: (Pretty op, Pretty v) => MState op v -> String
prettyMState s =
  (unlines $ header '=' "machine state")
  ++
  prettyRegisters
  ++
  prettyStack
{-
  ++
  prettyPc (s ^. pc)
  ++
  prettyInterrupt (s ^. interrupt)
  ++
  prettyFp (s ^. fp)
  ++
  prettyGp (s ^. gp)
-}
  ++
  prettyHeap
  ++
  prettyCode
  ++
  []
  where
    prettyHeap :: String
    prettyHeap =
      (unlines $ header '-' "heap")
      ++
      pretty (s ^. heap)

    prettyCode :: String
    prettyCode =
      (unlines $ header '-' "program code")
      ++
      pretty (s ^. code)

    prettyStack :: String
    prettyStack =
      (unlines $ header '-' "stack")
      ++
      pretty (s ^. stack)

    prettyRegisters :: String
    prettyRegisters =
      (unlines $ header '-' "registers")
      ++
      ( unlines $
        map fmt
        [ ["pc", pretty (s ^. pc)]
        , ["fp", pretty (s ^. fp)]
        , ["gp", pretty (s ^. gp)]
        , ["ir", pretty (s ^. interrupt)]
        ]
      )
      where
        fmt = fmtRow [("", alignR 6), (": ", alignR 8)]

-- ----------------------------------------
