{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPPL.MaMa.Run
where

import FPPL.Prelude
-- import FPPL.MaMa.SimpleTypes
-- import FPPL.MaMa.Value
-- import FPPL.MaMa.Heap
-- import FPPL.MaMa.Code
-- import FPPL.MaMa.Stack
import FPPL.MaMa.Interrupts
import FPPL.MaMa.MachineState

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- ----------------------------------------

data Options = MOPS { _mamaTrace :: Bool }
  deriving (Show)

defaultOptions :: Options
defaultOptions = MOPS { _mamaTrace = True }

-- ----------------------------------------

type MaMa v
    = ExceptT () (ReaderT Options (StateT (MState v) IO))

runMaMa :: MaMa v r
        -> Options
        -> MState v
        -> IO (Either () r, MState v)
runMaMa cmd opts state0
    = runStateT (runReaderT (runExceptT cmd) opts) state0

abort :: Interrupt -> MaMa v ()
abort ir = do
  interrupt .= ir
  throwError ()

-- ----------------------------------------
