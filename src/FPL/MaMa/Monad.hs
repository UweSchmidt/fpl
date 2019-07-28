{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa monad

module FPL.MaMa.Monad
  ( MaMa
  , runMaMa
  )
where

import FPL.Prelude ()
import FPL.MaMa.MachineState
import FPL.MaMa.Options


import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- ----------------------------------------
--
-- the MaMa monad is an exception-state-IO monad stack
--
-- exception type is (),
-- the error value is stored in
-- the interrupt register of the machine state
--
-- the reader is used for the runtime options

type MaMa op v
    = ExceptT () (ReaderT Options (StateT (MState op v) IO))

runMaMa :: MaMa op v r
        -> Options
        -> MState op v
        -> IO (Either () r, MState op v)
runMaMa cmd opts state0
  = runStateT (runReaderT (runExceptT cmd) opts) state0

-- ----------------------------------------
