module FPL.Core.CompMonad
where

import FPL.Prelude
import FPL.Core.CompEnv
import FPL.Core.CompState

import           Control.Monad.RWS.Strict
import qualified Data.Sequence as L
import           System.IO

-- ----------------------------------------

newtype CompErr = CE {_compError :: String}
  deriving (Show)

type CompErrs = Seq CompErr

-- ----------------------------------------

type Comp op v
  = RWST CompEnv CompErrs (CompState op) IO v

runComp :: CompOptions
  -> Comp op v
  -> IO (v, (CompState op), CompErrs)
runComp opts cmd
  = runRWST cmd (compEnv0 opts) empty'

-- ----------------------------------------

issue :: String -> Comp op ()
issue msg
  = do liftIO $ hPutStrLn stderr ("code gen error: " ++ msg)
       tell $ L.singleton (CE msg)

-- ----------------------------------------