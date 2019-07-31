{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module FPL.Core.CompMonad
where

import FPL.Prelude
import FPL.MaMa.SimpleTypes
import FPL.Core.MaMaCode

import Control.Monad.RWS.Strict

import qualified Data.Map as M
import qualified Data.Sequence as L

-- import Text.Pretty

-- ----------------------------------------

data CompState op
  = CS { _codeActive  :: ACode op
       , _codeDelayed :: [ACode op]
       , _codeDone    :: ACode op
       , _labGen   :: Int
       }
    deriving (Show)

instance Empty (CompState op) where
  empty'
    = CS { _codeActive  = empty'
         , _codeDelayed = []
         , _codeDone    = empty'
         , _labGen      = 0
         }
  null' (CS ac dcs dc l)
    = null' ac && null dcs && null' dc && l == 0

codeActive :: Lens' (CompState op) (ACode op)
codeActive k s = (\ n -> s { _codeActive = n}) <$> k (_codeActive s)

codeDelayed :: Lens' (CompState op) [ACode op]
codeDelayed k s = (\ n -> s { _codeDelayed = n}) <$> k (_codeDelayed s)

codeDone :: Lens' (CompState op) (ACode op)
codeDone k s = (\ n -> s { _codeDone = n}) <$> k (_codeDone s)

labGen :: Lens' (CompState op) Int
labGen k s = (\ n -> s { _labGen = n}) <$> k (_labGen s)


-- ----------------------------------------

data CompEnv
  = Env { _idMap      :: Map Name VDescr
        , _stackLevel :: Offset
        , _compOpts   :: CompOptions
        }
  deriving (Show)

compEnv0 :: CompOptions -> CompEnv
compEnv0 opts
  = Env { _idMap      = M.empty
        , _stackLevel = 0
        , _compOpts   = opts
        }
-- --------------------

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
--
-- basic code gen actions

gen :: AInstr op -> Comp op ()
gen instr = codeActive . instrSeq %= (<> L.singleton instr)

lab :: Label -> Comp op ()
lab l
  = do pos <- L.length <$> use (codeActive . instrSeq)
       codeActive . labMap . at l .= Just pos

newLab :: Comp op Label
newLab
  = do l <- genName "l"
       return (isoString # l)

genName :: String -> Comp op String
genName px
  = do labGen %= (+1)
       i <- use labGen
       return (px ++ show i)

open :: Comp op ()
open
  = do return ()

-- ----------------------------------------
--
-- TODO: move this stuff to extra module

data CompOptions
  = CO { _callByValue :: Bool
       }
  deriving (Show)

compOptions0 :: CompOptions
compOptions0
  = CO { _callByValue = False }

data VDescr
  = VD ()
  deriving (Show)

data Name
  = N { _orgName  :: String }
  deriving (Show)

-- ----------------------------------------
