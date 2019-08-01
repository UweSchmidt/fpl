{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE LambdaCase #-}

module FPL.Core.CompEnv
  ( CompEnv
  , CompOptions

  , compEnv0

  , idMap
  , insVar

  , stackLevel
  , incrStackLevel

  , compOpts
  , compOpts0
  , genCallByValue
  )
where

import FPL.Prelude
import FPL.Core.AbstractSyntax

import FPL.MaMa.SimpleTypes


import qualified Data.Map as M

-- import Text.Pretty

-- ----------------------------------------

data CompEnv
  = Env { _idMap      :: Map VarName Var
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

idMap :: Lens' CompEnv (Map VarName Var)
idMap k s = (\ n -> s { _idMap = n}) <$> k (_idMap s)

stackLevel :: Lens' CompEnv Offset
stackLevel k s = (\ n -> s { _stackLevel = n}) <$> k (_stackLevel s)

compOpts :: Lens' CompEnv CompOptions
compOpts k s = (\ n -> s { _compOpts = n}) <$> k (_compOpts s)

incrStackLevel :: CompEnv -> CompEnv
incrStackLevel env = env & stackLevel %~ (+ 1)

insVar :: Var -> CompEnv -> CompEnv
insVar v env = env & at (v ^. varName) .~ Just v

type instance Index   CompEnv = VarName
type instance IxValue CompEnv = Var

instance Ixed CompEnv

instance At CompEnv where
  at i = idMap . at i

-- --------------------

data CompOptions
  = CO { _genCallByValue :: Bool
       }
  deriving (Show)

compOpts0 :: CompOptions
compOpts0
  = CO { _genCallByValue = False
       }

genCallByValue :: Lens' CompOptions Bool
genCallByValue k s = (\ n -> s { _genCallByValue = n}) <$> k (_genCallByValue s)

-- ----------------------------------------
