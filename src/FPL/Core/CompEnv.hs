-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE LambdaCase #-}

module FPL.Core.CompEnv
  ( CompEnv
  , Ident
  , VarDescr
  , CompOptions

  , compEnv0

  , idMap
  , insVarDescr
  , vType

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
  = Env { _idMap      :: Map Ident VarDescr
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

idMap :: Lens' CompEnv (Map Ident VarDescr)
idMap k s = (\ n -> s { _idMap = n}) <$> k (_idMap s)

stackLevel :: Lens' CompEnv Offset
stackLevel k s = (\ n -> s { _stackLevel = n}) <$> k (_stackLevel s)

compOpts :: Lens' CompEnv CompOptions
compOpts k s = (\ n -> s { _compOpts = n}) <$> k (_compOpts s)

incrStackLevel :: CompEnv -> CompEnv
incrStackLevel env = env & stackLevel %~ (+ 1)

insVarDescr :: Ident -> VarDescr -> CompEnv -> CompEnv
insVarDescr n d env = env & idMap %~ M.insert n d

-- --------------------

data VarDescr
  = VD { _vType :: Type
       }
  deriving (Show)

vType :: Lens' VarDescr Type
vType k s = (\ n -> s { _vType = n}) <$> k (_vType s)

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
