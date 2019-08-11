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

  , exprComp
  , mamaOps
  )
where

import FPL.Prelude
import FPL.Core.AbstractSyntax

import FPL.MaMa.SimpleTypes


import qualified Data.Map as M

-- import Text.Pretty

-- ----------------------------------------

data CompEnv op
  = Env { _idMap      :: Map Name Var
        , _stackLevel :: Offset
        , _compOpts   :: CompOptions
        , _exprComp   :: ExprComp
        , _mamaOps    :: MaMaOpTable op
        }

compEnv0 :: CompOptions -> CompEnv op
compEnv0 opts
  = Env { _idMap      = M.empty
        , _stackLevel = 0
        , _compOpts   = opts
        , _exprComp   = defaultExprComp
        , _mamaOps    = defaultMaMaOpTable
        }

idMap :: Lens' (CompEnv op) (Map VarName Var)
idMap k s = (\ n -> s { _idMap = n}) <$> k (_idMap s)

stackLevel :: Lens' (CompEnv op) Offset
stackLevel k s = (\ n -> s { _stackLevel = n}) <$> k (_stackLevel s)

compOpts :: Lens' (CompEnv op) CompOptions
compOpts k s = (\ n -> s { _compOpts = n}) <$> k (_compOpts s)

exprComp :: Lens' (CompEnv op) ExprComp
exprComp k s = (\ n -> s { _exprComp = n}) <$> k (_exprComp s)

mamaOps :: Lens' (CompEnv op) (MaMaOpTable op)
mamaOps k s = (\ n -> s { _mamaOps = n}) <$> k (_mamaOps s)

incrStackLevel :: CompEnv op -> CompEnv op
incrStackLevel env = env & stackLevel %~ (+ 1)

insVar :: Var -> CompEnv op -> CompEnv op
insVar v env = env & at (v ^. varName) .~ Just v

type instance Index   (CompEnv op) = VarName
type instance IxValue (CompEnv op) = Var

instance Ixed (CompEnv op)

instance At (CompEnv op) where
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
