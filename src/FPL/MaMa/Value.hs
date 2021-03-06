{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- machine types for virtual MaMa machine

module FPL.MaMa.Value
  ( Value
  , Closure
  , Function
  , Vec
  , BasicValue(..)
  , CodePointer(..)
  , GlobalPointer(..)
  , ArgsPointer(..)

  , mkVec
  , asB
  , asC
  , asF
  , asV
  )
where

import FPL.Prelude
import FPL.MaMa.SimpleTypes

import Text.Pretty

import qualified Data.Vector as V

-- --------------------
--
-- the values stored in the heap
--
-- B: basic machine values, not fixed
-- C: closures, unevaluated expressions
-- F: function values
-- V: vectors of heap references

data Value v = B v
             | C Closure
             | F Function
             | V Vec
  deriving (Show)

asB :: Prism' (Value v) v
asB = prism
      B
      (\ case
          B y -> Right y
          x   -> Left  x
      )

asC :: Prism' (Value v) Closure
asC = prism
      C
      (\ case
          C y -> Right y
          x   -> Left  x
      )

asF :: Prism' (Value v) Function
asF = prism
      F
      (\ case
          F y -> Right y
          x   -> Left  x
      )

asV :: Prism' (Value v) Vec
asV = prism
      V
      (\ case
          V y -> Right y
          x   -> Left  x
      )

-- --------------------
--
-- features of a basic value
-- at least Int's and Bool's
-- must be representable in the machine

class BasicValue v where
  asInt  :: Prism' v Int

  asBool :: Prism' v Bool
  asBool = asInt . intBool

-- --------------------
--
-- closure value
-- with a code pointer and
-- a pointer to global variables

data Closure = CL { _ccp :: ! CodeAddr
                  , _cgp :: ! Addr
                  }
  deriving (Show)

-- --------------------
--
-- function value with a code pointer,
-- an argument pointer for already given args
-- and a pointer to global variables

data Function = FU { _fcp :: ! CodeAddr
                   , _fap :: ! Addr
                   , _fgp :: ! Addr
                   }
  deriving (Show)

-- --------------------
--
-- a vector of heap references
-- for function arguments and
-- references to global variables

newtype Vec = FC { _vec :: Vector Addr }
  deriving (Show)

mkVec :: [Addr] -> Vec
mkVec = FC . V.fromList

instance Ixed Vec where
  ix i = theVector . ix i

type instance Index   Vec = Offset
type instance IxValue Vec = Addr

theVector :: Lens' Vec (Vector Addr)
theVector k (FC v) = (\ n -> FC n) <$> k v

-- ----------------------------------------
--
-- lens for code pointer cp

instance CodePointer Closure where
  cp k c = (\ n -> c { _ccp = n}) <$> k (_ccp c)

instance CodePointer Function where
  cp k f = (\ n -> f { _fcp = n}) <$> k (_fcp f)

instance CodePointer (Value v) where
  cp k (C (CL cp' gp'    )) = (\ n -> C (CL n     gp')) <$> k cp'
  cp k (F (FU cp' ap' gp')) = (\ n -> F (FU n ap' gp')) <$> k cp'
  cp k v                    = const v                   <$> k empty'

-- ----------------------------------------
--
-- lens for global pointer gp

instance GlobalPointer Closure where
  gp k c = (\ n -> c { _cgp = n}) <$> k (_cgp c)

instance GlobalPointer Function where
  gp k f = (\ n -> f { _fgp = n}) <$> k (_fgp f)

instance GlobalPointer (Value v) where
  gp k (C (CL cp'     gp')) = (\ n -> C (CL cp'     n)) <$> k gp'
  gp k (F (FU cp' ap' gp')) = (\ n -> F (FU cp' ap' n)) <$> k gp'
  gp k v                    = const v                   <$> k empty'

-- ----------------------------------------
--
-- lens for arguments pointer argp

instance ArgsPointer Function where
  argp k f = (\ n -> f { _fap = n}) <$> k (_fap f)

instance ArgsPointer (Value v) where
  argp k (F (FU cp' ap' gp')) = (\ n -> F (FU cp' n gp')) <$> k ap'
  argp k v                    = const v                   <$> k empty'

-- ----------------------------------------
--
-- pretty printing

instance (Pretty v) => Pretty (Value v) where
  pretty (B val) = "B " ++ pretty val
  pretty (F fct) = pretty fct
  pretty (C cls) = pretty cls
  pretty (V vec) = pretty vec

instance Pretty Function where
  pretty (FU cp' ap' gp') =
    unwords ["F", pretty cp', pretty ap', pretty gp']

instance Pretty Closure where
  pretty (CL cp' gp') =
    unwords ["C", pretty cp', pretty gp']

instance Pretty Vec where
  pretty v =
    "V [" ++ intercalate "," (map pretty (v ^. theVector . isoVectorList)) ++ "]"
