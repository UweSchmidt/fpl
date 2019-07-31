{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- simple machine types for virtual MaMa machine

module FPL.MaMa.SimpleTypes
  ( module FPL.MaMa.SimpleTypes )
where

import FPL.Prelude
import Text.Pretty

-- --------------------
--
-- address arithmetic

class AddrArithm a where
  incr'     :: Offset -> a -> a
  disp'     :: a -> a -> Offset
  isoOffset :: Iso' a Offset

-- --------------------
--
-- offsets within stacks and vectors
-- maybe negative

type Offset = Int

instance AddrArithm Offset where
  incr'     = (+)
  disp'     = (-)
  isoOffset = id

instance Pretty Offset where
  pretty = show

-- --------------------
--
-- addresses of heap objects

newtype Addr = Addr Word
  deriving (Eq, Ord, Show)

instance Empty Addr where
  empty' = Addr 0

-- no address arithmetic here

-- --------------------
--
-- addresses into contiguous store

newtype Addr' a = AD Word
  deriving (Eq, Show)

instance Empty (Addr' a) where
  empty' = AD maxBound

instance AddrArithm (Addr' a) where
  incr' o (AD x)      = AD . fromIntegral $ fromIntegral x + o
  disp' (AD x) (AD y) = fromIntegral x - fromIntegral y
  isoOffset           = iso (\ (AD w) -> fromIntegral w) (AD . fromIntegral)

-- --------------------
--
-- code positions

data IntoCode

type CodeAddr = Addr' IntoCode

-- --------------------
--
-- stack adresses

data IntoStack

type StackAddr = Addr' IntoStack

-- --------------------
--
-- overloaded lenses

class CodePointer v where
  cp :: Lens' v CodeAddr

class GlobalPointer v where
  gp :: Lens' v Addr

class ArgsPointer v where
  argp :: Lens' v Addr

class StackPointer v where
  sp :: Lens' v StackAddr

-- ----------------------------------------
--
-- pretty printing

instance Pretty (Addr' a) where
  pretty v@(AD w)
    | null' v   = "null"
    | otherwise = show w

instance Pretty Addr where
  pretty v@(Addr w)
    | null' v   = "null"
    | otherwise = show w

instance Pretty Bool where
  pretty = map toLower . show

-- ----------------------------------------
