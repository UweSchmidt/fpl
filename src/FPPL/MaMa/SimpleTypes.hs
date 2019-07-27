{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- simple machine types for virtual MaMa machine

module FPPL.MaMa.SimpleTypes
where

import FPPL.Prelude

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
  ap :: Lens' v Addr

class StackPointer v where
  sp :: Lens' v StackAddr

-- ----------------------------------------
--
-- pretty printing

instance Pretty (Addr' a) where
  pretty v@(AD w)
    | null' v = "  <null>"
    | otherwise = printf "%8i" $ toInteger w

instance Pretty Addr where
  pretty :: Addr -> String
  pretty v@(Addr w)
    | null' v = "  <null>"
    | otherwise = printf "%8i" $ toInteger w

instance Pretty Offset where
  pretty  = printf "%8i"
  pretty' = show

instance Pretty Bool where
  pretty = map toLower . show

-- ----------------------------------------

app' :: Int -> String -> String -> String
app' n xs ys = xs ++ spc ++ ys
  where
    spc = replicate (n - length xs) ' '

app8 :: String -> String -> String
app8 = app' 8

app4 :: String -> String -> String
app4 = app' 4

-- ----------------------------------------
