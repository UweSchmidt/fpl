{-# LANGUAGE TypeFamilies #-}

module FPPL.MaMa.Code
where

import FPPL.Prelude
import FPPL.MaMa.SimpleTypes
import FPPL.MaMa.Instr

import qualified Data.Vector as V

-- ----------------------------------------

-- executable code
-- implemented as vector of instructions
-- due to static length of code segment

newtype Code op = CS { _cs :: Vector (Instr op) }
  deriving (Show)

instance Empty (Code op) where
  empty' = CS $ V.empty
  null'  = V.null . _cs

mkCode :: [Instr op] -> Code op
mkCode = CS . V.fromList

instance Ixed (Code op) where
  ix i = theCS . ix (i ^. isoOffset)

type instance Index   (Code op) = CodeAddr
type instance IxValue (Code op) = (Instr op)

theCS :: Lens' (Code op) (Vector (Instr op))
theCS k (CS v) = (\ n -> CS n) <$> k v

-- ----------------------------------------
