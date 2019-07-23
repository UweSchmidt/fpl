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

newtype Code = CS { _cs :: Vector Instr }
  deriving (Show)

mkCode :: [Instr] -> Code
mkCode = CS . V.fromList

instance Ixed Code where
  ix i = theCS . ix (i ^. codeAddr2Offset)

type instance Index   Code = CodeAddr
type instance IxValue Code = Instr

theCS :: Lens' Code (Vector Instr)
theCS k (CS v) = (\ n -> CS n) <$> k v

-- ----------------------------------------
