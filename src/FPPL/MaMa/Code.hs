{-# LANGUAGE TypeFamilies #-}

module FPPL.MaMa.Code
  ( Code
  , theCS
  , theInstrList
  , mkCode
  )
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
mkCode is = theInstrList # is

instance Ixed (Code op) where
  ix i = theCS . ix (i ^. isoOffset)

type instance Index   (Code op) = CodeAddr
type instance IxValue (Code op) = (Instr op)

theCS :: Iso' (Code op) (Vector (Instr op))
theCS = iso _cs CS

theInstrList :: Iso' (Code op) [Instr op]
theInstrList = theCS . isoVectorList

-- ----------------------------------------

instance (Pretty op) => Pretty (Code op) where
  pretty cs =
    unlines . map pretty $ zip is (cs ^. theInstrList)
    where
      is :: [CodeAddr]
      is = map (isoOffset #) [0..]
-- ----------------------------------------
