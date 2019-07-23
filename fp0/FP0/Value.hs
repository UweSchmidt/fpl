{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- machine types for virtual MaMa machine

module FP0.Value
where

import FPPL.Prelude
import FPPL.MaMa.Value

-- ----------------------------------------
--
-- simplest basic value: just an Int

newtype BV0 = BV0 { _bv0Int :: Int }
  deriving (Show)

instance BasicValue BV0 where
  asInt = prism' BV0 (Just . _bv0Int)

-- ----------------------------------------
