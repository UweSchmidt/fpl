{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- machine types for virtual MaMa machine

{-# LANGUAGE TypeSynonymInstances #-}

module FP0.Value
where

import FPPL.Prelude
import FPPL.MaMa.Value

-- ----------------------------------------
--
-- simplest basic value: just an Int

type BV0 = Int

instance BasicValue BV0 where
  asInt = prism' id Just

-- ----------------------------------------
