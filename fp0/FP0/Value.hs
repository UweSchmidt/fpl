{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- machine types for virtual MaMa machine

{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module FP0.Value
where

import FPL.Prelude
import FPL.MaMa.Value

-- ----------------------------------------
--
-- simplest basic value: just an Int

type BV0 = Int

instance BasicValue BV0 where
  asInt = prism' id Just

-- ----------------------------------------
