{-# LANGUAGE LambdaCase #-}

-- Value for MaMa with Int and Bool as basic values

module FP1.Value
where

import FPPL.Prelude
import FPPL.MaMa.Value

-- ----------------------------------------
--
-- 2. simplest basic value: an Int or a Bool

data BV1 = BV1Int  ! Int
         | BV1Bool ! Bool
         deriving (Show)

instance BasicValue BV1 where
  asInt = prism
          BV1Int
          (\ case
              BV1Int y -> Right y
              x        -> Left  x
          )

  asBool = prism
           BV1Bool
           (\ case
               BV1Bool y -> Right y
               x         -> Left  x
           )

-- ----------------------------------------
