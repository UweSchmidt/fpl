{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- the MaMa machine state

module FPL.MaMa.Options
  ( Options
  , defaultOptions
  , mamaTrace
  )
where

import FPL.Prelude

-- ----------------------------------------

data Options = MOPS { _mamaTrace :: Bool }
  deriving (Show)

defaultOptions :: Options
defaultOptions = MOPS { _mamaTrace = True }

mamaTrace :: Lens' Options Bool
mamaTrace k (MOPS v) = (\ n -> MOPS n) <$> k v

-- ----------------------------------------
