{-# LANGUAGE LambdaCase #-}

-- interrupts (errors)

module FPPL.MaMa.Interrupts
  ( Interrupt(..)
  , prettyIR
  )
where

import FPPL.Prelude

-- ----------------------------------------

infixr 5 +:+

data Interrupt
  = Running
  | Terminated
  | NullPointer
  | IllegalHeapAddr
  | IllegalCodeAddr
  | IllegalStackAddr
  | IllegalArgument String
  | NotImplemented  String
  | DivBy0
  deriving (Eq, Show)

instance Empty Interrupt where
  empty' = Running

-- --------------------
--
-- pretty printing

instance Pretty Interrupt where
  pretty = prettyIR

prettyIR :: Interrupt -> String
prettyIR = \ case
      Running           -> ""
      Terminated        -> "Program terminated"
      NullPointer       -> "Null pointer exception"
      IllegalHeapAddr   -> "Illegal heap access"
      IllegalCodeAddr   -> "Illegal code access"
      IllegalStackAddr  -> "Illegal value stack access"
      IllegalArgument s -> "Illegal argument" +:+ s
      NotImplemented  s -> "Not implemented"  +:+ s
      DivBy0            -> "division by zero"

(+:+) :: String -> String -> String
(+:+) s1 s2
  | null s2   = s1
  | otherwise = s1 ++ ": " ++ s2

-- ----------------------------------------
