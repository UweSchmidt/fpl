{-# LANGUAGE LambdaCase #-}

-- interrupts (errors)

module FPPL.MaMa.Interrupts
  ( Interrupt(..)
  , prettyIR
  )
where

import FPPL.Prelude

data Interrupt
  = Running
  | Terminated
  | NullPointer
  | IllegalCodeAddr
  | IllegalStackAddr
  | IllegalArgument
  deriving (Eq, Show)

instance Empty Interrupt where
  empty' = Running

prettyIR :: Interrupt -> String
prettyIR = \ case
  Running          -> ""
  Terminated       -> "Program terminated"
  NullPointer      -> "Null pointer exception"
  IllegalCodeAddr  -> "Illegal code access"
  IllegalStackAddr -> "Illegal value stack access"
  IllegalArgument  -> "Illegal argument"
