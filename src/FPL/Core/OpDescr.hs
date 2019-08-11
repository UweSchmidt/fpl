module FPL.Core.OpDescr
where

import FPL.Prelude
import FPL.Core.AbstractSyntax

-- ----------------------------------------
--
-- features of operators needed during code generation

class OpDescr op where

  -- prim ops for not, && and or
  -- && and or are not strict in the 2. arg
  -- and not can be evaluated at compile time
  isLogicalNot :: op -> Bool
  isLogicalAnd :: op -> Bool
  isLogicalOr  :: op -> Bool

  isLogicalNot = const False
  isLogicalAnd = const False
  isLogicalOr  = const False

-- ----------------------------------------
