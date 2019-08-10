module FPL.Core.OpDescr
where

import FPL.Prelude
import FPL.Core.AbstractSyntax

-- ----------------------------------------
--
-- features of operators needed during code generation

class OpDescr op where

  -- associated MaMa ops and types for pimitive core ops
  primOpMap :: PrimOp -> Maybe (op, Type)

  -- prim ops for not, && and or
  -- && and or are not strict in the 2. arg
  -- and not can be evaluated at compile time
  isLogicalNot :: op -> Bool
  isLogicalAnd :: op -> Bool
  isLogicalOr  :: op -> Bool

  -- MaMa ops for conversion of string literals into basic values
  litConvOp :: TypeName -> Maybe op


  -- dummy impl
  primOpMap = const Nothing

  isLogicalNot = const False
  isLogicalAnd = const False
  isLogicalOr  = const False

  -- dummy impl, no literal conversions
  -- except for Int and Bool implemented

  litConvOp tn
    = const Nothing $ n
    where
      n = tn ^. name

-- ----------------------------------------
