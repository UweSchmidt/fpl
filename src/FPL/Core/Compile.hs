{-# LANGUAGE ScopedTypeVariables #-}

module FPL.Core.Compile
where

import FPL.Prelude
import FPL.Core.AbstractSyntax
import FPL.Core.CodeGenBasic
import FPL.Core.CompEnv
import FPL.Core.CompMonad
-- import FPL.Core.CompState

-- ----------------------------------------
--
-- compile to basic value

compB :: Expr -> Comp op ()
compB e@(Lit _)         = compLit e

compB e@(App (Prim _) _)
                        = compPrim e

compB (If cond thenp elsep)
                        = do l1 <- newLab
                             l2 <- newLab
--                             compBranch False l1 cond
                             compB thenp
                             jump  l2
                             label l1
                             compB elsep
                             label l2
compB e                 = issue $ "compB: illegal expr: " ++ show e

-- --------------------
--
-- compile literal

compLit :: Expr -> Comp op ()
compLit e
  = view (mamaOps . litConvOp) >>= compLit' e

compLit' :: Expr -> (TypeName -> Maybe op) -> Comp op ()
compLit' e conv
  -- Int literal
  | Just i      <- e ^? intEx   = loadInt  i

  -- Bool literal
  | Just b      <- e ^? boolEx  = loadBool b

  -- literal of user defined basic type
  -- type must be a basic type
  -- and the string value can be
  -- converted by a machine op

  | Just (t, v) <- e ^? litEx
  , Just n      <- t ^? tyBasic
  , Just o      <- conv n       = loadLit o v

  | otherwise                   = issue $ "wrong literal " ++ show e

-- --------------------
--
-- compile prim ops
--
-- prim ops must be called with exactly the # of arguments
-- expected, no partial application (must be transformed before code gen)
-- and no over saturation (prim ops never return a function)

compPrim :: Expr -> Comp op ()
compPrim e0
  = view exprComp >>= compNonStrictPrim e0
  where

compNonStrictPrim :: Expr -> ExprComp -> Comp op ()
compNonStrictPrim e ec
  | Just (e1, e2) <- e ^? logicalAnd ec
                        = compB (ifEx # (e1, e2, falseExpr))

  | Just (e1, e2) <- e ^? logicalOr  ec
                        = compB (ifEx # (e1, trueExpr, e2))

  | Just (po, args) <- e ^? primEx
                        = view (mamaOps . primOpMap) >>= compStrictPrim po args

  | otherwise           = issue $ "compPrim: illegal prim expr " ++ show e

compStrictPrim :: PrimOp -> [Expr] -> (PrimOp -> Maybe (op, Type)) -> Comp op ()
compStrictPrim po args opmap
  | Just (o', t') <- opmap po
  , length args == arity t'
                        = do traverse_ compB args       -- compile args
                             comp o'                    -- gen compute instr

  | otherwise           = issue $
                          "compStrict: illegal prim op or type" ++ show (po, args)

-- ----------------------------------------

-- special handling for boolean operations &&, ||, not, True, False
{-
compBranch :: forall op . OpDescr op => Bool -> Label -> Expr -> Comp op ()

compBranch cond lab e@(App (Prim primop) args)
  | Just (op', TyFct _atypes rtype) <- primOpMap primop
  , rtype == boolType
    = compBranch' op' args
      where
        compBranch' :: op -> [Expr] -> Comp op ()
        compBranch' o [e1]
          | isLogicalNot o
                = compBranch (not cond) lab e1

        compBranch' o [e1, e2]
          | isLogicalAnd o
                = if cond
                  then do new <- newLab
                          compBranch (not cond) new e1
                          compBranch      cond  lab e2
                          label new
                  else do compBranch cond lab e1
                          compBranch cond lab e2

          | isLogicalOr o
                = if not cond
                  then do new <- newLab
                          compBranch      cond  new e1
                          compBranch (not cond)  lab e2
                          label new
                  else do compBranch (not cond) lab e1
                          compBranch (not cond) lab e2


        compBranch' _ _
                = do compB e
                     branch cond lab

-- partial evaluation for constant conditions

compBranch cond lab (Lit (Literal lty lval))
  | lty == boolType
  , Just v <- readMaybe lval
                = if cond == v
                  then jump lab
                  else return ()

compBranch cond lab e
                = do compB e
                     branch cond lab

-}
-- ----------------------------------------
