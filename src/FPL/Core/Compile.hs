{-# LANGUAGE ScopedTypeVariables #-}

module FPL.Core.Compile
where

import FPL.Prelude
import FPL.Core.AbstractSyntax
import FPL.Core.CodeGenBasic
-- import FPL.Core.CompEnv
import FPL.Core.CompMonad
-- import FPL.Core.CompState
import FPL.Core.OpDescr

-- ----------------------------------------
--
-- compile to basic value

compB :: OpDescr op => Expr -> Comp op ()
compB (Lit l)  = compLit l

compB (App (Prim primop) args)
               = compPrim primop args

compB (If cond thenp elsep)
               = do l1 <- newLab
                    l2 <- newLab
                    compBranch False l1 cond
                    compB thenp
                    jump  l2
                    label l1
                    compB elsep
                    label l2
compB e        = issue $ "compB: illegal expr: " ++ show e

-- --------------------
--
-- compile literal

compLit :: OpDescr op => Literal -> Comp op ()
compLit l
  | t == intType
  , Just i <- readMaybe v  = loadInt i

  | t == boolType
  , Just b <- readMaybe v  = loadBool b

  | Just n <- t ^? tyBasic
  , Just o <- litConvOp n
                           = loadLit o v
  | otherwise = issue $ "wrong literal " ++ show v
  where
    t = l ^. litType
    v = l ^. litVal

-- --------------------
--
-- compile prim ops
--
-- prim ops must be called with exactly the # of arguments
-- expected, no partial application (must be transformed before code gen)
-- and no over saturation (prim ops never return a function)

compPrim :: OpDescr op => PrimOp -> [Expr] -> Comp op ()

compPrim primop args
  | Just (op', TyFct ptypes _rtype) <- primOpMap primop
  , length args == length ptypes
    = compPrim' op' args
      where
        compPrim' o [e1, e2]
          | isLogicalAnd o
                = compB (If e1 e2 falseExpr)

          | isLogicalOr o
                = compB (If e1 trueExpr e2)

        compPrim' o args'
                = do traverse_ compB args'      -- compile args
                     comp o                     -- gen compute instr

compPrim primop args
                = issue $ "compPrim: illegal prim op or type" ++ show (primop, args)


-- ----------------------------------------

-- special handling for boolean operations &&, ||, not, True, False

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


-- ----------------------------------------
