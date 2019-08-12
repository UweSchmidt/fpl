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

compV :: Expr -> Comp op ()
compV e
  = issue $ "compV: not yet implemeted: " ++ show e

-- ----------------------------------------
--
-- compile to basic value

compB :: Expr -> Comp op ()

-- compile literals
compB e@(Lit _l)        = compLit e

-- compile prim ops
compB e@(App (Prim _) _)
                        = compPrim e

-- compile conditional expr
compB (If cond thenp elsep)
                        = compIf compB cond thenp elsep

compB e                 = do compV e
                             getBasic    -- unpack basic value

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
--
-- compile an if expr
-- used in compB and compV compile schema

compIf :: (Expr -> Comp op ())
       -> Expr -> Expr -> Expr -> Comp op ()

compIf compEx cond thenp elsep
                        = do l1 <- newLab
                             l2 <- newLab
                             compBranch False l1 cond
                             compEx thenp
                             jump   l2
                             label  l1
                             compEx elsep
                             label  l2

-- compile a condition of an "if" expr
--
-- the basic bool value(s) must be computed
-- for the conitional branch instructions

compBranch :: Bool -> Label -> Expr -> Comp op ()
compBranch cond lab e
  = view exprComp >>= compBranch' cond lab e

compBranch' :: Bool -> Label -> Expr -> ExprComp -> Comp op ()
compBranch' cond lab e ec
  -- not e1:
  -- just kind of branch (true od false) must be negated
  | Just e1 <- e ^? logicalNot ec
                        = compBranch (not cond) lab e1

  -- e1 && e2:
  -- cond branches for e1 and e2
  | Just (e1, e2) <- e ^? logicalAnd ec
                        = if cond
                          then do
                               lab' <- newLab
                               compBranch (not cond) lab' e1
                               compBranch      cond  lab  e2
                               label lab'
                          else do
                               compBranch cond lab e1
                               compBranch cond lab e2

  -- e1 || e2:
  -- transform into && and not
  -- not (not e1 && not e2)
  | Just (e1, e2) <- e ^? logicalOr ec
                        = compBranch cond lab $
                          logicalNot ec #
                          logicalAnd ec #
                          (logicalNot ec # e1, logicalNot ec # e2)

  -- literal: True or False
  -- unconditional jump or fall through
  | Just b <- e ^? boolEx
                        = if cond == b
                          then jump lab
                          else return ()

  -- all other boolean expr:
  -- code for computing the condition
  -- and a conditional branch instr
  | otherwise           = do compB e
                             branch cond lab

-- ----------------------------------------
