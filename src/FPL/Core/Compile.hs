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

codeV :: Expr -> Comp op ()
codeV e
  = issue $ "codeV: not yet implemeted: " ++ show e

-- ----------------------------------------
--
-- compile to basic value

codeB :: Expr -> Comp op ()

-- compile literals
codeB e@(Lit _l)        = codeLit e

-- compile prim ops
codeB e@(App (Prim _) _)
                        = codePrim e

-- compile conditional expr
codeB (If cond thenp elsep)
                        = codeIf codeB cond thenp elsep

codeB e                 = do codeV e
                             getBasic    -- unpack basic value

-- --------------------
--
-- compile literal

codeLit :: Expr -> Comp op ()
codeLit e
  = view (mamaOps . litConvOp) >>= codeLit' e

codeLit' :: Expr -> (TypeName -> Maybe op) -> Comp op ()
codeLit' e conv
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

codePrim :: Expr -> Comp op ()
codePrim e0
  = view exprComp >>= codeNonStrictPrim e0
  where

codeNonStrictPrim :: Expr -> ExprComp -> Comp op ()
codeNonStrictPrim e ec
  | Just (e1, e2) <- e ^? logicalAnd ec
                        = codeB (ifEx # (e1, e2, falseExpr))

  | Just (e1, e2) <- e ^? logicalOr  ec
                        = codeB (ifEx # (e1, trueExpr, e2))

  | Just (po, args) <- e ^? primEx
                        = view (mamaOps . primOpMap) >>= codeStrictPrim po args

  | otherwise           = issue $ "codePrim: illegal prim expr " ++ show e

codeStrictPrim :: PrimOp -> [Expr] -> (PrimOp -> Maybe (op, Type)) -> Comp op ()
codeStrictPrim po args opmap
  | Just (o', t') <- opmap po
  , length args == arity t'
                        = do traverse_ codeB args       -- compile args
                             comp o'                    -- gen compute instr

  | otherwise           = issue $
                          "codeStrict: illegal prim op or type" ++ show (po, args)

-- ----------------------------------------
--
-- compile an if expr
-- used in codeB and codeV compile schema

codeIf :: (Expr -> Comp op ())
       -> Expr -> Expr -> Expr -> Comp op ()

codeIf codeEx cond thenp elsep
                        = do l1 <- newLab
                             l2 <- newLab
                             codeBranch False l1 cond
                             codeEx thenp
                             jump   l2
                             label  l1
                             codeEx elsep
                             label  l2

-- compile a condition of an "if" expr
--
-- the basic bool value(s) must be computed
-- for the conitional branch instructions

codeBranch :: Bool -> Label -> Expr -> Comp op ()
codeBranch cond lab e
  = view exprComp >>= codeBranch' cond lab e

codeBranch' :: Bool -> Label -> Expr -> ExprComp -> Comp op ()
codeBranch' cond lab e ec
  -- not e1:
  -- just kind of branch (true od false) must be negated
  | Just e1 <- e ^? logicalNot ec
                        = codeBranch (not cond) lab e1

  -- e1 && e2:
  -- cond branches for e1 and e2
  | Just (e1, e2) <- e ^? logicalAnd ec
                        = if cond
                          then do
                               lab' <- newLab
                               codeBranch (not cond) lab' e1
                               codeBranch      cond  lab  e2
                               label lab'
                          else do
                               codeBranch cond lab e1
                               codeBranch cond lab e2

  -- e1 || e2:
  -- transform into && and not
  -- not (not e1 && not e2)
  | Just (e1, e2) <- e ^? logicalOr ec
                        = codeBranch cond lab $ -- rec call with transf. expr
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
  | otherwise           = do codeB e
                             branch cond lab

-- ----------------------------------------
