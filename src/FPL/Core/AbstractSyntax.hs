{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- Abstract Syntax for Core language

module FPL.Core.AbstractSyntax where

import FPL.Prelude ()

-- import qualified Data.Set as S

-- ----------------------------------------
--
-- b:     defining occurence with type
-- Ident: applied occurence

data Expr b
  = Var   Ident
  | Lit   Type Literal
  | App   (Expr b)  (Args b)
  | Lam   (Pars b) (Expr b)
  | Let   (Bind b)  (Expr b)
  | If    (Expr b)  (Expr b) (Expr b)
--  | Case  (Expr b) b Type [Alt b]     -- not yet implemented
  deriving (Show)

type Ident = String

data Literal
  = LitInt  Int
  | LitBool Bool
  | LitStr  String
  deriving (Show)

type Pars b = [b]
type Args b = [Expr b]

type Alt b = (AltCon, [b], Expr b)

data AltCon
  = DataAlt DataCon
  | LitAlt  Literal
  | DEFAULT
  deriving (Show)

data Bind b
  = NonRec b (Expr b)
  | Rec [(b, (Expr b))]
  deriving (Show)

data Type
  = TyBasic TypeName
  | TyFct Type Type
--  | TyData TypeName [DataCon]   -- data not yet implemented
  deriving (Show)

data DataCon
  = DataCon DataConName [Type]
  deriving (Show)

type TypeName    = String
type DataConName = String

-- ----------------------------------------
