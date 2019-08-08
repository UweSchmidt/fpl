{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Abstract Syntax for Core language

module FPL.Core.AbstractSyntax where

import FPL.Prelude

-- import qualified Data.Set as S

-- ----------------------------------------

type Prog = [CAF]

data CAF
  = CAF
    { _lhs :: VarName
    , _rhs :: Expr
    }

data Expr
  = Var    VarName
  | Lit    Literal
  | Prim   PrimOp
  | App    Expr  [Expr]
  | Lam    [Var]  Expr
  | Let    Var    Expr   Expr
  | LetRec [(Var, Expr)] Expr
  | If     Expr   Expr   Expr

data Var
  = Id { _varName    :: VarName
       , _varType    :: Type
       , _varScope   :: VarScope
       }

data PrimOp
  = PrimOp
    { _primName :: PrimName
    , _primType :: Type
    }

data VarScope
  = GlobalVar
  | LocalVar

data Literal
  = Literal
    { _litType :: Type
    , _litVal  :: LitVal
    }

type LitVal = String

data Type
  = TyAny               -- unknown basic type
  | TyBasic TypeName    -- basic type
  | TyFct [Type] Type   -- function type (uncurried)

-- --------------------
--
-- names

type BasicName  = String

data Name
  = Name
    { _name      :: BasicName
    , _nameSpace :: NameSpace
    }

data NameSpace   = VarNS | DataConNS | PrimNS | TypeNS

type VarName     = Name
type PrimName    = Name
type TypeName    = Name

-- ----------------------------------------
--
-- different names maybe stored in a single env
-- namespace tags are partitioning the map

deriving instance Eq  Name
deriving instance Ord Name
deriving instance Eq  NameSpace
deriving instance Ord NameSpace

-- ----------------------------------------
--
-- optics

-- Var

varName :: Lens' Var VarName
varName k s = (\ n -> s { _varName = n}) <$> k (_varName s)

varType :: Lens' Var Type
varType k s = (\ n -> s { _varType = n}) <$> k (_varType s)

varScope :: Lens' Var VarScope
varScope k s = (\ n -> s { _varScope = n}) <$> k (_varScope s)

-- Literals

litVal :: Lens' Literal LitVal
litVal  k s = (\ n -> s { _litVal = n}) <$> k (_litVal s)

litType :: Lens' Literal Type
litType  k s = (\ n -> s { _litType = n}) <$> k (_litType s)

-- Types

tyAny :: Prism' Type ()
tyAny = prism'
    (const TyAny)
    (\case
        TyAny -> Just ()
        _     -> Nothing
    )

tyBasic :: Prism' Type TypeName
tyBasic = prism
      TyBasic
      (\case
          TyBasic y -> Right y
          x         -> Left  x
      )

tyFct :: Prism' Type ([Type], Type)
tyFct = prism
      (uncurry TyFct)
      (\case
          TyFct t1 t2 -> Right (t1, t2)
          x           -> Left  x
      )

-- basic type constructors and ops

anyType :: Type
anyType = tyAny # ()

intType :: Type
intType = tyBasic # Name "Int" TypeNS

boolType :: Type
boolType = tyBasic . typeN # "Bool"

fctType :: [Type] -> Type -> Type
fctType ats rt = tyFct # (ats, rt)

-- arity of a function type

arity :: Type -> Int
arity t = fromMaybe 0 (t ^? tyFct . _1 . to length)

-- ----------------------------------------
--
-- optics and constructors for names

name :: Lens' Name BasicName
name k s = (\ n -> s { _name = n}) <$> k (_name s)

nameSpace :: Lens' Name NameSpace
nameSpace k s = (\ n -> s { _nameSpace = n}) <$> k (_nameSpace s)

varN :: Prism' Name BasicName
varN = prism
       (\ n -> Name n VarNS)
       (\case
           Name n VarNS -> Right n
           x            -> Left x
       )

dataConN :: Prism' Name BasicName
dataConN = prism
           (\ n -> Name n DataConNS)
           (\case
               Name n DataConNS -> Right n
               x                -> Left x
           )

typeN :: Prism' Name BasicName
typeN = prism
       (\ n -> Name n TypeNS)
       (\case
           Name n TypeNS -> Right n
           x             -> Left x
       )


mkVarName :: BasicName -> Name
mkVarName n = varN # n

mkDataConName :: BasicName -> Name
mkDataConName n = dataConN # n

-- ----------------------------------------
--
-- just for testing:

deriving instance Show CAF
deriving instance Show Expr
deriving instance Show Literal
deriving instance Show PrimOp
deriving instance Show Name
deriving instance Show NameSpace
deriving instance Show Type
deriving instance Show Var
deriving instance Show VarScope

-- deriving instance Show DataCon

-- ----------------------------------------
