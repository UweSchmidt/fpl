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
--
-- b:     defining occurence with type
-- Ident: applied occurence

data Expr b
  = Var   Var
  | Lit   Literal
  | App   (Expr b)  (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b)  (Expr b)
  | If    (Expr b)  (Expr b) (Expr b)
--  | Case  (Expr b) b Type [Alt b]     -- not yet implemented

data Var
  = Id { _varName    :: VarName
       , _varType    :: Type
       , _varScope   :: VarScope
       , _varDetails :: VarDetails
       , _varInfo    :: VarInfo
       }

data VarScope
  = GlobalVar
  | LocalVar

data VarDetails
  = VanillaId
  | PrimOpId

data VarInfo
  = VarInfo
    { _arityInfo      :: Arity
    , _strictnessInfo :: Strictness
    }

data Strictness
  = IsLazy
  | IsStrict

type Arity = Int

data Literal
  = LitInt  Int
  | LitBool Bool
  | LitVal Type String

type Arg b = (Expr b)

-- type Alt b = (AltCon, [b], Expr b)

-- data AltCon
--   = DataAlt DataCon
--   | LitAlt  Literal
--   | DEFAULT

data Bind b
  = NonRec b (Expr b)
  | Rec [(b, (Expr b))]

data Type
  = TyBasic TypeName
  | TyFct Type Type
--  | TyData TypeName [DataCon]   -- data not yet implemented

-- data DataCon
--  = DataCon DataConName [Type]

type Name        = String
type VarName     = Name
type TypeName    = Name
type DataConName = Name

-- ----------------------------------------

deriving instance Functor Expr
deriving instance Functor Bind

deriving instance Eq Type

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

varDetails :: Lens' Var VarDetails
varDetails k s = (\ n -> s { _varDetails = n}) <$> k (_varDetails s)

varInfo :: Lens' Var VarInfo
varInfo k s = (\ n -> s { _varInfo = n}) <$> k (_varInfo s)

-- VarInfo

arityInfo :: Lens' VarInfo Arity
arityInfo k s = (\ n -> s { _arityInfo = n}) <$> k (_arityInfo s)

strictnessInfo :: Lens' VarInfo Strictness
strictnessInfo k s = (\ n -> s { _strictnessInfo = n}) <$> k (_strictnessInfo s)

-- Literal

litInt :: Prism' Literal Int
litInt = prism
      LitInt
      (\ case
          LitInt y -> Right y
          x        -> Left  x
      )

litBool :: Prism' Literal Bool
litBool = prism
      LitBool
      (\ case
          LitBool y -> Right y
          x         -> Left  x
      )

litVal :: Prism' Literal (Type, String)
litVal = prism
      (uncurry LitVal)
      (\ case
          LitVal t s -> Right (t, s)
          x          -> Left  x
      )

litType :: Literal -> Type
litType = \case
  LitInt   _i -> intType
  LitBool  _b -> boolType
  LitVal t _v -> t

-- Bind b

nonRecBind :: Prism' (Bind b) (b, Expr b)
nonRecBind = prism
      (uncurry NonRec)
      (\case
          NonRec b e -> Right (b, e)
          x          -> Left  x
      )

recBind :: Prism' (Bind b) [(b, Expr b)]
recBind = prism
      Rec
      (\case
          Rec b -> Right b
          x     -> Left  x
      )


-- Type

tyInt :: Prism' Type ()
tyInt
  = prism'
    (const $ TyBasic "Int")
    (\case
        TyBasic "Int" -> Just ()
        _             -> Nothing
    )

tyBool :: Prism' Type ()
tyBool = prism'
    (const $ TyBasic "Bool")
    (\case
        TyBasic "Bool" -> Just ()
        _              -> Nothing
    )

tyBasic :: Prism' Type TypeName
tyBasic = prism
      TyBasic
      (\case
          TyBasic y -> Right y
          x         -> Left  x
      )

tyFct :: Prism' Type (Type, Type)
tyFct = prism
      (uncurry TyFct)
      (\case
          TyFct t1 t2 -> Right (t1, t2)
          x           -> Left  x
      )

intType :: Type
intType  = tyInt # ()

boolType :: Type
boolType = tyBool # ()

-- ----------------------------------------
--
-- just for testing:

deriving instance Show b => Show (Expr b)
deriving instance Show Var
deriving instance Show VarScope
deriving instance Show VarDetails
deriving instance Show VarInfo
deriving instance Show Literal
deriving instance Show Strictness
-- deriving instance Show AltCon
deriving instance Show b => Show (Bind b)
deriving instance Show Type
-- deriving instance Show DataCon

-- ----------------------------------------
