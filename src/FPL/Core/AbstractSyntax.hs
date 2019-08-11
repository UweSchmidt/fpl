{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

-- Abstract Syntax for Core language

module FPL.Core.AbstractSyntax where

import FPL.Prelude

-- import qualified Data.Set as S

-- ----------------------------------------

type Prog = [CAF]

data CAF
  = CAF
    { _lhs :: Var
    , _rhs :: Expr
    }

data Expr
  = Var    Var
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
deriving instance Eq  Type

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

bool'bool :: Type
bool'bool = fctType [boolType] boolType

bool2'bool :: Type
bool2'bool = fctType [boolType, boolType] boolType

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

primN :: Prism' Name BasicName
primN = prism
       (\ n -> Name n PrimNS)
       (\case
           Name n PrimNS -> Right n
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
--
-- expr optics

boolEx :: Prism' Expr Bool
boolEx = prism' f g
  where
    f b = Lit $ Literal boolType (show b)
    g (Lit (Literal t v))
      | t == boolType
      , Just b <- readMaybe v
                        = Just b
    g _                 = Nothing

intEx :: Prism' Expr Int
intEx = prism' f g
  where
    f i                 = Lit $ Literal intType (show i)
    g (Lit (Literal t v))
      | t == intType
      , Just i <- readMaybe v
                        = Just i
    g _                 = Nothing

litEx :: Prism' Expr (Type, LitVal)
litEx = prism' f g
  where
    f (t, v)            = Lit $ Literal t v
    g (Lit (Literal t v))
                        = Just (t, v)
    g _                 = Nothing

falseExpr :: Expr
falseExpr = boolEx # False

trueExpr  :: Expr
trueExpr  = boolEx # True

-- --------------------

intVar :: Prism' Expr VarName
intVar = prism' f g
  where
    f n                  = Var (Id { _varName  = n
                                   , _varType  = intType
                                   , _varScope = LocalVar
                                   }
                               )
    g (Var id')
      | _varType id' == intType
                         = Just (_varName id')
    g _                  = Nothing

boolVar :: Prism' Expr VarName
boolVar = prism' f g
  where
    f n                  = Var (Id { _varName  = n
                                   , _varType  = boolType
                                   , _varScope = LocalVar
                                   }
                               )
    g (Var id')
      | _varType id' == boolType
                         = Just (_varName id')
    g _                  = Nothing

-- --------------------
--
-- optics for unary expr

prim1 :: Prism' Expr (PrimOp, Expr)
prim1 = prism' f g
  where
    f (op, e1)          = App (Prim op) [e1]
    g (App (Prim op) [e1])
                        = Just (op, e1)
    g _                 = Nothing

prim1' :: Type -> Prism' Expr (BasicName, Expr)
prim1' t = prim1 . prism' f g
  where
    f (n, e1)           = ( PrimOp { _primName = primN # n
                                   , _primType = t
                                   }
                          , e1
                          )
    g ( PrimOp { _primName = n1
               , _primType = t1
               }
      , e1
      )
      | t == t1
      , Just n <- n1 ^? primN
                        = Just (n, e1)
    g _                 = Nothing

prim1Int :: Prism' Expr (BasicName, Expr)
prim1Int = prim1' intType

prim1Bool :: Prism' Expr (BasicName, Expr)
prim1Bool = prim1' boolType

unaryExpr :: Type -> BasicName -> Prism' Expr Expr
unaryExpr t n = prim1' t . hasName n

-- --------------------
--
-- optics for binary expr

prim2 :: Prism' Expr (PrimOp, (Expr, Expr))
prim2 = prism' f g
  where
    f (op, (e1, e2))    = App (Prim op) [e1, e2]
    g (App (Prim op) [e1, e2])
                        = Just (op, (e1, e2))
    g _                 = Nothing

prim2' :: Type -> Prism' Expr (BasicName, (Expr, Expr))
prim2' t = prim2 . prism' f g
  where
    f (n, e12)          = ( PrimOp { _primName = primN # n
                                     , _primType = t
                                     }
                            , e12
                            )
    g ( PrimOp { _primName = n1
               , _primType = t1
               }
      , e12
      )
      | t == t1
      , Just n <- n1 ^? primN
                        = Just (n, e12)
    g _                 = Nothing

prim2Int :: Prism' Expr (BasicName, (Expr, Expr))
prim2Int = prim2' intType

prim2Bool :: Prism' Expr (BasicName, (Expr, Expr))
prim2Bool = prim2' boolType

binaryExpr :: Type -> BasicName -> Prism' Expr (Expr, Expr)
binaryExpr t n = prim2' t . hasName n

hasName :: Eq n => n -> Prism' (n, e) e
hasName n = prism' f g
  where
    f e12        = (n, e12)
    g (n1, e12)
      | n1 == n  = Just e12
    g _          = Nothing

-- --------------------
--
-- optics for n-ary expr

primEx :: Prism' Expr (PrimOp, [Expr])
primEx = prism' f g
  where
    f (op, es)          = App (Prim op) es
    g (App (Prim op) es)
                        = Just (op, es)
    g _                 = Nothing

-- --------------------

ifEx :: Prism' Expr (Expr, Expr, Expr)
ifEx = prism' f g
  where
    f (cond, thenp, elsep)
                        = If cond thenp elsep
    g (If cond thenp elsep)
                        = Just (cond, thenp, elsep)
    g _                 = Nothing

-- --------------------
--
-- during code gen there are
-- some expr transformation necessary
--
-- matching and transforming is done
-- by prisms
--
-- example: short evaluation of
-- logical && and || need to be transformed
-- into if-then-else (branches and jumps)
--
-- the syntax of these prim expression
-- is configured in the ExprComp record

data ExprComp
  = ExprComp
    { logicalNot :: Prism' Expr Expr
    , logicalAnd :: Prism' Expr (Expr, Expr)
    , logicalOr  :: Prism' Expr (Expr, Expr)
    }

defaultExprComp :: ExprComp
defaultExprComp
  = ExprComp
    { logicalNot = unaryExpr  boolType "not"
    , logicalAnd = binaryExpr boolType "&&"
    , logicalOr  = binaryExpr boolType "||"
    }

-- prim ops must be mapped to MaMa compute ops
-- also conversion of literals for user defined
-- basic values needs conversion ops
-- from strings to the user defined types

data MaMaOpTable op
  = MaMaOps
    { _litConvOp :: TypeName -> Maybe op
    , _primOpMap :: PrimOp -> Maybe (op, Type)
    }

litConvOp :: Lens' (MaMaOpTable op) (TypeName -> Maybe op)
litConvOp k s = (\ n -> s { _litConvOp = n}) <$> k (_litConvOp s)

primOpMap :: Lens' (MaMaOpTable op) (PrimOp -> Maybe (op, Type))
primOpMap k s = (\ n -> s { _primOpMap = n}) <$> k (_primOpMap s)

defaultMaMaOpTable :: MaMaOpTable op
defaultMaMaOpTable
  = MaMaOps
    { _litConvOp = conv0     -- dummy
    , _primOpMap = op0       -- dummy
    }
    where
      conv0 tn = const Nothing $ n
        where
          n = tn ^. name

      op0 po = const Nothing $ po

-- ----------------------------------------
