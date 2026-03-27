{-# LANGUAGE DeriveGeneric #-}

module ElabCore where

import GHC.Generics (Generic)
import Data.Binary (Binary)

import Surface (Name, Expr, Lambda, Literal)
import ElabMetadata (BinderInfo)
import ElabObligation (SemanticObligation, SemanticWitness)

data ElabBinder = ElabBinder
  { elabBinderName :: Name
  , elabBinderType :: Expr
  , elabBinderInfo :: BinderInfo
  } deriving (Show, Eq, Generic)

instance Binary ElabBinder

data ElabArg = ElabArg
  { elabArgExpr :: ElabExpr
  , elabArgInfo :: BinderInfo
  } deriving (Show, Eq, Generic)

instance Binary ElabArg

data ElabCallKind
  = DirectCall
  | SemanticCall SemanticObligation
  | HandlerCall SemanticObligation
  | IntrinsicCall Name
  | EvidenceCall SemanticWitness
  deriving (Show, Eq, Generic)

instance Binary ElabCallKind

data ElabAlt = ElabAlt
  { elabAltChecks :: [ElabExpr]
  , elabAltBody   :: ElabExpr
  } deriving (Show, Eq, Generic)

instance Binary ElabAlt

data ElabExpr
  = EVar Name
  | EGlobal Name
  | ELit Literal
  | EType Expr
  | EAnn ElabExpr Expr
  | ELam [ElabBinder] ElabExpr Expr
  | ELet [(Name, ElabExpr)] ElabExpr
  | EProject (Name, Int) ElabExpr Expr
  | EIf ElabExpr ElabExpr ElabExpr Expr
  | ECase [ElabAlt] Expr
  | ECall ElabCallKind ElabExpr [ElabArg] Expr
  | ESurface Expr
  deriving (Show, Eq, Generic)

instance Binary ElabExpr

data ElabLambda = ElabLambda
  { elabLamName :: Name
  , elabLamBinders :: [ElabBinder]
  , elabLamBody :: ElabExpr
  , elabLamType :: Expr
  , elabLamSurface :: Lambda
  } deriving (Show, Eq, Generic)

instance Binary ElabLambda
