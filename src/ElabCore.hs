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

-- | T1.5: count ESurface nodes (Stage-R fallback points) in an elaborated
-- expression. A non-zero count means Stage-R didn't fully elaborate this
-- expression and the Surface-level monomorphizer has to pick up the slack.
-- When this reaches zero across the whole environment, the Surface path can
-- be retired.
countESurface :: ElabExpr -> Int
countESurface = go
  where
    go (ESurface _)    = 1
    go (EAnn x _)      = go x
    go (ELam _ b _)    = go b
    go (ELet bs b)     = sum (map (go . snd) bs) + go b
    go (EProject _ e _)= go e
    go (EIf c t f _)   = go c + go t + go f
    go (ECase alts _)  = sum (map (\a -> sum (map go (elabAltChecks a)) + go (elabAltBody a)) alts)
    go (ECall _ h as _)= go h + sum (map (go . elabArgExpr) as)
    go _               = 0

-- | T1.5: per-lambda Stage-R fallback counts. Returns lambdas with at least
-- one ESurface node in their body.
findStageRHoles :: [(Name, ElabLambda)] -> [(Name, Int)]
findStageRHoles lams =
    [ (n, c) | (n, lam) <- lams
             , let c = countESurface (elabLamBody lam)
             , c > 0 ]

data ElabLambda = ElabLambda
  { elabLamName :: Name
  , elabLamBinders :: [ElabBinder]
  , elabLamBody :: ElabExpr
  , elabLamType :: Expr
  , elabLamSurface :: Lambda
  } deriving (Show, Eq, Generic)

instance Binary ElabLambda
