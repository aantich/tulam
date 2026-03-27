{-# LANGUAGE DeriveGeneric #-}

module ElabMetadata
  ( Visibility(..)
  , Relevance(..)
  , Role(..)
  , BinderInfo(..)
  , ArgInfo(..)
  , defaultExplicitBinderInfo
  , defaultImplicitBinderInfo
  , binderInfoFromVar
  , argInfoFromBinderInfo
  , isSemanticRole
  , isSemanticHiddenArg
  , isNonSemanticHiddenArg
  ) where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import qualified Surface as S
import Surface (Var(..), Expr, Name)

data Visibility = Explicit | Implicit
  deriving (Show, Eq, Ord, Generic)

instance Binary Visibility

data Relevance = Runtime | Erased
  deriving (Show, Eq, Ord, Generic)

instance Binary Relevance

data Role
  = OrdinaryRole
  | EvidenceRole
  | HandlerRole
  | ReflectionRole
  | ReprRole
  | MetaRole
  deriving (Show, Eq, Ord, Generic)

instance Binary Role

data BinderInfo = BinderInfo
  { binderVisibility :: Visibility
  , binderRelevance  :: Relevance
  , binderRole       :: Role
  } deriving (Show, Eq, Ord, Generic)

instance Binary BinderInfo

data ArgInfo = ArgInfo
  { argVisibility :: Visibility
  , argRelevance  :: Relevance
  , argRole       :: Role
  } deriving (Show, Eq, Ord, Generic)

instance Binary ArgInfo

defaultExplicitBinderInfo :: BinderInfo
defaultExplicitBinderInfo = BinderInfo Explicit Runtime OrdinaryRole

defaultImplicitBinderInfo :: BinderInfo
defaultImplicitBinderInfo = BinderInfo Implicit Erased OrdinaryRole

binderInfoFromVar :: Var -> BinderInfo
binderInfoFromVar (Var n ty _) =
  case ty of
    S.Implicit inner -> classify n inner Implicit Erased
    _              -> classify n ty Explicit Runtime
  where
    classify :: Name -> Expr -> Visibility -> Relevance -> BinderInfo
    classify nm innerTy vis rel
      | nm == "__refl" = BinderInfo vis rel ReflectionRole
      | nm == "__repr" = BinderInfo vis rel ReprRole
      | take 6 nm == "__meta" = BinderInfo vis rel MetaRole
      | takesHandlerRole nm innerTy = BinderInfo vis rel HandlerRole
      | takesEvidenceRole nm innerTy = BinderInfo vis rel EvidenceRole
      | otherwise = BinderInfo vis rel OrdinaryRole

    takesHandlerRole name' innerTy =
      take 8 name' == "handler__" ||
      take 10 name' == "__handler_" ||
      mentions ["Handler", "Effect"] innerTy

    takesEvidenceRole name' innerTy =
      take 5 name' == "dict_" ||
      take 9 name' == "evidence_" ||
      mentions ["Implicit", "Structure", "Constraint"] innerTy

    mentions :: [Name] -> Expr -> Bool
    mentions ns (S.Id n') = n' `elem` ns
    mentions ns (S.App f args) = mentions ns f || any (mentions ns) args
    mentions ns (S.Pi _ a b) = mentions ns a || mentions ns b
    mentions ns (S.Sigma _ a b) = mentions ns a || mentions ns b
    mentions ns (S.Implicit e) = mentions ns e
    mentions ns (S.Typed e t) = mentions ns e || mentions ns t
    mentions ns (S.EffType r t) = mentions ns r || mentions ns t
    mentions ns (S.RowExtend _ a b) = mentions ns a || mentions ns b
    mentions _ _ = False

argInfoFromBinderInfo :: BinderInfo -> ArgInfo
argInfoFromBinderInfo bi = ArgInfo (binderVisibility bi) (binderRelevance bi) (binderRole bi)

isSemanticRole :: Role -> Bool
isSemanticRole EvidenceRole = True
isSemanticRole HandlerRole = True
isSemanticRole _ = False

isSemanticHiddenArg :: ArgInfo -> Bool
isSemanticHiddenArg ai = argVisibility ai == Implicit && isSemanticRole (argRole ai)

isNonSemanticHiddenArg :: ArgInfo -> Bool
isNonSemanticHiddenArg ai = argVisibility ai == Implicit && not (isSemanticRole (argRole ai))
