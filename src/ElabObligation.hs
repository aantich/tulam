{-# LANGUAGE DeriveGeneric #-}

module ElabObligation where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Surface (Name, Expr)

data ObligationOrigin
  = OriginTopLevelImplicit Name
  | OriginLocalLambda Name
  | OriginCallSite
  | OriginSuperclass ObligationOrigin Name
  deriving (Show, Eq, Generic)

instance Binary ObligationOrigin

data SemanticObligation
  = StructureObligation
      { oblMethod :: Name
      , oblClass  :: Name
      , oblTag    :: Maybe Name
      , oblTypeArgs :: [Name]
      , oblExprArgs :: [Expr]
      , oblOrigin :: ObligationOrigin
      }
  | HandlerObligation
      { oblMethod :: Name
      , oblClass  :: Name
      , oblTag    :: Maybe Name
      , oblTypeArgs :: [Name]
      , oblExprArgs :: [Expr]
      , oblOrigin :: ObligationOrigin
      }
  deriving (Show, Eq, Generic)

instance Binary SemanticObligation

data SemanticWitness
  = InstanceWitness
      { witnessMethod :: Name
      , witnessKey :: Name
      , witnessTypeArgs :: [Name]
      , witnessTag :: Maybe Name
      }
  | HandlerWitness
      { witnessMethod :: Name
      , witnessEffect :: Name
      }
  deriving (Show, Eq, Generic)

instance Binary SemanticWitness
