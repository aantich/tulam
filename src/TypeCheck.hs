{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

-- | Type checker for tulam: bidirectional type checking with row polymorphism.
-- Inserted as Pass 3 in the compilation pipeline (between case optimization and CLM conversion).
module TypeCheck
  ( -- * Internal type representation
    Ty(..), Row(..), TyVar
  , pattern TArrow, pattern TProd
    -- * Type checker state and monad
  , TCState(..), TCEnv(..), TCMode(..), TCError(..), Constraint(..)
  , initTCState, emptyTCEnv
    -- * Core operations
  , runTC, freshTyVar, freshRowVar
  , exprToTy
    -- * Unification & Subtyping
  , unify, applySubst, subtype
    -- * Bidirectional checking
  , infer, check
    -- * Polymorphism
  , instantiate, generalize, substTyVar
    -- * Internal helpers (for testing)
  , tcBind, tcPure, tcWarn, tcWarnOrFail, tcTry, tcLocal, tcWithContext, tcFail
  , checkTopLevel, buildTCEnvFromEnvironment, inferLambda, lamToTy
  , resolveConstraints, showTCError, showExprBrief, showTy, showRow
  , tyToName, bindRow
    -- * Type-level normalization
  , normalizeTy, tyToCLM, clmToTy, isConcreteTy, lambdaToCLMLam
    -- * Rigid variable analysis (for existential escape checking)
  , freeRigidVars
    -- * GADT support
  , gadtRefine, gadtExprToTy, extractTypeName, applyGADTRefinements
    -- * Pipeline integration
  , typeCheckPass
  ) where

import Surface
import State
import CLM (CLMExpr(..), CLMLam(..), CLMPatternCheck(..), evalCLMPure, inferTypePure)
import Logs

import Control.Monad (when, unless, zipWithM_, forM_, forM, foldM)
import Control.Monad.Trans.State.Strict (get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict as Map
import Data.List (intercalate, nub)

-- ============================================================================
-- Internal Type Representation
-- ============================================================================

-- | Type variables are unique integers
type TyVar = Int

-- | Internal type representation, separate from Surface.Expr.
-- Designed to support theorem proving (Curry-Howard) via dependent types.
data Ty
  = TVar TyVar              -- ^ Unification variable (mutable during inference)
  | TRigid Name             -- ^ Rigid/skolem variable (from forall, not unifiable)
  | TCon Name               -- ^ Type constructor: Int, Bool, Maybe, etc.
  | TApp Ty [Ty]            -- ^ Type application: Maybe Int = TApp (TCon "Maybe") [TCon "Int"]
  | TPi (Maybe Name) Ty Ty  -- ^ Dependent function type: (x:A) -> B(x)
                             --   TPi Nothing a b = simple arrow (a -> b)
                             --   TPi (Just "x") a b = dependent pi
  | TSigma (Maybe Name) Ty Ty -- ^ Dependent pair type: (x:A) * B(x)
                               --   TSigma Nothing a b = simple product
                               --   TSigma (Just "x") a b = dependent sigma
  | TId Ty Ty Ty            -- ^ Identity/propositional equality: Id(A, x, y)
  | TForall Name Ty         -- ^ Polymorphic type: forall a. Ty
  | TRecord Row             -- ^ Record type: {x:Int, y:Bool, ..r}
  | TEffect Row Ty          -- ^ Effect type: Eff {console:Console, ..r} a
  | TU Level                -- ^ Universe: TU (LConst 0) = Type, TU (LConst 1) = Type1, etc.
  | TLevel                  -- ^ The type of universe levels themselves
  | TLit Literal            -- ^ Value-level literal promoted to type level: TLit (LInt 3)
  deriving (Show, Eq)

-- | Row types for structural records
data Row
  = REmpty                   -- ^ Closed row (no more fields)
  | RExtend Name Ty Row      -- ^ Field extension: {name:Ty | rest}
  | RVar TyVar               -- ^ Row variable (for open records)
  | RRigid Name              -- ^ Rigid row variable
  deriving (Show, Eq)

-- | Convenience: non-dependent arrow type
pattern TArrow :: Ty -> Ty -> Ty
pattern TArrow a b = TPi Nothing a b

-- | Convenience: non-dependent product type
pattern TProd :: Ty -> Ty -> Ty
pattern TProd a b = TSigma Nothing a b

-- ============================================================================
-- Type Checker State & Monad
-- ============================================================================

data TCMode = TCStrict | TCRelaxed deriving (Show, Eq)

data TCError
  = Mismatch Ty Ty             -- ^ Expected vs actual type mismatch
  | OccursCheck TyVar Ty       -- ^ Infinite type detected
  | UnboundVar Name            -- ^ Variable not in scope
  | MissingField Name          -- ^ Record field not found
  | ArityMismatch Int Int      -- ^ Expected vs actual argument count
  | ConstraintUnsolved Constraint -- ^ Unresolved structure constraint
  | OtherError String          -- ^ Generic error
  | WithContext String TCError  -- ^ Error with location context
  | SubtypeMismatch Ty Ty      -- ^ Subtype relationship doesn't hold
  deriving (Show)

data Constraint
  = CStructure Name [Ty]       -- ^ e.g. CStructure "Eq" [TVar 3]
  | CRowLack Name Row          -- ^ Field absence constraint for row unification
  deriving (Show)

data TCState = TCState
  { nextVar      :: !Int                      -- ^ Fresh variable counter
  , substitution :: HashMap TyVar Ty          -- ^ Current substitution (unification results)
  , rowSubst     :: HashMap TyVar Row         -- ^ Row variable substitution
  , constraints  :: [Constraint]              -- ^ Deferred constraints
  , tcErrors     :: [TCError]                 -- ^ Accumulated errors
  , tcMode       :: TCMode                    -- ^ Strict or relaxed
  } deriving (Show)

data TCEnv = TCEnv
  { varTypes    :: HashMap Name Ty            -- ^ Local variable types (gamma context)
  , tyVarScope  :: [Name]                     -- ^ In-scope type variables
  , envCompiler :: Maybe Environment          -- ^ Compiler environment for lookups
  , tcContext   :: [String]                   -- ^ Error context stack (e.g., "function 'foo'")
  }

initTCState :: TCMode -> TCState
initTCState mode = TCState
  { nextVar      = 0
  , substitution = Map.empty
  , rowSubst     = Map.empty
  , constraints  = []
  , tcErrors     = []
  , tcMode       = mode
  }

emptyTCEnv :: TCEnv
emptyTCEnv = TCEnv
  { varTypes    = Map.empty
  , tyVarScope  = []
  , envCompiler = Nothing
  , tcContext   = []
  }

-- | The TC monad: state over TCState, with environment passed explicitly.
-- We use Either for error short-circuiting during unification,
-- but accumulate non-fatal errors in tcErrors.
type TC a = TCEnv -> TCState -> Either [TCError] (a, TCState)

runTC :: TC a -> TCEnv -> TCState -> Either [TCError] (a, TCState)
runTC tc env st = tc env st

-- Monad-like operations for TC (manual since it's a function type)
tcPure :: a -> TC a
tcPure a _env st = Right (a, st)

tcBind :: TC a -> (a -> TC b) -> TC b
tcBind ma f env st = case ma env st of
  Left errs -> Left errs
  Right (a, st') -> f a env st'

tcGet :: TC TCState
tcGet _env st = Right (st, st)

tcPut :: TCState -> TC ()
tcPut st' _env _st = Right ((), st')

tcModify :: (TCState -> TCState) -> TC ()
tcModify f _env st = Right ((), f st)

tcAsk :: TC TCEnv
tcAsk env st = Right (env, st)

tcLocal :: (TCEnv -> TCEnv) -> TC a -> TC a
tcLocal f ma env st = ma (f env) st

-- | Run a TC action with added context for error messages
tcWithContext :: String -> TC a -> TC a
tcWithContext ctx = tcLocal (\env -> env { tcContext = ctx : tcContext env })

tcFail :: TCError -> TC a
tcFail err _env _st = Left [err]

-- | Record a non-fatal error and continue
tcWarn :: TCError -> TC ()
tcWarn err _env st = Right ((), st { tcErrors = err : tcErrors st })

-- | Warn in relaxed mode, fail in strict mode. Attaches context if available.
tcWarnOrFail :: TCError -> TC ()
tcWarnOrFail err env st =
  let wrapped = case tcContext env of
        []      -> err
        (ctx:_) -> WithContext ctx err
  in case tcMode st of
    TCStrict  -> Left [wrapped]
    TCRelaxed -> Right ((), st { tcErrors = wrapped : tcErrors st })

tcMapM :: (a -> TC b) -> [a] -> TC [b]
tcMapM _ [] = tcPure []
tcMapM f (x:xs) = f x `tcBind` \b -> tcMapM f xs `tcBind` \bs -> tcPure (b:bs)

tcMapM_ :: (a -> TC ()) -> [a] -> TC ()
tcMapM_ _ [] = tcPure ()
tcMapM_ f (x:xs) = f x `tcBind` \_ -> tcMapM_ f xs

-- ============================================================================
-- Fresh Variables
-- ============================================================================

-- | Generate a fresh unification type variable
freshTyVar :: TC Ty
freshTyVar _env st =
  let v = nextVar st
  in Right (TVar v, st { nextVar = v + 1 })

-- | Generate a fresh row variable
freshRowVar :: TC Row
freshRowVar _env st =
  let v = nextVar st
  in Right (RVar v, st { nextVar = v + 1 })

-- ============================================================================
-- Expr → Ty Conversion
-- ============================================================================

-- | Convert a Surface AST type expression to the internal Ty representation.
exprToTy :: Expr -> TC Ty
exprToTy UNDEFINED = freshTyVar  -- unknown type → fresh var
exprToTy (U n) = tcPure (TU (LConst n))
-- Literal promotion: value-level literals → type-level literals (for dependent types)
exprToTy (Lit l) = tcPure (TLit l)
exprToTy (Id "Int") = tcPure (TCon "Int")
exprToTy (Id "Float64") = tcPure (TCon "Float64")
exprToTy (Id "String") = tcPure (TCon "String")
exprToTy (Id "Char") = tcPure (TCon "Char")
exprToTy (Id "Bool") = tcPure (TCon "Bool")
exprToTy (Id "Nat") = tcPure (TCon "Nat")
exprToTy (Id "Int8") = tcPure (TCon "Int8")
exprToTy (Id "Int16") = tcPure (TCon "Int16")
exprToTy (Id "Int32") = tcPure (TCon "Int32")
exprToTy (Id "Int64") = tcPure (TCon "Int64")
exprToTy (Id "UInt") = tcPure (TCon "UInt")
exprToTy (Id "UInt8") = tcPure (TCon "UInt8")
exprToTy (Id "UInt16") = tcPure (TCon "UInt16")
exprToTy (Id "UInt32") = tcPure (TCon "UInt32")
exprToTy (Id "UInt64") = tcPure (TCon "UInt64")
exprToTy (Id "Float32") = tcPure (TCon "Float32")
exprToTy (Id "Byte") = tcPure (TCon "Byte")
exprToTy (Id "Array") = tcPure (TCon "Array")
exprToTy (Id name) = do
  -- Check if it's a known type constructor or a type variable
  -- Capital-letter names are type constructors, lowercase are type variables
  tcAsk `tcBind` \env ->
    if not (Prelude.null name) && isUpper (Prelude.head name)
    then tcPure (TCon name)
    else if name `Prelude.elem` tyVarScope env
         then tcPure (TRigid name)
         else tcPure (TRigid name)  -- assume type variable even if not in scope
  where
    isUpper c = c >= 'A' && c <= 'Z'
-- PropEq(A, x, y) → TId (canonical name; PropEqT kept as legacy alias)
exprToTy (App (Id "PropEqT") [tyExpr, lhs, rhs]) =
  exprToTy tyExpr `tcBind` \t ->
    exprToTy lhs `tcBind` \l ->
      exprToTy rhs `tcBind` \r ->
        tcPure (TId t l r)
exprToTy (App (Id "PropEq") [tyExpr, lhs, rhs]) =
  exprToTy tyExpr `tcBind` \t ->
    exprToTy lhs `tcBind` \l ->
      exprToTy rhs `tcBind` \r ->
        tcPure (TId t l r)
exprToTy (App (Id name) args) =
  tcMapM exprToTy args `tcBind` \tyArgs ->
    tcPure (TApp (TCon name) tyArgs)
exprToTy (Pi Nothing a b) =
  exprToTy a `tcBind` \ta ->
    exprToTy b `tcBind` \tb ->
      tcPure (TPi Nothing ta tb)
exprToTy (Pi (Just n) a b) =
  exprToTy a `tcBind` \ta ->
    exprToTy b `tcBind` \tb ->
      tcPure (TPi (Just n) ta tb)
exprToTy (Implicit inner) =
  exprToTy inner  -- for now, just look through Implicit wrappers
exprToTy (NTuple fields) =
  -- Unified tuple/record type: positional → TSigma, named → TSigma with names
  tcMapM (\(mn, e) -> exprToTy e `tcBind` \ty -> tcPure (mn, ty)) fields `tcBind` \typedFields ->
    case typedFields of
      []        -> tcPure (TCon "Unit")
      [(_, t)]  -> tcPure t
      _         -> tcPure (Prelude.foldr (\(mn, t) acc -> TSigma mn t acc) (snd (Prelude.last typedFields)) (Prelude.init typedFields))
-- Record type: {x:Int, y:Bool, ..}
exprToTy (RecordType fields isOpen) =
  tcMapM (\(n, tpExpr) -> exprToTy tpExpr `tcBind` \ty -> tcPure (n, ty)) fields `tcBind` \typedFields ->
    if isOpen
    then freshRowVar `tcBind` \rv ->
           tcPure (TRecord (Prelude.foldr (\(n,t) r -> RExtend n t r) rv typedFields))
    else tcPure (TRecord (Prelude.foldr (\(n,t) r -> RExtend n t r) REmpty typedFields))
-- Effect type: Eff { row } resultType
exprToTy (EffType rowExpr resultExpr) =
  exprToEffRow rowExpr `tcBind` \row ->
    exprToTy resultExpr `tcBind` \resTy ->
      tcPure (TEffect row resTy)
-- General App (non-Id head)
exprToTy (App e args) =
  exprToTy e `tcBind` \headTy ->
    tcMapM exprToTy args `tcBind` \tyArgs ->
      tcPure (TApp headTy tyArgs)
-- Function in type position → arrow type
exprToTy (Function lam) = lamToTy lam
-- ConTuple in type position
exprToTy (ConTuple (ConsTag cname _) args) =
  tcMapM exprToTy args `tcBind` \tyArgs ->
    tcPure (TApp (TCon cname) tyArgs)
-- Known declaration forms — not types, skip silently
exprToTy (ModuleDecl _) = freshTyVar
exprToTy (Import _ _ _) = freshTyVar
exprToTy (Export _ _) = freshTyVar
exprToTy (PrivateDecl _) = freshTyVar
exprToTy (SumType _) = freshTyVar
exprToTy (Structure _ _) = freshTyVar
exprToTy (Instance _ _ _ _) = freshTyVar
exprToTy (ClassDecl _ _) = freshTyVar
exprToTy Intrinsic = freshTyVar
exprToTy Derive = freshTyVar
-- Fallback: if we can't convert, use a fresh variable with warning
exprToTy e =
  tcWarn (OtherError $ "Cannot convert to type: " ++ showExprBrief e)
    `tcBind` \_ -> freshTyVar

-- | Convert an effect row expression to a Row
exprToEffRow :: Expr -> TC Row
exprToEffRow (RecordType fields isOpen) =
  tcMapM (\(n, tpExpr) -> exprToTy tpExpr `tcBind` \ty -> tcPure (n, ty)) fields `tcBind` \typedFields ->
    if isOpen
    then freshRowVar `tcBind` \rv ->
           tcPure (Prelude.foldr (\(n,t) r -> RExtend n t r) rv typedFields)
    else tcPure (Prelude.foldr (\(n,t) r -> RExtend n t r) REmpty typedFields)
exprToEffRow _ = freshRowVar  -- unknown row → fresh row var

-- ============================================================================
-- Substitution Application
-- ============================================================================

-- | Apply the current substitution to a type, fully resolving all known variables.
applySubst :: Ty -> TC Ty
applySubst (TVar v) =
  tcGet `tcBind` \st ->
    case Map.lookup v (substitution st) of
      Just ty -> applySubst ty  -- chase the chain
      Nothing -> tcPure (TVar v)
applySubst (TRigid n) = tcPure (TRigid n)
applySubst (TCon n) = tcPure (TCon n)
applySubst (TApp c args) =
  applySubst c `tcBind` \c' ->
    tcMapM applySubst args `tcBind` \args' ->
      tcPure (TApp c' args')
applySubst (TPi mn a b) =
  applySubst a `tcBind` \a' ->
    applySubst b `tcBind` \b' ->
      tcPure (TPi mn a' b')
applySubst (TSigma mn a b) =
  applySubst a `tcBind` \a' ->
    applySubst b `tcBind` \b' ->
      tcPure (TSigma mn a' b')
applySubst (TId t a b) =
  applySubst t `tcBind` \t' ->
    applySubst a `tcBind` \a' ->
      applySubst b `tcBind` \b' ->
        tcPure (TId t' a' b')
applySubst (TForall n t) =
  applySubst t `tcBind` \t' ->
    tcPure (TForall n t')
applySubst (TRecord row) =
  applySubstRow row `tcBind` \row' ->
    tcPure (TRecord row')
applySubst (TEffect row ty) =
  applySubstRow row `tcBind` \row' ->
    applySubst ty `tcBind` \ty' ->
      tcPure (TEffect row' ty')
applySubst (TU l) = tcPure (TU l)
applySubst TLevel = tcPure TLevel
applySubst (TLit l) = tcPure (TLit l)

applySubstRow :: Row -> TC Row
applySubstRow REmpty = tcPure REmpty
applySubstRow (RExtend l t r) =
  applySubst t `tcBind` \t' ->
    applySubstRow r `tcBind` \r' ->
      tcPure (RExtend l t' r')
applySubstRow (RVar v) =
  tcGet `tcBind` \st ->
    case Map.lookup v (rowSubst st) of
      Just row -> applySubstRow row
      Nothing  -> tcPure (RVar v)
applySubstRow (RRigid n) = tcPure (RRigid n)

-- ============================================================================
-- Unification
-- ============================================================================

-- | Bind a type variable to a type (with occurs check)
bind :: TyVar -> Ty -> TC ()
bind v ty =
  applySubst ty `tcBind` \ty' ->
    case ty' of
      TVar v' | v == v' -> tcPure ()  -- already same
      _ -> if occursIn v ty'
           then tcFail (OccursCheck v ty')
           else tcModify (\st -> st { substitution = Map.insert v ty' (substitution st) })

-- | Check if a type variable occurs in a type
occursIn :: TyVar -> Ty -> Bool
occursIn v (TVar v')      = v == v'
occursIn _ (TRigid _)     = False
occursIn _ (TCon _)       = False
occursIn v (TApp c args)  = occursIn v c || Prelude.any (occursIn v) args
occursIn v (TPi _ a b)    = occursIn v a || occursIn v b
occursIn v (TSigma _ a b) = occursIn v a || occursIn v b
occursIn v (TId t a b)    = occursIn v t || occursIn v a || occursIn v b
occursIn v (TForall _ t)  = occursIn v t
occursIn v (TRecord row)  = occursInRow v row
occursIn v (TEffect row ty) = occursInRow v row || occursIn v ty
occursIn _ (TU _)         = False
occursIn _ TLevel         = False
occursIn _ (TLit _)       = False

occursInRow :: TyVar -> Row -> Bool
occursInRow _ REmpty          = False
occursInRow v (RExtend _ t r) = occursIn v t || occursInRow v r
occursInRow v (RVar v')       = v == v'
occursInRow _ (RRigid _)      = False

-- | Unify two types. Tries standard unification first; if that fails,
-- normalizes both sides (evaluating type-level functions) and retries.
unify :: Ty -> Ty -> TC ()
unify t1 t2 =
  applySubst t1 `tcBind` \t1' ->
    applySubst t2 `tcBind` \t2' ->
      tcTry (unify' t1' t2') `tcBind` \result ->
        case result of
          Just _ -> tcPure ()   -- unified without normalization
          Nothing ->            -- retry with type-level normalization
            tcAsk `tcBind` \tenv ->
              case envCompiler tenv of
                Just env ->
                  let t1n = normalizeTy env t1'
                      t2n = normalizeTy env t2'
                  in if t1n /= t1' || t2n /= t2'
                     then unify' t1n t2n      -- normalization changed something, retry
                     else unify' t1' t2'       -- nothing changed, fail with original error
                Nothing -> unify' t1' t2'      -- no env, fail with original error

unify' :: Ty -> Ty -> TC ()
unify' (TVar v) t = bind v t
unify' t (TVar v) = bind v t
unify' (TRigid a) (TRigid b) | a == b = tcPure ()
unify' (TCon a) (TCon b) | a == b = tcPure ()
unify' TLevel TLevel = tcPure ()
unify' (TU l1) (TU l2) = case (levelEq l1 l2, levelLeq l1 l2) of
  (Just True, _) -> tcPure ()            -- equal levels
  (_, Just True)  -> tcPure ()           -- cumulativity: U l1 ≤ U l2
  _ -> tcFail (Mismatch (TU l1) (TU l2))  -- can't decide or l1 > l2
unify' (TPi mn1 a1 r1) (TPi mn2 a2 r2) =
  unify a1 a2 `tcBind` \_ ->
  case (mn1, mn2) of
    (Just n1, Just n2) | n1 /= n2 ->
      tcGet `tcBind` \st ->
        let freshName = "__pi_" ++ show (nextVar st)
            rigid = TRigid freshName
        in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
          unify (substTyVar n1 rigid r1) (substTyVar n2 rigid r2)
    _ -> unify r1 r2
unify' (TSigma mn1 a1 r1) (TSigma mn2 a2 r2) =
  unify a1 a2 `tcBind` \_ ->
  case (mn1, mn2) of
    (Just n1, Just n2) | n1 /= n2 ->
      tcGet `tcBind` \st ->
        let freshName = "__sigma_" ++ show (nextVar st)
            rigid = TRigid freshName
        in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
          unify (substTyVar n1 rigid r1) (substTyVar n2 rigid r2)
    _ -> unify r1 r2
unify' (TLit l1) (TLit l2) | l1 == l2 = tcPure ()
unify' (TId t1 a1 b1) (TId t2 a2 b2) =
  unify t1 t2 `tcBind` \_ -> unify a1 a2 `tcBind` \_ -> unify b1 b2
unify' (TApp c1 as1) (TApp c2 as2)
  | Prelude.length as1 == Prelude.length as2 =
      unify c1 c2 `tcBind` \_ ->
        tcMapM_ (\(a, b) -> unify a b) (Prelude.zip as1 as2)
unify' (TRecord r1) (TRecord r2) = unifyRows r1 r2
unify' (TEffect r1 t1) (TEffect r2 t2) =
  unifyRows r1 r2 `tcBind` \_ -> unify t1 t2
unify' (TForall a t1) (TForall b t2) =
  -- Alpha-rename: replace both bound vars with the same fresh rigid var, then unify bodies
  -- Using a rigid var ensures we check structural equivalence, not unifiability
  tcGet `tcBind` \st ->
    let freshName = "__forall_" ++ show (nextVar st)
        rigid = TRigid freshName
    in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
      let t1' = substTyVar a rigid t1
          t2' = substTyVar b rigid t2
      in unify t1' t2'
-- Structural subtyping: TCon (nominal record) vs TRecord (structural record)
-- If a nominal type has a single constructor whose fields match the structural record, unify succeeds.
unify' (TCon name) (TRecord row) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv -> case expandNominalToRow name cenv of
        Just nomRow -> unifyRows nomRow row
        Nothing -> tcFail (Mismatch (TCon name) (TRecord row))
      Nothing -> tcFail (Mismatch (TCon name) (TRecord row))
unify' (TRecord row) (TCon name) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv -> case expandNominalToRow name cenv of
        Just nomRow -> unifyRows row nomRow
        Nothing -> tcFail (Mismatch (TRecord row) (TCon name))
      Nothing -> tcFail (Mismatch (TRecord row) (TCon name))
unify' t1 t2 = tcFail (Mismatch t1 t2)

-- | Row unification (Remy-style)
unifyRows :: Row -> Row -> TC ()
unifyRows REmpty REmpty = tcPure ()
unifyRows (RExtend l1 t1 r1) row2 =
  rowExtract l1 row2 `tcBind` \(t2, r2') ->
    unify t1 t2 `tcBind` \_ ->
      unifyRows r1 r2'
unifyRows (RVar v) row = bindRow v row
unifyRows row (RVar v) = bindRow v row
unifyRows r1 r2 = tcFail (OtherError $ "Cannot unify rows: " ++ show r1 ++ " vs " ++ show r2)

-- | Extract a field from a row
rowExtract :: Name -> Row -> TC (Ty, Row)
rowExtract l (RExtend l' t r)
  | l == l'   = tcPure (t, r)
  | otherwise = rowExtract l r `tcBind` \(t', r') ->
                  tcPure (t', RExtend l' t r')
rowExtract l (RVar v) =
  freshTyVar `tcBind` \t ->
    freshRowVar `tcBind` \r ->
      bindRow v (RExtend l t r) `tcBind` \_ ->
        case t of
          TVar _ -> tcPure (t, r)
          _      -> tcPure (t, r)
rowExtract l REmpty = tcFail (MissingField l)
rowExtract l (RRigid n) = tcFail (OtherError $ "Cannot extract field " ++ l ++ " from rigid row " ++ n)

-- | Bind a row variable
bindRow :: TyVar -> Row -> TC ()
bindRow v row =
  applySubstRow row `tcBind` \row' ->
    case row' of
      RVar v' | v == v' -> tcPure ()
      _ -> if occursInRow v row'
           then tcFail (OtherError "Occurs check in row unification")
           else tcModify (\st -> st { rowSubst = Map.insert v row' (rowSubst st) })

-- | Expand a nominal type (TCon) to a structural row, if it's a single-constructor type (record).
-- Returns Just Row if the type has exactly one constructor with named fields, Nothing otherwise.
expandNominalToRow :: Name -> Environment -> Maybe Row
expandNominalToRow typeName env =
  -- Look up the constructor with the same name as the type (record convention)
  case lookupConstructor typeName env of
    Just (lam, _tag) ->
      let ps = params lam
      in if not (Prelude.null ps) && Prelude.all (\v -> Surface.name v /= "" && Surface.name v /= "_") ps
         then Just (Prelude.foldr (\v r -> RExtend (Surface.name v) (exprToTyPure (typ v)) r) REmpty ps)
         else Nothing
    Nothing -> Nothing
  where
    -- Simple expression-to-type conversion for constructor parameter types
    -- (no TC monad needed — constructor types are already concrete)
    exprToTyPure :: Expr -> Ty
    exprToTyPure (Id n) = TCon n
    exprToTyPure (App (Id n) args) = TApp (TCon n) (Prelude.map exprToTyPure args)
    exprToTyPure (U n) = TU (LConst n)
    exprToTyPure (Pi mn a b) = TPi mn (exprToTyPure a) (exprToTyPure b)
    exprToTyPure UNDEFINED = TCon "?"
    exprToTyPure _ = TCon "?"

-- ============================================================================
-- Subtype Checking
-- ============================================================================

-- | Check if t1 is a subtype of t2 (t1 <: t2).
-- First tries unification (exact match); falls back to class hierarchy walk.
subtype :: Ty -> Ty -> TC ()
subtype t1 t2 =
  applySubst t1 `tcBind` \t1' ->
    applySubst t2 `tcBind` \t2' ->
      tcTry (unify t1' t2') `tcBind` \result ->
        case result of
          Just _  -> tcPure ()  -- exact match is always a subtype
          Nothing -> subtype' t1' t2'

subtype' :: Ty -> Ty -> TC ()
subtype' (TCon a) (TCon b) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv | isSubclassOf a b cenv -> tcPure ()
      _ -> tcFail (SubtypeMismatch (TCon a) (TCon b))
-- TApp: covariant subtyping for immutable type constructors
subtype' (TApp (TCon a) as1) (TApp (TCon b) as2)
  | a == b && Prelude.length as1 == Prelude.length as2 =
      tcMapM_ (\(x,y) -> subtype x y) (Prelude.zip as1 as2)
  | otherwise =
      tcAsk `tcBind` \env ->
        case envCompiler env of
          Just cenv | isSubclassOf a b cenv && Prelude.length as1 == Prelude.length as2 ->
            tcMapM_ (\(x,y) -> subtype x y) (Prelude.zip as1 as2)
          _ -> tcFail (SubtypeMismatch (TApp (TCon a) as1) (TApp (TCon b) as2))
-- Universe cumulativity: U n is a subtype of U m when n ≤ m
subtype' TLevel TLevel = tcPure ()
subtype' (TU l1) (TU l2) = case levelLeq l1 l2 of
  Just True -> tcPure ()
  _ -> tcFail (SubtypeMismatch (TU l1) (TU l2))
subtype' t1 t2 = tcFail (SubtypeMismatch t1 t2)

-- ============================================================================
-- Polymorphism: Instantiation & Generalization
-- ============================================================================

-- | Instantiate a polymorphic type by replacing bound type variables with fresh unification vars
instantiate :: Ty -> TC Ty
instantiate (TForall a bodyTy) =
  freshTyVar `tcBind` \freshVar ->
    instantiate (substTyVar a freshVar bodyTy)
instantiate ty = tcPure ty

-- | Substitute a rigid type variable name with a Ty throughout a type
substTyVar :: Name -> Ty -> Ty -> Ty
substTyVar n replacement (TRigid m) | n == m = replacement
substTyVar n r (TApp c args) = TApp (substTyVar n r c) (Prelude.map (substTyVar n r) args)
substTyVar n r (TPi mn a b)
  | mn == Just n = TPi mn (substTyVar n r a) b  -- shadowed: don't substitute in body
  | otherwise    = TPi mn (substTyVar n r a) (substTyVar n r b)
substTyVar n r (TSigma mn a b)
  | mn == Just n = TSigma mn (substTyVar n r a) b  -- shadowed: don't substitute in body
  | otherwise    = TSigma mn (substTyVar n r a) (substTyVar n r b)
substTyVar n r (TId t a b) = TId (substTyVar n r t) (substTyVar n r a) (substTyVar n r b)
substTyVar n r (TForall m t) | n == m = TForall m t  -- shadowed
                              | otherwise = TForall m (substTyVar n r t)
substTyVar n r (TRecord row) = TRecord (substTyVarRow n r row)
substTyVar n r (TEffect row ty) = TEffect (substTyVarRow n r row) (substTyVar n r ty)
substTyVar n r (TU l) = TU (substLevel n (tyToLevel r) l)
  where
    tyToLevel (TU l') = l'
    tyToLevel _ = l  -- if replacement isn't a level, keep original
substTyVar _ _ ty = ty  -- TVar, TCon, TLevel, TLit — atoms don't contain rigid vars

substTyVarRow :: Name -> Ty -> Row -> Row
substTyVarRow _ _ REmpty = REmpty
substTyVarRow n r (RExtend l t rest) = RExtend l (substTyVar n r t) (substTyVarRow n r rest)
substTyVarRow _ _ row = row  -- RVar, RRigid

-- | Generalize a type by quantifying over free unification variables
-- that are not bound in the environment
generalize :: Ty -> TC Ty
generalize ty =
  applySubst ty `tcBind` \ty' ->
    tcAsk `tcBind` \env ->
      let freeVars = nub (freeTyVars ty')
          envVars = freeEnvVars env
          genVars = Prelude.filter (`Prelude.notElem` envVars) freeVars
          -- Create names for generalized vars: a, b, c, ...
          names = Prelude.zipWith (\v i -> (v, "t" ++ show i)) genVars [(0::Int)..]
      in if Prelude.null genVars
         then tcPure ty'
         else let substed = Prelude.foldl (\t (v, nm) -> replaceTVar v (TRigid nm) t) ty' names
              in tcPure (Prelude.foldr (\(_, nm) t -> TForall nm t) substed names)

-- | Find free (unresolved) type variables in a type
freeTyVars :: Ty -> [TyVar]
freeTyVars (TVar v) = [v]
freeTyVars (TApp c args) = freeTyVars c ++ Prelude.concatMap freeTyVars args
freeTyVars (TPi _ a b) = freeTyVars a ++ freeTyVars b
freeTyVars (TSigma _ a b) = freeTyVars a ++ freeTyVars b
freeTyVars (TId t a b) = freeTyVars t ++ freeTyVars a ++ freeTyVars b
freeTyVars (TForall _ t) = freeTyVars t
freeTyVars (TRecord row) = freeRowVars row
freeTyVars (TEffect row ty) = freeRowVars row ++ freeTyVars ty
freeTyVars _ = []

freeRowVars :: Row -> [TyVar]
freeRowVars REmpty = []
freeRowVars (RExtend _ t r) = freeTyVars t ++ freeRowVars r
freeRowVars (RVar v) = [v]
freeRowVars _ = []

-- | Find all free type vars mentioned in the environment's varTypes
freeEnvVars :: TCEnv -> [TyVar]
freeEnvVars env = Prelude.concatMap freeTyVars (Map.elems (varTypes env))

-- | Collect free rigid variable names from a type (for existential escape checking)
freeRigidVars :: Ty -> [Name]
freeRigidVars (TRigid n) = [n]
freeRigidVars (TPi (Just n) a b) = freeRigidVars a ++ Prelude.filter (/= n) (freeRigidVars b)
freeRigidVars (TPi Nothing a b) = freeRigidVars a ++ freeRigidVars b
freeRigidVars (TSigma (Just n) a b) = freeRigidVars a ++ Prelude.filter (/= n) (freeRigidVars b)
freeRigidVars (TSigma Nothing a b) = freeRigidVars a ++ freeRigidVars b
freeRigidVars (TApp f args) = freeRigidVars f ++ Prelude.concatMap freeRigidVars args
freeRigidVars (TRecord row) = freeRigidVarsRow row
freeRigidVars (TEffect row t) = freeRigidVarsRow row ++ freeRigidVars t
freeRigidVars (TForall _ t) = freeRigidVars t
freeRigidVars (TId t a b) = freeRigidVars t ++ freeRigidVars a ++ freeRigidVars b
freeRigidVars _ = []

freeRigidVarsRow :: Row -> [Name]
freeRigidVarsRow REmpty = []
freeRigidVarsRow (RExtend _ t r) = freeRigidVars t ++ freeRigidVarsRow r
freeRigidVarsRow (RRigid n) = [n]
freeRigidVarsRow _ = []

-- | Replace a TVar with a Ty throughout a type
replaceTVar :: TyVar -> Ty -> Ty -> Ty
replaceTVar v r (TVar v') | v == v' = r
replaceTVar v r (TApp c args) = TApp (replaceTVar v r c) (Prelude.map (replaceTVar v r) args)
replaceTVar v r (TPi mn a b) = TPi mn (replaceTVar v r a) (replaceTVar v r b)
replaceTVar v r (TSigma mn a b) = TSigma mn (replaceTVar v r a) (replaceTVar v r b)
replaceTVar v r (TId t a b) = TId (replaceTVar v r t) (replaceTVar v r a) (replaceTVar v r b)
replaceTVar v r (TForall n t) = TForall n (replaceTVar v r t)
replaceTVar v r (TRecord row) = TRecord (replaceTVarRow v r row)
replaceTVar v r (TEffect row ty) = TEffect (replaceTVarRow v r row) (replaceTVar v r ty)
replaceTVar _ _ ty = ty

replaceTVarRow :: TyVar -> Ty -> Row -> Row
replaceTVarRow _ _ REmpty = REmpty
replaceTVarRow v r (RExtend l t rest) = RExtend l (replaceTVar v r t) (replaceTVarRow v r rest)
replaceTVarRow v r (RVar v') | v == v' = case r of { TRecord row -> row; _ -> RVar v' }
replaceTVarRow _ _ row = row

-- ============================================================================
-- GADT Type Refinement
-- ============================================================================
-- When pattern matching on a GADT constructor, the constructor's specific return
-- type is unified with the scrutinee's type, producing type refinements.
-- E.g., matching VNil : Vec(a, Z) against scrutinee : Vec(a, n) gives n = Z.

-- | Extract GADT type refinements from ExpandedCase pattern checks.
-- Returns a list of (rigid var name, refined type) pairs that should be
-- applied as substitutions in the branch body.
gadtRefine :: [Expr] -> TC [(Name, Ty)]
gadtRefine checks =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> tcPure []
      Just cenv ->
        -- Collect all constructor tag checks
        let tagChecks = [(ct, scrut) | PatternGuard (PCheckTag ct) scrut <- checks]
        in case tagChecks of
             [] -> tcPure []
             ((ConsTag cname _, scrutExpr) : _) ->
               case lookupConstructor cname cenv of
                 Nothing -> tcPure []
                 Just (conLam, _) ->
                   -- Get the parent sum type name from the constructor's return type
                   let parentName = extractTypeName (lamType conLam)
                   in case Map.lookup parentName (types cenv) of
                        Nothing -> tcPure []
                        Just (SumType parentLam) ->
                          let typeParams = Prelude.map name (params parentLam)
                          in if Prelude.null typeParams
                             then tcPure []  -- no type params, no refinement possible
                             else gadtRefineWithParams typeParams conLam scrutExpr
                        _ -> tcPure []

-- | Extract the type name from an expression like App (Id "Vec") [...] or Id "Bool"
extractTypeName :: Expr -> Name
extractTypeName (App (Id n) _) = n
extractTypeName (Id n) = n
extractTypeName _ = ""

-- | Perform GADT refinement: instantiate type params with fresh vars,
-- convert the constructor's return type, unify with scrutinee type.
gadtRefineWithParams :: [Name] -> Lambda -> Expr -> TC [(Name, Ty)]
gadtRefineWithParams typeParams conLam scrutExpr =
  -- Create fresh unification vars for each type parameter
  tcMapM (\_ -> freshTyVar) typeParams `tcBind` \freshVars ->
    let paramMapping = Prelude.zip typeParams freshVars
        -- Build reverse map: TVar id → TRigid paramName (for resolving var chains)
        reverseMap = [(v, TRigid n) | (n, TVar v) <- paramMapping]
        -- Convert the constructor's GADT return type, replacing type params with fresh vars
    in gadtExprToTy paramMapping (lamType conLam) `tcBind` \gadtRetTy ->
       -- Infer the scrutinee's type
       infer scrutExpr `tcBind` \scrutTy ->
         applySubst scrutTy `tcBind` \scrutTy' ->
           -- Replace TRigid type params in scrutinee with the SAME fresh vars
           -- so that unification can produce refinements
           let scrutTyFresh = Prelude.foldl (\t (n, fv) -> substTyVar n fv t) scrutTy' paramMapping
           -- Unify the GADT return type with the freshened scrutinee type
           in tcTry (unify gadtRetTy scrutTyFresh) `tcBind` \_ ->
             -- Extract refinements: for each type param, see what its fresh var resolved to
             tcMapM (\(paramName, tv) ->
               applySubst tv `tcBind` \resolved ->
                 case resolved of
                   TVar v' | tv == TVar v' -> tcPure Nothing  -- unresolved, no refinement
                   ty ->
                     -- Resolve any remaining fresh vars back to their original param rigids.
                     -- E.g., if matching Refl : PropEq(a,x,x) against PropEq(a,x,y),
                     -- y's fresh var resolves to x's fresh var. We map x's fresh var
                     -- back to TRigid "x" so the refinement is y → TRigid "x".
                     let resolvedTy = Prelude.foldl (\t' (v, rigid) -> replaceTVar v rigid t') ty reverseMap
                     in tcPure (Just (paramName, resolvedTy))
             ) paramMapping `tcBind` \results ->
               tcPure [(n, t) | Just (n, t) <- results]

-- | Convert a GADT return type expression to Ty, using fresh vars for type params.
-- E.g., Vec(a, Z) with mapping [(a, ?0), (n, ?1)] → TApp (TCon "Vec") [TVar 0, TCon "Z"]
gadtExprToTy :: [(Name, Ty)] -> Expr -> TC Ty
gadtExprToTy mapping (Id n) =
  case Prelude.lookup n mapping of
    Just tv -> tcPure tv  -- type param → fresh var
    Nothing ->
      -- Check uppercase → TCon, lowercase → try mapping then TRigid
      if not (Prelude.null n) && n >= "A" && Prelude.head n <= 'Z'
      then tcPure (TCon n)
      else tcPure (TRigid n)
gadtExprToTy mapping (App (Id n) args) =
  tcMapM (gadtExprToTy mapping) args `tcBind` \tyArgs ->
    tcPure (TApp (TCon n) tyArgs)
gadtExprToTy mapping (Pi mn a b) =
  gadtExprToTy mapping a `tcBind` \ta ->
    gadtExprToTy mapping b `tcBind` \tb ->
      tcPure (TPi mn ta tb)
gadtExprToTy mapping (NTuple fields) =
  tcMapM (\(_, e) -> gadtExprToTy mapping e) fields `tcBind` \tys ->
    case tys of
      []  -> tcPure (TCon "Unit")
      [t] -> tcPure t
      _   -> tcPure (Prelude.foldr1 TProd tys)
gadtExprToTy _ (U n) = tcPure (TU (LConst n))
gadtExprToTy _ (Lit l) = tcPure (TLit l)
gadtExprToTy _ e = exprToTy e  -- fallback to normal conversion

-- | Apply GADT refinements to the type environment.
-- Replaces TRigid occurrences of refined type params with their concrete types.
applyGADTRefinements :: [(Name, Ty)] -> TCEnv -> TCEnv
applyGADTRefinements [] env = env
applyGADTRefinements refinements env =
  env { varTypes = Map.map applyRefs (varTypes env) }
  where
    applyRefs ty = Prelude.foldl (\t (n, r) -> substTyVar n r t) ty refinements

-- ============================================================================
-- Type-Level Normalization
-- ============================================================================
-- Uses evalCLMPure from CLM.hs to evaluate type-level function applications.
-- This is the same evaluation logic the runtime uses (applyCLMLam, resolveCases),
-- just without IO/effects. One codebase for all universe levels.

-- | Normalize a Ty by evaluating type-level function applications.
-- Only attempts reduction when all arguments are concrete (no unification variables).
-- Falls back to the original type if reduction fails.
normalizeTy :: Environment -> Ty -> Ty
normalizeTy env = go
  where
    go (TApp (TCon name) args) =
        let args' = Prelude.map go args
        in tryReduce name args'
    go (TPi mn a b)    = TPi mn (go a) (go b)
    go (TSigma (Just n) a b) =
        let a' = go a
        in if isConcreteTy a'
           then TSigma (Just n) a' (go (substTyVar n a' b))
           else TSigma (Just n) a' (go b)
    go (TSigma Nothing a b) = TSigma Nothing (go a) (go b)
    go (TApp f args)   = TApp (go f) (Prelude.map go args)
    go (TId t a b)     = TId (go t) (go a) (go b)
    go (TForall n t)   = TForall n (go t)
    go (TRecord row)   = TRecord (goRow row)
    go (TEffect row t) = TEffect (goRow row) (go t)
    go t = t  -- TVar, TRigid, TCon, TU — already normal forms

    goRow REmpty = REmpty
    goRow (RExtend l t r) = RExtend l (go t) (goRow r)
    goRow r = r

    tryReduce name argTys
        | Prelude.all isConcreteTy argTys =
            let argCLMs = Prelude.map (tyToCLM env) argTys
                -- Top-level function lookup (topLambdas + clmLambdas)
                lookupFn n = case lookupLambda n env of
                    Just lam -> Just (lambdaToCLMLam env lam)
                    Nothing  -> Map.lookup n (clmLambdas env)
                -- Instance lookup: infer types from concrete args, then find instance.
                -- Mirrors runtime dispatchInstance: multi-param → prefix → single-param fallback.
                -- Checks both clmInstances (available after Pass 4) and instanceLambdas
                -- (available during Pass 3/type checking, converted on-the-fly).
                consLookup cn = lookupTypeOfConstructor cn env
                lookupInstanceFn funcNm args =
                    let typeNames = [t | Just t <- Prelude.map (inferTypePure consLookup) args]
                    in if Prelude.null typeNames then Nothing
                       else tryInstanceLookup funcNm typeNames
                -- Try CLM instance first, then Surface instanceLambda (convert on-the-fly)
                tryInstance funcNm tns =
                    case lookupCLMInstance funcNm tns env of
                        Just clm -> Just clm
                        Nothing  -> case lookupInstanceLambda funcNm tns env of
                            Just lam -> Just (lambdaToCLMLam env lam)
                            Nothing  -> Nothing
                tryInstanceLookup funcNm typeNames =
                    -- 1. Try full multi-param exact key
                    case tryInstance funcNm typeNames of
                        Just clm -> Just clm
                        Nothing ->
                            -- 2. Try prefix match (morphism dispatch, CLM only)
                            case lookupCLMInstancePrefix funcNm typeNames env of
                                Just clm -> Just clm
                                Nothing ->
                                    -- 3. Fall back to single-param: try each type
                                    let tryKeys [] = Nothing
                                        tryKeys (t:ts) = case tryInstance funcNm [t] of
                                            Just clm -> Just clm
                                            Nothing  -> tryKeys ts
                                    in tryKeys (Data.List.nub typeNames)
                result = evalCLMPure lookupFn lookupInstanceFn 0 (CLMAPP (CLMID name) argCLMs)
            in case clmToTy result of
                Just ty -> ty
                Nothing -> TApp (TCon name) argTys  -- can't convert back
        | otherwise = TApp (TCon name) argTys       -- has unification vars

-- | Convert a Surface Lambda to CLMLam (pure, on-the-fly).
-- Lightweight version of Pipeline.lambdaToCLMLambda for use during type checking
-- when clmLambdas aren't built yet (Pass 3 runs before Pass 4).
lambdaToCLMLam :: Environment -> Lambda -> CLMLam
lambdaToCLMLam env (Lambda _ ps Intrinsic _ _) = CLMLam [] CLMPRIMCALL
lambdaToCLMLam env (Lambda _ ps Derive _ _) = CLMLam [] CLMPRIMCALL
lambdaToCLMLam env (Lambda _ ps (PatternMatches exs) _ _) =
    CLMLamCases (varsToCLMVarsTC env ps) (Prelude.map (exprToCLMTC env) exs)
lambdaToCLMLam env (Lambda _ ps bdy _ _) =
    CLMLam (varsToCLMVarsTC env ps) (exprToCLMTC env bdy)

-- | Minimal Expr → CLMExpr conversion for type-level normalization.
-- Handles the subset of Expr that appears in function bodies used at the type level.
exprToCLMTC :: Environment -> Expr -> CLMExpr
exprToCLMTC _ UNDEFINED = CLMEMPTY
exprToCLMTC env (Id name) =
    case lookupConstructor name env of
        Just (_, idx) -> CLMCON (ConsTag name idx) []
        Nothing -> CLMID name
exprToCLMTC env (App (Id name) args) =
    case lookupConstructor name env of
        Just (lam, idx) | Prelude.length args == arity lam ->
            CLMCON (ConsTag name idx) (Prelude.map (exprToCLMTC env) args)
        _ -> CLMAPP (CLMID name) (Prelude.map (exprToCLMTC env) args)
exprToCLMTC env (App e args) = CLMAPP (exprToCLMTC env e) (Prelude.map (exprToCLMTC env) args)
exprToCLMTC env (ConTuple ct fields) = CLMCON ct (Prelude.map (exprToCLMTC env) fields)
exprToCLMTC env (Function lam) = CLMLAM (lambdaToCLMLam env lam)
exprToCLMTC env (PatternMatches exs) = CLMAPP CLMEMPTY (Prelude.map (exprToCLMTC env) exs)
exprToCLMTC env (ExpandedCase checks ex _) = CLMCASE (Prelude.map (pcToCLM env) checks) (exprToCLMTC env ex)
exprToCLMTC _ (Lit l) = CLMLIT l
exprToCLMTC _ (U n) = CLMU (LConst n)
exprToCLMTC env (RecFieldAccess ac e) = CLMFieldAccess ac (exprToCLMTC env e)
exprToCLMTC _ Intrinsic = CLMPRIMCALL
exprToCLMTC _ (Pi _ _ _) = CLMEMPTY
exprToCLMTC _ _ = CLMEMPTY  -- other nodes: not needed for type-level eval

pcToCLM :: Environment -> Expr -> CLMPatternCheck
pcToCLM env (PatternGuard (PCheckTag ct) ex) = CLMCheckTag ct (exprToCLMTC env ex)
pcToCLM env (PatternGuard (PCheckLit lit) ex) = CLMCheckLit lit (exprToCLMTC env ex)
pcToCLM _ _ = CLMCheckTag (ConsTag "ERROR" (-1)) CLMEMPTY

varsToCLMVarsTC :: Environment -> [Var] -> [(Name, CLMExpr)]
varsToCLMVarsTC env = Prelude.map (\v -> (name v, exprToCLMTC env (val v)))

-- | Convert Ty → CLMExpr (for feeding into the pure evaluator).
-- Constructors become CLMCON, type names become CLMID, applications become CLMAPP.
tyToCLM :: Environment -> Ty -> CLMExpr
tyToCLM env (TCon name) =
    case lookupConstructor name env of
        Just (_, idx) -> CLMCON (ConsTag name idx) []
        Nothing       -> CLMID name
tyToCLM env (TApp (TCon name) args) =
    case lookupConstructor name env of
        Just (_, idx) -> CLMCON (ConsTag name idx) (Prelude.map (tyToCLM env) args)
        Nothing       -> CLMAPP (CLMID name) (Prelude.map (tyToCLM env) args)
tyToCLM _ (TRigid n) = CLMID n
tyToCLM _ (TU l) = CLMU l
tyToCLM _ TLevel = CLMID "__Level"
tyToCLM env (TApp f args) = CLMAPP (tyToCLM env f) (Prelude.map (tyToCLM env) args)
tyToCLM env (TId t a b) = CLMAPP (CLMID "PropEq") [tyToCLM env t, tyToCLM env a, tyToCLM env b]
tyToCLM _ (TLit l) = CLMLIT l
tyToCLM _ _ = CLMEMPTY

-- | Convert CLMExpr → Ty (for reading back evaluation results).
clmToTy :: CLMExpr -> Maybe Ty
clmToTy (CLMCON (ConsTag name _) [])   = Just (TCon name)
clmToTy (CLMCON (ConsTag name _) args) = TApp (TCon name) <$> mapM clmToTy args
clmToTy (CLMID "__Level")              = Just TLevel
clmToTy (CLMID name)                   = Just (TCon name)
clmToTy (CLMU l)                       = Just (TU l)
clmToTy (CLMLIT l)                     = Just (TLit l)
clmToTy CLMEMPTY                       = Nothing
clmToTy _                              = Nothing

-- | Is a Ty fully concrete (no unification variables)?
-- Rigid type variables are considered concrete (they're bound by forall).
isConcreteTy :: Ty -> Bool
isConcreteTy (TVar _)       = False
isConcreteTy (TRigid _)     = True
isConcreteTy (TCon _)       = True
isConcreteTy (TApp f args)  = isConcreteTy f && Prelude.all isConcreteTy args
isConcreteTy (TU _)         = True
isConcreteTy TLevel         = True
isConcreteTy (TPi _ a b)    = isConcreteTy a && isConcreteTy b
isConcreteTy (TSigma _ a b) = isConcreteTy a && isConcreteTy b
isConcreteTy (TId t a b)    = isConcreteTy t && isConcreteTy a && isConcreteTy b
isConcreteTy (TLit _)       = True
isConcreteTy _              = False

-- ============================================================================
-- Bidirectional Type Checking
-- ============================================================================

-- | Try an action, returning Nothing on failure instead of propagating error
tcTry :: TC a -> TC (Maybe a)
tcTry action env st = case action env st of
  Left _        -> Right (Nothing, st)
  Right (a, st') -> Right (Just a, st')

-- | Lookup a variable in the local TCEnv, then in the compiler Environment
lookupVarType :: Name -> TC (Maybe Ty)
lookupVarType name =
  tcAsk `tcBind` \env ->
    case Map.lookup name (varTypes env) of
      Just ty -> tcPure (Just ty)
      Nothing -> case envCompiler env of
        Nothing -> tcPure Nothing
        Just cenv -> lookupFromCompilerEnv name cenv

-- | Look up a name's type from the compiler environment
lookupFromCompilerEnv :: Name -> Environment -> TC (Maybe Ty)
lookupFromCompilerEnv name cenv =
  -- Try constructor first
  case lookupConstructor name cenv of
    Just (lam, _tag) -> lamToTy lam `tcBind` \ty -> tcPure (Just ty)
    Nothing ->
      -- Try top-level lambda
      case lookupLambda name cenv of
        Just lam ->
          -- If it's an implicit-param function (from a structure), emit constraint
          lamToTy lam `tcBind` \ty ->
            (if hasImplicit lam
             then emitStructConstraint name lam
             else tcPure ()) `tcBind` \_ ->
              tcPure (Just ty)
        Nothing ->
          -- Try type (for Type-level expressions)
          case lookupType name cenv of
            Just _ -> tcPure (Just (TU (LConst 0)))  -- types have type Type
            Nothing -> tcPure Nothing

-- | Emit a structure constraint for an implicit-param function
emitStructConstraint :: Name -> Lambda -> TC ()
emitStructConstraint funcName lam =
  case params lam of
    (Var _ (Implicit implTy) _ : _) ->
      -- Extract the structure name from the implicit type
      -- Implicit types look like: App (Id "StructName") [Id "a"]
      case implTy of
        App (Id structName) typeArgs ->
          tcMapM exprToTy typeArgs `tcBind` \tyArgs ->
            tcModify (\st -> st { constraints = CStructure structName tyArgs : constraints st })
        Id structName ->
          tcModify (\st -> st { constraints = CStructure structName [] : constraints st })
        _ -> tcPure ()
    _ -> tcPure ()

-- | Convert a Lambda's signature to a Ty (params -> return type)
-- For implicit-param lambdas, wraps in TForall for the type parameters.
-- Preserves parameter names as TPi (Just name) when the param has a meaningful name,
-- enabling dependent type checking (return type can reference param names).
lamToTy :: Lambda -> TC Ty
lamToTy lam =
  let (implVars, ps) = let (imps, rest) = span isImplicitVar (params lam)
                        in (Prelude.map name imps, rest)
      retExpr = lamType lam
  in tcMapM (\(Var nm tp _) -> exprToTy tp `tcBind` \ty -> tcPure (nm, ty)) ps `tcBind` \namedParams ->
       exprToTy retExpr `tcBind` \retTy ->
         let piTy = Prelude.foldr (\(nm, ty) acc ->
               if nm == "" || nm == "_"
               then TPi Nothing ty acc
               else TPi (Just nm) ty acc) retTy namedParams
         in tcPure (Prelude.foldr TForall piTy implVars)

-- | Extract type variable names from implicit params
-- e.g., [a:Type] gives us ["a"], [a:Type, b:Type] gives ["a", "b"]
extractImplicitTypeVars :: Lambda -> [Name]
extractImplicitTypeVars lam =
  Prelude.map name (takeWhile isImplicitVar (params lam))

-- | Synthesize/infer the type of an expression
infer :: Expr -> TC Ty
-- Literals
infer (Lit (LInt _))    = tcPure (TCon "Int")
infer (Lit (LFloat _))  = tcPure (TCon "Float64")
infer (Lit (LString _)) = tcPure (TCon "String")
infer (Lit (LChar _))   = tcPure (TCon "Char")
infer (Lit (LInt8 _))   = tcPure (TCon "Int8")
infer (Lit (LInt16 _))  = tcPure (TCon "Int16")
infer (Lit (LInt32 _))  = tcPure (TCon "Int32")
infer (Lit (LInt64 _))  = tcPure (TCon "Int64")
infer (Lit (LWord8 _))  = tcPure (TCon "UInt8")
infer (Lit (LWord16 _)) = tcPure (TCon "UInt16")
infer (Lit (LWord32 _)) = tcPure (TCon "UInt32")
infer (Lit (LWord64 _)) = tcPure (TCon "UInt64")
infer (Lit (LFloat32 _)) = tcPure (TCon "Float32")
infer (Lit (LList []))  = freshTyVar `tcBind` \a -> tcPure (TApp (TCon "List") [a])
infer (Lit (LList (x:_))) = infer x `tcBind` \ty -> tcPure (TApp (TCon "List") [ty])

-- Variables / Identifiers
infer (Id name) =
  lookupVarType name `tcBind` \mty ->
    case mty of
      Just ty -> instantiate ty  -- instantiate polymorphic types
      Nothing -> tcWarn (UnboundVar name) `tcBind` \_ -> freshTyVar

-- Typed expression: e : T
infer (Typed e tExpr) =
  exprToTy tExpr `tcBind` \ty ->
    check e ty `tcBind` \_ ->
      tcPure ty

-- Class constructor: ClassName.new(args) — check arity and abstract
infer (App (RecFieldAccess ("new", _) (Id className)) args) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> tcMapM infer args `tcBind` \_ -> tcPure (TCon className)
      Just cenv -> case lookupClass className cenv of
        Nothing -> tcMapM infer args `tcBind` \_ -> tcPure (TCon className)
        Just cm
          | cmModifier cm == ClassAbstract ->
            tcWarnOrFail (OtherError $ "Cannot instantiate abstract class " ++ className)
              `tcBind` \_ -> tcPure (TCon className)
          | otherwise ->
            let expected = Prelude.length (cmAllFields cm)
                actual = Prelude.length args
            in (if expected /= actual
                then tcWarnOrFail (ArityMismatch expected actual)
                else tcPure ()) `tcBind` \_ ->
              tcMapM infer args `tcBind` \_ ->
                tcPure (TCon className)

-- Function application: f(args)
infer (App f args) =
  infer f `tcBind` \fTy ->
    applySubst fTy `tcBind` \fTy' ->
      inferApp fTy' args

-- Constructor tuple: Tag(args) — with GADT support
-- For GADT constructors, instantiate type params with fresh vars so that
-- unification with arg types produces the specific return type.
infer (ConTuple (ConsTag name _tag) args) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> freshTyVar
      Just cenv -> case lookupConstructor name cenv of
        Nothing -> freshTyVar
        Just (lam, _) ->
          let ps = params lam
          in if Prelude.length ps /= Prelude.length args
             then tcWarnOrFail (ArityMismatch (Prelude.length ps) (Prelude.length args)) `tcBind` \_ -> freshTyVar
             else
               -- Get parent type's type params for GADT instantiation
               let parentName = extractTypeName (lamType lam)
                   mParentLam = case Map.lookup parentName (types cenv) of
                                  Just (SumType pl) -> Just pl
                                  _ -> Nothing
                   typeParams = case mParentLam of
                                  Just pl -> Prelude.map Surface.name (params pl)
                                  Nothing -> []
               in if Prelude.null typeParams
                  then -- Non-parameterized: use original behavior
                    tcMapM (\(Var _ tp _, arg) -> exprToTy tp `tcBind` \pty -> infer arg `tcBind` \aty ->
                      tcTry (unify pty aty) `tcBind` \_ -> tcPure ())
                      (Prelude.zip ps args) `tcBind` \_ ->
                    exprToTy (lamType lam)
                  else -- GADT: instantiate type params with fresh vars
                    tcMapM (\_ -> freshTyVar) typeParams `tcBind` \freshVars ->
                    let mapping = Prelude.zip typeParams freshVars
                    in tcMapM (\(Var _ tp _, arg) ->
                         gadtExprToTy mapping tp `tcBind` \pty ->
                           infer arg `tcBind` \aty ->
                             tcTry (unify pty aty) `tcBind` \_ -> tcPure ())
                         (Prelude.zip ps args) `tcBind` \_ ->
                       gadtExprToTy mapping (lamType lam)

-- NTuple: unified tuple/record literal {a, b} or {x = a, y = b}
-- Positional fields → product type (TSigma), named fields → record type (TRecord)
infer (NTuple fields)
  | hasNamedFields fields =
    -- Named: infer as structural record type
    tcMapM (\(mn, e) -> infer e `tcBind` \ty -> tcPure (case mn of Just n -> n; Nothing -> "", ty)) fields `tcBind` \typedFields ->
      tcPure (TRecord (Prelude.foldr (\(n,t) r -> RExtend n t r) REmpty typedFields))
  | otherwise =
    -- Positional: infer as product type (TSigma chain)
    tcMapM (\(_, e) -> infer e) fields `tcBind` \tys ->
      case tys of
        []  -> tcPure (TCon "Unit")
        [t] -> tcPure t
        _   -> tcPure (Prelude.foldr1 TProd tys)

-- Function definition
infer (Function lam) = inferLambda lam

-- Pattern matches: infer the type of the first branch
infer (PatternMatches cases) =
  case cases of
    [] -> freshTyVar
    (c:_) -> infer c

-- CaseOf: infer the body type
infer (CaseOf _pats bodyExpr _si) = infer bodyExpr

-- ExpandedCase: apply GADT refinements, then infer body type
infer (ExpandedCase checks bodyExpr _si) =
  gadtRefine checks `tcBind` \refinements ->
    if Prelude.null refinements
    then infer bodyExpr
    else tcLocal (applyGADTRefinements refinements) (infer bodyExpr)

-- Statements: type is the type of the last statement
infer (Statements stmts) =
  case stmts of
    [] -> tcPure (TCon "Unit")
    _  -> infer (Prelude.last stmts)

-- Unary/Binary ops (already desugared to App, but handle anyway)
infer (UnaryOp name e) = infer (App (Id name) [e])
infer (BinaryOp name e1 e2) = infer (App (Id name) [e1, e2])

-- Universe
infer (U n) = tcPure (TU (LConst (n + 1)))  -- Type : Type1, etc.

-- Sum type definitions: parameterized types have higher kind
-- type Nat = ... → Type (no params)
-- type Maybe(a:Type) = ... → Type → Type (one param, lives at Type1)
infer (SumType lam) =
  if Prelude.null (params lam)
  then tcPure (TU (LConst 0))
  else
    tcMapM (\(Var _ tp _) -> exprToTy tp) (params lam) `tcBind` \paramKinds ->
    let kind = Prelude.foldr (\k acc -> TPi Nothing k acc) (TU (LConst 0)) paramKinds
    in tcPure kind

-- Structure definitions have type Type
infer (Structure _ _) = tcPure (TU (LConst 0))

-- Primitive definitions
infer (Primitive _) = tcPure (TU (LConst 0))

-- Instance, Intrinsic, etc. — skip
infer (Instance _ _ _ _) = tcPure (TCon "Unit")
infer Intrinsic = freshTyVar
infer Derive = freshTyVar
infer UNDEFINED = freshTyVar

-- Pi type expression: (x:A) -> B or A -> B is a type, so it has type Type
infer (Pi _ _ _) = tcPure (TU (LConst 0))

-- If-then-else (should be desugared but handle anyway)
infer (IfThenElse cond thenE elseE) =
  check cond (TCon "Bool") `tcBind` \_ ->
    infer thenE `tcBind` \tTy ->
      check elseE tTy `tcBind` \_ ->
        tcPure tTy

-- Let-in (should be desugared but handle anyway)
infer (LetIn binds bodyExpr) =
  inferLetBindsAndBody binds bodyExpr

-- PropEq, Implies, Law — type-level/proof constructs
infer (PropEq _ _) = tcPure (TU (LConst 0))
infer (Implies _ _) = tcPure (TU (LConst 0))
infer (Law _ _) = tcPure (TCon "Unit")

-- Repr, ReprCast
infer (Repr _ _ _ _ _) = tcPure (TCon "Unit")
infer (ReprCast e tp) =
  exprToTy tp `tcBind` \targetTy ->
    tcAsk `tcBind` \env ->
      case envCompiler env of
        Just cenv | isClassType targetTy cenv -> tcPure (TApp (TCon "Maybe") [targetTy])
        _ -> tcPure targetTy
  where
    isClassType (TCon n) cenv = Map.member n (classDecls cenv)
    isClassType _ _ = False
infer (Value v _) = exprToTy (typ v)

-- Record type expression: {x:Int, y:Bool}
infer (RecordType _ _) = tcPure (TU (LConst 0))

-- Field access
infer (RecFieldAccess (fieldName, _idx) e) =
  infer e `tcBind` \eTy ->
    applySubst eTy `tcBind` \eTy' ->
      case eTy' of
        TRecord row ->
          tcTry (rowExtract fieldName row) `tcBind` \result ->
            case result of
              Just (ty, _) -> tcPure ty
              Nothing -> freshTyVar
        _ -> freshTyVar

-- Array literal
infer (ArrayLit []) = freshTyVar `tcBind` \a -> tcPure (TApp (TCon "Array") [a])
infer (ArrayLit (x:_)) = infer x `tcBind` \ty -> tcPure (TApp (TCon "Array") [ty])

-- Module system nodes
infer (ModuleDecl _) = tcPure (TCon "Unit")
infer (Import _ _ _) = tcPure (TCon "Unit")
infer (Open _) = tcPure (TCon "Unit")
infer (Export _ _) = tcPure (TCon "Unit")
infer (PrivateDecl e) = infer e
infer (OpaqueTy _ _) = tcPure (TU (LConst 0))
infer (TargetBlock _ _) = tcPure (TCon "Unit")
infer (TargetSwitch _) = tcPure (TCon "Unit")

-- Effect system nodes
infer (EffectDecl _ _ _) = tcPure (TCon "Unit")
infer (HandlerDecl _ _ _ _) = tcPure (TCon "Unit")
infer (HandleWith computation _handler) =
  -- handle expr with handler: the handler eliminates an effect from the row
  -- For now, just infer the computation type and return the result type
  infer computation `tcBind` \compTy ->
    applySubst compTy `tcBind` \compTy' ->
      case compTy' of
        TEffect _row resTy -> tcPure resTy  -- strip effect row
        _ -> tcPure compTy'                   -- not an effect type, pass through
infer (ActionBlock stmts) =
  -- Action blocks: infer the type of the last statement
  -- The overall type is Eff {effects} lastStmtType
  case stmts of
    [] -> tcPure (TCon "Unit")
    _ -> inferActionStmt (Prelude.last stmts)
infer (EffType rowExpr resExpr) =
  -- Effect type expression: Eff { row } a has type Type
  tcPure (TU (LConst 0))

-- Implicit, Binding, Constructors, etc.
infer (Implicit e) = infer e
infer (Binding v) = exprToTy (typ v)
infer (Constructors _) = tcPure (TU (LConst 0))
infer (ERROR msg) = tcWarn (OtherError $ "ERROR node: " ++ msg) `tcBind` \_ -> freshTyVar

-- Catch-all
infer e =
  tcWarn (OtherError $ "Cannot infer type: " ++ showExprBrief e)
    `tcBind` \_ -> freshTyVar

-- | Apply a function type to arguments
inferApp :: Ty -> [Expr] -> TC Ty
inferApp fTy [] = tcPure fTy
-- Dependent Pi: substitute the bound variable in the return type, then normalize
inferApp (TPi (Just name) paramTy retTy) (arg:rest) =
  tcWithContext "argument of application" (check arg paramTy) `tcBind` \_ ->
    infer arg `tcBind` \argTy ->
      tcAsk `tcBind` \tenv ->
        let retTy' = substTyVar name argTy retTy
            -- Normalize type-level function applications after substitution
            retTy'' = case envCompiler tenv of
                Just env -> normalizeTy env retTy'
                Nothing  -> retTy'
        in applySubst retTy'' `tcBind` \retTy''' ->
             inferApp retTy''' rest
-- Non-dependent arrow: no substitution needed
inferApp (TArrow paramTy retTy) (arg:rest) =
  tcWithContext "argument of application" (check arg paramTy) `tcBind` \_ ->
    applySubst retTy `tcBind` \retTy' ->
      inferApp retTy' rest
inferApp (TVar v) args =
  -- Unknown function type: create fresh vars for params and result
  tcMapM (\arg -> infer arg) args `tcBind` \argTys ->
    freshTyVar `tcBind` \retTy ->
      let fnTy = Prelude.foldr TArrow retTy argTys
      in bind v fnTy `tcBind` \_ -> tcPure retTy
inferApp ty args =
  -- Not an arrow type — might be polymorphic or error
  -- For now, just return a fresh var
  tcMapM infer args `tcBind` \_ -> freshTyVar

-- | Infer type of a Lambda definition
inferLambda :: Lambda -> TC Ty
inferLambda lam =
  let ps = case params lam of
        (Var _ (Implicit _) _):rest -> rest
        p -> p
      selfName = lamName lam
      ctx = if selfName /= "" then "function '" ++ selfName ++ "'" else "anonymous lambda"
  in tcWithContext ctx (
    tcMapM (\(Var n tp _) ->
       exprToTy tp `tcBind` \ty -> tcPure (n, ty)) ps `tcBind` \paramBindings ->
     freshTyVar `tcBind` \selfTy ->
     let paramNames = Prelude.map fst paramBindings
         extendEnv env = env { varTypes =
           (if selfName /= "" && selfName `Prelude.notElem` paramNames
            then Map.insert selfName selfTy else id) $
           Prelude.foldl (\m (n,t) -> Map.insert n t m) (varTypes env) paramBindings }
     in exprToTy (lamType lam) `tcBind` \retTy ->
          tcLocal extendEnv (
            case retTy of
              TVar _ -> infer (body lam)
              _      -> check (body lam) retTy `tcBind` \_ -> tcPure retTy
          ) `tcBind` \bodyTy ->
            let paramTys = Prelude.map snd paramBindings
                funcTy = Prelude.foldr TArrow bodyTy paramTys
            in tcTry (unify selfTy funcTy) `tcBind` \_ ->
                 tcPure funcTy
    )

-- | Infer the type of an action statement
inferActionStmt :: ActionStmt -> TC Ty
inferActionStmt (ActionBind _name expr) = infer expr
inferActionStmt (ActionLet _name expr) = infer expr
inferActionStmt (ActionExpr expr) = infer expr

-- | Infer let bindings and extend environment
inferLetBinds :: [(Var, Expr)] -> TC ()
inferLetBinds [] = tcPure ()
inferLetBinds ((v, e):rest) =
  infer e `tcBind` \ty ->
    tcLocal (\env -> env { varTypes = Map.insert (name v) ty (varTypes env) }) (
      inferLetBinds rest
    )

-- | Infer let bindings with body — keeps bindings in scope for body
inferLetBindsAndBody :: [(Var, Expr)] -> Expr -> TC Ty
inferLetBindsAndBody [] bodyExpr = infer bodyExpr
inferLetBindsAndBody ((v, e):rest) bodyExpr =
  infer e `tcBind` \ty ->
    tcLocal (\env -> env { varTypes = Map.insert (name v) ty (varTypes env) }) (
      inferLetBindsAndBody rest bodyExpr
    )

-- | Check that an expression has the expected type
check :: Expr -> Ty -> TC ()
-- Function definition: check body against expected return type
check (Function lam) expectedTy =
  inferLambda lam `tcBind` \inferredTy ->
    tcTry (unify inferredTy expectedTy) `tcBind` \result ->
      case result of
        Just _ -> tcPure ()
        Nothing ->
          tcTry (subtype inferredTy expectedTy) `tcBind` \subResult ->
            case subResult of
              Just _  -> tcPure ()
              Nothing -> tcWarnOrFail (Mismatch expectedTy inferredTy)

-- Pattern matches: check each branch
check (PatternMatches cases) expectedTy =
  tcMapM_ (\c -> check c expectedTy) cases

-- CaseOf: check the body
check (CaseOf pats bodyExpr si) expectedTy =
  -- Extend env with pattern variable bindings
  let bindings = [(name v, tp) | v@(Var n tp val) <- pats, n /= ""]
  in tcMapM (\(n, tpExpr) -> exprToTy tpExpr `tcBind` \ty -> tcPure (n, ty)) bindings `tcBind` \typedBindings ->
       let extendEnv env = env { varTypes = Prelude.foldl (\m (n,t) -> Map.insert n t m) (varTypes env) typedBindings }
       in tcLocal extendEnv (check bodyExpr expectedTy)

-- ExpandedCase: apply GADT refinements from constructor patterns, then check body
check (ExpandedCase checks bodyExpr _si) expectedTy =
  gadtRefine checks `tcBind` \refinements ->
    if Prelude.null refinements
    then check bodyExpr expectedTy
    else
      -- Apply refinements to both the environment and the expected type
      let refinedExpected = Prelude.foldl (\t (n, r) -> substTyVar n r t) expectedTy refinements
      in tcLocal (applyGADTRefinements refinements) (check bodyExpr refinedExpected)

-- Subsumption: infer, try unify, then try subtype
check expr expectedTy =
  infer expr `tcBind` \inferredTy ->
    tcTry (unify inferredTy expectedTy) `tcBind` \result ->
      case result of
        Just _  -> tcPure ()
        Nothing ->
          tcTry (subtype inferredTy expectedTy) `tcBind` \subResult ->
            case subResult of
              Just _  -> tcPure ()  -- subtype match (Dog <: Animal), no runtime coercion needed
              Nothing -> tcWarnOrFail (Mismatch expectedTy inferredTy)

-- ============================================================================
-- Pipeline Integration
-- ============================================================================

-- | Type check pass — inserted between case optimization and CLM conversion.
-- In permissive mode (default): type errors are logged as warnings.
-- With --strict-types: type errors are fatal.
typeCheckPass :: IntState ()
typeCheckPass = do
    st <- get
    let env = currentEnvironment st
        md  = parsedModule st
        isStrict = strictTypes (currentFlags st)
        mode = if isStrict then TCStrict else TCRelaxed
        tcSt = initTCState mode
        tcEnv = buildTCEnvFromEnvironment env
        logFn = if isStrict then logError else logWarning
    -- Type check each top-level declaration with accumulated state
    _ <- foldM (\accSt (expr, srcInfo) ->
        case runTC (checkTopLevel expr) tcEnv accSt of
            Left errs -> do
                forM_ errs $ \err ->
                    logFn (mkTCLogPayload srcInfo err)
                return accSt  -- continue with previous state on error
            Right (_, tcSt') -> do
                forM_ (tcErrors tcSt') $ \err ->
                    logFn (mkTCLogPayload srcInfo err)
                return (tcSt' { tcErrors = [] })  -- clear reported errors
      ) tcSt md
    pure () -- Type-directed dispatch annotation is done in lamToCLMPass (Pipeline.hs)

-- | Build a TCEnv from the compiler's Environment
buildTCEnvFromEnvironment :: Environment -> TCEnv
buildTCEnvFromEnvironment env = emptyTCEnv { envCompiler = Just env }

-- | Resolve accumulated structure constraints against the compiler environment.
-- For each CStructure constraint, check that an instance exists.
resolveConstraints :: TC ()
resolveConstraints =
  tcGet `tcBind` \st ->
    tcAsk `tcBind` \env ->
      let cs = constraints st
      in tcMapM_ (resolveOne env) cs `tcBind` \_ ->
           tcModify (\s -> s { constraints = [] })
  where
    resolveOne env (CStructure structName tyArgs) =
      -- Try to resolve type args via substitution
      tcMapM applySubst tyArgs `tcBind` \resolvedArgs ->
        let typeNames = Prelude.map tyToName resolvedArgs
        in if Prelude.any (== "") typeNames
           then tcPure ()  -- unresolved vars, can't check yet — that's OK
           else case envCompiler env of
             Nothing -> tcPure ()
             Just cenv ->
               -- Check if instance exists for any function in this structure
               case lookupType structName cenv of
                 Just (Structure structLam _) -> case body structLam of
                   DeclBlock exs ->
                     let funcNames = [lamName l | Function l <- exs]
                     in if Prelude.any (\fn -> instanceExists cenv fn typeNames) funcNames
                        then tcPure ()
                        else tcWarn (ConstraintUnsolved (CStructure structName resolvedArgs))
                   _ -> tcPure ()
                 _ -> tcPure ()  -- structure not found, skip
    resolveOne _ (CRowLack _ _) = tcPure ()  -- row constraints handled during unification

    -- Check if an instance exists for a function with given types
    instanceExists cenv funcName typeNames =
      case lookupInstanceLambda funcName typeNames cenv of
        Just _  -> True
        Nothing -> -- try with structure inheritance
          Prelude.any (\tn ->
            let parents = getAllParents tn cenv
            in Prelude.any (\p -> case lookupInstanceLambda funcName [p] cenv of
                                    Just _ -> True
                                    Nothing -> False) parents
          ) typeNames

-- | Convert a resolved Ty to a type name (for instance lookup)
tyToName :: Ty -> Name
tyToName (TCon n) = n
tyToName (TApp (TCon n) _) = n
tyToName _ = ""  -- unresolved

-- | Check a top-level declaration
checkTopLevel :: Expr -> TC ()
-- Type check a function definition
checkTopLevel (Function lam) =
  inferLambda lam `tcBind` \_ -> resolveConstraints

-- Type check an instance: check each implementation function
checkTopLevel (Instance _structName _targs impls _reqs) =
  tcMapM_ (\impl -> infer impl `tcBind` \_ -> tcPure ()) impls

-- Effect and handler declarations — register but no deep checking needed
checkTopLevel (EffectDecl _ _ _) = tcPure ()
checkTopLevel (HandlerDecl _ _ _ _) = tcPure ()

-- Sum types, structures, primitives — just validate form, no deep checking needed
checkTopLevel (SumType _) = tcPure ()
checkTopLevel (Structure _ _) = tcPure ()
checkTopLevel (Primitive _) = tcPure ()
checkTopLevel (Repr _ _ _ _ _) = tcPure ()
checkTopLevel Intrinsic = tcPure ()
checkTopLevel Derive = tcPure ()

-- Type check a class declaration: validate fields, check method bodies, overrides, implements
checkTopLevel (ClassDecl lam cinfo) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> tcPure ()
      Just cenv ->
        let className = lamName lam
        in tcWithContext ("class " ++ className) (
        -- 1. Validate field types are well-formed
        tcMapM_ (\(Var _ tp _) ->
          tcTry (exprToTy tp) `tcBind` \_ -> tcPure ()
          ) (params lam) `tcBind` \_ ->
        -- 2. Check each method body
        let methods = case body lam of
              DeclBlock exs -> [l | Function l <- exs]
              _ -> []
            selfTy = TCon className
        in tcMapM_ (\methodLam ->
             let methodEnv envT = envT { varTypes =
                   Map.insert "self" selfTy (varTypes envT) }
             in tcLocal methodEnv (
                  tcWithContext ("method '" ++ lamName methodLam ++ "'") (
                    inferLambda methodLam `tcBind` \_ -> tcPure ()
                  ))
             ) methods `tcBind` \_ ->
        -- 3. Validate override signatures match parent
        (case classParent cinfo of
          Nothing -> tcPure ()
          Just (parentName, _) ->
            case lookupType parentName cenv of
              Just (ClassDecl parentLam _) ->
                let parentMethods = case body parentLam of
                      DeclBlock exs -> [l | Function l <- exs]
                      _ -> []
                    overrideNames = [n | (n, MOverride) <- classMethodMods cinfo]
                in tcMapM_ (\overrideName ->
                     case (findLam overrideName methods, findLam overrideName parentMethods) of
                       (Just child, Just parent) ->
                         lamToTy child `tcBind` \childTy ->
                           lamToTy parent `tcBind` \parentTy ->
                             tcTry (unify childTy parentTy) `tcBind` \result ->
                               case result of
                                 Just _ -> tcPure ()
                                 Nothing -> tcWarnOrFail (OtherError $
                                   "override '" ++ overrideName ++ "' in " ++ className
                                   ++ " has incompatible type with " ++ parentName)
                       _ -> tcPure ()
                   ) overrideNames
              _ -> tcPure ()
        ) `tcBind` \_ ->
        -- 4. Validate implements contracts
        let implNames = [n | Id n <- classImplements cinfo]
            methodNames = [lamName l | l <- methods]
        in tcMapM_ (\algName ->
             case lookupType algName cenv of
               Just (Structure structLam structInf) ->
                 let reqFuncs = case body structLam of
                       DeclBlock exs -> [lamName l | Function l <- exs]
                       _ -> []
                     missing = Prelude.filter (`Prelude.notElem` methodNames) reqFuncs
                 in if Prelude.null missing || not (Prelude.null (structDerive structInf))
                    then tcPure ()
                    else tcWarnOrFail (OtherError $
                      "class " ++ className ++ " implements " ++ algName
                      ++ " but missing: " ++ intercalate ", " missing)
               _ -> tcPure ()
           ) implNames `tcBind` \_ ->
        resolveConstraints
        )

-- For any other top-level expression, just infer its type
checkTopLevel expr = infer expr `tcBind` \_ -> resolveConstraints

-- | Convert a TCError to a LogPayload for reporting
mkTCLogPayload :: SourceInfo -> TCError -> LogPayload
mkTCLogPayload si err = mkLogPayload si (showTCError err)

-- | Pretty-print a type checker error
showTCError :: TCError -> String
showTCError (Mismatch t1 t2) = "[TC] Type mismatch: expected " ++ showTy t1 ++ " but got " ++ showTy t2
showTCError (OccursCheck v t) = "[TC] Infinite type: " ++ showTyVar v ++ " occurs in " ++ showTy t
showTCError (UnboundVar n) = "[TC] Unbound variable: " ++ n
showTCError (MissingField n) = "[TC] Missing record field: " ++ n
showTCError (ArityMismatch e a) = "[TC] Arity mismatch: expected " ++ show e ++ " arguments, got " ++ show a
showTCError (ConstraintUnsolved c) = "[TC] Unsolved constraint: " ++ showConstraint c ++ " — no matching instance found"
showTCError (OtherError s) = "[TC] " ++ s
showTCError (WithContext ctx inner) = showTCError inner ++ "\n    in " ++ ctx
showTCError (SubtypeMismatch t1 t2) = "[TC] Subtype mismatch: " ++ showTy t1 ++ " is not a subtype of " ++ showTy t2

showConstraint :: Constraint -> String
showConstraint (CStructure n tys) = n ++ "(" ++ intercalate ", " (Prelude.map showTy tys) ++ ")"
showConstraint (CRowLack n _) = "row lacks " ++ n

-- | Pretty-print a Ty
showTy :: Ty -> String
showTy (TVar v) = showTyVar v
showTy (TRigid n) = n
showTy (TCon n) = n
showTy (TApp c args) = showTy c ++ "(" ++ intercalate ", " (Prelude.map showTy args) ++ ")"
showTy (TPi Nothing a b) = showTyArg a ++ " -> " ++ showTy b
showTy (TPi (Just n) a b) = "(" ++ n ++ ":" ++ showTy a ++ ") -> " ++ showTy b
showTy (TSigma Nothing a b) = "(" ++ showTy a ++ ", " ++ showTy b ++ ")"
showTy (TSigma (Just n) a b) = "(" ++ n ++ ":" ++ showTy a ++ " * " ++ showTy b ++ ")"
showTy (TId t a b) = "Id(" ++ showTy t ++ ", " ++ showTy a ++ ", " ++ showTy b ++ ")"
showTy (TForall n t) = "forall " ++ n ++ ". " ++ showTy t
showTy (TRecord row) = "{" ++ showRow row ++ "}"
showTy (TEffect row ty) = "Eff {" ++ showRow row ++ "} " ++ showTy ty
showTy TLevel = "Level"
showTy (TU (LConst 0)) = "Type"
showTy (TU (LConst n)) = "Type" ++ show n
showTy (TU l) = "U(" ++ showLevel l ++ ")"
showTy (TLit l) = show l

showTyArg :: Ty -> String
showTyArg t@(TPi Nothing _ _) = "(" ++ showTy t ++ ")"
showTyArg t = showTy t

showTyVar :: TyVar -> String
showTyVar v = "?" ++ show v

showRow :: Row -> String
showRow REmpty = ""
showRow (RExtend l t REmpty) = l ++ ":" ++ showTy t
showRow (RExtend l t r) = l ++ ":" ++ showTy t ++ ", " ++ showRow r
showRow (RVar v) = ".." ++ showTyVar v
showRow (RRigid n) = ".." ++ n

-- | Truncated show for expressions (for error messages)
showExprBrief :: Expr -> String
showExprBrief e = let s = show e in if length s > 80 then take 77 s ++ "..." else s

-- | Find a lambda by name in a list
findLam :: Name -> [Lambda] -> Maybe Lambda
findLam n lams = case [l | l <- lams, lamName l == n] of { (l:_) -> Just l; [] -> Nothing }
