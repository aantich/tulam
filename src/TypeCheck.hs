{-# LANGUAGE OverloadedStrings #-}

-- | Type checker for tulam: bidirectional type checking with row polymorphism.
-- Inserted as Pass 3 in the compilation pipeline (between case optimization and CLM conversion).
--
-- Uses Expr from Surface.hs directly as the type representation.
-- No separate Ty/Row types — types ARE expressions (unified architecture).
module TypeCheck
  ( -- * Type checker state and monad
    TCState(..), TCEnv(..), TCMode(..), TCError(..), Constraint(..)
  , initTCState, emptyTCEnv
    -- * Core operations
  , runTC, freshMeta
  , normalizeTypeExpr
    -- * Generic type-expr traversal (Phase 1)
  , mapTypeChildren, foldMapTypeChildren
    -- * Unification & Subtyping
  , unify, applySubst, subtype
    -- * Bidirectional checking
  , infer, check
    -- * Polymorphism
  , instantiate, generalize, substTyVar
    -- * Internal helpers (for testing)
  , tcBind, tcPure, tcWarn, tcWarnOrFail, tcTry, tcLocal, tcWithContext, tcFail, tcModify, withCompilerEnv
  , checkTopLevel, buildTCEnvFromEnvironment, inferLambda, inferLamType
  , resolveConstraints, showTCError, showExprBrief, showTy, showRow
  , tyToName, freeMetas, replaceMeta, occursIn
    -- * Type-level normalization
  , normalizeTy, clmToExpr, isConcreteTy, lambdaToCLMLam, exprToCLMTC
  , bind
    -- * Zonking (Phase 4)
  , zonk
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
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.List (intercalate, nub)

-- ============================================================================
-- Type Checker State & Monad
-- ============================================================================

data TCMode = TCStrict | TCRelaxed deriving (Show, Eq)

data TCError
  = Mismatch Expr Expr             -- ^ Expected vs actual type mismatch
  | OccursCheck Int Expr           -- ^ Infinite type detected
  | UnboundVar Name                -- ^ Variable not in scope
  | MissingField Name              -- ^ Record field not found
  | ArityMismatch Int Int          -- ^ Expected vs actual argument count
  | ConstraintUnsolved Constraint  -- ^ Unresolved structure constraint
  | OtherError String              -- ^ Generic error
  | WithContext String TCError     -- ^ Error with location context
  | WithSource SourceInfo TCError  -- ^ Error with source location (Phase 5)
  | SubtypeMismatch Expr Expr      -- ^ Subtype relationship doesn't hold
  deriving (Show)

data Constraint
  = CStructure Name [Expr]         -- ^ e.g. CStructure "Eq" [Meta 3]
  | CRowLack Name Expr             -- ^ Field absence constraint for row unification
  deriving (Show)

data TCState = TCState
  { nextMeta      :: !Int                       -- ^ Fresh metavariable counter
  , substitution  :: HashMap Int Expr           -- ^ Unified substitution (type vars AND row vars)
  , constraints   :: [Constraint]               -- ^ Deferred constraints
  , tcErrors      :: [TCError]                  -- ^ Accumulated errors
  , tcMode        :: TCMode                     -- ^ Strict or relaxed
  } deriving (Show)

data TCEnv = TCEnv
  { varTypes    :: HashMap Name Expr            -- ^ Local variable types (gamma context)
  , tyVarScope  :: [Name]                       -- ^ In-scope type variables
  , envCompiler :: Maybe Environment            -- ^ Compiler environment for lookups
  , tcContext   :: [String]                      -- ^ Error context stack
  , tcSourceInfo :: Maybe SourceInfo            -- ^ Current source location for error messages (Phase 5)
  }

initTCState :: TCMode -> TCState
initTCState mode = TCState
  { nextMeta      = 0
  , substitution  = Map.empty
  , constraints   = []
  , tcErrors      = []
  , tcMode        = mode
  }

emptyTCEnv :: TCEnv
emptyTCEnv = TCEnv
  { varTypes    = Map.empty
  , tyVarScope  = []
  , envCompiler = Nothing
  , tcContext   = []
  , tcSourceInfo = Nothing
  }

-- | The TC monad: state over TCState, with environment passed explicitly.
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

-- | Fail with multiple errors (re-raise preserved errors)
tcFails :: [TCError] -> TC a
tcFails errs _env _st = Left errs

-- | Record a non-fatal error and continue
tcWarn :: TCError -> TC ()
tcWarn err _env st = Right ((), st { tcErrors = err : tcErrors st })

-- | Warn in relaxed mode, fail in strict mode. Attaches context and source info if available.
tcWarnOrFail :: TCError -> TC ()
tcWarnOrFail err env st =
  let withCtx = case tcContext env of
        []      -> err
        (ctx:_) -> WithContext ctx err
      wrapped = case tcSourceInfo env of
        Just si -> WithSource si withCtx
        Nothing -> withCtx
  in case tcMode st of
    TCStrict  -> Left [wrapped]
    TCRelaxed -> Right ((), st { tcErrors = wrapped : tcErrors st })

tcMapM :: (a -> TC b) -> [a] -> TC [b]
tcMapM _ [] = tcPure []
tcMapM f (x:xs) = f x `tcBind` \b -> tcMapM f xs `tcBind` \bs -> tcPure (b:bs)

tcMapM_ :: (a -> TC ()) -> [a] -> TC ()
tcMapM_ _ [] = tcPure ()
tcMapM_ f (x:xs) = f x `tcBind` \_ -> tcMapM_ f xs

-- | Access the compiler environment, applying a function if available.
-- Returns the default action when no compiler environment is set (e.g., in unit tests).
withCompilerEnv :: TC a -> (Environment -> TC a) -> TC a
withCompilerEnv def f =
  tcAsk `tcBind` \tenv ->
    case envCompiler tenv of
      Just cenv -> f cenv
      Nothing   -> def

-- ============================================================================
-- Fresh Variables
-- ============================================================================

-- | Generate a fresh unification metavariable
freshMeta :: TC Expr
freshMeta _env st =
  let v = nextMeta st
  in Right (Meta v, st { nextMeta = v + 1 })

-- ============================================================================
-- Expr → Type Normalization
-- ============================================================================

-- | Normalize a Surface AST expression into a form suitable for type checking.
-- Replaces exprToTy: most Expr forms are already valid types (Id, App, Pi, etc.).
-- Only a few forms need conversion (UNDEFINED → fresh meta, RecordType → RowExtend chain, etc.).
normalizeTypeExpr :: Expr -> TC Expr
normalizeTypeExpr UNDEFINED = freshMeta
normalizeTypeExpr (U l) = tcPure (U l)
normalizeTypeExpr (Lit l) = tcPure (Lit l)
normalizeTypeExpr (Id name) = tcPure (Id name)
normalizeTypeExpr (Meta n) = tcPure (Meta n)
normalizeTypeExpr RowEmpty = tcPure RowEmpty
normalizeTypeExpr (RowExtend n t r) =
  normalizeTypeExpr t `tcBind` \t' ->
    normalizeTypeExpr r `tcBind` \r' ->
      tcPure (RowExtend n t' r')
-- PropEq / PropEqT → App (Id "PropEq") [t, l, r]
normalizeTypeExpr (App (Id "PropEqT") [tyExpr, lhs, rhs]) =
  tcMapM normalizeTypeExpr [tyExpr, lhs, rhs] `tcBind` \args' ->
    tcPure (App (Id "PropEq") args')
normalizeTypeExpr (App (Id "PropEq") args@[_, _, _]) =
  tcMapM normalizeTypeExpr args `tcBind` \args' ->
    tcPure (App (Id "PropEq") args')
normalizeTypeExpr (App f args) =
  normalizeTypeExpr f `tcBind` \f' ->
    tcMapM normalizeTypeExpr args `tcBind` \args' ->
      tcPure (App f' args')
normalizeTypeExpr (Pi mn a b) =
  normalizeTypeExpr a `tcBind` \a' ->
    normalizeTypeExpr b `tcBind` \b' ->
      tcPure (Pi mn a' b')
normalizeTypeExpr (Sigma mn a b) =
  normalizeTypeExpr a `tcBind` \a' ->
    normalizeTypeExpr b `tcBind` \b' ->
      tcPure (Sigma mn a' b')
normalizeTypeExpr (Implicit inner) = normalizeTypeExpr inner
normalizeTypeExpr (NTuple fields) =
  tcMapM (\(mn, e) -> normalizeTypeExpr e `tcBind` \ty -> tcPure (mn, ty)) fields `tcBind` \typedFields ->
    case typedFields of
      []        -> tcPure (Id "Unit")
      [(_, t)]  -> tcPure t
      _         -> tcPure (Prelude.foldr (\(mn, t) acc -> Sigma mn t acc) (snd (Prelude.last typedFields)) (Prelude.init typedFields))
normalizeTypeExpr (RecordType fields isOpen) =
  tcMapM (\(n, tpExpr) -> normalizeTypeExpr tpExpr `tcBind` \ty -> tcPure (n, ty)) fields `tcBind` \typedFields ->
    if isOpen
    then freshMeta `tcBind` \rv ->
           tcPure (Prelude.foldr (\(n,t) r -> RowExtend n t r) rv typedFields)
    else tcPure (Prelude.foldr (\(n,t) r -> RowExtend n t r) RowEmpty typedFields)
normalizeTypeExpr (EffType rowExpr resExpr) =
  normalizeEffRow rowExpr `tcBind` \row ->
    normalizeTypeExpr resExpr `tcBind` \resTy ->
      tcPure (EffType row resTy)
normalizeTypeExpr (Function lam) = inferLamType lam
normalizeTypeExpr (ConTuple (ConsTag cname _) args) =
  tcMapM normalizeTypeExpr args `tcBind` \tyArgs ->
    tcPure (App (Id cname) tyArgs)
-- Known declaration/statement forms — not types, map to fresh meta silently (Phase 7A: exhaustive)
normalizeTypeExpr (ModuleDecl _) = freshMeta
normalizeTypeExpr (Import _ _ _) = freshMeta
normalizeTypeExpr (Export _ _) = freshMeta
normalizeTypeExpr (PrivateDecl _) = freshMeta
normalizeTypeExpr (SumType _) = freshMeta
normalizeTypeExpr (Structure _ _) = freshMeta
normalizeTypeExpr (Instance _ _ _ _) = freshMeta
normalizeTypeExpr (ClassDecl _ _) = freshMeta
normalizeTypeExpr Intrinsic = freshMeta
normalizeTypeExpr Derive = freshMeta
-- Value-level forms that are not types (Phase 7A: explicit cases, no catch-all)
normalizeTypeExpr (BinaryOp _ _ _) = freshMeta
normalizeTypeExpr (UnaryOp _ _) = freshMeta
normalizeTypeExpr (Statements _) = freshMeta
normalizeTypeExpr (ActionBlock _) = freshMeta
normalizeTypeExpr (PatternMatches _) = freshMeta
normalizeTypeExpr (CaseOf _ _ _) = freshMeta
normalizeTypeExpr (ExpandedCase _ _ _) = freshMeta
normalizeTypeExpr (IfThenElse _ _ _) = freshMeta
normalizeTypeExpr (LetIn _ _) = freshMeta
normalizeTypeExpr (Binding _) = freshMeta
normalizeTypeExpr (Value _ _) = freshMeta
normalizeTypeExpr (Primitive _) = freshMeta
normalizeTypeExpr (Action _) = freshMeta
normalizeTypeExpr (Constructors _) = freshMeta
normalizeTypeExpr (DeclBlock _) = freshMeta
normalizeTypeExpr (PatternGuard _ _) = freshMeta
normalizeTypeExpr (RecFieldAccess _ _) = freshMeta
normalizeTypeExpr (Repr _ _ _ _ _) = freshMeta
normalizeTypeExpr (ReprCast _ _) = freshMeta
normalizeTypeExpr (Law _ _) = freshMeta
normalizeTypeExpr (PropEq _ _) = freshMeta
normalizeTypeExpr (Implies _ _) = freshMeta
normalizeTypeExpr (Open _) = freshMeta
normalizeTypeExpr (OpaqueTy _ _) = freshMeta
normalizeTypeExpr (TargetBlock _ _) = freshMeta
normalizeTypeExpr (TargetSwitch _) = freshMeta
normalizeTypeExpr (RecordConstruct _ _) = freshMeta
normalizeTypeExpr (RecordUpdate _ _) = freshMeta
normalizeTypeExpr (RecordPattern _ _) = freshMeta
normalizeTypeExpr (EffectDecl _ _ _) = freshMeta
normalizeTypeExpr (HandlerDecl _ _ _ _) = freshMeta
normalizeTypeExpr (HandleWith _ _) = freshMeta
normalizeTypeExpr (FixityDecl _ _ _) = freshMeta
normalizeTypeExpr (ERROR msg) =
  tcWarn (OtherError $ "Error node in type position: " ++ msg) `tcBind` \_ -> freshMeta

-- | Convert an effect row expression to an Expr row.
-- Reuses normalizeTypeExpr for RecordType; defaults to fresh meta for unknowns.
normalizeEffRow :: Expr -> TC Expr
normalizeEffRow e@(RecordType _ _) = normalizeTypeExpr e
normalizeEffRow _ = freshMeta

-- ============================================================================
-- Generic Type-Expr Traversal Infrastructure (Phase 1)
-- ============================================================================

-- | Apply a function to all immediate type-relevant children of an Expr node,
-- reconstructing the node. Atoms (Meta, Id, U, Lit, RowEmpty) return self.
-- This is the single point of truth for "what children does a type-Expr have?"
mapTypeChildren :: (Expr -> Expr) -> Expr -> Expr
mapTypeChildren _ e@(Meta _)     = e
mapTypeChildren _ e@(Id _)       = e
mapTypeChildren _ e@(U _)        = e
mapTypeChildren _ e@(Lit _)      = e
mapTypeChildren _ e@RowEmpty     = e
mapTypeChildren _ e@UNDEFINED    = e
mapTypeChildren f (App c args)        = App (f c) (Prelude.map f args)
mapTypeChildren f (Pi mn a b)         = Pi mn (f a) (f b)
mapTypeChildren f (Sigma mn a b)      = Sigma mn (f a) (f b)
mapTypeChildren f (RowExtend l t r)   = RowExtend l (f t) (f r)
mapTypeChildren f (EffType r t)       = EffType (f r) (f t)
mapTypeChildren f (NTuple fields)     = NTuple (Prelude.map (\(mn, e) -> (mn, f e)) fields)
mapTypeChildren f (ConTuple ct exprs) = ConTuple ct (Prelude.map f exprs)
mapTypeChildren f (Typed e t)         = Typed (f e) (f t)
mapTypeChildren f (ArrayLit exprs)    = ArrayLit (Prelude.map f exprs)
mapTypeChildren f (Implicit e)        = Implicit (f e)
mapTypeChildren f (RecordType fs o)   = RecordType (Prelude.map (\(n, e) -> (n, f e)) fs) o
mapTypeChildren _ e                   = e  -- all other Expr forms are non-type atoms for our purposes

-- | Accumulation variant: fold a monoid over all immediate type-relevant children.
foldMapTypeChildren :: Monoid m => (Expr -> m) -> Expr -> m
foldMapTypeChildren _ (Meta _)     = mempty
foldMapTypeChildren _ (Id _)       = mempty
foldMapTypeChildren _ (U _)        = mempty
foldMapTypeChildren _ (Lit _)      = mempty
foldMapTypeChildren _ RowEmpty     = mempty
foldMapTypeChildren _ UNDEFINED    = mempty
foldMapTypeChildren f (App c args)        = f c <> mconcat (Prelude.map f args)
foldMapTypeChildren f (Pi _ a b)          = f a <> f b
foldMapTypeChildren f (Sigma _ a b)       = f a <> f b
foldMapTypeChildren f (RowExtend _ t r)   = f t <> f r
foldMapTypeChildren f (EffType r t)       = f r <> f t
foldMapTypeChildren f (NTuple fields)     = mconcat (Prelude.map (f . snd) fields)
foldMapTypeChildren f (ConTuple _ exprs)  = mconcat (Prelude.map f exprs)
foldMapTypeChildren f (Typed e t)         = f e <> f t
foldMapTypeChildren f (ArrayLit exprs)    = mconcat (Prelude.map f exprs)
foldMapTypeChildren f (Implicit e)        = f e
foldMapTypeChildren f (RecordType fs _)   = mconcat (Prelude.map (f . snd) fs)
foldMapTypeChildren _ _                   = mempty

-- ============================================================================
-- Substitution Application
-- ============================================================================

-- | Apply the current substitution to a type expression, fully resolving all known metavariables.
-- Uses mapTypeChildren for structural recursion; Meta gets special path-compression treatment (Phase 6B).
applySubst :: Expr -> TC Expr
applySubst (Meta v) =
  tcGet `tcBind` \st ->
    case Map.lookup v (substitution st) of
      Just ty -> applySubst ty `tcBind` \resolved ->
        -- Path compression (Phase 6B): point v directly to the final resolved type
        case resolved of
          Meta v' | v' == v -> tcPure resolved  -- avoid self-loop
          _ -> tcModify (\s -> s { substitution = Map.insert v resolved (substitution s) })
               `tcBind` \_ -> tcPure resolved
      Nothing -> tcPure (Meta v)
applySubst e = applySubstChildren e
  where
    applySubstChildren (App c args) =
      applySubst c `tcBind` \c' ->
        tcMapM applySubst args `tcBind` \args' ->
          tcPure (App c' args')
    applySubstChildren (Pi mn a b) =
      applySubst a `tcBind` \a' ->
        applySubst b `tcBind` \b' ->
          tcPure (Pi mn a' b')
    applySubstChildren (Sigma mn a b) =
      applySubst a `tcBind` \a' ->
        applySubst b `tcBind` \b' ->
          tcPure (Sigma mn a' b')
    applySubstChildren (RowExtend l t r) =
      applySubst t `tcBind` \t' ->
        applySubst r `tcBind` \r' ->
          tcPure (RowExtend l t' r')
    applySubstChildren (EffType r t) =
      applySubst r `tcBind` \r' ->
        applySubst t `tcBind` \t' ->
          tcPure (EffType r' t')
    applySubstChildren (NTuple fields) =
      tcMapM (\(mn, e') -> applySubst e' `tcBind` \e'' -> tcPure (mn, e'')) fields `tcBind` \fields' ->
        tcPure (NTuple fields')
    applySubstChildren (ConTuple ct exprs) =
      tcMapM applySubst exprs `tcBind` \exprs' ->
        tcPure (ConTuple ct exprs')
    applySubstChildren (Typed e' t) =
      applySubst e' `tcBind` \e'' ->
        applySubst t `tcBind` \t' ->
          tcPure (Typed e'' t')
    applySubstChildren (ArrayLit exprs) =
      tcMapM applySubst exprs `tcBind` \exprs' ->
        tcPure (ArrayLit exprs')
    applySubstChildren (Implicit e') =
      applySubst e' `tcBind` \e'' -> tcPure (Implicit e'')
    applySubstChildren (RecordType fields isOpen) =
      tcMapM (\(n, e') -> applySubst e' `tcBind` \e'' -> tcPure (n, e'')) fields `tcBind` \fields' ->
        tcPure (RecordType fields' isOpen)
    applySubstChildren other = tcPure other  -- Id, U, Lit, RowEmpty, UNDEFINED, etc.

-- ============================================================================
-- Unification
-- ============================================================================

-- | Bind a metavariable to a type (with occurs check)
bind :: Int -> Expr -> TC ()
bind v ty =
  applySubst ty `tcBind` \ty' ->
    case ty' of
      Meta v' | v == v' -> tcPure ()  -- already same
      _ -> if occursIn v ty'
           then tcFail (OccursCheck v ty')
           else tcModify (\st -> st { substitution = Map.insert v ty' (substitution st) })

-- | Check if a metavariable occurs in an expression.
-- Uses foldMapTypeChildren for structural recursion (Phase 1).
occursIn :: Int -> Expr -> Bool
occursIn v = go
  where
    go (Meta v') = v == v'
    go e = Prelude.any go (typeChildren e)

-- | Extract the list of immediate type-relevant children (utility for folds).
typeChildren :: Expr -> [Expr]
typeChildren (App c args)        = c : args
typeChildren (Pi _ a b)          = [a, b]
typeChildren (Sigma _ a b)       = [a, b]
typeChildren (RowExtend _ t r)   = [t, r]
typeChildren (EffType r t)       = [r, t]
typeChildren (NTuple fields)     = Prelude.map snd fields
typeChildren (ConTuple _ exprs)  = exprs
typeChildren (Typed e t)         = [e, t]
typeChildren (ArrayLit exprs)    = exprs
typeChildren (Implicit e)        = [e]
typeChildren (RecordType fs _)   = Prelude.map snd fs
typeChildren _                   = []  -- Meta, Id, U, Lit, RowEmpty, UNDEFINED, etc.

-- | Unify two types. Tries standard unification first; if that fails,
-- normalizes both sides (evaluating type-level functions) and retries.
unify :: Expr -> Expr -> TC ()
unify t1 t2
  | t1 == t2  = tcPure ()  -- structural equality short-circuit
  | otherwise =
  applySubst t1 `tcBind` \t1' ->
    applySubst t2 `tcBind` \t2' ->
      tcTryE (unify' t1' t2') `tcBind` \result ->
        case result of
          Right _ -> tcPure ()
          Left origErrs ->
            withCompilerEnv (tcFails origErrs) (\env ->
              let t1n = normalizeTy env t1'
                  t2n = normalizeTy env t2'
              in tcTryE (unify' t1n t2n) `tcBind` \r2 ->
                   case r2 of
                     Right _ -> tcPure ()
                     Left _  -> tcFails origErrs)  -- normalization didn't help, re-raise original error

-- | Shared helper for Pi/Sigma unification (Phase 1: unifyBinder).
-- Uses "#$sk_" prefix for Skolem variables — # and $ cannot appear in user identifiers (Phase 2A).
unifyBinder :: Maybe Name -> Expr -> Expr -> Maybe Name -> Expr -> Expr -> TC ()
unifyBinder mn1 a1 r1 mn2 a2 r2 =
  unify a1 a2 `tcBind` \_ ->
  case (mn1, mn2) of
    (Just n1, Just n2) | n1 /= n2 ->
      tcGet `tcBind` \st ->
        let freshName = "#$sk_" ++ show (nextMeta st)
            rigid = Id freshName
        in tcModify (\s -> s { nextMeta = nextMeta s + 1 }) `tcBind` \_ ->
          unify (substTyVar n1 rigid r1) (substTyVar n2 rigid r2)
    _ -> unify r1 r2

unify' :: Expr -> Expr -> TC ()
unify' (Meta v) t = bind v t
unify' t (Meta v) = bind v t
unify' (Id a) (Id b) | a == b = tcPure ()
unify' (U l1) (U l2) = case levelEq l1 l2 of
  Just True -> tcPure ()
  -- Cumulativity (U n <= U m) is handled in subtype', not here.
  -- Unification requires exact equality of universe levels.
  _ -> tcFail (Mismatch (U l1) (U l2))
-- Pi/Sigma unification uses shared helper (Phase 1: unifyBinder)
unify' (Pi mn1 a1 r1) (Pi mn2 a2 r2) = unifyBinder mn1 a1 r1 mn2 a2 r2
unify' (Sigma mn1 a1 r1) (Sigma mn2 a2 r2) = unifyBinder mn1 a1 r1 mn2 a2 r2
unify' (Lit l1) (Lit l2) | l1 == l2 = tcPure ()
unify' (App f1 as1) (App f2 as2)
  | Prelude.length as1 == Prelude.length as2 =
      unify f1 f2 `tcBind` \_ ->
        tcMapM_ (\(a, b) -> unify a b) (Prelude.zip as1 as2)
-- Row unification (Remy-style) — merged into unify'
unify' (RowExtend l1 t1 r1) row2 =
  rowExtract l1 row2 `tcBind` \(t2, r2') ->
    unify t1 t2 `tcBind` \_ ->
      unify r1 r2'
unify' RowEmpty RowEmpty = tcPure ()
unify' (EffType r1 t1) (EffType r2 t2) =
  unify r1 r2 `tcBind` \_ -> unify t1 t2
-- Structural subtyping: Id (nominal record) vs RowExtend (structural record)
unify' (Id name) row@(RowExtend _ _ _) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv -> case expandNominalToRow name cenv of
        Just nomRow -> unify nomRow row
        Nothing -> tcFail (Mismatch (Id name) row)
      Nothing -> tcFail (Mismatch (Id name) row)
unify' row@(RowExtend _ _ _) (Id name) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv -> case expandNominalToRow name cenv of
        Just nomRow -> unify row nomRow
        Nothing -> tcFail (Mismatch row (Id name))
      Nothing -> tcFail (Mismatch row (Id name))
unify' t1 t2 = tcFail (Mismatch t1 t2)

-- | Extract a field from a row
rowExtract :: Name -> Expr -> TC (Expr, Expr)
rowExtract l (RowExtend l' t r)
  | l == l'   = tcPure (t, r)
  | otherwise = rowExtract l r `tcBind` \(t', r') ->
                  tcPure (t', RowExtend l' t r')
rowExtract l (Meta v) =
  freshMeta `tcBind` \t ->
    freshMeta `tcBind` \r ->
      bind v (RowExtend l t r) `tcBind` \_ ->
        tcPure (t, r)
rowExtract l RowEmpty = tcFail (MissingField l)
rowExtract l (Id n) = tcFail (OtherError $ "Cannot extract field " ++ l ++ " from rigid row " ++ n)
rowExtract l _ = tcFail (MissingField l)

-- | Expand a nominal type to a structural row, if it's a single-constructor type (record).
expandNominalToRow :: Name -> Environment -> Maybe Expr
expandNominalToRow typeName env =
  case lookupConstructor typeName env of
    Just (lam, _tag) ->
      let ps = params lam
      in if not (Prelude.null ps) && Prelude.all (\v -> Surface.name v /= "" && Surface.name v /= "_") ps
         then Just (Prelude.foldr (\v r -> RowExtend (Surface.name v) (typ v) r) RowEmpty ps)
         else Nothing
    Nothing -> Nothing

-- ============================================================================
-- Subtype Checking
-- ============================================================================

-- | Check if t1 is a subtype of t2 (t1 <: t2).
subtype :: Expr -> Expr -> TC ()
subtype t1 t2 =
  applySubst t1 `tcBind` \t1' ->
    applySubst t2 `tcBind` \t2' ->
      tcTry (unify t1' t2') `tcBind` \result ->
        case result of
          Just _  -> tcPure ()
          Nothing -> subtype' t1' t2'

subtype' :: Expr -> Expr -> TC ()
subtype' (Id a) (Id b) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Just cenv | isSubclassOf a b cenv -> tcPure ()
      _ -> tcFail (SubtypeMismatch (Id a) (Id b))
subtype' (App (Id a) as1) (App (Id b) as2)
  | a == b && Prelude.length as1 == Prelude.length as2 =
      tcMapM_ (\(x,y) -> subtype x y) (Prelude.zip as1 as2)
  | otherwise =
      tcAsk `tcBind` \env ->
        case envCompiler env of
          Just cenv | isSubclassOf a b cenv && Prelude.length as1 == Prelude.length as2 ->
            tcMapM_ (\(x,y) -> subtype x y) (Prelude.zip as1 as2)
          _ -> tcFail (SubtypeMismatch (App (Id a) as1) (App (Id b) as2))
subtype' (Id "Level") (Id "Level") = tcPure ()
subtype' (U l1) (U l2) = case levelLeq l1 l2 of
  Just True -> tcPure ()
  _ -> tcFail (SubtypeMismatch (U l1) (U l2))
subtype' t1 t2 = tcFail (SubtypeMismatch t1 t2)

-- ============================================================================
-- Polymorphism: Instantiation & Generalization
-- ============================================================================

-- | Instantiate a polymorphic type by replacing bound type variables with fresh metavars.
-- Handles any universe level (Type, Type1, Type2, ...), not just Type.
-- Phase 2C: also recognizes Id "Type", Id "Type1" etc. as universe-like domains.
instantiate :: Expr -> TC Expr
instantiate (Pi (Just a) dom bodyTy)
  | isUniverseLike dom =
    freshMeta `tcBind` \freshVar ->
      instantiate (substTyVar a freshVar bodyTy)
instantiate ty = tcPure ty

-- | Instantiate free lowercase type variable Ids with fresh metas.
-- Used for constructor types (which use rigid type names from the type definition).
-- Each unique free lowercase Id gets a consistent fresh meta.
instantiateFreeTypeVars :: Environment -> Expr -> TC Expr
instantiateFreeTypeVars cenv ty =
  let freeVars = Set.toList (collectFreeTypeVarIds ty)
      unboundVars = Prelude.filter (\n -> isLowercaseTypeVar n && not (isKnownType n)) freeVars
  in if Prelude.null unboundVars
     then tcPure ty
     else tcMapM (\_ -> freshMeta) unboundVars `tcBind` \freshVars ->
       let mapping = Prelude.zip unboundVars freshVars
           result = Prelude.foldl (\t (n, fv) -> substTyVar n fv t) ty mapping
       in tcPure result
  where
    isKnownType n = Map.member n (types cenv)
                 || Map.member n (constructors cenv)

-- | Check if an expression represents a universe type (for instantiation).
-- Recognizes U _, Id "Type", Id "Type1", Id "Type2", etc.
isUniverseLike :: Expr -> Bool
isUniverseLike (U _) = True
isUniverseLike (Id "Type") = True
isUniverseLike (Id "Type1") = True
isUniverseLike (Id "Type2") = True
isUniverseLike _ = False

-- | Substitute a named type variable with an Expr throughout a type.
-- Special cases for: Id match, Pi/Sigma shadowing, U level substitution.
-- Falls through to mapTypeChildren for all other structural recursion (Phase 1).
substTyVar :: Name -> Expr -> Expr -> Expr
substTyVar n r = go
  where
    go (Id m) | n == m = r
    go (Pi mn a b)
      | mn == Just n = Pi mn (go a) b  -- shadowed in body
      | otherwise    = Pi mn (go a) (go b)
    go (Sigma mn a b)
      | mn == Just n = Sigma mn (go a) b  -- shadowed in body
      | otherwise    = Sigma mn (go a) (go b)
    go (U l) = U (substLevel n (exprToLevel r) l)
    go e = mapTypeChildren go e
    -- Phase 2D fix: handle Id "Type" / Id "Type1" in level conversion
    exprToLevel (U l') = l'
    exprToLevel (Id "Type") = LConst 0
    exprToLevel (Id "Type1") = LConst 1
    exprToLevel (Id "Type2") = LConst 2
    exprToLevel _ = LConst 0  -- conservative fallback

-- | Generalize a type by quantifying over free metavariables
-- that are not bound in the environment.
-- Phase 2B: uses U 1 for metas that appear as type constructor heads.
generalize :: Expr -> TC Expr
generalize ty =
  applySubst ty `tcBind` \ty' ->
    tcAsk `tcBind` \env ->
      let freeVars = nub (freeMetas ty')
          envVars = freeEnvVars env
          genVars = Prelude.filter (`Prelude.notElem` envVars) freeVars
          -- Phase 2B: collect metas appearing as App heads (type constructors need U 1)
          appHeadMetas = collectAppHeadMetas ty'
          metaKind v = if IntSet.member v appHeadMetas then U (LConst 1) else U (LConst 0)
          names = Prelude.zipWith (\v i -> (v, "t" ++ show i)) genVars [(0::Int)..]
      in if Prelude.null genVars
         then tcPure ty'
         else let substed = Prelude.foldl (\t (v, nm) -> replaceMeta v (Id nm) t) ty' names
              in tcPure (Prelude.foldr (\(v, nm) t -> Pi (Just nm) (metaKind v) t) substed names)

-- | Collect metavariable IDs that appear as the head of an App node.
-- These represent type constructors and should be quantified at U 1.
collectAppHeadMetas :: Expr -> IntSet.IntSet
collectAppHeadMetas (App (Meta v) _) = IntSet.singleton v `IntSet.union`
  IntSet.empty  -- don't recurse into args for the head itself
collectAppHeadMetas e = Prelude.foldl (\acc child -> acc `IntSet.union` collectAppHeadMetas child) IntSet.empty (typeChildren e)

-- ============================================================================
-- Zonking (Phase 4)
-- ============================================================================

-- | Deep substitution pass: resolves all known metavars and replaces remaining
-- unresolved metas with readable names (e.g., "?t0", "?t1").
-- Apply after type checking to clean up types for error messages and annotations.
zonk :: Expr -> TC Expr
zonk e =
  applySubst e `tcBind` \e' ->
    tcPure (zonkRemaining e')
  where
    -- Replace unresolved Meta n with Id "?tN" for readable output
    zonkRemaining (Meta n) = Id ("?t" ++ show n)
    zonkRemaining expr = mapTypeChildren zonkRemaining expr

-- | Find free (unresolved) metavariables in a type expression.
-- Uses IntSet internally (Phase 6A) for O(n log n) instead of O(n^2) list concat.
freeMetas :: Expr -> [Int]
freeMetas e = IntSet.toList (freeMetasSet e)

-- | IntSet variant of freeMetas for efficient accumulation.
freeMetasSet :: Expr -> IntSet.IntSet
freeMetasSet (Meta v) = IntSet.singleton v
freeMetasSet e = Prelude.foldl (\acc child -> acc `IntSet.union` freeMetasSet child) IntSet.empty (typeChildren e)

-- | Find all free metavars in the environment's varTypes
freeEnvVars :: TCEnv -> [Int]
freeEnvVars env = Prelude.concatMap freeMetas (Map.elems (varTypes env))

-- | Collect free rigid variable names from a type (for existential escape checking).
-- Uses Set internally (Phase 6A) for efficient accumulation.
-- Pi/Sigma binders shadow their bound name in the body.
freeRigidVars :: Expr -> [Name]
freeRigidVars e = Set.toList (freeRigidVarsSet e)

freeRigidVarsSet :: Expr -> Set.Set Name
freeRigidVarsSet (Id n)
  | not (Prelude.null n) && Prelude.head n >= 'a' && Prelude.head n <= 'z' = Set.singleton n
  | otherwise = Set.empty
freeRigidVarsSet (Pi (Just n) a b) = freeRigidVarsSet a `Set.union` Set.delete n (freeRigidVarsSet b)
freeRigidVarsSet (Sigma (Just n) a b) = freeRigidVarsSet a `Set.union` Set.delete n (freeRigidVarsSet b)
freeRigidVarsSet e = Prelude.foldl (\acc child -> acc `Set.union` freeRigidVarsSet child) Set.empty (typeChildren e)

-- | Replace a Meta with an Expr throughout a type.
-- Uses mapTypeChildren for structural recursion (Phase 1).
replaceMeta :: Int -> Expr -> Expr -> Expr
replaceMeta v r = go
  where
    go (Meta v') | v == v' = r
    go e = mapTypeChildren go e

-- ============================================================================
-- GADT Type Refinement
-- ============================================================================

-- | Extract GADT type refinements from ExpandedCase pattern checks.
gadtRefine :: [Expr] -> TC [(Name, Expr)]
gadtRefine checks =
  withCompilerEnv (tcPure []) (\cenv ->
    let tagChecks = [(ct, scrut) | PatternGuard (PCheckTag ct) scrut <- checks]
        in case tagChecks of
             [] -> tcPure []
             ((ConsTag cname _, scrutExpr) : _) ->
               case lookupConstructor cname cenv of
                 Nothing -> tcPure []
                 Just (conLam, _) ->
                   let parentName = extractTypeName (lamType conLam)
                   in case Map.lookup parentName (types cenv) of
                        Nothing -> tcPure []
                        Just (SumType parentLam) ->
                          let typeParams = Prelude.map name (params parentLam)
                          -- Only apply GADT refinement when the constructor has a
                          -- non-default return type. For regular constructors like
                          -- Pair(a,b) : Pair(a,b), the return type trivially matches
                          -- the parent type and no refinement is needed. Applying
                          -- refinement to non-GADT constructors causes false type
                          -- errors when the function's type variables shadow the
                          -- constructor's type params.
                          in if Prelude.null typeParams || not (isGADTConstructor typeParams conLam)
                             then tcPure []
                             else gadtRefineWithParams typeParams conLam scrutExpr
                        _ -> tcPure [])

-- | Check if a constructor is a GADT constructor (has a non-default return type).
-- A default return type is `TypeName(a, b, ...)` where the args are exactly the
-- parent's type params in order. GADT constructors have more specific return types
-- like `TypeName(Int, b)` or `TypeName(a, a)`.
isGADTConstructor :: [Name] -> Lambda -> Bool
isGADTConstructor typeParams conLam =
  case lamType conLam of
    App (Id _) args ->
      -- Check if args are exactly the type params as Ids, in order
      not (Prelude.length args == Prelude.length typeParams
        && Prelude.and (Prelude.zipWith (\arg tp -> case arg of Id n -> n == tp; _ -> False) args typeParams))
    Id _ -> not (Prelude.null typeParams)  -- Nullary but has type params → GADT
    _ -> True  -- Complex return type → likely GADT

-- | Extract the type name from an expression
extractTypeName :: Expr -> Name
extractTypeName (App (Id n) _) = n
extractTypeName (Id n) = n
extractTypeName _ = ""

-- | Perform GADT refinement
gadtRefineWithParams :: [Name] -> Lambda -> Expr -> TC [(Name, Expr)]
gadtRefineWithParams typeParams conLam scrutExpr =
  tcMapM (\_ -> freshMeta) typeParams `tcBind` \freshVars ->
    let paramMapping = Prelude.zip typeParams freshVars
        reverseMap = [(v, Id n) | (n, Meta v) <- paramMapping]
    in gadtExprToTy paramMapping (lamType conLam) `tcBind` \gadtRetTy ->
       infer scrutExpr `tcBind` \scrutTy ->
         applySubst scrutTy `tcBind` \scrutTy' ->
           let scrutTyFresh = Prelude.foldl (\t (n, fv) -> substTyVar n fv t) scrutTy' paramMapping
           in tcTry (unify gadtRetTy scrutTyFresh) `tcBind` \unifyResult ->
             case unifyResult of
               Nothing -> tcPure []  -- unification failed, no refinements possible
               Just _ ->
                 tcMapM (\(paramName, tv) ->
                   applySubst tv `tcBind` \resolved ->
                     case resolved of
                       Meta v' | tv == Meta v' -> tcPure Nothing
                       ty ->
                         let resolvedTy = Prelude.foldl (\t' (v, rigid) -> replaceMeta v rigid t') ty reverseMap
                         in tcPure (Just (paramName, resolvedTy))
                 ) paramMapping `tcBind` \results ->
                   tcPure [(n, t) | Just (n, t) <- results]

-- | Convert a GADT return type expression, using fresh vars for type params.
gadtExprToTy :: [(Name, Expr)] -> Expr -> TC Expr
gadtExprToTy mapping (Id n) =
  case Prelude.lookup n mapping of
    Just tv -> tcPure tv
    Nothing -> tcPure (Id n)
gadtExprToTy mapping (App (Id n) args) =
  tcMapM (gadtExprToTy mapping) args `tcBind` \tyArgs ->
    tcPure (App (Id n) tyArgs)
gadtExprToTy mapping (Pi mn a b) =
  gadtExprToTy mapping a `tcBind` \ta ->
    gadtExprToTy mapping b `tcBind` \tb ->
      tcPure (Pi mn ta tb)
gadtExprToTy mapping (NTuple fields) =
  tcMapM (\(_, e) -> gadtExprToTy mapping e) fields `tcBind` \tys ->
    case tys of
      []  -> tcPure (Id "Unit")
      [t] -> tcPure t
      _   -> tcPure (Prelude.foldr1 (\a b -> Sigma Nothing a b) tys)
gadtExprToTy _ (U l) = tcPure (U l)
gadtExprToTy _ (Lit l) = tcPure (Lit l)
gadtExprToTy _ e = normalizeTypeExpr e

-- | Apply GADT refinements to the type environment.
applyGADTRefinements :: [(Name, Expr)] -> TCEnv -> TCEnv
applyGADTRefinements [] env = env
applyGADTRefinements refinements env =
  env { varTypes = Map.map applyRefs (varTypes env) }
  where
    applyRefs ty = Prelude.foldl (\t (n, r) -> substTyVar n r t) ty refinements

-- ============================================================================
-- Type-Level Normalization
-- ============================================================================

-- | Monadic variant of normalizeTy: applies substitution first (Phase 7B).
-- Precondition: envCompiler must be set in the TCEnv.
normalizeTyTC :: Expr -> TC Expr
normalizeTyTC ty =
  applySubst ty `tcBind` \ty' ->
    withCompilerEnv (tcPure ty') (\env -> tcPure (normalizeTy env ty'))

-- | Normalize a type expression by evaluating type-level function applications.
normalizeTy :: Environment -> Expr -> Expr
normalizeTy env = go
  where
    go (App (Id name) args) =
        let args' = Prelude.map go args
        in tryReduce name args'
    go (Pi mn a b)    = Pi mn (go a) (go b)
    go (Sigma (Just n) a b) =
        let a' = go a
        in if isConcreteTy a'
           then Sigma (Just n) a' (go (substTyVar n a' b))
           else Sigma (Just n) a' (go b)
    go (Sigma Nothing a b) = Sigma Nothing (go a) (go b)
    go (App f args)   = App (go f) (Prelude.map go args)
    go (RowExtend l t r) = RowExtend l (go t) (go r)
    go (EffType row t) = EffType (go row) (go t)
    go t = t  -- Meta, Id, U, Lit, RowEmpty — already normal forms

    tryReduce name argTys
        | Prelude.all isConcreteTy argTys =
            let argCLMs = Prelude.map (exprToCLMTC env) argTys
                lookupFn n = case lookupLambda n env of
                    Just lam -> Just (lambdaToCLMLam env lam)
                    Nothing  -> Map.lookup n (clmLambdas env)
                consLookup cn = lookupTypeOfConstructor cn env
                lookupInstanceFn funcNm args =
                    let typeNames = [t | Just t <- Prelude.map (inferTypePure consLookup) args]
                    in if Prelude.null typeNames then Nothing
                       else tryInstanceLookup funcNm typeNames
                tryInstance funcNm tns =
                    case lookupCLMInstance funcNm tns env of
                        Just clm -> Just clm
                        Nothing  -> case lookupInstanceLambda funcNm tns env of
                            Just lam -> Just (lambdaToCLMLam env lam)
                            Nothing  -> Nothing
                tryInstanceLookup funcNm typeNames =
                    case tryInstance funcNm typeNames of
                        Just clm -> Just clm
                        Nothing ->
                            case lookupCLMInstancePrefix funcNm typeNames env of
                                Just clm -> Just clm
                                Nothing ->
                                    let tryKeys [] = Nothing
                                        tryKeys (t:ts) = case tryInstance funcNm [t] of
                                            Just clm -> Just clm
                                            Nothing  -> tryKeys ts
                                    in tryKeys (Data.List.nub typeNames)
                result = evalCLMPure lookupFn lookupInstanceFn 0 (CLMAPP (CLMID name) argCLMs)
            in case clmToExpr result of
                Just ty -> ty
                Nothing -> App (Id name) argTys
        | otherwise = App (Id name) argTys

-- | Convert a Surface Lambda to CLMLam (pure, on-the-fly).
lambdaToCLMLam :: Environment -> Lambda -> CLMLam
lambdaToCLMLam env (Lambda _ ps Intrinsic _ _ _) = CLMLam [] CLMPRIMCALL
lambdaToCLMLam env (Lambda _ ps Derive _ _ _) = CLMLam [] CLMPRIMCALL
lambdaToCLMLam env (Lambda _ ps (PatternMatches exs) _ _ _) =
    CLMLamCases (varsToCLMVarsTC env ps) (Prelude.map (exprToCLMTC env) exs)
lambdaToCLMLam env (Lambda _ ps bdy _ _ _) =
    CLMLam (varsToCLMVarsTC env ps) (exprToCLMTC env bdy)

-- | Minimal Expr → CLMExpr conversion for type-level normalization.
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
exprToCLMTC _ (U l) = CLMU l
exprToCLMTC env (RecFieldAccess ac e) = CLMFieldAccess ac (exprToCLMTC env e)
exprToCLMTC _ Intrinsic = CLMPRIMCALL
exprToCLMTC _ (Pi _ _ _) = CLMEMPTY
exprToCLMTC _ _ = CLMEMPTY

pcToCLM :: Environment -> Expr -> CLMPatternCheck
pcToCLM env (PatternGuard (PCheckTag ct) ex) = CLMCheckTag ct (exprToCLMTC env ex)
pcToCLM env (PatternGuard (PCheckLit lit) ex) = CLMCheckLit lit (exprToCLMTC env ex)
pcToCLM _ _ = CLMCheckTag (ConsTag "ERROR" (-1)) CLMEMPTY

varsToCLMVarsTC :: Environment -> [Var] -> [(Name, CLMExpr)]
varsToCLMVarsTC env = Prelude.map (\v -> (name v, exprToCLMTC env (val v)))

-- | Convert CLMExpr → Expr (for reading back evaluation results).
clmToExpr :: CLMExpr -> Maybe Expr
clmToExpr (CLMCON (ConsTag name _) [])   = Just (Id name)
clmToExpr (CLMCON (ConsTag name _) args) = App (Id name) <$> mapM clmToExpr args
clmToExpr (CLMID "__Level")              = Just (Id "Level")
clmToExpr (CLMID name)                   = Just (Id name)
clmToExpr (CLMU l)                       = Just (U l)
clmToExpr (CLMLIT l)                     = Just (Lit l)
clmToExpr (CLMAPP f args)               = App <$> clmToExpr f <*> mapM clmToExpr args
clmToExpr (CLMIAP f args)               = App <$> clmToExpr f <*> mapM clmToExpr args
clmToExpr (CLMFieldAccess (nm, idx) e)  = RecFieldAccess (nm, idx) <$> clmToExpr e
clmToExpr CLMEMPTY                       = Nothing
clmToExpr _                              = Nothing

-- | Is an Expr fully concrete (no unification metavariables)?
-- Uses typeChildren for structural recursion (Phase 1).
isConcreteTy :: Expr -> Bool
isConcreteTy (Meta _) = False
isConcreteTy e = Prelude.all isConcreteTy (typeChildren e)

-- ============================================================================
-- Bidirectional Type Checking
-- ============================================================================

-- | Try an action, returning Nothing on failure
tcTry :: TC a -> TC (Maybe a)
tcTry action env st = case action env st of
  Left _        -> Right (Nothing, st)
  Right (a, st') -> Right (Just a, st')

-- | Try an action, returning Left errors on failure (preserves error info)
tcTryE :: TC a -> TC (Either [TCError] a)
tcTryE action env st = case action env st of
  Left errs      -> Right (Left errs, st)
  Right (a, st') -> Right (Right a, st')

-- | Lookup a variable in the local TCEnv, then in the compiler Environment
lookupVarType :: Name -> TC (Maybe Expr)
lookupVarType name =
  tcAsk `tcBind` \env ->
    case Map.lookup name (varTypes env) of
      Just ty -> tcPure (Just ty)
      Nothing -> case envCompiler env of
        Nothing -> tcPure Nothing
        Just cenv -> lookupFromCompilerEnv name cenv

-- | Look up a name's type from the compiler environment
lookupFromCompilerEnv :: Name -> Environment -> TC (Maybe Expr)
lookupFromCompilerEnv name cenv =
  case lookupConstructor name cenv of
    Just (lam, _tag) -> inferLamType lam `tcBind` \ty ->
      -- Constructors of parameterized types use rigid type names (Id "a", Id "b")
      -- from the type definition. Instantiate these with fresh metas so each use
      -- gets its own copy (standard Hindley-Milner polymorphism).
      instantiateFreeTypeVars cenv ty `tcBind` \ty' -> tcPure (Just ty')
    Nothing ->
      case lookupLambda name cenv of
        Just lam ->
          inferLamType lam `tcBind` \ty ->
            (if hasImplicit lam
             then emitStructConstraint name lam
             else tcPure ()) `tcBind` \_ ->
              -- Instantiate free type variables (standard HM polymorphism).
              -- After instantiate strips Pi-bound type vars, remaining lowercase
              -- Ids (like 'a', 'b' in structure method sigs) need fresh metas.
              instantiate ty `tcBind` \ty' ->
                instantiateFreeTypeVars cenv ty' `tcBind` \ty'' ->
                  tcPure (Just ty'')
        Nothing ->
          case lookupType name cenv of
            Just _ -> tcPure (Just (U (LConst 0)))
            Nothing -> tcPure Nothing

-- | Emit a structure constraint for an implicit-param function
emitStructConstraint :: Name -> Lambda -> TC ()
emitStructConstraint funcName lam =
  case params lam of
    (Var _ (Implicit implTy) _ : _) ->
      case implTy of
        App (Id structName) typeArgs ->
          tcMapM normalizeTypeExpr typeArgs `tcBind` \tyArgs ->
            tcModify (\st -> st { constraints = CStructure structName tyArgs : constraints st })
        Id structName ->
          tcModify (\st -> st { constraints = CStructure structName [] : constraints st })
        _ -> tcPure ()
    _ -> tcPure ()

-- | Convert a Lambda's signature to a type (params → return type).
-- For implicit-param lambdas, wraps in Pi (Just n) (U (LConst 0)) for type params.
-- Preserves parameter names as Pi (Just name) for dependent types.
inferLamType :: Lambda -> TC Expr
inferLamType lam =
  let (implVars, ps) = let (imps, rest) = span isImplicitVar (params lam)
                        in (Prelude.map name imps, rest)
      retExpr = lamType lam
  in tcMapM (\(Var nm tp _) -> normalizeTypeExpr tp `tcBind` \ty -> tcPure (nm, ty)) ps `tcBind` \namedParams ->
       normalizeTypeExpr retExpr `tcBind` \retTy ->
         let piTy = Prelude.foldr (\(nm, ty) acc ->
               if nm == "" || nm == "_"
               then Pi Nothing ty acc
               else Pi (Just nm) ty acc) retTy namedParams
         in tcPure (Prelude.foldr (\n t -> Pi (Just n) (U (LConst 0)) t) piTy implVars)

-- | Extract type variable names from implicit params
extractImplicitTypeVars :: Lambda -> [Name]
extractImplicitTypeVars lam =
  Prelude.map name (takeWhile isImplicitVar (params lam))

-- | Infer the universe level of a type expression.
-- Returns the Level such that the type lives in U(level).
-- For non-type expressions or unknowns, defaults to LConst 0.
inferUniverse :: Expr -> TC Level
inferUniverse expr =
  infer expr `tcBind` \ty ->
    applySubst ty `tcBind` \ty' ->
      case ty' of
        U l -> tcPure l
        _   -> tcPure (LConst 0)  -- default: assume Type-level

-- | Resolve field type from constructor definitions for parameterized types.
-- Given a RecFieldAccess on a type like Pair(Int, Bool), looks up the constructor
-- fields and substitutes type arguments to produce the concrete field type.
resolveConstructorField :: Name -> Int -> Expr -> TC Expr
resolveConstructorField fieldName idx eTy =
  withCompilerEnv freshMeta (\cenv ->
    let typeName = extractTypeName eTy
        typeArgs = extractTypeArgs eTy
    in case Map.lookup typeName (types cenv) of
      Just (SumType parentLam) ->
        let typeParams = Prelude.map Surface.name (params parentLam)
            paramMapping = Prelude.zip typeParams typeArgs
        in case body parentLam of
          Constructors constrs ->
            resolveFieldFromConstrs fieldName idx paramMapping constrs
          _ -> freshMeta  -- non-sum type body, field access not applicable
      _ -> freshMeta)  -- type not found in env
  where
    extractTypeArgs (App _ args) = args
    extractTypeArgs _ = []

-- | Search constructors for a field by name or index, then substitute type params.
resolveFieldFromConstrs :: Name -> Int -> [(Name, Expr)] -> [Lambda] -> TC Expr
resolveFieldFromConstrs fieldName idx paramMapping constrs =
  case findField fieldName idx constrs of
    Just fieldTy ->
      let substituted = Prelude.foldl (\t (n, arg) -> substTyVar n arg t) fieldTy paramMapping
      in normalizeTypeExpr substituted
    Nothing -> freshMeta

-- | Find a field type in constructors by name (if non-empty) or by index.
findField :: Name -> Int -> [Lambda] -> Maybe Expr
findField fieldName idx constrs
  -- Named field access: search all constructors for matching field name
  | fieldName /= "" =
    let allFields = Prelude.concatMap (\c -> params c) constrs
    in case Prelude.filter (\v -> Surface.name v == fieldName) allFields of
         (v:_) -> Just (typ v)
         []    -> Nothing
  -- Positional field access: only resolve when all constructors that have this
  -- index agree on the field type. Otherwise it's ambiguous (e.g., Leaf.0:a vs
  -- Branch.0:Tree(a)) and we return Nothing to let the TC use freshMeta.
  | otherwise =
    let candidates = Prelude.filter (\c -> idx < Prelude.length (params c)) constrs
        fieldTypes = Prelude.map (\c -> typ (params c !! idx)) candidates
    in case fieldTypes of
      []    -> Nothing
      (t:ts) | Prelude.all (== t) ts -> Just t  -- All agree
      _     -> Nothing  -- Ambiguous — different types at same index

-- | Synthesize/infer the type of an expression
infer :: Expr -> TC Expr
-- Literals
infer (Lit (LInt _))    = tcPure (Id "Int")
infer (Lit (LFloat _))  = tcPure (Id "Float64")
infer (Lit (LString _)) = tcPure (Id "String")
infer (Lit (LChar _))   = tcPure (Id "Char")
infer (Lit (LInt8 _))   = tcPure (Id "Int8")
infer (Lit (LInt16 _))  = tcPure (Id "Int16")
infer (Lit (LInt32 _))  = tcPure (Id "Int32")
infer (Lit (LInt64 _))  = tcPure (Id "Int64")
infer (Lit (LWord8 _))  = tcPure (Id "UInt8")
infer (Lit (LWord16 _)) = tcPure (Id "UInt16")
infer (Lit (LWord32 _)) = tcPure (Id "UInt32")
infer (Lit (LWord64 _)) = tcPure (Id "UInt64")
infer (Lit (LFloat32 _)) = tcPure (Id "Float32")
infer (Lit (LList []))  = freshMeta `tcBind` \a -> tcPure (App (Id "List") [a])
infer (Lit (LList (x:_))) = infer x `tcBind` \ty -> tcPure (App (Id "List") [ty])

-- Variables / Identifiers
infer (Id name) =
  lookupVarType name `tcBind` \mty ->
    case mty of
      Just ty -> instantiate ty
      Nothing -> tcWarnOrFail (UnboundVar name) `tcBind` \_ -> freshMeta

-- Typed expression: e : T
infer (Typed e tExpr) =
  normalizeTypeExpr tExpr `tcBind` \ty ->
    check e ty `tcBind` \_ ->
      tcPure ty

-- Class constructor: ClassName.new(args)
infer (App (RecFieldAccess ("new", _) (Id className)) args) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> tcMapM infer args `tcBind` \_ -> tcPure (Id className)
      Just cenv -> case lookupClass className cenv of
        Nothing -> tcMapM infer args `tcBind` \_ -> tcPure (Id className)
        Just cm
          | cmModifier cm == ClassAbstract ->
            tcWarnOrFail (OtherError $ "Cannot instantiate abstract class " ++ className)
              `tcBind` \_ -> tcPure (Id className)
          | otherwise ->
            let expected = Prelude.length (cmAllFields cm)
                actual = Prelude.length args
            in (if expected /= actual
                then tcWarnOrFail (ArityMismatch expected actual)
                else tcPure ()) `tcBind` \_ ->
              tcMapM infer args `tcBind` \_ ->
                tcPure (Id className)

-- Function application: f(args)
infer (App f args) =
  infer f `tcBind` \fTy ->
    applySubst fTy `tcBind` \fTy' ->
      inferApp fTy' args

-- Constructor tuple: Tag(args) — with GADT support
infer (ConTuple (ConsTag name _tag) args) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> freshMeta
      Just cenv -> case lookupConstructor name cenv of
        Nothing -> freshMeta
        Just (lam, _) ->
          let ps = params lam
          in if Prelude.length ps /= Prelude.length args
             then tcWarnOrFail (ArityMismatch (Prelude.length ps) (Prelude.length args)) `tcBind` \_ -> freshMeta
             else
               let parentName = extractTypeName (lamType lam)
                   mParentLam = case Map.lookup parentName (types cenv) of
                                  Just (SumType pl) -> Just pl
                                  _ -> Nothing
                   typeParams = case mParentLam of
                                  Just pl -> Prelude.map Surface.name (params pl)
                                  Nothing -> []
               in if Prelude.null typeParams
                  then
                    tcMapM (\(Var _ tp _, arg) -> normalizeTypeExpr tp `tcBind` \pty -> infer arg `tcBind` \aty ->
                      tcTry (unify pty aty) `tcBind` \result ->
                        case result of
                          Just _  -> tcPure ()
                          Nothing -> tcWarnOrFail (Mismatch pty aty))
                      (Prelude.zip ps args) `tcBind` \_ ->
                    normalizeTypeExpr (lamType lam)
                  else
                    tcMapM (\_ -> freshMeta) typeParams `tcBind` \freshVars ->
                    let mapping = Prelude.zip typeParams freshVars
                    in tcMapM (\(Var _ tp _, arg) ->
                         gadtExprToTy mapping tp `tcBind` \pty ->
                           infer arg `tcBind` \aty ->
                             tcTry (unify pty aty) `tcBind` \result ->
                               case result of
                                 Just _  -> tcPure ()
                                 Nothing -> tcWarnOrFail (Mismatch pty aty))
                         (Prelude.zip ps args) `tcBind` \_ ->
                       gadtExprToTy mapping (lamType lam)

-- NTuple: positional → Sigma chain, named → RowExtend chain
infer (NTuple fields)
  | hasNamedFields fields =
    tcMapM (\(mn, e) -> infer e `tcBind` \ty -> tcPure (case mn of Just n -> n; Nothing -> "", ty)) fields `tcBind` \typedFields ->
      tcPure (Prelude.foldr (\(n,t) r -> RowExtend n t r) RowEmpty typedFields)
  | otherwise =
    tcMapM (\(_, e) -> infer e) fields `tcBind` \tys ->
      case tys of
        []  -> tcPure (Id "Unit")
        [t] -> tcPure t
        _   -> tcPure (Prelude.foldr1 (\a b -> Sigma Nothing a b) tys)

-- Function definition (Phase 5: set source info)
infer (Function lam) =
  tcLocal (\env -> env { tcSourceInfo = Just (lamSrcInfo lam) }) (inferLambda lam)

-- Pattern matches
infer (PatternMatches cases) =
  case cases of
    [] -> freshMeta
    (c:_) -> infer c

-- CaseOf: bind pattern variables before inferring body
infer (CaseOf pats bodyExpr _si) =
  let bindings = [(Surface.name v, typ v) | v <- pats, Surface.name v /= "", Surface.name v /= "_"]
  in tcMapM (\(n, tpExpr) -> normalizeTypeExpr tpExpr `tcBind` \ty -> tcPure (n, ty)) bindings `tcBind` \typedBindings ->
       let extendEnv env = env { varTypes = Prelude.foldl (\m (n,t) -> Map.insert n t m) (varTypes env) typedBindings }
       in tcLocal extendEnv (infer bodyExpr)

-- ExpandedCase: apply GADT refinements (Phase 5: set source info)
infer (ExpandedCase checks bodyExpr si) =
  tcLocal (\env -> env { tcSourceInfo = Just si }) $
    gadtRefine checks `tcBind` \refinements ->
      if Prelude.null refinements
      then infer bodyExpr
      else tcLocal (applyGADTRefinements refinements) (infer bodyExpr)

-- Statements
infer (Statements stmts) =
  case stmts of
    [] -> tcPure (Id "Unit")
    _  -> infer (Prelude.last stmts)

-- Unary/Binary ops
infer (UnaryOp name e) = infer (App (Id name) [e])
infer (BinaryOp name e1 e2) = infer (App (Id name) [e1, e2])

-- Universe
infer (U l) = tcPure (U (levelSucc l))

-- Sum types
infer (SumType lam) =
  if Prelude.null (params lam)
  then tcPure (U (LConst 0))
  else
    tcMapM (\(Var _ tp _) -> normalizeTypeExpr tp) (params lam) `tcBind` \paramKinds ->
    let kind = Prelude.foldr (\k acc -> Pi Nothing k acc) (U (LConst 0)) paramKinds
    in tcPure kind

-- Structure definitions
infer (Structure _ _) = tcPure (U (LConst 0))

-- Primitives
infer (Primitive _) = tcPure (U (LConst 0))

-- Instance, Intrinsic, etc.
infer (Instance _ _ _ _) = tcPure (Id "Unit")
infer Intrinsic = freshMeta
infer Derive = freshMeta
infer UNDEFINED = freshMeta

-- Pi type expression: universe is max of domain and codomain universes
infer (Pi mn domTy codTy) =
  inferUniverse domTy `tcBind` \domLevel ->
    inferUniverse codTy `tcBind` \codLevel ->
      tcPure (U (levelMax domLevel codLevel))

-- If-then-else
infer (IfThenElse cond thenE elseE) =
  check cond (Id "Bool") `tcBind` \_ ->
    infer thenE `tcBind` \tTy ->
      check elseE tTy `tcBind` \_ ->
        tcPure tTy

-- Let-in
infer (LetIn binds bodyExpr) = inferLetBindsAndBody binds bodyExpr

-- PropEq, Implies, Law
infer (PropEq _ _) = tcPure (U (LConst 0))
infer (Implies _ _) = tcPure (U (LConst 0))
infer (Law _ _) = tcPure (Id "Unit")

-- Repr, ReprCast
infer (Repr _ _ _ _ _) = tcPure (Id "Unit")
infer (ReprCast e tp) =
  normalizeTypeExpr tp `tcBind` \targetTy ->
    tcAsk `tcBind` \env ->
      case envCompiler env of
        Just cenv | isClassType targetTy cenv -> tcPure (App (Id "Maybe") [targetTy])
        _ -> tcPure targetTy
  where
    isClassType (Id n) cenv = Map.member n (classDecls cenv)
    isClassType _ _ = False
infer (Value v _) = normalizeTypeExpr (typ v)

-- Record type expression: universe is max of all field type universes
infer (RecordType fields _isOpen) =
  if Prelude.null fields
  then tcPure (U (LConst 0))
  else
    tcMapM (\(_, fTy) -> inferUniverse fTy) fields `tcBind` \levels ->
      tcPure (U (Prelude.foldl1 levelMax levels))

-- Field access
infer (RecFieldAccess (fieldName, idx) e) =
  infer e `tcBind` \eTy ->
    applySubst eTy `tcBind` \eTy' ->
      case eTy' of
        RowExtend _ _ _ ->
          tcTry (rowExtract fieldName eTy') `tcBind` \result ->
            case result of
              Just (ty, _) -> tcPure ty
              Nothing -> resolveConstructorField fieldName idx eTy'
        _ -> resolveConstructorField fieldName idx eTy'

-- Array literal
infer (ArrayLit []) = freshMeta `tcBind` \a -> tcPure (App (Id "Array") [a])
infer (ArrayLit (x:_)) = infer x `tcBind` \ty -> tcPure (App (Id "Array") [ty])

-- Module system nodes
infer (ModuleDecl _) = tcPure (Id "Unit")
infer (Import _ _ _) = tcPure (Id "Unit")
infer (Open _) = tcPure (Id "Unit")
infer (Export _ _) = tcPure (Id "Unit")
infer (PrivateDecl e) = infer e
infer (OpaqueTy _ _) = tcPure (U (LConst 0))
infer (TargetBlock _ _) = tcPure (Id "Unit")
infer (TargetSwitch _) = tcPure (Id "Unit")

-- Effect system nodes
infer (EffectDecl _ _ _) = tcPure (Id "Unit")
infer (HandlerDecl _ _ _ _) = tcPure (Id "Unit")
infer (HandleWith computation _handler) =
  infer computation `tcBind` \compTy ->
    applySubst compTy `tcBind` \compTy' ->
      case compTy' of
        EffType _row resTy -> tcPure resTy
        _ -> tcPure compTy'
infer (ActionBlock stmts) =
  case stmts of
    [] -> tcPure (Id "Unit")
    _ -> inferActionStmt (Prelude.last stmts)
infer (EffType rowExpr resExpr) = tcPure (U (LConst 0))

-- Implicit, Binding, Constructors, etc.
infer (Implicit e) = infer e
infer (Binding v) = normalizeTypeExpr (typ v)
infer (Constructors _) = tcPure (U (LConst 0))
infer (ERROR msg) = tcWarnOrFail (OtherError $ "ERROR node: " ++ msg) `tcBind` \_ -> freshMeta

-- Catch-all
infer e =
  tcWarnOrFail (OtherError $ "Cannot infer type: " ++ showExprBrief e)
    `tcBind` \_ -> freshMeta

-- | Apply a function type to arguments
inferApp :: Expr -> [Expr] -> TC Expr
inferApp fTy [] = tcPure fTy
-- Dependent Pi: substitute the bound variable in return type
inferApp (Pi (Just name) paramTy retTy) (arg:rest) =
  infer arg `tcBind` \argTy ->
    tcWithContext "argument of dependent application" (unify argTy paramTy) `tcBind` \_ ->
      withCompilerEnv (tcPure (substTyVar name argTy retTy)) (\env ->
        let retTy' = normalizeTy env (substTyVar name argTy retTy)
        in tcPure retTy') `tcBind` \retTy'' ->
          applySubst retTy'' `tcBind` \retTy''' ->
             inferApp retTy''' rest
-- Non-dependent arrow
inferApp (Pi Nothing paramTy retTy) (arg:rest) =
  tcWithContext "argument of application" (check arg paramTy) `tcBind` \_ ->
    applySubst retTy `tcBind` \retTy' ->
      inferApp retTy' rest
-- Unknown function type: create fresh vars
inferApp (Meta v) args =
  tcMapM (\arg -> infer arg) args `tcBind` \argTys ->
    freshMeta `tcBind` \retTy ->
      let fnTy = Prelude.foldr (\a b -> Pi Nothing a b) retTy argTys
      in bind v fnTy `tcBind` \_ -> tcPure retTy
-- Not an arrow type
inferApp ty args =
  tcMapM infer args `tcBind` \_ -> freshMeta

-- | Infer type of a Lambda definition
inferLambda :: Lambda -> TC Expr
inferLambda lam =
  let ps = case params lam of
        (Var _ (Implicit _) _):rest -> rest
        p -> p
      selfName = lamName lam
      ctx = if selfName /= "" then "function '" ++ selfName ++ "'" else "anonymous lambda"
  in tcWithContext ctx (
    tcMapM (\(Var n tp _) ->
       normalizeTypeExpr tp `tcBind` \ty -> tcPure (n, ty)) ps `tcBind` \paramBindings ->
     normalizeTypeExpr (lamType lam) `tcBind` \retTy ->
     -- Self-reference: when return type is known, use the declared signature so
     -- recursive calls type-check correctly. When unknown, use a fresh meta.
     -- Note: only add self-reference when the name is NOT in the compiler environment,
     -- since global lookup provides the correct polymorphic type (with fresh metas)
     -- and avoids issues where instance methods shadow global dispatch
     -- (e.g., show(val:a) inside Show(Maybe(a)) should dispatch globally, not self-recurse).
     let paramTys0 = Prelude.map snd paramBindings
         paramNames = Prelude.map fst paramBindings
     in tcAsk `tcBind` \tcEnv ->
     let nameInGlobalEnv = case envCompiler tcEnv of
           Nothing -> False
           Just cenv -> case lookupLambda selfName cenv of
             Just _ -> True
             Nothing -> case lookupConstructor selfName cenv of
               Just _ -> True
               Nothing -> False
     in (case retTy of
           Meta _ -> freshMeta
           _      -> tcPure (Prelude.foldr (\a b -> Pi Nothing a b) retTy paramTys0)
        ) `tcBind` \selfTy ->
     let extendEnv env = env { varTypes =
           (if selfName /= "" && selfName `Prelude.notElem` paramNames && not nameInGlobalEnv
            then Map.insert selfName selfTy else id) $
           Prelude.foldl (\m (n,t) -> Map.insert n t m) (varTypes env) paramBindings }
     in tcLocal extendEnv (
            case retTy of
              Meta _ -> infer (body lam)
              _      -> check (body lam) retTy `tcBind` \_ -> tcPure retTy
          ) `tcBind` \bodyTy ->
            let paramTys = Prelude.map snd paramBindings
                funcTy = Prelude.foldr (\a b -> Pi Nothing a b) bodyTy paramTys
            in tcPure funcTy
    )

-- | Collect all free Id names that look like type variables from a type expression.
-- Descends into Pi, App, Sigma, etc. Respects Pi/Sigma binders (but for simplicity
-- we just collect all Ids — the caller filters by known types).
collectFreeTypeVarIds :: Expr -> Set.Set Name
collectFreeTypeVarIds (Id n) = Set.singleton n
collectFreeTypeVarIds (App f args) = collectFreeTypeVarIds f <> foldMap collectFreeTypeVarIds args
collectFreeTypeVarIds (Pi _ a b) = collectFreeTypeVarIds a <> collectFreeTypeVarIds b
collectFreeTypeVarIds (Sigma _ a b) = collectFreeTypeVarIds a <> collectFreeTypeVarIds b
collectFreeTypeVarIds (RowExtend _ t r) = collectFreeTypeVarIds t <> collectFreeTypeVarIds r
collectFreeTypeVarIds _ = Set.empty

-- | Check if a name looks like a type variable (lowercase start, short).
isLowercaseTypeVar :: Name -> Bool
isLowercaseTypeVar [] = False
isLowercaseTypeVar (c:_) = c >= 'a' && c <= 'z'

-- | Check if a name is a known type in the environment (types map, constructors, or built-in primitives).
isKnownTypeName :: Name -> TCEnv -> Bool
isKnownTypeName n env =
  case envCompiler env of
    Nothing -> False
    Just cenv -> Map.member n (types cenv)
             || Map.member n (constructors cenv)
             || Map.member n (topLambdas cenv)

-- | Infer the type of an action statement
inferActionStmt :: ActionStmt -> TC Expr
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

-- | Infer let bindings with body
inferLetBindsAndBody :: [(Var, Expr)] -> Expr -> TC Expr
inferLetBindsAndBody [] bodyExpr = infer bodyExpr
inferLetBindsAndBody ((v, e):rest) bodyExpr =
  infer e `tcBind` \ty ->
    tcLocal (\env -> env { varTypes = Map.insert (name v) ty (varTypes env) }) (
      inferLetBindsAndBody rest bodyExpr
    )

-- | Check that an expression has the expected type
check :: Expr -> Expr -> TC ()

-- Pattern matches
check (PatternMatches cases) expectedTy =
  tcMapM_ (\c -> check c expectedTy) cases

-- CaseOf
check (CaseOf pats bodyExpr si) expectedTy =
  let bindings = [(name v, tp) | v@(Var n tp val) <- pats, n /= ""]
  in tcMapM (\(n, tpExpr) -> normalizeTypeExpr tpExpr `tcBind` \ty -> tcPure (n, ty)) bindings `tcBind` \typedBindings ->
       let extendEnv env = env { varTypes = Prelude.foldl (\m (n,t) -> Map.insert n t m) (varTypes env) typedBindings }
       in tcLocal extendEnv (check bodyExpr expectedTy)

-- ExpandedCase: apply GADT refinements
check (ExpandedCase checks bodyExpr _si) expectedTy =
  gadtRefine checks `tcBind` \refinements ->
    if Prelude.null refinements
    then check bodyExpr expectedTy
    else
      let refinedExpected = Prelude.foldl (\t (n, r) -> substTyVar n r t) expectedTy refinements
      in tcLocal (applyGADTRefinements refinements) (check bodyExpr refinedExpected)

-- Subsumption
check expr expectedTy =
  infer expr `tcBind` \inferredTy ->
    tcTry (unify inferredTy expectedTy) `tcBind` \result ->
      case result of
        Just _  -> tcPure ()
        Nothing ->
          tcTry (subtype inferredTy expectedTy) `tcBind` \subResult ->
            case subResult of
              Just _  -> tcPure ()
              Nothing -> tcWarnOrFail (Mismatch expectedTy inferredTy)

-- ============================================================================
-- Pipeline Integration
-- ============================================================================

-- | Type check pass — inserted between case optimization and CLM conversion.
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
        tl = topLambdas env
        il = instanceLambdas env
        -- Use post-optimization lambdas from topLambdas/instanceLambdas when
        -- available. Case optimization (Pass 2) transforms CaseOf patterns
        -- into ExpandedCase/PatternGuard forms in topLambdas but does NOT
        -- update parsedModule. Using pre-optimization CaseOf causes false
        -- "unbound variable" warnings because pattern-bound variables (from
        -- val fields) aren't in the TC's scope.
        useOptimized (Function lam, si) =
            case Map.lookup (lamName lam) tl of
                Just optLam -> (Function optLam, si)
                Nothing     -> (Function lam, si)
        useOptimized (Instance sn targs impls reqs, si) =
            let typeNames = Prelude.map exprToName targs
                optImpl (Function implLam) =
                    let key = lamName implLam ++ "\0" ++ intercalate "\0" typeNames
                    in case Map.lookup key il of
                        Just optLam -> Function optLam
                        Nothing     -> Function implLam
                optImpl other = other
            in (Instance sn targs (Prelude.map optImpl impls) reqs, si)
        useOptimized other = other
        exprToName (Id n) = n
        exprToName (App (Id n) _) = n
        exprToName e = showExprBrief e
        md' = Prelude.map useOptimized md
    _ <- foldM (\accSt (expr, srcInfo) ->
        case runTC (checkTopLevel expr) tcEnv accSt of
            Left errs -> do
                let errMsgs = Prelude.map showTCError errs
                forM_ errs $ \err ->
                    logFn (mkTCLogPayload srcInfo err)
                -- Count and collect errors
                modify (\s -> s { tcErrorCount = tcErrorCount s + Prelude.length errs
                                , tcCollectedErrors = tcCollectedErrors s ++ errMsgs })
                return accSt
            Right (_, tcSt') -> do
                let newErrors = tcErrors tcSt'
                let errMsgs = Prelude.map showTCError newErrors
                forM_ newErrors $ \err ->
                    logFn (mkTCLogPayload srcInfo err)
                -- Count and collect errors
                unless (Prelude.null newErrors) $
                    modify (\s -> s { tcErrorCount = tcErrorCount s + Prelude.length newErrors
                                    , tcCollectedErrors = tcCollectedErrors s ++ errMsgs })
                return (tcSt' { tcErrors = [] })
      ) tcSt md'
    pure ()

-- | Build a TCEnv from the compiler's Environment
buildTCEnvFromEnvironment :: Environment -> TCEnv
buildTCEnvFromEnvironment env = emptyTCEnv { envCompiler = Just env }

-- | Resolve accumulated structure constraints.
-- Phase 3: fixpoint loop — resolving constraint A may solve a meta that enables constraint B.
-- Loops up to 10 iterations until no more constraints are resolved.
resolveConstraints :: TC ()
resolveConstraints = resolveLoop 10
  where
    resolveLoop 0 = warnRemaining
    resolveLoop n =
      tcGet `tcBind` \st ->
        let before = Prelude.length (constraints st)
        in if before == 0 then tcPure ()
           else resolveOnce `tcBind` \_ ->
             tcGet `tcBind` \st' ->
               let after = Prelude.length (constraints st')
               in if after < before then resolveLoop (n - 1) else warnRemaining

    warnRemaining =
      tcGet `tcBind` \st ->
        tcAsk `tcBind` \env ->
          tcMapM_ (warnOne env) (constraints st) `tcBind` \_ ->
            tcModify (\s -> s { constraints = [] })

    warnOne env (CStructure structName tyArgs) =
      tcMapM applySubst tyArgs `tcBind` \resolvedArgs ->
        let typeNames = Prelude.map tyToName resolvedArgs
        in if Prelude.any (== "") typeNames then tcPure ()
           else tcWarnOrFail (ConstraintUnsolved (CStructure structName resolvedArgs))
    warnOne _ (CRowLack _ _) = tcPure ()

    resolveOnce =
      tcGet `tcBind` \st ->
        tcAsk `tcBind` \env ->
          let cs = constraints st
          in tcModify (\s -> s { constraints = [] }) `tcBind` \_ ->
             tcMapM_ (resolveOne env) cs

    resolveOne env (CStructure structName tyArgs) =
      tcMapM applySubst tyArgs `tcBind` \resolvedArgs ->
        let typeNames = Prelude.map tyToName resolvedArgs
        in if Prelude.any (== "") typeNames
           then tcModify (\s -> s { constraints = CStructure structName tyArgs : constraints s })
           else case envCompiler env of
             Nothing -> tcPure ()
             Just cenv ->
               case lookupType structName cenv of
                 Just (Structure structLam _) -> case body structLam of
                   DeclBlock exs ->
                     let funcNames = [lamName l | Function l <- exs]
                     in if Prelude.any (\fn -> instanceExists cenv fn typeNames) funcNames
                        then tcPure ()
                        else tcModify (\s -> s { constraints = CStructure structName resolvedArgs : constraints s })
                   _ -> tcPure ()
                 _ -> tcPure ()
    resolveOne _ (CRowLack n e) =
      tcModify (\s -> s { constraints = CRowLack n e : constraints s })

    instanceExists cenv funcName typeNames =
      case lookupInstanceLambda funcName typeNames cenv of
        Just _  -> True
        Nothing ->
          Prelude.any (\tn ->
            let parents = getAllParents tn cenv
            in Prelude.any (\p -> case lookupInstanceLambda funcName [p] cenv of
                                    Just _ -> True
                                    Nothing -> False) parents
          ) typeNames

-- | Convert a resolved Expr to a type name (for instance lookup)
tyToName :: Expr -> Name
tyToName (Id n) = n
tyToName (App (Id n) _) = n
tyToName _ = ""

-- | Check a top-level declaration
checkTopLevel :: Expr -> TC ()
checkTopLevel (Function lam) =
  inferLambda lam `tcBind` \_ -> resolveConstraints

checkTopLevel (Instance structName targs impls _reqs) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> inferImpls impls
      Just cenv ->
        tcWithContext ("instance " ++ structName ++ "(" ++ intercalate "," (Prelude.map showExprBrief targs) ++ ")") $
          case lookupType structName cenv of
            Just (Structure structLam _) ->
              let sigFuncs = [l | Function l <- case body structLam of DeclBlock exs -> exs; _ -> []]
                  typeVars = Prelude.map Surface.name (Prelude.filter isImplicitVar (params structLam))
              in tcMapM_ (checkInstanceImpl cenv typeVars targs sigFuncs) impls
                 `tcBind` \_ -> resolveConstraints
            _ -> inferImpls impls  -- fallback for non-structure targets
  where
    inferImpls is = tcMapM_ (\impl -> infer impl `tcBind` \_ -> tcPure ()) is

    isImplicitVar (Var _ (Implicit _) _) = True
    isImplicitVar (Var _ tp _) = isUniverseLike tp

    isUniverseLike (U _) = True
    isUniverseLike (Id "Type") = True
    isUniverseLike (Id "Type1") = True
    isUniverseLike (Id "Type2") = True
    isUniverseLike _ = False

    checkInstanceImpl cenv typeVars targs' sigFuncs impl = case impl of
      Function implLam ->
        case [sf | sf <- sigFuncs, lamName sf == lamName implLam] of
          (sigLam:_) -> checkImplAgainstSig typeVars targs' implLam sigLam
          [] -> infer impl `tcBind` \_ -> tcPure ()  -- no matching sig, just infer
      _ -> infer impl `tcBind` \_ -> tcPure ()  -- Intrinsic, Derive, etc.

    checkImplAgainstSig typeVars' targs' implLam sigLam =
      -- Instantiate sig with concrete type args
      let paramMapping = Prelude.zip typeVars' targs'
          substSigTy ty = Prelude.foldl (\t (n, arg) -> substTyVar n arg t) ty paramMapping
          -- Only check return types when they are concrete (no remaining type vars)
          sigRetTy = substSigTy (lamType sigLam)
          hasTypeVars = hasUnsubstitutedVars typeVars' sigRetTy
      in if hasTypeVars
         -- When the algebra has type variables that aren't in targs (e.g. Functor has 'a','b'),
         -- just infer the implementation without checking against the signature
         then inferLambda implLam `tcBind` \_ -> tcPure ()
         else inferLambda implLam `tcBind` \implTy ->
           normalizeTypeExpr sigRetTy `tcBind` \expectedRetTy ->
             let implRetTy = stripPiReturn implTy
             in tcTry (unify implRetTy expectedRetTy) `tcBind` \result ->
               case result of
                 Just _  -> tcPure ()
                 Nothing -> tcWarnOrFail (OtherError $
                   "Instance method '" ++ lamName implLam ++ "' return type mismatch: expected "
                   ++ showTy expectedRetTy ++ " but got " ++ showTy implRetTy)

    stripPiReturn (Pi _ _ b) = stripPiReturn b
    stripPiReturn t = t

    -- Check if a type expression still contains unsubstituted type variables
    hasUnsubstitutedVars :: [Name] -> Expr -> Bool
    hasUnsubstitutedVars allTypeVars expr =
      let freeIds = collectFreeIds expr
      in Prelude.any (`Prelude.elem` freeIds) allTypeVars
       || Prelude.any isLowercaseFreeVar freeIds

    collectFreeIds :: Expr -> [Name]
    collectFreeIds (Id n) = [n]
    collectFreeIds (App e args) = collectFreeIds e ++ Prelude.concatMap collectFreeIds args
    collectFreeIds (Pi _ a b) = collectFreeIds a ++ collectFreeIds b
    collectFreeIds _ = []

    isLowercaseFreeVar :: Name -> Bool
    isLowercaseFreeVar [] = False
    isLowercaseFreeVar (c:_) = c >= 'a' && c <= 'z'

checkTopLevel (EffectDecl _ _ _) = tcPure ()
checkTopLevel (HandlerDecl _ _ _ _) = tcPure ()

checkTopLevel (SumType lam) =
  tcWithContext ("type " ++ lamName lam) $
    case body lam of
      Constructors constrs ->
        tcMapM_ (\cLam ->
          tcMapM_ (\(Var _ tp _) ->
            tcTry (normalizeTypeExpr tp) `tcBind` \_ -> tcPure ()
          ) (params cLam)
        ) constrs
      _ -> tcPure ()

checkTopLevel (Structure lam _si) =
  tcWithContext ("structure " ++ lamName lam) $
    case body lam of
      DeclBlock exs ->
        tcMapM_ (\e -> case e of
          Function methodLam ->
            tcTry (inferLamType methodLam) `tcBind` \_ -> tcPure ()
          _ -> tcPure ()
        ) exs
      _ -> tcPure ()
checkTopLevel (Primitive _) = tcPure ()
checkTopLevel (Repr _ _ _ _ _) = tcPure ()
checkTopLevel Intrinsic = tcPure ()
checkTopLevel Derive = tcPure ()

-- Type check a class declaration
checkTopLevel (ClassDecl lam cinfo) =
  tcAsk `tcBind` \env ->
    case envCompiler env of
      Nothing -> tcPure ()
      Just cenv ->
        let className = lamName lam
        in tcWithContext ("class " ++ className) (
        tcMapM_ (\(Var _ tp _) ->
          tcTry (normalizeTypeExpr tp) `tcBind` \_ -> tcPure ()
          ) (params lam) `tcBind` \_ ->
        let methods = case body lam of
              DeclBlock exs -> [l | Function l <- exs]
              _ -> []
            selfTy = Id className
        in tcMapM_ (\methodLam ->
             let methodEnv envT = envT { varTypes =
                   Map.insert "self" selfTy (varTypes envT) }
             in tcLocal methodEnv (
                  tcWithContext ("method '" ++ lamName methodLam ++ "'") (
                    inferLambda methodLam `tcBind` \_ -> tcPure ()
                  ))
             ) methods `tcBind` \_ ->
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
                         inferLamType child `tcBind` \childTy ->
                           inferLamType parent `tcBind` \parentTy ->
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

-- Fixity declarations are non-semantic, no type checking needed
checkTopLevel (FixityDecl _ _ _) = tcPure ()

-- Module system nodes: no type checking needed
checkTopLevel (ModuleDecl _) = tcPure ()
checkTopLevel (Import _ _ _) = tcPure ()
checkTopLevel (Open _) = tcPure ()
checkTopLevel (Export _ _) = tcPure ()
checkTopLevel (PrivateDecl e) = checkTopLevel e

checkTopLevel expr = infer expr `tcBind` \_ -> resolveConstraints

-- ============================================================================
-- Pretty Printing
-- ============================================================================

-- | Convert a TCError to a LogPayload for reporting.
-- Phase 5: extracts source info from WithSource wrapper when available.
mkTCLogPayload :: SourceInfo -> TCError -> LogPayload
mkTCLogPayload si err =
  let (si', err') = extractSourceInfo si err
  in mkLogPayload si' (showTCError err')
  where
    extractSourceInfo _defSi (WithSource si' inner) = (si', inner)
    extractSourceInfo defSi other = (defSi, other)

-- | Pretty-print a type checker error.
-- Phase 4: uses zonkRemaining to show resolved types instead of raw metas.
showTCError :: TCError -> String
showTCError (Mismatch t1 t2) =
    "[TC] Type mismatch: expected " ++ showTyZonked t1 ++ " but got " ++ showTyZonked t2
    ++ mismatchHint t1 t2
showTCError (OccursCheck v t) = "[TC] Infinite type: ?" ++ show v ++ " occurs in " ++ showTyZonked t
    ++ "\n    Hint: This usually means a recursive type needs an explicit annotation"
showTCError (UnboundVar n) = "[TC] Unbound variable: " ++ n
    ++ "\n    Hint: Check spelling or add a type annotation. Is '" ++ n ++ "' defined and in scope?"
showTCError (MissingField n) = "[TC] Missing record field: " ++ n
showTCError (ArityMismatch e a) = "[TC] Arity mismatch: expected " ++ show e ++ " arguments, got " ++ show a
showTCError (ConstraintUnsolved c) = "[TC] Unsolved constraint: " ++ showConstraint c ++ " — no matching instance found"
    ++ "\n    Hint: Add an instance declaration or a 'requires " ++ showConstraint c ++ "' clause"
showTCError (OtherError s) = "[TC] " ++ s
showTCError (WithContext ctx inner) = showTCError inner ++ "\n    in " ++ ctx
showTCError (WithSource si inner) = showSourceLoc si ++ showTCError inner
showTCError (SubtypeMismatch t1 t2) = "[TC] Subtype mismatch: " ++ showTyZonked t1 ++ " is not a subtype of " ++ showTyZonked t2

-- | Generate a hint for type mismatches when patterns are recognizable
mismatchHint :: Expr -> Expr -> String
mismatchHint _ _ = ""

-- | Show a type with remaining metas replaced by readable names (Phase 4).
-- Assigns sequential letter names (_a, _b, _c, ...) instead of raw meta IDs.
showTyZonked :: Expr -> String
showTyZonked expr =
    let metas = collectMetas expr
        -- Assign unique letter names: _a, _b, _c, ...
        metaNames = Prelude.zip (nub metas) (Prelude.map metaLetter [0..])
        metaLetter i
            | i < 26   = '_' : [toEnum (fromEnum 'a' + i)]
            | otherwise = "_t" ++ show i
        nameMap = Map.fromList [(m, n) | (m, n) <- metaNames]
        zonkReadable (Meta n) = case Map.lookup n nameMap of
            Just nm -> Id nm
            Nothing -> Id ("_?" ++ show n)
        zonkReadable e = mapTypeChildren zonkReadable e
    in showTy (zonkReadable expr)
  where
    collectMetas (Meta n) = [n]
    collectMetas e = concatMap collectMetas (typeChildren e)

-- | Format source location for error messages (Phase 5).
showSourceLoc :: SourceInfo -> String
showSourceLoc (SourceInfo l c f _) = f ++ ":" ++ show l ++ ":" ++ show c ++ ": "
showSourceLoc SourceInteractive = ""

showConstraint :: Constraint -> String
showConstraint (CStructure n tys) = n ++ "(" ++ intercalate ", " (Prelude.map showTy tys) ++ ")"
showConstraint (CRowLack n _) = "row lacks " ++ n

-- | Pretty-print a type (Expr used as type)
showTy :: Expr -> String
showTy (Meta v) = "?" ++ show v
showTy (Id n) = n
showTy (App (Id "PropEq") [t, a, b]) = "PropEq(" ++ showTy t ++ ", " ++ showTy a ++ ", " ++ showTy b ++ ")"
showTy (App f args) = showTy f ++ "(" ++ intercalate ", " (Prelude.map showTy args) ++ ")"
showTy (Pi (Just n) (U (LConst 0)) t) = "forall " ++ n ++ ". " ++ showTy t
showTy (Pi Nothing a b) = showTyArg a ++ " -> " ++ showTy b
showTy (Pi (Just n) a b) = "(" ++ n ++ ":" ++ showTy a ++ ") -> " ++ showTy b
showTy (Sigma Nothing a b) = "(" ++ showTy a ++ ", " ++ showTy b ++ ")"
showTy (Sigma (Just n) a b) = "(" ++ n ++ ":" ++ showTy a ++ " * " ++ showTy b ++ ")"
showTy (EffType r t) = "Eff {" ++ showRow r ++ "} " ++ showTy t
showTy (U (LConst 0)) = "Type"
showTy (U (LConst n)) = "Type" ++ show n
showTy (U l) = "U(" ++ showLevel l ++ ")"
showTy (Lit l) = show l
showTy e@(RowExtend _ _ _) = "{" ++ showRow e ++ "}"
showTy RowEmpty = "{}"
showTy _ = "?"

-- | Pretty-print a row
showRow :: Expr -> String
showRow RowEmpty = ""
showRow (RowExtend l t RowEmpty) = l ++ ":" ++ showTy t
showRow (RowExtend l t r) = l ++ ":" ++ showTy t ++ ", " ++ showRow r
showRow (Meta v) = ".." ++ "?" ++ show v
showRow (Id n) = ".." ++ n
showRow _ = "?"

showTyArg :: Expr -> String
showTyArg t@(Pi Nothing _ _) = "(" ++ showTy t ++ ")"
showTyArg t = showTy t

-- | Depth-limited show for expressions (for error messages).
-- Avoids generating the full string for deeply nested expressions.
showExprBrief :: Expr -> String
showExprBrief = go 3
  where
    go 0 _ = "..."
    go d (Id n) = n
    go d (Meta n) = "?" ++ show n
    go d (U (LConst 0)) = "Type"
    go d (U (LConst n)) = "Type" ++ show n
    go d (U l) = "Type(" ++ showLevel l ++ ")"
    go d (App f args) = go (d-1) f ++ "(" ++ intercalate ", " (Prelude.map (go (d-1)) args) ++ ")"
    go d (Pi Nothing a b) = go (d-1) a ++ " -> " ++ go (d-1) b
    go d (Pi (Just n) a b) = "(" ++ n ++ ":" ++ go (d-1) a ++ ") -> " ++ go (d-1) b
    go d (Sigma Nothing a b) = go (d-1) a ++ " * " ++ go (d-1) b
    go d (Sigma (Just n) a b) = "(" ++ n ++ ":" ++ go (d-1) a ++ ") * " ++ go (d-1) b
    go d (RowExtend n t r) = "{" ++ n ++ ":" ++ go (d-1) t ++ " | " ++ go (d-1) r ++ "}"
    go _ RowEmpty = "{}"
    go d (NTuple fields) = "{" ++ intercalate ", " (Prelude.map (goField (d-1)) fields) ++ "}"
    go d (Lit l) = show l
    go d (Function lam) = "function " ++ lamName lam ++ "(...)"
    go _ UNDEFINED = "_"
    go _ Intrinsic = "intrinsic"
    go _ Derive = "derive"
    go d (RecordType fs _) = "{" ++ intercalate ", " [n ++ ":" ++ go (d-1) t | (n,t) <- fs] ++ "}"
    go d (EffType r t) = "Eff " ++ go (d-1) r ++ " " ++ go (d-1) t
    go d (Implicit e) = "Implicit(" ++ go (d-1) e ++ ")"
    go d (ConTuple (ConsTag n _) args) = n ++ "(" ++ intercalate ", " (Prelude.map (go (d-1)) args) ++ ")"
    go d (ArrayLit es) = "[" ++ intercalate ", " (Prelude.map (go (d-1)) es) ++ "]"
    go _ e = let s = show e in if Prelude.length s > 60 then Prelude.take 57 s ++ "..." else s
    goField d (Nothing, e) = go d e
    goField d (Just n, e) = n ++ " = " ++ go d e

-- | Find a lambda by name in a list
findLam :: Name -> [Lambda] -> Maybe Lambda
findLam n lams = case [l | l <- lams, lamName l == n] of { (l:_) -> Just l; [] -> Nothing }
