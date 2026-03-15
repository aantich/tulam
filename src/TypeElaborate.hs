{-# LANGUAGE OverloadedStrings #-}
-- | Pass 3.2: Type Elaboration — wraps sub-expressions with Typed annotations.
--
-- Runs after Pass 3.1 (type annotate) which fills in Lambda param/return types.
-- This pass walks every lambda body bottom-up, wrapping each sub-expression with
-- `Typed expr type` where the type can be determined from:
--   - Literal types (Int, Float64, String, Char, etc.)
--   - Variable types from the local environment (params, let-bindings, case patterns)
--   - Constructor application: infer type params from args, apply to constructor return type
--   - Function application: return type from lambda lookup
--   - Instance keys: return type from instanceLambdas lookup
--
-- Downstream passes (Monomorphize, Specialize) read types directly from Typed
-- wrappers instead of re-inferring them.
module TypeElaborate
    ( typeElaboratePass
    , elaborateExpr
    , elaborateLambda
    , typeOfExpr
    , ElabEnv
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HSet
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Control.Monad.Trans.State.Strict (get, modify)

import Surface
import State

-- | Type environment for elaboration: maps variable names to their full type expressions.
type ElabEnv = HashMap Name Expr

-- ============================================================================
-- Pipeline Integration
-- ============================================================================

-- | Pass 3.2: Elaborate all top-level and instance lambdas with Typed wrappers.
-- Writes to topLambdasElab/instanceLambdasElab (NOT raw maps).
-- TC elaboration (from typeCheckPass) takes precedence via left-biased union.
typeElaboratePass :: IntState ()
typeElaboratePass = do
    st <- get
    let env = currentEnvironment st
        -- Synthesize elaborated views from raw lambdas
        synthTop  = Map.map (elaborateLambda env) (topLambdas env)
        synthInst = Map.map (elaborateLambda env) (instanceLambdas env)
        -- TC-produced elaboration wins (left-biased union)
        env' = env
            { topLambdasElab = Map.union (topLambdasElab env) synthTop
            , instanceLambdasElab = Map.union (instanceLambdasElab env) synthInst
            }
    modify (\s -> s { currentEnvironment = env' })
    verboseLog "  Pass 3.2: type elaboration complete"

-- ============================================================================
-- Lambda-level Entry Point
-- ============================================================================

-- | Elaborate a single Lambda: build ElabEnv from params, elaborate body.
elaborateLambda :: Environment -> Lambda -> Lambda
elaborateLambda env lam =
    let elabEnv = buildElabEnv (params lam)
        body' = elaborateExpr env elabEnv (body lam)
    in lam { body = body' }

-- | Build initial ElabEnv from Lambda parameters.
-- After Pass 3.1, params have concrete types (not UNDEFINED).
buildElabEnv :: [Var] -> ElabEnv
buildElabEnv = Map.fromList . mapMaybe extract
  where
    extract (Var _ (Implicit _) _) = Nothing  -- type params, not values
    extract (Var n t _) = case t of
        UNDEFINED -> Nothing
        _         -> Just (n, t)

-- ============================================================================
-- Core Elaboration
-- ============================================================================

-- | Elaborate an expression bottom-up: elaborate children, then wrap with Typed.
-- Never fails — returns the expression unwrapped if the type can't be determined.
elaborateExpr :: Environment -> ElabEnv -> Expr -> Expr
elaborateExpr env elabEnv expr = case expr of
    -- Already typed: elaborate inner but preserve outer type
    Typed e t ->
        let e' = elaborateExpr env elabEnv e
        in Typed e' t

    -- Literals: known types
    Lit lit -> case litTypeName lit of
        Just tn -> Typed (Lit lit) (Id tn)
        Nothing -> Lit lit

    -- Variables: look up in ElabEnv
    Id n -> case Map.lookup n elabEnv of
        Just t  -> Typed (Id n) t
        Nothing ->
            -- Check if it's a constructor (capitalized)
            if isUpperName n
                then case lookupConstructor n env of
                    Just (lam, _) -> case lamType lam of
                        UNDEFINED -> Id n
                        t -> Typed (Id n) t
                    Nothing -> Id n
                else Id n

    -- Constructor application: resolve type params from arg types
    App (Id n) args | isUpperName n ->
        let args' = map (elaborateExpr env elabEnv) args
        in case lookupConstructor n env of
            Just (consLam, _) ->
                let resolvedType = resolveConstructorType env consLam args'
                in wrapTyped (App (Id n) args') resolvedType
            Nothing -> App (Id n) args'

    -- Instance key call (contains \0): look up return type from instanceLambdas
    App (Id n) args | '\0' `elem` n ->
        let args' = map (elaborateExpr env elabEnv) args
        in case Map.lookup n (instanceLambdas env) of
            Just instLam ->
                let retType = resolveReturnType env instLam args'
                in wrapTyped (App (Id n) args') retType
            Nothing -> App (Id n) args'

    -- Regular function call: look up return type
    App (Id n) args ->
        let args' = map (elaborateExpr env elabEnv) args
        in case lookupLambda n env of
            Just lam ->
                let retType = resolveReturnType env lam args'
                in wrapTyped (App (Id n) args') retType
            Nothing -> App (Id n) args'

    -- Desugared let-in: App (Function lam) [args]
    App (Function lam) args ->
        let args' = map (elaborateExpr env elabEnv) args
            -- Infer param types from elaborated args
            paramEnv = Map.fromList
                [ (name v, t)
                | (v, arg) <- zip (params lam) args'
                , Just t <- [typeOfExpr arg]
                ]
            innerEnv = Map.union paramEnv (Map.union (buildElabEnv (params lam)) elabEnv)
            body' = elaborateExpr env innerEnv (body lam)
            lam' = lam { body = body' }
        in wrapTyped (App (Function lam') args') (typeOfExpr body')

    -- General application
    App f args ->
        let f' = elaborateExpr env elabEnv f
            args' = map (elaborateExpr env elabEnv) args
        in App f' args'

    -- Function/Lambda: elaborate body with extended env
    Function lam ->
        let innerEnv = Map.union (buildElabEnv (params lam)) elabEnv
            body' = elaborateExpr env innerEnv (body lam)
            lam' = lam { body = body' }
        in Function lam'

    -- Let-in: elaborate bindings, extend env, elaborate body
    LetIn binds bodyExpr ->
        let (elabEnv', binds') = elaborateLetBinds env elabEnv binds
            body' = elaborateExpr env elabEnv' bodyExpr
        in wrapTyped (LetIn binds' body') (typeOfExpr body')

    -- ConTuple: tagged constructor
    ConTuple ct@(ConsTag cname _) args ->
        let args' = map (elaborateExpr env elabEnv) args
        in case lookupConstructor cname env of
            Just (consLam, _) ->
                let resolvedType = resolveConstructorType env consLam args'
                in wrapTyped (ConTuple ct args') resolvedType
            Nothing -> ConTuple ct args'

    -- Pattern matching: elaborate each case
    PatternMatches cases ->
        PatternMatches (map (elaborateExpr env elabEnv) cases)

    -- ExpandedCase: elaborate checks and body
    ExpandedCase checks bodyExpr si ->
        let checks' = map (elaborateCheck env elabEnv) checks
            body' = elaborateExpr env elabEnv bodyExpr
        in ExpandedCase checks' body' si

    -- CaseOf: extract pattern-bound types, extend env
    CaseOf cv bodyExpr si ->
        let patEnv = extractCasePatternTypes env elabEnv cv
            innerEnv = Map.union patEnv elabEnv
            cv' = map (\v -> v { val = elaborateExpr env innerEnv (val v) }) cv
            body' = elaborateExpr env innerEnv bodyExpr
        in wrapTyped (CaseOf cv' body' si) (typeOfExpr body')

    -- If-then-else: type of then-branch
    IfThenElse c t e ->
        let c' = elaborateExpr env elabEnv c
            t' = elaborateExpr env elabEnv t
            e' = elaborateExpr env elabEnv e
        in wrapTyped (IfThenElse c' t' e') (typeOfExpr t')

    -- Action blocks return Unit
    ActionBlock stmts ->
        let stmts' = map (elaborateActionStmt env elabEnv) stmts
        in Typed (ActionBlock stmts') (Id "Unit")

    -- Binary operators: elaborate children
    BinaryOp op l r ->
        let l' = elaborateExpr env elabEnv l
            r' = elaborateExpr env elabEnv r
        in BinaryOp op l' r'

    -- NTuple: elaborate fields
    NTuple fields ->
        NTuple (map (\(mn, e) -> (mn, elaborateExpr env elabEnv e)) fields)

    -- DeclBlock: elaborate members
    DeclBlock exprs ->
        DeclBlock (map (elaborateExpr env elabEnv) exprs)

    -- Array literals: infer element type
    ArrayLit es ->
        let es' = map (elaborateExpr env elabEnv) es
            elemType = case es' of
                (e:_) -> typeOfExpr e
                [] -> Nothing
            arrType = fmap (\et -> App (Id "Array") [et]) elemType
        in wrapTyped (ArrayLit es') arrType

    -- RecFieldAccess: elaborate inner
    RecFieldAccess ac e ->
        RecFieldAccess ac (elaborateExpr env elabEnv e)

    -- ReprCast: elaborate inner
    ReprCast e t ->
        Typed (ReprCast (elaborateExpr env elabEnv e) t) t

    -- Instance: elaborate implementations
    Instance sn mTag ta impls reqs ->
        Instance sn mTag ta (map (elaborateExpr env elabEnv) impls) reqs

    -- Everything else: pass through
    _ -> expr

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract type from a Typed wrapper.
typeOfExpr :: Expr -> Maybe Expr
typeOfExpr (Typed _ t) = Just t
typeOfExpr _ = Nothing

-- | Wrap with Typed only if type is known.
wrapTyped :: Expr -> Maybe Expr -> Expr
wrapTyped e (Just t) = Typed e t
wrapTyped e Nothing  = e

-- | Check if type expression is valid (not UNDEFINED).
validType :: Expr -> Maybe Expr
validType UNDEFINED = Nothing
validType t = Just t

-- | Check if a name starts with uppercase (constructor/type name).
isUpperName :: Name -> Bool
isUpperName [] = False
isUpperName (c:_) = c >= 'A' && c <= 'Z'

-- | Elaborate let-bindings sequentially, extending the env as we go.
elaborateLetBinds :: Environment -> ElabEnv -> [(Var, Expr)] -> (ElabEnv, [(Var, Expr)])
elaborateLetBinds env = go []
  where
    go acc elabEnv [] = (elabEnv, reverse acc)
    go acc elabEnv ((v, rhs):rest) =
        let rhs' = elaborateExpr env elabEnv rhs
            elabEnv' = case typeOfExpr rhs' of
                Just t  -> Map.insert (name v) t elabEnv
                Nothing -> case typ v of
                    UNDEFINED -> elabEnv
                    t         -> Map.insert (name v) t elabEnv
        in go ((v, rhs'):acc) elabEnv' rest

-- | Elaborate a pattern check expression.
elaborateCheck :: Environment -> ElabEnv -> Expr -> Expr
elaborateCheck env elabEnv (PatternGuard pc e) =
    PatternGuard pc (elaborateExpr env elabEnv e)
elaborateCheck _ _ other = other

-- | Elaborate action statements.
elaborateActionStmt :: Environment -> ElabEnv -> ActionStmt -> ActionStmt
elaborateActionStmt env elabEnv (ActionExpr e) = ActionExpr (elaborateExpr env elabEnv e)
elaborateActionStmt env elabEnv (ActionBind n e) = ActionBind n (elaborateExpr env elabEnv e)
elaborateActionStmt env elabEnv (ActionLet n e) = ActionLet n (elaborateExpr env elabEnv e)

-- ============================================================================
-- Constructor Type Resolution
-- ============================================================================

-- | Resolve the return type of a constructor application by matching
-- actual arg types against the constructor's parameter types.
-- E.g., Cons(1, Nil) → List(Int): match h:a against Int → {a=Int}, apply to List(a).
resolveConstructorType :: Environment -> Lambda -> [Expr] -> Maybe Expr
resolveConstructorType _env consLam args' =
    let implParams = filter isImplicitVar (params consLam)
        valParams = filter (not . isImplicitVar) (params consLam)
        typeVarNames = HSet.fromList (map name implParams)
        -- Match each (value param type, actual arg type) to extract bindings
        bindings = catMaybes
            [ case typeOfExpr arg of
                Just argTy -> unifyTypes typeVarNames (typ vp) argTy
                Nothing -> Nothing
            | (vp, arg) <- zip valParams args'
            ]
        subst = foldl (Map.unionWith const) Map.empty bindings
        resultType = applyTypeSubst subst (lamType consLam)
    in validType resultType

-- | Resolve the return type of a function/instance call.
-- If the function has implicit params, try to substitute them from arg types.
resolveReturnType :: Environment -> Lambda -> [Expr] -> Maybe Expr
resolveReturnType _env lam args' =
    let retType = lamType lam
    in case retType of
        UNDEFINED -> Nothing
        _ ->
            let implParams = filter isImplicitVar (params lam)
            in if null implParams
                then Just retType
                else
                    -- Try to resolve type variables in the return type
                    let valParams = filter (not . isImplicitVar) (params lam)
                        typeVarNames = HSet.fromList (map name implParams)
                        bindings = catMaybes
                            [ case typeOfExpr arg of
                                Just argTy -> unifyTypes typeVarNames (typ vp) argTy
                                Nothing -> Nothing
                            | (vp, arg) <- zip valParams args'
                            ]
                        subst = foldl (Map.unionWith const) Map.empty bindings
                    in Just (applyTypeSubst subst retType)

-- ============================================================================
-- Pattern Type Extraction (for CaseOf)
-- ============================================================================

-- | Extract types for pattern-bound variables from CaseOf vars.
-- Uses constructor parameter types + type arg substitution from the scrutinee type.
extractCasePatternTypes :: Environment -> ElabEnv -> [Var] -> ElabEnv
extractCasePatternTypes env _elabEnv caseVars =
    Map.fromList $ concatMap extractFromVar caseVars
  where
    extractFromVar (Var _scrutName varTypExpr pat) =
        let concreteTypeExpr = case varTypExpr of
                UNDEFINED -> Nothing
                other     -> Just other
        in case pat of
            App (Id consName) patArgs | isUpperName consName ->
                extractConsPattern concreteTypeExpr consName patArgs
            _ -> []

    extractConsPattern mConcreteType consName patArgs =
        case lookupConstructor consName env of
            Just (consLam, _) ->
                let implParams = filter isImplicitVar (params consLam)
                    valParams = filter (not . isImplicitVar) (params consLam)
                    typeArgs = case mConcreteType of
                        Just (App _ args) -> args
                        _ -> []
                    typeSubst = Map.fromList $ zip (map name implParams) typeArgs
                in catMaybes
                    [ case patArg of
                        Id varName | isLowerName varName ->
                            let fieldType = applyTypeSubst typeSubst (typ fieldParam)
                            in case fieldType of
                                UNDEFINED -> Nothing
                                _         -> Just (varName, fieldType)
                        _ -> Nothing
                    | (patArg, fieldParam) <- zip patArgs valParams
                    ]
            Nothing -> []

    isLowerName n = not (null n) && head n >= 'a' && head n <= 'z'

-- ============================================================================
-- Type Unification and Substitution (shared with Specialize)
-- ============================================================================

-- | Simple structural unification to extract type variable bindings.
-- Matches a pattern type (with type vars) against a concrete type.
unifyTypes :: HSet.HashSet Name -> Expr -> Expr -> Maybe (HashMap Name Expr)
unifyTypes typeVars (Id n) concrete
    | HSet.member n typeVars = Just (Map.singleton n concrete)
    | otherwise = case concrete of
        Id m | n == m -> Just Map.empty
        _ -> Nothing
unifyTypes typeVars (App (Id pn) pargs) (App (Id cn) cargs)
    | pn == cn && length pargs == length cargs =
        let bindings = catMaybes [unifyTypes typeVars pa ca | (pa, ca) <- zip pargs cargs]
        in Just (foldl (Map.unionWith const) Map.empty bindings)
unifyTypes _ _ _ = Nothing

-- | Apply a type substitution to a type expression.
applyTypeSubst :: HashMap Name Expr -> Expr -> Expr
applyTypeSubst subst (Id n) = fromMaybe (Id n) (Map.lookup n subst)
applyTypeSubst subst (App f args) = App (applyTypeSubst subst f) (map (applyTypeSubst subst) args)
applyTypeSubst _ e = e
