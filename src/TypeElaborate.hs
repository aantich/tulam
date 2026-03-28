{-# LANGUAGE OverloadedStrings #-}
-- | Pass 3.2: Type Elaboration — wraps sub-expressions with Typed annotations.
--
-- Runs after Pass 3.1 (type annotate) which fills in Lambda param/return types.
-- This pass walks every lambda body using bidirectional elaboration:
--
--   Bottom-up (inference): wraps each sub-expression with `Typed expr type`
--   where the type can be determined from literals, variables, constructors,
--   function return types, or instance keys.
--
--   Top-down (checking): propagates expected types from call sites downward.
--   When a function call like f(g(x)) is elaborated and f's parameter type is
--   known, that type is passed as expected context to g(x). If bottom-up
--   inference produces an unresolved type variable, the expected type is used
--   as a fallback. This enables morphism dispatch in the downstream
--   monomorphizer (e.g., convert(True) inside nat_to_int gets expected type Nat).
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
-- The lambda's declared return type is passed as expected context for the body.
elaborateLambda :: Environment -> Lambda -> Lambda
elaborateLambda env lam =
    let elabEnv = buildElabEnv (params lam)
        mExpected = case lamType lam of
            UNDEFINED -> Nothing
            t         -> concreteType t
        body' = elaborateExpr env elabEnv mExpected (body lam)
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

-- | Elaborate an expression with optional expected-type context.
-- Bottom-up: elaborate children, infer types from structure.
-- Top-down: when mExpected is Just, use it as fallback if bottom-up produces
-- an unresolved type variable or no type at all.
-- Never fails — returns the expression unwrapped if the type can't be determined.
elaborateExpr :: Environment -> ElabEnv -> Maybe Expr -> Expr -> Expr
elaborateExpr env elabEnv mExpected expr = case expr of
    -- Already typed: elaborate inner but preserve outer type
    Typed e t ->
        let e' = elaborateExpr env elabEnv Nothing e
        in Typed e' t

    -- Literals: known types (no need for expected type)
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
        let args' = elaborateArgsWithCallee env elabEnv (lookupConstructorLam n env) args
        in case lookupConstructor n env of
            Just (consLam, _) ->
                let resolvedType = resolveConstructorType env consLam args'
                in wrapTypedWithFallback mExpected (App (Id n) args') resolvedType
            Nothing -> App (Id n) args'

    -- Instance key call (contains \0): look up return type from instanceLambdas
    App (Id n) args | '\0' `elem` n ->
        let mLam = Map.lookup n (instanceLambdas env)
            args' = elaborateArgsWithCallee env elabEnv mLam args
        in case mLam of
            Just instLam ->
                let retType = resolveReturnType env instLam args'
                in wrapTypedWithFallback mExpected (App (Id n) args') retType
            Nothing -> App (Id n) args'

    -- Regular function call: look up return type
    App (Id n) args ->
        let mLam = lookupLambda n env
            args' = elaborateArgsWithCallee env elabEnv mLam args
        in case mLam of
            Just lam ->
                let retType = resolveReturnType env lam args'
                in wrapTypedWithFallback mExpected (App (Id n) args') retType
            Nothing -> App (Id n) args'

    -- Desugared let-in: App (Function lam) [args]
    App (Function lam) args ->
        let args' = map (elaborateExpr env elabEnv Nothing) args
            -- Infer param types from elaborated args
            paramEnv = Map.fromList
                [ (name v, t)
                | (v, arg) <- zip (params lam) args'
                , Just t <- [typeOfExpr arg]
                ]
            innerEnv = Map.union paramEnv (Map.union (buildElabEnv (params lam)) elabEnv)
            body' = elaborateExpr env innerEnv mExpected (body lam)
            lam' = lam { body = body' }
        in wrapTypedWithFallback mExpected (App (Function lam') args') (typeOfExpr body')

    -- General application
    App f args ->
        let f' = elaborateExpr env elabEnv Nothing f
            args' = map (elaborateExpr env elabEnv Nothing) args
        in App f' args'

    -- Function/Lambda: elaborate body with extended env, wrap with return type
    Function lam ->
        let innerEnv = Map.union (buildElabEnv (params lam)) elabEnv
            innerExpected = case lamType lam of
                UNDEFINED -> mExpected
                t         -> concreteType t
            body' = elaborateExpr env innerEnv innerExpected (body lam)
            lam' = lam { body = body' }
            -- If lambda has known return type, use it; otherwise infer from body
            retType = case lamType lam of
                UNDEFINED -> typeOfExpr body'
                t         -> Just t
        in wrapTyped (Function lam') retType

    -- Let-in: elaborate bindings, extend env, elaborate body
    LetIn binds bodyExpr ->
        let (elabEnv', binds') = elaborateLetBinds env elabEnv binds
            body' = elaborateExpr env elabEnv' mExpected bodyExpr
        in wrapTypedWithFallback mExpected (LetIn binds' body') (typeOfExpr body')

    -- ConTuple: tagged constructor
    ConTuple ct@(ConsTag cname _) args ->
        let args' = elaborateArgsWithCallee env elabEnv (lookupConstructorLam cname env) args
        in case lookupConstructor cname env of
            Just (consLam, _) ->
                let resolvedType = resolveConstructorType env consLam args'
                in wrapTyped (ConTuple ct args') resolvedType
            Nothing -> ConTuple ct args'

    -- Pattern matching: elaborate each case but do NOT wrap with Typed.
    -- PatternMatches must remain unwrapped because lambdaToCLMLambda' pattern-matches
    -- on Lambda{body = PatternMatches cases} directly. Wrapping breaks this.
    PatternMatches cases ->
        PatternMatches (map (elaborateExpr env elabEnv mExpected) cases)

    -- ExpandedCase: elaborate checks and body. Don't wrap — ExpandedCase is a
    -- structural form used by CLM converter and wrapping can interfere.
    ExpandedCase checks bodyExpr si ->
        let checks' = map (elaborateCheck env elabEnv) checks
            body' = elaborateExpr env elabEnv mExpected bodyExpr
        in ExpandedCase checks' body' si

    -- CaseOf: extract pattern-bound types, extend env
    CaseOf cv bodyExpr si ->
        let patEnv = extractCasePatternTypes env elabEnv cv
            innerEnv = Map.union patEnv elabEnv
            cv' = map (\v -> v { val = elaborateExpr env innerEnv mExpected (val v) }) cv
            body' = elaborateExpr env innerEnv mExpected bodyExpr
        in wrapTypedWithFallback mExpected (CaseOf cv' body' si) (typeOfExpr body')

    -- If-then-else: propagate expected type to both branches
    IfThenElse c t e ->
        let c' = elaborateExpr env elabEnv Nothing c
            t' = elaborateExpr env elabEnv mExpected t
            e' = elaborateExpr env elabEnv mExpected e
        in wrapTypedWithFallback mExpected (IfThenElse c' t' e') (typeOfExpr t')

    -- Action blocks return Unit
    ActionBlock stmts ->
        let stmts' = map (elaborateActionStmt env elabEnv) stmts
        in Typed (ActionBlock stmts') (Id "Unit")

    -- Binary operators: elaborate children, look up operator return type.
    -- BinaryOp is desugared to App in afterparserPass, but may appear in
    -- pre-desugar contexts. Look up the operator as a function for return type.
    BinaryOp op l r ->
        let mLam = lookupLambda op env
            (l', r') = elaborateOperands env elabEnv mLam l r
            retType = case mLam of
                Just lam -> resolveReturnType env lam [l', r']
                Nothing  -> Nothing
        in wrapTyped (BinaryOp op l' r') retType

    -- NTuple: elaborate fields, produce record type if all fields are named+typed
    NTuple fields ->
        let fields' = map (\(mn, e) -> (mn, elaborateExpr env elabEnv Nothing e)) fields
            -- If all fields have names and types, create a RecordType
            namedTypes = [ (n, t) | (Just n, e) <- fields', Just t <- [typeOfExpr e] ]
            recType = if length namedTypes == length fields' && not (null fields')
                      then Just (RecordType [(n, t) | (n, t) <- namedTypes] False)
                      else Nothing
        in wrapTyped (NTuple fields') recType

    -- DeclBlock: elaborate members
    DeclBlock exprs ->
        DeclBlock (map (elaborateExpr env elabEnv Nothing) exprs)

    -- Array literals: infer element type
    ArrayLit es ->
        let es' = map (elaborateExpr env elabEnv Nothing) es
            elemType = case es' of
                (e:_) -> typeOfExpr e
                [] -> Nothing
            arrType = fmap (\et -> App (Id "Array") [et]) elemType
        in wrapTyped (ArrayLit es') arrType

    -- RecFieldAccess: elaborate inner, try to resolve field type from class/record
    RecFieldAccess ac@(fieldName, _) e ->
        let e' = elaborateExpr env elabEnv Nothing e
            -- Try to find field type from base expression's type (class fields)
            fieldType = case typeOfExpr e' of
                Just (Id className) -> case lookupClass className env of
                    Just cm -> case [typ v | v <- cmAllFields cm, name v == fieldName] of
                        (ft:_) -> validType ft
                        []     -> Nothing
                    Nothing -> Nothing
                _ -> Nothing
        in wrapTyped (RecFieldAccess ac e') fieldType

    -- ReprCast: elaborate inner
    ReprCast e t ->
        Typed (ReprCast (elaborateExpr env elabEnv Nothing e) t) t

    -- Instance: elaborate implementations
    Instance sn mTag ta impls reqs ->
        Instance sn mTag ta (map (elaborateExpr env elabEnv Nothing) impls) reqs

    -- RecordConstruct: elaborate fields, look up constructor type
    RecordConstruct consName fields ->
        let fields' = map (\(n, e) -> (n, elaborateExpr env elabEnv Nothing e)) fields
            retType = case lookupConstructor consName env of
                Just (consLam, _) ->
                    let args' = map snd fields'
                        resolvedType = resolveConstructorType env consLam args'
                    in resolvedType
                Nothing -> Nothing
        in wrapTyped (RecordConstruct consName fields') retType

    -- RecordUpdate: elaborate base and fields, preserve base type
    RecordUpdate e fields ->
        let e' = elaborateExpr env elabEnv Nothing e
            fields' = map (\(n, ex) -> (n, elaborateExpr env elabEnv Nothing ex)) fields
        in wrapTyped (RecordUpdate e' fields') (typeOfExpr e')

    -- RecordPattern: elaborate fields
    RecordPattern nm fields ->
        RecordPattern nm (map (\(n, e) -> (n, elaborateExpr env elabEnv Nothing e)) fields)

    -- HandleWith: elaborate both, result type is the computation's type
    HandleWith c h ->
        let c' = elaborateExpr env elabEnv Nothing c
            h' = elaborateExpr env elabEnv Nothing h
        in wrapTyped (HandleWith c' h') (typeOfExpr c')

    -- UnaryOp: elaborate operand, look up operator
    UnaryOp op e ->
        let mLam = lookupLambda op env
            paramType = case mLam of
                Just lam -> getValParamType lam 0
                Nothing  -> Nothing
            e' = elaborateExpr env elabEnv (paramType >>= concreteType) e
            retType = case mLam of
                Just lam -> resolveReturnType env lam [e']
                Nothing  -> Nothing
        in wrapTyped (UnaryOp op e') retType

    -- Statements: type is the last statement's type
    Statements stmts ->
        let stmts' = map (elaborateExpr env elabEnv Nothing) stmts
            lastType = case reverse stmts' of
                (s:_) -> typeOfExpr s
                []    -> Nothing
        in wrapTyped (Statements stmts') lastType

    -- PropEq: type is a PropEq type
    PropEq e1 e2 ->
        let e1' = elaborateExpr env elabEnv Nothing e1
            e2' = elaborateExpr env elabEnv Nothing e2
        in PropEq e1' e2'

    -- Implies: elaborate both sides
    Implies e1 e2 ->
        let e1' = elaborateExpr env elabEnv Nothing e1
            e2' = elaborateExpr env elabEnv Nothing e2
        in Implies e1' e2'

    -- Everything else: pass through
    _ -> expr

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract type from a Typed wrapper, filtering out non-type expressions.
-- Phase 2.1 fix: reject Function/Lambda values, PatternMatches, etc. that
-- may have been stored as "types" — these are values, not types.
typeOfExpr :: Expr -> Maybe Expr
typeOfExpr (Typed _ t) = validType t
typeOfExpr _ = Nothing

-- | Wrap with Typed only if type is known and valid.
wrapTyped :: Expr -> Maybe Expr -> Expr
wrapTyped e (Just t) = Typed e t
wrapTyped e Nothing  = e

-- | Wrap with Typed, using expected type as fallback when the inferred type
-- is an unresolved type variable or missing.  This is the key mechanism for
-- top-down type propagation: if bottom-up inference produced (Id "b") but the
-- call site expects (Id "Nat"), use (Id "Nat").
wrapTypedWithFallback :: Maybe Expr -> Expr -> Maybe Expr -> Expr
wrapTypedWithFallback mExpected e mInferred =
    case mInferred >>= validType of
        Just t
            | isUnresolvedTypeVar t
            , Just expected <- mExpected >>= concreteType
            -> Typed e expected
            | otherwise -> Typed e t
        Nothing
            | Just expected <- mExpected >>= concreteType
            -> Typed e expected
            | otherwise -> wrapTyped e mInferred

-- | Check if a type expression is an unresolved type variable (lowercase Id).
-- These come from polymorphic function return types that couldn't be resolved
-- bottom-up (e.g., convert(x:a):b where b is unresolved).
isUnresolvedTypeVar :: Expr -> Bool
isUnresolvedTypeVar (Id (c:_)) = c >= 'a' && c <= 'z'
isUnresolvedTypeVar _ = False

-- | Filter to only concrete types — not unresolved type variables.
-- Used to avoid propagating unresolved vars as expected types.
concreteType :: Expr -> Maybe Expr
concreteType t
    | isUnresolvedTypeVar t = Nothing
    | otherwise = validType t

-- | Check if an expression is a valid type expression (not a value form).
-- Rejects UNDEFINED, Function/Lambda, PatternMatches, and other value-level forms
-- that should never appear as type annotations.
validType :: Expr -> Maybe Expr
validType UNDEFINED = Nothing
validType (Function _) = Nothing
validType (PatternMatches _) = Nothing
validType (ExpandedCase _ _ _) = Nothing
validType (CaseOf _ _ _) = Nothing
validType (ActionBlock _) = Nothing
validType (LetIn _ _) = Nothing
validType (BinaryOp _ _ _) = Nothing
validType (UnaryOp _ _) = Nothing
validType (HandleWith _ _) = Nothing
validType (Statements _) = Nothing
validType (IfThenElse _ _ _) = Nothing
validType t = Just t

-- | Check if a name starts with uppercase (constructor/type name).
isUpperName :: Name -> Bool
isUpperName [] = False
isUpperName (c:_) = c >= 'A' && c <= 'Z'

-- | Elaborate function arguments, propagating callee parameter types as
-- expected types for each argument (top-down checking context).
elaborateArgsWithCallee :: Environment -> ElabEnv -> Maybe Lambda -> [Expr] -> [Expr]
elaborateArgsWithCallee env elabEnv mLam args =
    [ elaborateExpr env elabEnv (expectedForArg i) arg
    | (i, arg) <- zip [0..] args
    ]
  where
    expectedForArg i = case mLam of
        Just lam -> getValParamType lam i >>= concreteType
        Nothing  -> Nothing

-- | Elaborate binary operator operands with callee param type context.
elaborateOperands :: Environment -> ElabEnv -> Maybe Lambda -> Expr -> Expr -> (Expr, Expr)
elaborateOperands env elabEnv mLam l r =
    let lExpected = case mLam of
            Just lam -> getValParamType lam 0 >>= concreteType
            Nothing  -> Nothing
        rExpected = case mLam of
            Just lam -> getValParamType lam 1 >>= concreteType
            Nothing  -> Nothing
    in (elaborateExpr env elabEnv lExpected l, elaborateExpr env elabEnv rExpected r)

-- | Get the type of the i-th value parameter (skipping implicit params).
getValParamType :: Lambda -> Int -> Maybe Expr
getValParamType lam i =
    let valParams = filter (not . isImplicitVar) (params lam)
    in if i < length valParams
       then case typ (valParams !! i) of
           UNDEFINED -> Nothing
           t         -> Just t
       else Nothing

-- | Look up a constructor's lambda for param type propagation.
lookupConstructorLam :: Name -> Environment -> Maybe Lambda
lookupConstructorLam n env = fmap fst (lookupConstructor n env)

-- | Elaborate let-bindings sequentially, extending the env as we go.
elaborateLetBinds :: Environment -> ElabEnv -> [(Var, Expr)] -> (ElabEnv, [(Var, Expr)])
elaborateLetBinds env = go []
  where
    go acc elabEnv [] = (elabEnv, reverse acc)
    go acc elabEnv ((v, rhs):rest) =
        let rhs' = elaborateExpr env elabEnv Nothing rhs
            elabEnv' = case typeOfExpr rhs' of
                Just t  -> Map.insert (name v) t elabEnv
                Nothing -> case typ v of
                    UNDEFINED -> elabEnv
                    t         -> Map.insert (name v) t elabEnv
        in go ((v, rhs'):acc) elabEnv' rest

-- | Elaborate a pattern check expression.
elaborateCheck :: Environment -> ElabEnv -> Expr -> Expr
elaborateCheck env elabEnv (PatternGuard pc e) =
    PatternGuard pc (elaborateExpr env elabEnv Nothing e)
elaborateCheck _ _ other = other

-- | Elaborate action statements.
elaborateActionStmt :: Environment -> ElabEnv -> ActionStmt -> ActionStmt
elaborateActionStmt env elabEnv (ActionExpr e) = ActionExpr (elaborateExpr env elabEnv Nothing e)
elaborateActionStmt env elabEnv (ActionBind n e) = ActionBind n (elaborateExpr env elabEnv Nothing e)
elaborateActionStmt env elabEnv (ActionLet n e) = ActionLet n (elaborateExpr env elabEnv Nothing e)

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
