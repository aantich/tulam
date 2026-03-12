{-# LANGUAGE OverloadedStrings #-}
-- | Pre-CLM monomorphization: resolve implicit-param dispatch at the Surface AST level.
--
-- This pass runs BEFORE CLM conversion (between Pass 3 and Pass 4).
-- At the Surface level, we have full type information: Lambda params carry
-- concrete types, the type checker has run, and we can directly substitute
-- target instance bodies.
--
-- After this pass, no implicit-param function calls remain in the AST for
-- functions that have target instance implementations. exprToCLM then generates
-- only CLMAPP (direct calls), never CLMIAP (dispatch).
--
-- Design principle: monomorphize where types are available (Surface AST),
-- not where they've been erased (CLM).
module Monomorphize
    ( monomorphizeForTarget
    , monomorphizeLambdas
    , resolveImplicitCalls
    , inferExprTypeName
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (intercalate, foldl')
-- import qualified Debug.Trace as DT

import Surface
import State

-- | Type environment: maps variable names to their concrete type names.
type TypeEnv = HashMap Name Name

-- | Monomorphize all top-level and instance lambdas for a given target.
-- Replaces implicit-param calls (like (+)(x,y)) with target instance bodies
-- (like __add_i64(x,y)) wherever concrete types can be determined.
-- Also resolves effect handler operations (putStrLn, readRef, etc.) to their
-- target handler bodies.
--
-- This modifies topLambdas and instanceLambdas in the Environment.
monomorphizeForTarget :: Name -> Environment -> Environment
monomorphizeForTarget targetName env =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
        -- Resolve in topLambdas
        topLams' = Map.map (resolveInLambda env tInstances handlerOps) (topLambdas env)
        -- Resolve in instanceLambdas
        instLams' = Map.map (resolveInLambda env tInstances handlerOps) (instanceLambdas env)
    in env { topLambdas = topLams', instanceLambdas = instLams' }

-- | Monomorphize specific sets of lambdas, using the full environment for lookups.
-- This is for reachability-driven compilation: we only transform the reachable
-- functions, but need the full env to find dispatch templates (like +, show, ==).
monomorphizeLambdas :: Name                  -- ^ Target name
                    -> Environment           -- ^ Full environment (for lookups)
                    -> HashMap Name Lambda   -- ^ Top-level lambdas to transform
                    -> HashMap Name Lambda   -- ^ Instance lambdas to transform
                    -> (HashMap Name Lambda, HashMap Name Lambda)
monomorphizeLambdas targetName env topLams instLams =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
    in ( Map.map (resolveInLambda env tInstances handlerOps) topLams
       , Map.map (resolveInLambda env tInstances handlerOps) instLams
       )

-- | Build a map from effect operation name → handler Lambda body.
-- Extracts function implementations from all target handlers.
-- E.g., "putStrLn" → Lambda with body `action { __print_string(s); __print_newline() }`
buildHandlerOpsMap :: Name -> Environment -> HashMap Name Lambda
buildHandlerOpsMap targetName env =
    case Map.lookup targetName (targetHandlers env) of
        Nothing -> Map.empty
        Just handlers ->
            Map.fromList
                [ (lamName lam, lam)
                | (_handlerName, (_effectName, _params, impls)) <- Map.toList handlers
                , Function lam <- impls
                ]

-- | Resolve implicit-param calls within a single Lambda.
-- Builds a TypeEnv from the Lambda's parameters, then walks the body.
resolveInLambda :: Environment -> NameMap Lambda -> HashMap Name Lambda -> Lambda -> Lambda
resolveInLambda env tInstances handlerOps lam =
    let -- Build type env from params: name → concrete type name
        typeEnv = buildTypeEnv (params lam)
        -- Resolve the body
        body' = resolveImplicitCalls env tInstances handlerOps typeEnv (body lam)
    in lam { body = body' }

-- | Build a type environment from function parameters.
-- Extracts concrete type names from Var types.
buildTypeEnv :: [Var] -> TypeEnv
buildTypeEnv = Map.fromList . mapMaybe extractVarType
  where
    extractVarType (Var n (Implicit _) _) = Nothing  -- type param, not a value
    extractVarType (Var n t _) = case exprToTypeName t of
        Just tn -> Just (n, tn)
        Nothing -> Nothing

-- | Extract a concrete type name from a Surface type expression.
exprToTypeName :: Expr -> Maybe Name
exprToTypeName (Id n)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n  -- capitalized = type name
exprToTypeName (App (Id n) _)
    | not (null n) && head n >= 'A' && head n <= 'Z' = Just n  -- parameterized type
exprToTypeName _ = Nothing

-- | Walk a Surface expression, resolving implicit-param function calls
-- to their target instance bodies where possible.
-- Also resolves effect handler operations (putStrLn, readRef, etc.).
resolveImplicitCalls :: Environment -> NameMap Lambda -> HashMap Name Lambda -> TypeEnv -> Expr -> Expr
resolveImplicitCalls env tInstances handlerOps typeEnv = go
  where
    go expr = case expr of
        -- The key case: function application where funcName has implicit params
        App (Id funcName) args ->
            let isImplicit = case lookupLambda funcName env of
                    Just fun | hasImplicit fun -> True
                    _ -> False
                -- Also try resolution for intrinsic functions with target instances
                -- (like ++ which is an intrinsic but not an algebra method in topLambdas)
                hasTargetInstance = hasAnyTargetInstance funcName tInstances
            in if isImplicit || hasTargetInstance
                then
                    let args' = map go args  -- resolve nested calls first
                    in  -- Try handler operation first (putStrLn, readRef, etc.)
                        case Map.lookup funcName handlerOps of
                            Just handlerLam ->
                                -- Recursively resolve the substituted body
                                go (substituteInstanceBody handlerLam args')
                            Nothing ->
                                -- Try target instance (algebra methods)
                                let argTypes = map (inferExprTypeName env typeEnv) args'
                                in case sequence argTypes of
                                    Just types ->
                                        case resolveTargetInstanceMulti tInstances funcName types of
                                            Just resolvedBody ->
                                                go (substituteInstanceBody resolvedBody args')
                                            Nothing ->
                                                App (Id funcName) args'
                                    Nothing ->
                                        -- Partial type dispatch: try with just the known types.
                                        -- Handles cases like `v == 1` where v's type is unknown
                                        -- but 1's type (Int) suffices for dispatch (algebra methods
                                        -- have same type for all params).
                                        let knownTypes = nub' (catMaybes argTypes)
                                        in if null knownTypes
                                            then App (Id funcName) args'
                                            else case resolveTargetInstanceMulti tInstances funcName knownTypes of
                                                Just resolvedBody ->
                                                    go (substituteInstanceBody resolvedBody args')
                                                Nothing ->
                                                    App (Id funcName) args'
                else
                    -- Not an implicit-param function and no target instance — just recurse
                    App (Id funcName) (map go args)

        -- Nullary implicit-param reference (e.g., `zero`, `one`, `empty`, `readLine`)
        Id funcName ->
            case lookupLambda funcName env of
                Just fun | hasImplicit fun ->
                    -- Try handler operation (nullary like readLine())
                    case Map.lookup funcName handlerOps of
                        Just handlerLam -> substituteInstanceBody handlerLam []
                        Nothing -> expr  -- can't resolve without type context
                _ -> expr

        -- Desugared let-in: App (Function λ(x). body) [val]
        -- Infer param types from the actual arguments to extend typeEnv in the body.
        App (Function lam) args ->
            let args' = map go args
                -- Build type env from explicit param annotations
                paramTypeEnv = buildTypeEnv (params lam)
                -- Also infer param types from the actual args (for untyped let-bindings)
                inferredParamTypes = Map.fromList
                    [ (name v, tn)
                    | (v, arg) <- zip (params lam) args'
                    , Nothing <- [exprToTypeName (typ v)]  -- only when param has no annotation
                    , Just tn <- [inferExprTypeName env typeEnv arg]
                    ]
                innerTypeEnv = Map.unions [inferredParamTypes, paramTypeEnv, typeEnv]
                body' = resolveImplicitCalls env tInstances handlerOps innerTypeEnv (body lam)
                lam' = lam { body = body' }
            in App (Function lam') args'

        -- Recurse into all other expression forms
        App f args -> App (go f) (map go args)
        Function lam ->
            let innerTypeEnv = Map.union (buildTypeEnv (params lam)) typeEnv
            in Function $ lam { body = resolveImplicitCalls env tInstances handlerOps innerTypeEnv (body lam) }
        LetIn binds bodyExpr ->
            -- Extend type env with let bindings where possible.
            -- First resolve RHS expressions, then infer types from resolved forms.
            let binds' = map (\(v, ex) -> (v, go ex)) binds
                inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds'
                letTypeEnv = Map.union inferredEnv typeEnv
                bodyExpr' = resolveImplicitCalls env tInstances handlerOps letTypeEnv bodyExpr
            in LetIn binds' bodyExpr'
        Typed e t -> Typed (go e) t
        ExpandedCase checks bodyExpr si ->
            ExpandedCase (map goPatternCheck checks) (go bodyExpr) si
        PatternMatches cases ->
            PatternMatches (map go cases)
        CaseOf cv ex si ->
            CaseOf (map (\v -> v { val = go (val v) }) cv) (go ex) si
        ConTuple ct args -> ConTuple ct (map go args)
        NTuple fields -> NTuple (map (\(mn, e) -> (mn, go e)) fields)
        DeclBlock exprs -> DeclBlock (map go exprs)
        ActionBlock stmts -> ActionBlock (map goActionStmt stmts)
        IfThenElse c t e -> IfThenElse (go c) (go t) (go e)
        BinaryOp op l r -> BinaryOp op (go l) (go r)
        RecFieldAccess ac e -> RecFieldAccess ac (go e)
        ReprCast e t -> ReprCast (go e) t
        ArrayLit es -> ArrayLit (map go es)
        Instance sn ta impls reqs -> Instance sn ta (map go impls) reqs
        _ -> expr  -- Lit, Id (non-implicit), U, UNDEFINED, Intrinsic, etc.

    goPatternCheck (PatternGuard pc e) = PatternGuard pc (go e)
    goPatternCheck other = other

    goActionStmt (ActionExpr e) = ActionExpr (go e)
    goActionStmt (ActionBind n e) = ActionBind n (go e)
    goActionStmt (ActionLet n e) = ActionLet n (go e)

-- | Build type env from let bindings.
-- First tries explicit Var type annotations, then infers from the RHS expression.
-- Uses foldl' so later bindings can see types inferred from earlier ones.
buildTypeEnvFromLetPairs :: Environment -> TypeEnv -> [(Var, Expr)] -> TypeEnv
buildTypeEnvFromLetPairs env outerTypeEnv = foldl' addBinding Map.empty
  where
    addBinding acc (Var n t _, rhs) =
        case exprToTypeName t of
            Just tn -> Map.insert n tn acc
            Nothing ->
                -- Try to infer from RHS expression using accumulated + outer env
                let currentEnv = Map.union acc outerTypeEnv
                in case inferExprTypeName env currentEnv rhs of
                    Just tn -> Map.insert n tn acc
                    Nothing -> acc

-- | Infer the concrete type name of a Surface expression.
-- Uses the type environment (param types), literal types, and constructor info.
inferExprTypeName :: Environment -> TypeEnv -> Expr -> Maybe Name
-- Variables and constructors as Id
inferExprTypeName env typeEnv (Id n) =
    -- First try type env (for variables with known types)
    case Map.lookup n typeEnv of
        Just tn -> Just tn
        Nothing ->
            -- Try nullary constructor (True, False, Nothing, Unit, etc.)
            if not (null n) && head n >= 'A' && head n <= 'Z'
                then lookupTypeOfConstructor n env
                else Nothing
-- Literals: known types
inferExprTypeName _ _ (Lit lit) = litTypeName lit
-- Typed annotation: extract the type
inferExprTypeName _ _ (Typed _ t) = exprToTypeName t
-- Application: could be constructor, function call, or extern call
inferExprTypeName env _ (App (Id n) _) =
    -- Try constructor first
    case lookupConstructor n env of
        Just (lam, _) -> exprToTypeName (lamType lam)
        Nothing ->
            -- Try function
            case lookupLambda n env of
                Just lam -> exprToTypeName (lamType lam)
                Nothing ->
                    -- Try target extern
                    lookupExternRetType n env
-- Nullary constructor: same
inferExprTypeName env _ (ConTuple (ConsTag cname _) _) =
    lookupTypeOfConstructor cname env
-- IfThenElse: infer from then branch
inferExprTypeName env typeEnv (IfThenElse _ thenBr _) =
    inferExprTypeName env typeEnv thenBr
-- LetIn: infer from body (the result type)
inferExprTypeName env typeEnv (LetIn binds bodyExpr) =
    let inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds
        letTypeEnv = Map.union inferredEnv typeEnv
    in inferExprTypeName env letTypeEnv bodyExpr
-- ActionBlock: result type is Unit
inferExprTypeName _ _ (ActionBlock _) = Just "Unit"
-- Can't determine
inferExprTypeName _ _ _ = Nothing

-- | Look up the return type of a target extern function.
-- Searches all targets (typically just "native").
lookupExternRetType :: Name -> Environment -> Maybe Name
lookupExternRetType fname env =
    let allTargetExts = Map.elems (targetExterns env)
    in firstJust [case Map.lookup fname te of
            Just (_, retTy, _) -> exprToTypeName retTy
            Nothing -> Nothing
        | te <- allTargetExts]

-- | Look up a target instance for a function + concrete types.
-- Tries multi-param key first, then deduped, then single-param for each unique type.
-- This mirrors the interpreter's dispatch cascade.
resolveTargetInstanceMulti :: NameMap Lambda -> Name -> [Name] -> Maybe Lambda
resolveTargetInstanceMulti tInstances funcName types =
    -- 1. Try exact multi-param key: funcName\0type1\0type2
    case Map.lookup (mkInstanceKey funcName types) tInstances of
        Just lam -> Just lam
        Nothing ->
            -- 2. Try deduped: funcName\0Int (for +(Int,Int) → key is +\0Int)
            let uniqueTypes = nub' types
            in case Map.lookup (mkInstanceKey funcName uniqueTypes) tInstances of
                Just lam -> Just lam
                Nothing ->
                    -- 3. Try single-param for each unique type
                    firstJust [Map.lookup (mkInstanceKey funcName [t]) tInstances | t <- uniqueTypes]

-- | Like nub but simple (preserves order, removes duplicates)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/= x) xs)

-- | Check if there's any target instance for a function name.
-- Instance keys are "funcName\0type1\0type2" or just "funcName".
hasAnyTargetInstance :: Name -> HashMap Name a -> Bool
hasAnyTargetInstance funcName m =
    let prefix = funcName ++ "\0"
    in any (\k -> k == funcName || take (length prefix) k == prefix) (Map.keys m)

-- | Return the first Just from a list of Maybes
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest

-- | Substitute an instance Lambda's parameters with actual arguments.
-- The instance body references its own param names; we replace them with
-- the actual argument expressions.
substituteInstanceBody :: Lambda -> [Expr] -> Expr
substituteInstanceBody instLam args =
    let -- Get value params (skip implicit type params)
        valueParams = filter (not . isImplicitVar) (params instLam)
        -- Build substitution map: param name → arg expression
        subst = Map.fromList $ zip (map name valueParams) args
    in substituteNames subst (body instLam)

-- | Substitute all Id references in an expression according to a name map.
substituteNames :: HashMap Name Expr -> Expr -> Expr
substituteNames subst = go
  where
    go (Id n) = fromMaybe (Id n) (Map.lookup n subst)
    go (App f args) = App (go f) (map go args)
    go (Lit l) = Lit l
    go (Function lam) =
        -- Don't substitute into lambda params that shadow outer names
        let shadowed = Map.fromList [(name v, ()) | v <- params lam]
            subst' = Map.difference subst shadowed
        in Function $ lam { body = substituteNames subst' (body lam) }
    go (LetIn binds bodyExpr) =
        let binds' = map (\(v, ex) -> (v, go ex)) binds
            shadowed = Map.fromList [(name v, ()) | (v, _) <- binds]
            subst' = Map.difference subst shadowed
        in LetIn binds' (substituteNames subst' bodyExpr)
    go (Typed e t) = Typed (go e) t
    go (IfThenElse c t e) = IfThenElse (go c) (go t) (go e)
    go (BinaryOp op l r) = BinaryOp op (go l) (go r)
    go (ConTuple ct args) = ConTuple ct (map go args)
    go (NTuple fields) = NTuple (map (\(mn, e) -> (mn, go e)) fields)
    go (ExpandedCase checks bodyExpr si) =
        ExpandedCase (map goPC checks) (go bodyExpr) si
    go (CaseOf cv ex si) = CaseOf (map (\v -> v { val = go (val v) }) cv) (go ex) si
    go (PatternMatches cases) = PatternMatches (map go cases)
    go (RecFieldAccess ac e) = RecFieldAccess ac (go e)
    go (ActionBlock stmts) = ActionBlock (map goAS stmts)
    go (ArrayLit es) = ArrayLit (map go es)
    go e = e  -- U, UNDEFINED, Intrinsic, etc.

    goPC (PatternGuard pc e) = PatternGuard pc (go e)
    goPC other = other

    goAS (ActionExpr e) = ActionExpr (go e)
    goAS (ActionBind n e) = ActionBind n (go e)
    goAS (ActionLet n e) = ActionLet n (go e)
