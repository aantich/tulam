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
module Backends.LLVM.Monomorphize
    ( monomorphizeForTarget
    , monomorphizeLambdas
    , resolveImplicitCalls
    , inferExprTypeName
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intercalate)

import Surface
import State

-- | Type environment: maps variable names to their concrete type names.
type TypeEnv = HashMap Name Name

-- | Monomorphize all top-level and instance lambdas for a given target.
-- Replaces implicit-param calls (like (+)(x,y)) with target instance bodies
-- (like __add_i64(x,y)) wherever concrete types can be determined.
--
-- This modifies topLambdas and instanceLambdas in the Environment.
monomorphizeForTarget :: Name -> Environment -> Environment
monomorphizeForTarget targetName env =
    case Map.lookup targetName (targetInstances env) of
        Nothing -> env  -- no target declarations loaded, nothing to resolve
        Just tInstances ->
            let -- Resolve in topLambdas
                topLams' = Map.map (resolveInLambda env tInstances) (topLambdas env)
                -- Resolve in instanceLambdas
                instLams' = Map.map (resolveInLambda env tInstances) (instanceLambdas env)
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
    case Map.lookup targetName (targetInstances env) of
        Nothing -> (topLams, instLams)
        Just tInstances ->
            ( Map.map (resolveInLambda env tInstances) topLams
            , Map.map (resolveInLambda env tInstances) instLams
            )

-- | Resolve implicit-param calls within a single Lambda.
-- Builds a TypeEnv from the Lambda's parameters, then walks the body.
resolveInLambda :: Environment -> NameMap Lambda -> Lambda -> Lambda
resolveInLambda env tInstances lam =
    let -- Build type env from params: name → concrete type name
        typeEnv = buildTypeEnv (params lam)
        -- Resolve the body
        body' = resolveImplicitCalls env tInstances typeEnv (body lam)
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
resolveImplicitCalls :: Environment -> NameMap Lambda -> TypeEnv -> Expr -> Expr
resolveImplicitCalls env tInstances typeEnv = go
  where
    go expr = case expr of
        -- The key case: function application where funcName has implicit params
        App (Id funcName) args ->
            case lookupLambda funcName env of
                Just fun | hasImplicit fun ->
                    let args' = map go args  -- resolve nested calls first
                        argTypes = map (inferExprTypeName env typeEnv) args'
                    in case sequence argTypes of
                        Just types ->
                            -- Try to resolve: first exact multi-param key, then single-param
                            case resolveTargetInstanceMulti tInstances funcName types of
                                Just resolvedBody ->
                                    -- Substitute instance params with actual args
                                    substituteInstanceBody resolvedBody args'
                                Nothing ->
                                    -- No target instance — leave as-is for interpreter/CLMIAP
                                    App (Id funcName) args'
                        Nothing ->
                            -- Can't determine all types — leave as-is
                            App (Id funcName) args'
                _ ->
                    -- Not an implicit-param function — just recurse into args
                    App (Id funcName) (map go args)

        -- Nullary implicit-param reference (e.g., `zero`, `one`, `empty`)
        Id funcName ->
            case lookupLambda funcName env of
                Just fun | hasImplicit fun -> expr  -- can't resolve without type context
                _ -> expr

        -- Recurse into all other expression forms
        App f args -> App (go f) (map go args)
        Function lam ->
            let innerTypeEnv = Map.union (buildTypeEnv (params lam)) typeEnv
            in Function $ lam { body = resolveImplicitCalls env tInstances innerTypeEnv (body lam) }
        LetIn binds bodyExpr ->
            -- Extend type env with let bindings where possible
            let binds' = map (\(v, ex) -> (v, go ex)) binds
                letTypeEnv = Map.union (buildTypeEnvFromLetPairs binds') typeEnv
                bodyExpr' = resolveImplicitCalls env tInstances letTypeEnv bodyExpr
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

-- | Try to build type env from let bindings (infer from Var types)
buildTypeEnvFromLetPairs :: [(Var, Expr)] -> TypeEnv
buildTypeEnvFromLetPairs = Map.fromList . mapMaybe inferFromBinding
  where
    inferFromBinding (Var n t _, _) = case exprToTypeName t of
        Just tn -> Just (n, tn)
        Nothing -> Nothing

-- | Infer the concrete type name of a Surface expression.
-- Uses the type environment (param types), literal types, and constructor info.
inferExprTypeName :: Environment -> TypeEnv -> Expr -> Maybe Name
-- Variables: look up in type env
inferExprTypeName _ typeEnv (Id n) = Map.lookup n typeEnv
-- Literals: known types
inferExprTypeName _ _ (Lit lit) = litTypeName lit
-- Typed annotation: extract the type
inferExprTypeName _ _ (Typed _ t) = exprToTypeName t
-- Constructor application: look up constructor's parent type
inferExprTypeName env _ (App (Id cname) _) =
    case lookupConstructor cname env of
        Just (lam, _) -> exprToTypeName (lamType lam)
        Nothing -> Nothing
-- Nullary constructor: same
inferExprTypeName env _ (ConTuple (ConsTag cname _) _) =
    lookupTypeOfConstructor cname env
-- Function call: return type of the function
inferExprTypeName env _ (App (Id fname) _) =
    case lookupLambda fname env of
        Just lam -> exprToTypeName (lamType lam)
        Nothing -> Nothing
-- Can't determine
inferExprTypeName _ _ _ = Nothing

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
