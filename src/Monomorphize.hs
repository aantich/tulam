{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
    , MonomorphLevel(..)
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.List (foldl', partition, sortOn)

import Surface
import State

-- | How aggressively to resolve instance dispatch at the Surface level.
data MonomorphLevel
    = MonoTargetOnly   -- ^ Only resolve target instances (current behavior)
    | MonoFull         -- ^ Also resolve instanceLambdas (for native/bytecode)
    deriving (Show, Eq)

-- | Type environment: maps variable names to their concrete type names.
type TypeEnv = HashMap Name Name

-- | Monomorphize all top-level and instance lambdas for a given target.
-- Replaces implicit-param calls (like (+)(x,y)) with target instance bodies
-- (like __add_i64(x,y)) wherever concrete types can be determined.
-- Also resolves effect handler operations (putStrLn, readRef, etc.) to their
-- target handler bodies.
--
-- This modifies topLambdas and instanceLambdas in the Environment.
monomorphizeForTarget :: MonomorphLevel -> Name -> Environment -> Environment
monomorphizeForTarget monoLevel targetName env =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
        -- Resolve in topLambdas
        topLams' = Map.map (resolveInLambda monoLevel env tInstances handlerOps) (topLambdas env)
        -- Resolve in instanceLambdas
        instLams' = Map.map (resolveInLambda monoLevel env tInstances handlerOps) (instanceLambdas env)
    in env { topLambdas = topLams', instanceLambdas = instLams' }

-- | Monomorphize specific sets of lambdas, using the full environment for lookups.
-- This is for reachability-driven compilation: we only transform the reachable
-- functions, but need the full env to find dispatch templates (like +, show, ==).
monomorphizeLambdas :: MonomorphLevel         -- ^ How aggressively to resolve
                    -> Name                  -- ^ Target name
                    -> Environment           -- ^ Full environment (for lookups)
                    -> HashMap Name Lambda   -- ^ Top-level lambdas to transform
                    -> HashMap Name Lambda   -- ^ Instance lambdas to transform
                    -> (HashMap Name Lambda, HashMap Name Lambda)
monomorphizeLambdas monoLevel targetName env topLams instLams =
    let tInstances = fromMaybe Map.empty (Map.lookup targetName (targetInstances env))
        handlerOps = buildHandlerOpsMap targetName env
    in ( Map.map (resolveInLambda monoLevel env tInstances handlerOps) topLams
       , Map.map (resolveInLambda monoLevel env tInstances handlerOps) instLams
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
-- Threads the Lambda's return type as expected-type context for morphism dispatch.
resolveInLambda :: MonomorphLevel -> Environment -> NameMap Lambda -> HashMap Name Lambda -> Lambda -> Lambda
resolveInLambda monoLevel env tInstances handlerOps lam =
    let -- Build type env from params: name → concrete type name
        typeEnv = buildTypeEnv (params lam)
        -- Extract return type from the Lambda for expected-type-directed dispatch
        mRetType = exprToTypeName (lamType lam)
        -- Resolve the body with expected return type context
        body' = resolveImplicitCalls monoLevel env tInstances handlerOps typeEnv mRetType (body lam)
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
--
-- The expected return type (Maybe Name) is threaded from the containing
-- function's lamType through control structures. This enables morphism
-- dispatch where the return type is needed (e.g., Convertible(Nat, Int)
-- needs both arg type Nat and return type Int).
resolveImplicitCalls :: MonomorphLevel -> Environment -> NameMap Lambda
                     -> HashMap Name Lambda -> TypeEnv -> Maybe Name -> Expr -> Expr
resolveImplicitCalls monoLevel env tInstances handlerOps typeEnv mExpectedRet = goWith mExpectedRet
  where
    -- | Main walker, parameterized by expected return type from context.
    goWith :: Maybe Name -> Expr -> Expr
    goWith mRet expr = case expr of
        -- The key case: function application where funcName has implicit params
        App (Id funcName) args ->
            let isImplicit = case lookupLambda funcName env of
                    Just fun | hasImplicit fun -> True
                    _ -> False
                -- Also try resolution for intrinsic functions with target instances
                hasTargetInstance = hasAnyTargetInstance funcName tInstances
            in if isImplicit || hasTargetInstance
                then
                    -- For implicit-param calls, propagate type context to args.
                    -- For homogeneous algebras (+, -, etc.), all args share the same type.
                    -- Use the best known type (from outer mRet or from any arg's inferred type)
                    -- as expected return type when resolving nested implicit calls in args.
                    let argExpectedType = inferImplicitArgType funcName mRet args
                        args' = map (goWith argExpectedType) args
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
                                                -- Also try with return type appended (for morphisms)
                                                case mRet of
                                                    Just ret ->
                                                        case resolveTargetInstanceMulti tInstances funcName (types ++ [ret]) of
                                                            Just resolvedBody ->
                                                                go (substituteInstanceBody resolvedBody args')
                                                            Nothing ->
                                                                -- Fallback: try instanceLambdas when MonoFull
                                                                tryInstanceLambdaFallback funcName types mRet args'
                                                    Nothing ->
                                                        tryInstanceLambdaFallback funcName types mRet args'
                                    Nothing ->
                                        -- Partial type dispatch: try with just the known types.
                                        let knownTypes = nub' (catMaybes argTypes)
                                        in if null knownTypes
                                            then App (Id funcName) args'
                                            else case resolveTargetInstanceMulti tInstances funcName knownTypes of
                                                Just resolvedBody ->
                                                    go (substituteInstanceBody resolvedBody args')
                                                Nothing ->
                                                    tryInstanceLambdaFallback funcName knownTypes mRet args'
                else
                    -- Not an implicit-param function — infer expected types from param signatures
                    -- (e.g., nat_to_int(convert(True)): param type is Nat, so convert gets Nat)
                    let paramTypes = inferParamExpectedTypes funcName (length args)
                        args' = zipWith goWith paramTypes args
                    in App (Id funcName) args'

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
                -- The desugared let body has the same expected type as the outer context
                body' = resolveImplicitCalls monoLevel env tInstances handlerOps innerTypeEnv mRet (body lam)
                lam' = lam { body = body' }
            in App (Function lam') args'

        -- Recurse into all other expression forms, threading expected types
        App f args -> App (go f) (map go args)
        Function lam ->
            let innerTypeEnv = Map.union (buildTypeEnv (params lam)) typeEnv
                lamRetType = exprToTypeName (lamType lam)
            in Function $ lam { body = resolveImplicitCalls monoLevel env tInstances handlerOps innerTypeEnv lamRetType (body lam) }
        LetIn binds bodyExpr ->
            let binds' = map (\(v, ex) -> (v, go ex)) binds
                inferredEnv = buildTypeEnvFromLetPairs env typeEnv binds'
                letTypeEnv = Map.union inferredEnv typeEnv
                -- Let body has the same expected type as outer context
                bodyExpr' = resolveImplicitCalls monoLevel env tInstances handlerOps letTypeEnv mRet bodyExpr
            in LetIn binds' bodyExpr'
        -- Typed annotation: the type IS the expected return type for the inner expr
        Typed e t -> Typed (goWith (exprToTypeName t) e) t
        ExpandedCase checks bodyExpr si ->
            ExpandedCase (map goPatternCheck checks) (goWith mRet bodyExpr) si
        PatternMatches cases ->
            PatternMatches (map (goWith mRet) cases)
        CaseOf cv ex si ->
            let patTypes = extractPatternTypes env typeEnv cv
                innerTypeEnv = Map.union patTypes typeEnv
                goInner = resolveImplicitCalls monoLevel env tInstances handlerOps innerTypeEnv mRet
            in CaseOf (map (\v -> v { val = goInner (val v) }) cv) (goInner ex) si
        ConTuple ct args -> ConTuple ct (map go args)
        NTuple fields -> NTuple (map (\(mn, e) -> (mn, go e)) fields)
        DeclBlock exprs -> DeclBlock (map go exprs)
        ActionBlock stmts -> ActionBlock (map goActionStmt stmts)
        -- Both branches of if/else have same expected type as outer context
        IfThenElse c t e -> IfThenElse (go c) (goWith mRet t) (goWith mRet e)
        BinaryOp op l r -> BinaryOp op (go l) (go r)
        RecFieldAccess ac e -> RecFieldAccess ac (go e)
        ReprCast e t -> ReprCast (go e) t
        ArrayLit es -> ArrayLit (map go es)
        Instance sn mTag ta impls reqs -> Instance sn mTag ta (map go impls) reqs
        _ -> expr  -- Lit, Id (non-implicit), U, UNDEFINED, Intrinsic, etc.

    -- | Convenience: recurse with no expected return type (for subexpressions
    -- where the expected type is unknown, like function arguments).
    go :: Expr -> Expr
    go = goWith Nothing

    goPatternCheck (PatternGuard pc e) = PatternGuard pc (goWith mExpectedRet e)
    goPatternCheck other = other

    goActionStmt (ActionExpr e) = ActionExpr (go e)
    goActionStmt (ActionBind n e) = ActionBind n (go e)
    goActionStmt (ActionLet n e) = ActionLet n (go e)

    -- | Infer expected types for each argument position from a function's parameter signatures.
    -- For non-implicit functions like nat_to_int(n:Nat), this returns [Just "Nat"].
    -- Falls back to Nothing for unknown functions or type variables.
    inferParamExpectedTypes :: Name -> Int -> [Maybe Name]
    inferParamExpectedTypes funcName numArgs =
        case lookupLambda funcName env of
            Just fun ->
                let valueParams = filter (not . isImplicitVar) (params fun)
                    paramTys = map (exprToTypeName . typ) valueParams
                in take numArgs (paramTys ++ repeat Nothing)
            Nothing ->
                -- Try constructor (e.g., Succ(convert(n-1)) — Succ has param type Nat)
                case lookupConstructor funcName env of
                    Just (consLam, _) ->
                        let valueParams = filter (not . isImplicitVar) (params consLam)
                            paramTys = map (exprToTypeName . typ) valueParams
                        in take numArgs (paramTys ++ repeat Nothing)
                    Nothing ->
                        replicate numArgs Nothing

    -- | Infer the expected type for arguments of an implicit-param function.
    -- For homogeneous algebra functions (all value params share the same type var),
    -- the expected arg type = the expected return type or the type of any known arg.
    -- This allows nested implicit calls (like convert(m) inside 1 + convert(m))
    -- to receive the type context they need for morphism dispatch.
    inferImplicitArgType :: Name -> Maybe Name -> [Expr] -> Maybe Name
    inferImplicitArgType funcName outerRet args =
        -- First try outer expected return type (from containing function's lamType)
        case outerRet of
            Just t -> Just t
            Nothing ->
                -- Try to infer from any arg's known type
                firstJust [inferExprTypeName env typeEnv a | a <- args]

    -- | Try resolving via instanceLambdas when MonoFull and target instance not found.
    -- Rewrites the call to a direct reference to the instance function (by key),
    -- rather than inlining the body, to avoid infinite expansion for recursive instances.
    -- Uses both arg types and expected return type for morphism dispatch.
    tryInstanceLambdaFallback :: Name -> [Name] -> Maybe Name -> [Expr] -> Expr
    tryInstanceLambdaFallback funcName argTypes mRet args'
        | monoLevel /= MonoFull = App (Id funcName) args'
        | otherwise =
            -- Try with return type appended first (for morphisms like Convertible)
            let withRet = case mRet of
                    Just ret -> resolveInstanceLambdaWithKey env funcName (argTypes ++ [ret])
                    Nothing  -> Nothing
            in case withRet of
                Just (instKey, instLam) | body instLam /= Intrinsic ->
                    App (Id instKey) args'
                _ ->
                    -- Fallback: try with arg types only (for algebras, single-param)
                    case resolveInstanceLambdaWithKey env funcName argTypes of
                        Just (instKey, instLam) | body instLam /= Intrinsic ->
                            App (Id instKey) args'
                        _ -> App (Id funcName) args'

-- | Resolve an instance lambda using a cascade mirroring the runtime dispatcher.
-- Returns both the instance key and the lambda.
resolveInstanceLambdaWithKey :: Environment -> Name -> [Name] -> Maybe (Name, Lambda)
resolveInstanceLambdaWithKey env funcName types =
    let instLams = instanceLambdas env
        key1 = mkInstanceKey funcName types Nothing
        exact = Map.lookup key1 instLams
    in firstJust'
    -- 1. Exact multi-param: "convert\0Nat\0Int"
    [ fmap (key1,) exact
    -- 2. Any-tag: "combine\0Nat\0@Additive"
    , let baseKey = mkInstanceKey funcName types Nothing
          tagPrefix = baseKey ++ "\0@"
          matches = sortOn fst $ Map.toList $ Map.filterWithKey
                      (\k _ -> take (length tagPrefix) k == tagPrefix) instLams
      in case matches of
          ((k, v):_) -> Just (k, v)
          [] -> Nothing
    -- 3. Prefix (morphisms): "convert\0Nat" matches "convert\0Nat\0Int"
    , let key = mkInstanceKey funcName types Nothing
          prefix = key ++ "\0"
      in case Map.lookup key instLams of
          Just lam -> Just (key, lam)
          Nothing ->
              let matches = Map.toList $ Map.filterWithKey
                      (\k _ -> take (length prefix) k == prefix) instLams
                  (direct, composed) = partition (isDirectInstanceLam funcName . snd) matches
              in case direct of
                  ((k, v):_) -> Just (k, v)
                  [] -> case composed of
                      ((k, v):_) -> Just (k, v)
                      [] -> Nothing
    -- 4. Deduped types
    , let uniq = nub' types
      in if uniq /= types
         then let key = mkInstanceKey funcName uniq Nothing
              in fmap (key,) $ Map.lookup key instLams
         else Nothing
    -- 5. Single-param for each type
    , firstJust [ let key = mkInstanceKey funcName [t] Nothing
                  in fmap (key,) $ Map.lookup key instLams
                | t <- nub' types ]
    -- 6. Single-param any-tag
    , firstJust [ let tagPrefix = mkInstanceKey funcName [t] Nothing ++ "\0@"
                      matches = sortOn fst $ Map.toList $ Map.filterWithKey
                                  (\k _ -> take (length tagPrefix) k == tagPrefix) instLams
                  in case matches of
                      ((k, v):_) -> Just (k, v)
                      [] -> Nothing
                | t <- nub' types ]
    ]

-- | Return the first Just from a list of Maybes (non-shadowing name).
firstJust' :: [Maybe a] -> Maybe a
firstJust' [] = Nothing
firstJust' (Just x : _) = Just x
firstJust' (Nothing : rest) = firstJust' rest

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

-- | Extract concrete type names for pattern-bound variables from CaseOf vars.
-- For a CaseOf like [Var "p" (App (Id "Pair") [Id "Int", Id "Int"]) (App (Id "Pair") [Id "a", Id "b"])],
-- this extracts {a → "Int", b → "Int"} by looking up the constructor's field types
-- and substituting the concrete type arguments.
extractPatternTypes :: Environment -> TypeEnv -> [Var] -> TypeEnv
extractPatternTypes env outerTypeEnv caseVars =
    Map.fromList $ concatMap extractFromVar caseVars
  where
    extractFromVar (Var _scrutName varTypExpr pattern) =
        -- Get the concrete type expression for the scrutinee
        let concreteTypeExpr = case varTypExpr of
                UNDEFINED -> Nothing
                other     -> Just other
        in case pattern of
            App (Id consName) patArgs ->
                extractConsPattern concreteTypeExpr consName patArgs
            -- Nullary constructor or literal — no variable bindings
            _ -> []

    extractConsPattern mConcreteType consName patArgs =
        case lookupConstructor consName env of
            Just (consLam, _) ->
                let implParams = filter isImplicitParam (params consLam)
                    valParams  = filter (not . isImplicitParam) (params consLam)
                    -- Extract concrete type args from the scrutinee's type expression
                    typeArgs = case mConcreteType of
                        Just (App _ args) -> args  -- e.g., Pair(Int, Int) → [Int, Int]
                        _ -> []
                    -- Build substitution: type param name → concrete type expression
                    typeSubst = Map.fromList $ zip (map name implParams) typeArgs
                in catMaybes
                    [ case patArg of
                        Id varName | isLowerCase varName ->
                            -- Pattern variable binding — resolve its type
                            let fieldType = substTypeExpr typeSubst (typ fieldParam)
                            in case exprToTypeName fieldType of
                                Just tn -> Just (varName, tn)
                                Nothing -> Nothing
                        _ -> Nothing  -- nested pattern or literal, skip for now
                    | (patArg, fieldParam) <- zip patArgs valParams
                    ]
            Nothing -> []

    isImplicitParam (Var _ (Implicit _) _) = True
    isImplicitParam _ = False

    isLowerCase n = not (null n) && head n >= 'a' && head n <= 'z'

    -- Substitute type parameter names in a type expression
    substTypeExpr subst (Id n) = fromMaybe (Id n) (Map.lookup n subst)
    substTypeExpr subst (App f args) = App (substTypeExpr subst f) (map (substTypeExpr subst) args)
    substTypeExpr _ e = e

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
-- RecFieldAccess: infer field type from container type + constructor info
inferExprTypeName env typeEnv (RecFieldAccess (_, idx) e) =
    case inferExprTypeName env typeEnv e of
        Just typeName -> inferFieldType env typeName idx
        Nothing -> Nothing
-- Can't determine
inferExprTypeName _ _ _ = Nothing

-- | Infer the type of a field at a given index for a type.
-- Scans constructors of the type and returns the field type if unambiguous.
inferFieldType :: Environment -> Name -> Int -> Maybe Name
inferFieldType env typeName idx =
    let allCons = Map.toList (constructors env)
        -- Find constructors whose return type matches
        typeCons = [cLam | (_cName, (cLam, _)) <- allCons
                   , exprToTypeName (lamType cLam) == Just typeName]
        -- Get field type at idx from constructors that have enough fields
        fieldTypes = catMaybes
                     [exprToTypeName (typ vp)
                     | cLam <- typeCons
                     , let vps = filter (not . isImplicitVar) (params cLam)
                     , idx >= 0 && idx < length vps
                     , let vp = vps !! idx
                     ]
    in case nub' fieldTypes of
        [t] -> Just t
        _ -> Nothing

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
    case Map.lookup (mkInstanceKey funcName types Nothing) tInstances of
        Just lam -> Just lam
        Nothing ->
            -- 2. Try deduped: funcName\0Int (for +(Int,Int) → key is +\0Int)
            let uniqueTypes = nub' types
            in case Map.lookup (mkInstanceKey funcName uniqueTypes Nothing) tInstances of
                Just lam -> Just lam
                Nothing ->
                    -- 3. Try single-param for each unique type
                    firstJust [Map.lookup (mkInstanceKey funcName [t] Nothing) tInstances | t <- uniqueTypes]

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
