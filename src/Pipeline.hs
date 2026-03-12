{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Pipeline
where

import State
import Surface
import CLM
import Logs
import CaseOptimization (caseOptimizationPass, checkSealedExhaustiveness)
import qualified TypeCheck

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Text as T hiding (intercalate, map)
import qualified Data.Text.Lazy as TL
import Data.List.Index
import Data.List (intercalate, partition)
import Data.Maybe (isNothing, listToMaybe)
import Data.Char (ord)
import Data.Word (Word8)
import Data.Bits ((.&.), (.|.), shiftR)

import Util.PrettyPrinting as TC
import Text.Pretty.Simple (pPrint, pShow)

import Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Wrap a pass with wall-clock timing when verbose is on
timedPass :: String -> IntState () -> IntState ()
timedPass name action = do
    v <- verbose . currentFlags <$> get
    if v
    then do
        t0 <- liftIO getCurrentTime
        action
        t1 <- liftIO getCurrentTime
        liftIO $ putStrLn $ "  [" ++ name ++ "] " ++ show (diffUTCTime t1 t0)
    else action

--------------------------------------------------------------------------------
-- PASS 0: initial desugaring and environment building after parsing -
-- combined two passes into 1 for better performance
--------------------------------------------------------------------------------
-- this needs to be run right after parsing:
-- desugaring (changing op calls to App calls etc)
-- fixing remaining tags for Tuples in data constructors (what wasn't fixed during parsing)
-- reversing the order
afterparserPass :: IntState ()
afterparserPass = do
    s <- get
    let mod = (runExprPassAndReverse (traverseExpr afterparse) (parsedModule s))
    put (s { parsedModule = mod } )
    verboseLog $ "  Pass 0: desugared " ++ show (Prelude.length mod) ++ " expressions"
    return ()

-- the only reason we need this is because parser reverses the order of the program while parsing,
-- so we need to reverse it again first before we start mapping stuff    
runExprPassAndReverse :: (Expr -> Expr) -> LTProgram -> LTProgram
runExprPassAndReverse f l = rev f l []
    where rev f [] a = a
          rev f ((ex, srci):xs) a = rev f xs ( (f ex, srci):a )


afterparse :: Expr -> Expr
afterparse (BinaryOp n e1 e2) = App (Id n) ( e1:e2:[])
afterparse (UnaryOp "-" e) = App (Id "negate") [afterparse e]
afterparse (UnaryOp n e) = App (Id n) ( e:[])
-- if/then/else desugars to a 1-arg lambda with expanded pattern match on Bool
-- We use a fresh variable name "__cond" and produce ExpandedCase directly
-- so it doesn't need to go through caseOptimizationPass
afterparse (IfThenElse cond thenE elseE) =
    App (Function (mkLambda "" [Var "__cond" (Id "Bool") UNDEFINED]
        (PatternMatches
            [ ExpandedCase [ExprConsTagCheck (ConsTag "True" 0) (Id "__cond")]  thenE SourceInteractive
            , ExpandedCase [ExprConsTagCheck (ConsTag "False" 1) (Id "__cond")] elseE SourceInteractive
            ]) UNDEFINED)) [cond]
-- let/in desugars to nested lambda application
afterparse (LetIn [(v, val)] bdy) =
    App (Function (mkLambda "" [v] bdy UNDEFINED)) [val]
afterparse (LetIn ((v,val):rest) bdy) =
    App (Function (mkLambda "" [v] (afterparse (LetIn rest bdy)) UNDEFINED)) [val]
-- Law desugaring: law declarations become functions returning PropEq proof terms
-- Curry-Howard: propositions are types, proofs are programs
--   law name(params) = lhs === rhs
--     ==> function name(params) : PropEq(_, lhs, rhs) = Refl
--   law name(params) = P ==> lhs === rhs
--     ==> function name(params, __proof0: PropEq(_, P, True)) : PropEq(_, lhs, rhs) = Refl
afterparse (Law lam lawBody) = desugarLaw lam lawBody
-- value declarations desugar to nullary functions
afterparse (Value (Var nm tp _) val) = Function (mkLambda nm [] val tp)
-- repr cast: just recurse into children, resolved in exprToCLM
afterparse (ReprCast e tp) = ReprCast (afterparse e) tp
afterparse e = e

-- Desugar a law declaration into a function returning a PropEq proof term
desugarLaw :: Lambda -> Expr -> Expr
desugarLaw lam lawBody =
    let (premises, conclusion) = collectImplies lawBody
        (retLhs, retRhs) = case conclusion of
            PropEq l r -> (l, r)
            other      -> (other, Id "True")  -- bare expr treated as === True
        proofParams = imap mkProofParam premises
        returnType = App (Id "PropEq") [UNDEFINED, retLhs, retRhs]
        allParams = params lam ++ proofParams
    in Function (lam { params = allParams, body = Id "Refl", lamType = returnType })

-- Collect premises from nested Implies: P ==> Q ==> R  ->  ([P, Q], R)
collectImplies :: Expr -> ([Expr], Expr)
collectImplies (Implies premise rest) =
    let (prems, concl) = collectImplies rest
    in (premise : prems, concl)
collectImplies e = ([], e)

-- Create a proof parameter from a premise expression
-- If premise is PropEq l r, proof type is PropEq(_, l, r)
-- If premise is a bare expression, proof type is PropEq(_, expr, True)
mkProofParam :: Int -> Expr -> Var
mkProofParam i premise =
    let (lhs, rhs) = case premise of
            PropEq l r -> (l, r)
            other      -> (other, Id "True")
        proofType = App (Id "PropEq") [UNDEFINED, lhs, rhs]
    in Var ("__proof" ++ show i) proofType UNDEFINED


--------------------------------------------------------------------------------
-- PASS 0.5: Action block desugaring - converts ActionBlock to bind chains
--------------------------------------------------------------------------------
actionDesugarPass :: IntState ()
actionDesugarPass = do
    s <- get
    let mod' = Prelude.map (\(e, si) -> (desugarActions e, si)) (parsedModule s)
    put (s { parsedModule = mod' })
    verboseLog "  Pass 0.5: action block desugaring"

-- Desugar ActionBlock nodes into bind/let chains throughout the AST
desugarActions :: Expr -> Expr
desugarActions = traverseExpr desugarAction

desugarAction :: Expr -> Expr
desugarAction (ActionBlock stmts) = desugarActionStmts stmts
desugarAction e = e

-- Convert a list of action statements into nested bind/let applications
-- name <- expr, rest  =>  bind(expr, \name -> rest)
-- name = expr, rest   =>  let name = expr in rest
-- expr, rest          =>  seq(expr, rest)  i.e. bind(expr, \_ -> rest)
-- expr (last)         =>  expr
desugarActionStmts :: [ActionStmt] -> Expr
desugarActionStmts [] = App (Id "pure") [NTuple []]  -- pure(())
desugarActionStmts [ActionExpr e] = e
desugarActionStmts [ActionBind nm e] = e  -- last statement is bind — just return the expr
desugarActionStmts [ActionLet nm e] = e   -- last statement is let — just return the expr
desugarActionStmts (ActionBind nm e : rest) =
    -- effectBind(expr, \name -> rest)
    App (Id "effectBind") [e, Function (mkLambda "" [Var nm UNDEFINED UNDEFINED] (desugarActionStmts rest) UNDEFINED)]
desugarActionStmts (ActionLet nm e : rest) =
    -- let name = expr in rest
    App (Function (mkLambda "" [Var nm UNDEFINED UNDEFINED] (desugarActionStmts rest) UNDEFINED)) [e]
desugarActionStmts (ActionExpr e : rest) =
    -- effectSeq(expr, \_ -> rest)
    App (Id "effectSeq") [e, Function (mkLambda "" [Var "_" UNDEFINED UNDEFINED] (desugarActionStmts rest) UNDEFINED)]

--------------------------------------------------------------------------------
-- PASS 0.25: String literal desugaring
-- "hello" → fromStringLiteral([0x68, 0x65, 0x6C, 0x6C, 0x6F])
-- Controlled by `newStrings` flag. When off, string literals pass through
-- as primitive LString for backward compatibility.
--------------------------------------------------------------------------------
stringLiteralDesugarPass :: IntState ()
stringLiteralDesugarPass = do
    s <- get
    let ns = newStrings (currentFlags s)
    when ns $ do
        let mod' = Prelude.map (\(e, si) -> (desugarStringLiterals e, si)) (parsedModule s)
        put (s { parsedModule = mod' })
        verboseLog "  Pass 0.25: string literal desugaring"

-- Desugar LString literals into fromStringLiteral(Array(Byte)) calls
desugarStringLiterals :: Expr -> Expr
desugarStringLiterals = traverseExpr desugarStringLit

desugarStringLit :: Expr -> Expr
desugarStringLit (Lit (LString s)) =
    let bytes = encodeUtf8 s
        byteExprs = Prelude.map (Lit . LWord8) bytes
        byteLen = Prelude.length bytes
    in App (Id "Str") [ArrayLit byteExprs, Lit (LInt byteLen)]
desugarStringLit e = e

-- | Encode a Haskell String to UTF-8 bytes (compile-time)
encodeUtf8 :: String -> [Word8]
encodeUtf8 = Prelude.concatMap encodeChar
  where
    encodeChar c
        | cp <= 0x7F    = [fromIntegral cp]
        | cp <= 0x7FF   = [ fromIntegral (0xC0 .|. (cp `shiftR` 6))
                          , fromIntegral (0x80 .|. (cp .&. 0x3F)) ]
        | cp <= 0xFFFF  = [ fromIntegral (0xE0 .|. (cp `shiftR` 12))
                          , fromIntegral (0x80 .|. ((cp `shiftR` 6) .&. 0x3F))
                          , fromIntegral (0x80 .|. (cp .&. 0x3F)) ]
        | otherwise     = [ fromIntegral (0xF0 .|. (cp `shiftR` 18))
                          , fromIntegral (0x80 .|. ((cp `shiftR` 12) .&. 0x3F))
                          , fromIntegral (0x80 .|. ((cp `shiftR` 6) .&. 0x3F))
                          , fromIntegral (0x80 .|. (cp .&. 0x3F)) ]
      where cp = ord c

-- Resolve spread fields (..Name) in a constructor lambda's params
-- by looking up the source record/type's constructor fields from the environment
resolveSpreadFields :: Environment -> Lambda -> Lambda
resolveSpreadFields env lam =
    let args' = Prelude.concatMap resolveField (params lam)
    in  lam { params = args' }
    where
        resolveField v@(Var fieldNm _ _)
            | Prelude.take 2 fieldNm == ".." =
                let srcTypeName = Prelude.drop 2 fieldNm
                in  case lookupType srcTypeName env of
                        Just (SumType stLam) -> case body stLam of
                            Constructors cons -> case cons of
                                (c : _) -> params c
                                _ -> [v]
                            _ -> [v]
                        _ -> [v]  -- type not found, keep marker
            | otherwise = [v]

-- Only VarDefinition, Binding and PatternMatch should be seen at the top level
buildEnvironmentM :: (Expr, SourceInfo) -> IntState ()
buildEnvironmentM x@(e,si) = do
    s <- get
    let env = currentEnvironment s
    env' <- processBinding x env
    put s{currentEnvironment = env'}

processBinding :: (Expr, SourceInfo) -> Environment -> IntState Environment
-- function definition
-- Standalone intrinsic functions (e.g., reflection primitives) get a dummy implicit param
-- so they route through CLMIAP dispatch (like effect operations).
processBinding (Function lam, si) env
    | body lam == Intrinsic =
        let implParams = [Var "__refl" (Implicit Type) UNDEFINED]
            opBody = CaseOf [] (Function lam) si
            wrapper = (mkLambda (lamName lam) implParams (PatternMatches [opBody]) (Function lam)) { lamSrcInfo = si }
        in pure $ addNamedLambda wrapper env
    | hasImplicit lam =
        -- Standalone function with implicit type params (e.g., function foo [s:Type] (x:s) : Int = ...)
        -- Type params are erased at compile time; register as regular function with value params only.
        -- Inner calls to algebra methods handle their own dispatch through CLMIAP.
        let valueParams = Prelude.dropWhile isImplicitVar (params lam)
            lam' = lam { params = valueParams }
        in pure $ addLambda (lamName lam') lam' env
    | otherwise = pure $ addLambda (lamName lam) lam env
-- treating actions the same way - they will have some proper type eventually anyway
processBinding (Action lam, si) env = pure $ addLambda (lamName lam) lam env
-- (Prim removed — legacy primitive functions use Function + Intrinsic instead)

-- now extracting constructors from SumTypes, body is guaranteed to be
-- a list of Lambdas under Constructors constructor
processBinding ( tp@(SumType lam@(Lambda typName typArgs (Constructors cons) typTyp _ _)), si) env = do
    -- resolve any spread fields (..Name) in constructor params
    let cons' = Prelude.map (resolveSpreadFields env) cons
    -- For constructors with no args that match existing record types, inherit their fields.
    -- This allows: record Rect = {w:Int, h:Int}; type Shape = Rect | Circle;
    -- to create Shape with Rect(w:Int, h:Int) | Circle(radius:Int)
    let cons'' = Prelude.map (inheritRecordFields env) cons'
    let newCons = imap fixCons cons''
    let newTp = SumType lam { body = Constructors newCons }
    -- Register type constructor as a type-level function in topLambdas.
    -- For type Maybe(a:Type) = ..., register Maybe : (a:Type) -> Type
    -- This enables kind checking via evalCLMPure/normalizeTy.
    -- The body constructs a type application node: App (Id typName) [Id a, ...]
    let typeLevelLam = if Prelude.null typArgs
            then Nothing  -- no params → no function to register (it's just a Type)
            else
                let retBody = App (Id typName) (Prelude.map (\v -> Id (name v)) typArgs)
                in Just $ (mkLambda typName typArgs retBody (U (LConst 0))) { lamSrcInfo = lamSrcInfo lam }
    let env1 = addManyNamedConstructors 0 newCons (addNamedSumType newTp env)
    pure $ case typeLevelLam of
        Just tll -> addLambda typName tll env1
        Nothing  -> env1
    where fixCons i lam@(Lambda nm args ex typ _ _) = if (ex /= UNDEFINED)
            then lam
            else lam { body = ConTuple (ConsTag nm i) $ Prelude.map (\v -> Id $ name v) args}
          -- If a constructor has no args and its name matches an existing record constructor,
          -- inherit that record's fields so the sum type variant carries the same data.
          inheritRecordFields e cl@(Lambda nm args _ _ _ _)
            | Prelude.null args, body cl == UNDEFINED
            , Just (recLam, _tag) <- lookupConstructor nm e
            , not (Prelude.null (params recLam))
            = cl { params = params recLam }
            | otherwise = cl

-- CLASS declarations
processBinding (ClassDecl lam cinfo, si) env = do
    -- 1. Resolve parent class fields and methods
    let parentName = fst <$> classParent cinfo
    let superArgs  = maybe [] snd (classParent cinfo)
    (parentFields, parentMethods, parentStatics) <- case parentName of
        Nothing -> pure ([], Map.empty, Map.empty)
        Just pn -> case lookupClass pn env of
            Just pm -> pure (cmAllFields pm, cmMethods pm, cmStaticMethods pm)
            Nothing -> do
                logError (mkLogPayload si ("Parent class " ++ pn ++ " not found for class " ++ lamName lam))
                pure ([], Map.empty, Map.empty)

    -- 2. Build field list: inherited + own
    let ownFields = params lam
    let allFields = parentFields ++ ownFields
    let fieldIndices = Map.fromList (Prelude.zip (Prelude.map name allFields) [0..])

    -- 3. Allocate class tag
    let tag = classTagCounter env
    let env1 = env { classTagCounter = tag + 1 }

    -- 4. Extract own methods (from DeclBlock) and convert to CLMLam
    let ownMethods = case body lam of
            DeclBlock exs -> [(lamName l, l) | Function l <- exs]
            _ -> []
    let ownMethodsMap = Map.fromList [(n, lambdaToCLMLambda env1 l) | (n, l) <- ownMethods]
    let isStatic n = case Prelude.lookup n (classMethodMods cinfo) of
            Just MStatic -> True
            _ -> False
    let ownStaticMap = Map.filterWithKey (\k _ -> isStatic k) ownMethodsMap
    let ownInstanceMap = Map.filterWithKey (\k _ -> not (isStatic k)) ownMethodsMap

    -- 5. Merge methods: own override parent (Map.union prefers left)
    let mergedMethods = Map.union ownInstanceMap parentMethods
    let mergedStatics = Map.union ownStaticMap parentStatics

    -- 6. Build ClassMeta
    let srcFile = case si of { SourceInfo _ _ sf _ -> sf; _ -> "<interactive>" }
    let cm = ClassMeta
            { cmParent        = parentName
            , cmAllFields     = allFields
            , cmOwnFields     = ownFields
            , cmMethods       = mergedMethods
            , cmStaticMethods = mergedStatics
            , cmFieldIndices  = fieldIndices
            , cmModifier      = classModifier cinfo
            , cmChildren      = []
            , cmImplements    = [n | Id n <- classImplements cinfo]
            , cmSuperArgs     = superArgs
            , cmExtern        = classExtern cinfo
            , cmTag           = tag
            , cmSourceFile    = srcFile
            }

    -- 7. Register ClassMeta in classDecls
    let env2 = env1 { classDecls = Map.insert (lamName lam) cm (classDecls env1) }

    -- 8. Update parent's children list
    let env3 = case parentName of
            Just pn -> env2 { classDecls = Map.adjust (\p -> p { cmChildren = lamName lam : cmChildren p }) pn (classDecls env2) }
            Nothing -> env2

    -- 8.5 Validate sealed constraint: child of sealed parent must be in same file
    case parentName of
        Just pn -> case lookupClass pn env3 of
            Just pm | cmModifier pm == ClassSealed
                    , srcFile /= "<interactive>"
                    , cmSourceFile pm /= "<interactive>"
                    , srcFile /= cmSourceFile pm ->
                logError (mkLogPayload si
                    ("sealed class " ++ pn ++ " cannot be subclassed from a different file ("
                        ++ srcFile ++ " vs " ++ cmSourceFile pm ++ ")"))
            _ -> pure ()
        Nothing -> pure ()

    -- 9. Register constructor (like sum types — enables CLMCON + pattern matching)
    let consLam = (mkLambda (lamName lam) allFields
                    (ConTuple (ConsTag (lamName lam) tag)
                        (Prelude.map (\v -> Id (name v)) allFields))
                    (Id (lamName lam)))
    let env4 = env3 { constructors = Map.insert (lamName lam) (consLam, tag) (constructors env3) }

    -- 10. Register type
    let env5 = env4 { types = Map.insert (lamName lam) (ClassDecl lam cinfo) (types env4) }

    -- 11. Generate algebra instances from implements clause
    env6 <- foldM (generateClassAlgebraInstance (lamName lam) ownMethods si) env5 (cmImplements cm)

    verboseLog $ "  Registered class " ++ lamName lam ++ " (tag=" ++ show tag
        ++ ", fields=" ++ show (Prelude.length allFields)
        ++ ", methods=" ++ show (Map.size mergedMethods) ++ ")"

    pure env6

-- so, structures (typeclasses) are very interesting.
-- what we do here is extract all the functions inside the typeclass
-- and turn them into functions dependent on type which they are
-- then we instantiate type from implicit arguments (type of the arguments)
-- and ask environment for a specific function.
-- When there's instance of some typeclass - our functions in the env
-- are updated with functions for a certain type!
-- E.g., for Eq class (==) is really is a func:
{- (==) (a:Type, x:a, y:a) : Bool = {
   {Bool} -> function(x,y) = ...,
   {Nat} -> function(x,y) = ...
},
i.e. it can be represented as 
CaseOf [(Var "a") (Id "Bool")] Lam $ Lambda {...}
then, we encounter something like 4 == plus(x,y) and go through:
lookup ==: (==) (a,x:a,y:a) (4:Int, plus(x,y):Int) ->
    deduce from this that a = Int
    apply (==) (Int) to hopefully get a function
    if we get it - all ok, if we don't - ERROR

Another consideration: for compilation we will actually want
separate functions for each concrete type, so when a structure is 
instanctiated to a type, we need to create all the needed functions
on the top level and then look them up in 2 steps
-}
processBinding ( st@(Structure lam sinfo), si) env = do
    -- Validate param count for algebra/morphism (warning only)
    case structKind sinfo of
        SAlgebra  -> when (Prelude.length (params lam) /= 1) $
            logWarning (mkLogPayload si
                ("algebra " ++ lamName lam ++ " should have exactly 1 type parameter, has "
                    ++ show (Prelude.length (params lam)) ++ "\n"))
        SMorphism -> when (Prelude.length (params lam) < 2) $
            logWarning (mkLogPayload si
                ("morphism " ++ lamName lam ++ " should have 2+ type parameters, has "
                    ++ show (Prelude.length (params lam)) ++ "\n"))
        SGeneral  -> pure ()

    -- Resolve extends: inherit parent functions and laws
    lam' <- resolveExtends env lam (structExtends sinfo)

    -- Register inheritance
    let parentNames = [n | ext <- structExtends sinfo, let n = extractStructRefName ext, n /= ""]
    let env0 = registerInheritance (lamName lam') parentNames env

    -- Validate requires
    validateRequires env0 (structRequires sinfo) si

    let st' = Structure lam' sinfo
    let env' = addNamedStructure st' env0
    case (body lam') of
        DeclBlock exs -> do
            -- going over all members of a structure and making needed
            -- bindings / transformations
            env'' <- foldM fixStr env' exs
            pure env''
        _ -> do
                let lpl = mkLogPayload si
                            ("Encountered wrong Structure expressions:\n" ++ (ppr st') ++ "\n")
                logError lpl
                pure env'
    where fixStr env1 (Function l) = do
            let body' = CaseOf [] (Function l) (lamSrcInfo l)
            let res = (mkLambda (lamName l) (params lam) (PatternMatches [body']) (Function l)) { lamSrcInfo = lamSrcInfo l }
            let env1' = addNamedLambda res env1
            return env1'
          -- value declarations are desugared to nullary functions by afterparse,
          -- so they appear as Function here; but if raw Value slips through:
          fixStr env1 (Value v ex) = do
            let l = mkLambda (name v) [] ex (typ v)
            let body' = CaseOf [] (Function l) si
            let res = (mkLambda (name v) (params lam) (PatternMatches [body']) (Function l)) { lamSrcInfo = si }
            let env1' = addNamedLambda res env1
            return env1'
          fixStr env1 (Law _ _) = pure env1  -- skip law declarations
          fixStr env1 _ = pure env1
    

-- instance declaration processing
-- instance Eq(Nat) = { function (==)(x:Nat,y:Nat):Bool = eq(x,y) }
processBinding (Instance structName typeArgs impls reqs, si) env = do
    -- Validate requires (basic structure existence check)
    validateRequires env reqs si
    -- extract all type names from type args (e.g., [Id "Nat", Id "Bool"] -> ["Nat", "Bool"])
    -- also handles parameterized types like List(a) -> "List"
    let extractTypeName (Id nm) = Just nm
        extractTypeName (App (Id nm) _) = Just nm
        extractTypeName _ = Nothing
    let typeNames = [nm | Just nm <- Prelude.map extractTypeName typeArgs]
    -- Validate requires instances exist (with type substitution from structure params)
    unless (Prelude.null typeNames) $
        validateRequiresForInstance env reqs typeNames structName si
    if Prelude.null typeNames
    then do
        let lpl = mkLogPayload si
                ("Instance declaration has no valid type argument: "
                    ++ structName ++ "\n")
        logError lpl
        pure env
    else case impls of
        [Derive] -> do
            -- Derive instance: look up algebra's derive block and expand
            case lookupType structName env of
                Just (Structure structLam structInfo) -> case structDerive structInfo of
                    [] -> do
                        logWarning (mkLogPayload si
                            ("derive instance: algebra " ++ structName
                                ++ " has no derive block\n"))
                        pure env
                    deriveExprs -> do
                        -- Register each derive function as an instance lambda
                        env' <- foldM (addInstanceFunc typeNames) env deriveExprs
                        -- propagate to parent structures
                        env'' <- propagateToParent env' structName typeNames deriveExprs si
                        env''' <- composeMorphismInstances env'' structName typeNames si
                        pure env'''
                _ -> do
                    logWarning (mkLogPayload si
                        ("derive instance: algebra " ++ structName
                            ++ " not found in environment\n"))
                    pure env
        [Intrinsic] -> do
            -- Intrinsic instance: generate placeholder lambdas for all structure functions
            case lookupType structName env of
                Just (Structure structLam _) -> case body structLam of
                    DeclBlock exs -> do
                        let funcNames = [lamName l | Function l <- exs]
                                     ++ [name v | Value v _ <- exs]
                        let intrinsicImpls = [Function (mkLambda fn [] Intrinsic UNDEFINED) | fn <- funcNames]
                        let env' = Prelude.foldl (\e fn ->
                                addInstanceLambda fn typeNames
                                    (mkLambda fn [] Intrinsic UNDEFINED) e
                                ) env funcNames
                        env'' <- propagateToParent env' structName typeNames intrinsicImpls si
                        pure env''
                    _ -> do
                        logWarning (mkLogPayload si
                            ("intrinsic instance: structure " ++ structName ++ " body is not a DeclBlock\n"))
                        pure env
                _ -> do
                    logWarning (mkLogPayload si
                        ("intrinsic instance: structure " ++ structName ++ " not found in environment\n"))
                    pure env
        _ -> do
            -- for each function in the instance, store a specialized lambda
            env' <- foldM (addInstanceFunc typeNames) env impls
            -- validate minimal definition completeness (before default propagation)
            validateMinimalDefinition structName typeNames env' si
            -- propagate default methods from the structure itself
            env'a <- propagateDefaults typeNames env' structName
            -- propagate instance functions to parent structures
            env'' <- propagateToParent env'a structName typeNames impls si
            -- auto-compose morphism instances (for 2-param morphisms)
            env''' <- composeMorphismInstances env'' structName typeNames si
            pure env'''
    where
        addInstanceFunc typeNms env1 (Function lam) = do
            let funcNm = lamName lam
            pure $ addInstanceLambda funcNm typeNms lam env1
        -- value declarations are desugared to nullary functions by afterparse
        addInstanceFunc typeNms env1 (Value v ex) = do
            let lam = mkLambda (name v) [] ex (typ v)
            pure $ addInstanceLambda (name v) typeNms lam env1
        addInstanceFunc _ env1 e = do
            logWarning (mkLogPayload si
                    ("Invalid expression inside instance declaration, expected function: "
                        ++ ppr e ++ "\n"))
            pure env1

-- repr declaration: register toRepr/fromRepr as instance lambdas + store in reprMap
processBinding (Repr userTypeExpr reprTypeExpr isDefault fns maybeInv, si) env = do
    let userTypeName = mkReprKey userTypeExpr
    let reprTypeName = case reprTypeExpr of { Id nm -> nm; App (Id nm) _ -> nm; _ -> "unknown" }
    let findFn fname = listToMaybe [lam | Function lam <- fns, lamName lam == fname]
    case (findFn "toRepr", findFn "fromRepr") of
        (Just toR, Just fromR) -> do
            -- Create implicit-param wrapper lambdas so CLMIAP dispatch is triggered
            let implParam = Var "a" (Implicit Type) UNDEFINED
            let toRBody = CaseOf [] (Function toR) si
            let toRWrapper = (mkLambda "toRepr" [implParam] (PatternMatches [toRBody]) (Function toR)) { lamSrcInfo = si }
            let fromRBody = CaseOf [] (Function fromR) si
            let fromRWrapper = (mkLambda "fromRepr" [implParam] (PatternMatches [fromRBody]) (Function fromR)) { lamSrcInfo = si }
            -- Only add wrapper if no existing toRepr/fromRepr yet
            let env1 = case lookupLambda "toRepr" env of
                    Nothing -> addNamedLambda toRWrapper env
                    Just _  -> env
            let env2 = case lookupLambda "fromRepr" env1 of
                    Nothing -> addNamedLambda fromRWrapper env1
                    Just _  -> env1
            -- Register as instance lambdas for type-directed dispatch
            let env3 = addInstanceLambda "toRepr" [userTypeName] toR env2
            let env4 = addInstanceLambda "fromRepr" [reprTypeName] fromR env3
            -- Store in repr map
            pure $ addRepr userTypeName reprTypeName isDefault toR fromR maybeInv env4
        _ -> do
            logError (mkLogPayload si "repr declaration must define both toRepr and fromRepr\n")
            pure env

-- primitive type declaration
processBinding (Primitive lam, si) env = do
    let typeName = lamName lam
    pure $ env { types = Map.insert typeName (Primitive lam) (types env) }

-- Module system declarations: processed for environment building but
-- main module resolution happens in the module loading pipeline
processBinding (ModuleDecl _, _) env = pure env  -- module declaration: tracked in loadFileQuiet
processBinding (Import path _ (Just tgt), _) env =   -- target import: store for codegen metadata resolution
    pure $ env { targetImports = (path, tgt) : targetImports env }
processBinding (Import _ _ Nothing, _) env = pure env   -- import: resolved during module loading
processBinding (Open _, _) env = pure env          -- open: resolved during module loading
processBinding (Export _ _, _) env = pure env      -- export: resolved during module loading
processBinding (PrivateDecl inner, si) env = processBinding (inner, si) env  -- private: process inner decl
processBinding (OpaqueTy lam reprTy, si) env = do
    -- Opaque type: register as a primitive type (opaque to outside, transparent inside)
    let typeName = lamName lam
    pure $ env { types = Map.insert typeName (Primitive lam) (types env) }
-- Target block: parse inner declarations and store target-qualified instances/handlers
processBinding (TargetBlock targetName decls, si) env = foldM (processTargetDecl targetName si) env decls
processBinding (TargetSwitch _, _) env = pure env   -- target switches: resolved during codegen
-- Fixity declarations: register in fixity table (already done at parse time, but needed for cache restore)
processBinding (FixityDecl assoc prec ops, _) env =
    pure $ Prelude.foldl (\e o -> addFixity o (OperatorFixity assoc prec) e) env ops

-- Effect declaration: register effect and its operations as implicit-param functions
processBinding (EffectDecl effName effParams ops, si) env = do
    -- Store effect declaration
    let env1 = env { effectDecls = Map.insert effName (effParams, ops) (effectDecls env) }
    -- Register each operation as a top-level implicit-param function
    -- so they can be called like regular functions and resolved via CLMIAP
    let env2 = Prelude.foldl (registerEffectOp effName effParams) env1 ops
    -- Build reverse map: operation name -> effect name
    let env3 = Prelude.foldl (\e op -> e { effectOps = Map.insert (lamName op) effName (effectOps e) }) env2 ops
    pure env3
  where
    registerEffectOp eName eParams env0 op =
        let -- Effect type params become implicit params for CLMIAP routing.
            -- If effect has no type params, add a dummy implicit so hasImplicit = True
            -- and exprToCLM routes through CLMIAP → dispatchIOIntrinsic.
            implParams
                | not (Prelude.null eParams) = Prelude.map (\v -> v { typ = Implicit (typ v) }) eParams
                | otherwise = [Var "__eff" (Implicit Type) UNDEFINED]
            opBody = CaseOf [] (Function op) (lamSrcInfo op)
            wrapper = (mkLambda (lamName op) implParams (PatternMatches [opBody]) (Function op)) { lamSrcInfo = lamSrcInfo op }
        in addNamedLambda wrapper env0

-- Handler declaration: register handler implementations
processBinding (HandlerDecl handlerName effName isDefault hParams impls, si) env = do
    let env1 = env { effectHandlers = Map.insert handlerName (effName, isDefault, hParams, impls) (effectHandlers env) }
    -- If this handler is marked as default, register it as the default for its effect
    let env2 = if isDefault
               then env1 { defaultHandlers = Map.insert effName handlerName (defaultHandlers env1) }
               else env1
    -- For stateless handlers (no params), also register as instance lambdas
    -- so intrinsic dispatch can find them as fallback
    env3 <- if Prelude.null hParams
            then foldM (registerHandlerImpl effName) env2 [f | f@(Function _) <- impls]
            else pure env2
    pure env3
  where
    registerHandlerImpl eName env0 (Function lam) = do
        let funcNm = lamName lam
        pure $ addInstanceLambda funcNm [eName] lam env0
    registerHandlerImpl _ env0 _ = pure env0

processBinding (ex, si) env = do
    let lpl = mkLogPayload si
                ("Cannot add the following expression to the Environment during initial Environment Building pass:\n"
                    ++ (ppr ex) ++ "\n"
                    -- ++ (TL.unpack (pShow ex))
                    ++ "\nThe expression is parsed and stored in LTProgram, but is not in the Environment.")
    logWarning lpl
    return env

-- | Process a declaration inside a target block.
-- Instances and handlers are stored in target-qualified maps (not in the main env).
-- Other declarations inside target blocks are silently ignored.
processTargetDecl :: Name -> SourceInfo -> Environment -> Expr -> IntState Environment
processTargetDecl targetName si env (Instance structName typeArgs impls _reqs) = do
    let extractTypeName (Id nm) = Just nm
        extractTypeName (App (Id nm) _) = Just nm
        extractTypeName _ = Nothing
    let typeNames = [nm | Just nm <- Prelude.map extractTypeName typeArgs]
    if Prelude.null typeNames
    then do
        logWarning (mkLogPayload si
            ("Target " ++ targetName ++ ": instance has no valid type argument: " ++ structName ++ "\n"))
        pure env
    else do
        -- Store each function/value in the target-qualified instance map
        foldM (addTargetInstanceFunc targetName structName typeNames si) env impls
processTargetDecl targetName _si env (HandlerDecl handlerName effectName _isDefault hParams impls) = do
    pure $ addTargetHandler targetName handlerName (effectName, hParams, impls) env
processTargetDecl targetName _si env (ExternFunc funcName funcParams retType spec) = do
    pure $ addTargetExtern targetName funcName (funcParams, retType, spec) env
processTargetDecl targetName si env expr = do
    logWarning (mkLogPayload si
        ("Target " ++ targetName ++ ": unsupported declaration inside target block: " ++ ppr expr ++ "\n"))
    pure env

-- | Add a function/value from a target block instance to target-qualified storage.
addTargetInstanceFunc :: Name -> Name -> [Name] -> SourceInfo -> Environment -> Expr -> IntState Environment
addTargetInstanceFunc targetName _structName typeNames _si env (Function lam) =
    pure $ addTargetInstance targetName (lamName lam) typeNames lam env
addTargetInstanceFunc targetName _structName typeNames _si env (Value v ex) =
    let lam = mkLambda (name v) [] ex (typ v)
    in pure $ addTargetInstance targetName (name v) typeNames lam env
addTargetInstanceFunc targetName _structName _typeNames si env expr = do
    logWarning (mkLogPayload si
        ("Target " ++ targetName ++ ": invalid expression inside instance, expected function: " ++ ppr expr ++ "\n"))
    pure env

-- Generate algebra instance from class methods for the implements clause
generateClassAlgebraInstance :: Name -> [(Name, Lambda)] -> SourceInfo -> Environment -> Name -> IntState Environment
generateClassAlgebraInstance className ownMethods si env algebraName = do
    case lookupType algebraName env of
        Just (Structure structLam structInfo) -> do
            -- Get algebra function names
            let algebraFuncNames = case body structLam of
                    DeclBlock exs -> [lamName l | Function l <- exs]
                                  ++ [name v | Value v _ <- exs]
                    _ -> []
            -- Find matching methods from class body
            let matchedImpls = [(fn, l) | fn <- algebraFuncNames
                                        , (mn, l) <- ownMethods
                                        , mn == fn]
            if not (Prelude.null matchedImpls)
            then do
                -- Register each matched method as an instance lambda
                let env' = Prelude.foldl (\e (fn, l) ->
                        addInstanceLambda fn [className] l e
                        ) env matchedImpls
                -- Propagate to parent structures and compose morphisms
                let implExprs = [Function l | (_, l) <- matchedImpls]
                env'' <- propagateToParent env' algebraName [className] implExprs si
                env''' <- composeMorphismInstances env'' algebraName [className] si
                pure env'''
            else case structDerive structInfo of
                [] -> do
                    logWarning (mkLogPayload si
                        ("class " ++ className ++ " implements " ++ algebraName
                            ++ " but has no matching methods and no derive block\n"))
                    pure env
                deriveExprs -> do
                    -- Fall back to auto-derive
                    let env' = Prelude.foldl (\e expr -> case expr of
                            Function l -> addInstanceLambda (lamName l) [className] l e
                            _ -> e
                            ) env deriveExprs
                    env'' <- propagateToParent env' algebraName [className] deriveExprs si
                    env''' <- composeMorphismInstances env'' algebraName [className] si
                    pure env'''
        _ -> do
            logWarning (mkLogPayload si
                ("class " ++ className ++ " implements " ++ algebraName
                    ++ " but algebra not found in environment\n"))
            pure env

-- The pass itself
buildEnvPass :: IntState ()
buildEnvPass = buildPrimitivePass >> get >>= pure . parsedModule >>= mapM_ buildEnvironmentM

-- No legacy primitive bindings needed — all primitives use the intrinsic system now
buildPrimitivePass :: IntState ()
buildPrimitivePass = pure ()

-- Extract structure name from a structure reference (Id "Eq" or App (Id "Eq") [Id "a"])
extractStructRefName :: Expr -> Name
extractStructRefName (Id nm)       = nm
extractStructRefName (App (Id nm) _) = nm
extractStructRefName _             = ""

-- Resolve extends: inherit parent functions and laws into child structure
resolveExtends :: Environment -> Lambda -> [Expr] -> IntState Lambda
resolveExtends _   lam [] = pure lam
resolveExtends env lam extends = do
    -- Step 2: Validate extends-reference arguments are valid child parameters
    let childParamNames = [name v | v <- params lam]
    mapM_ (validateExtendsArgs childParamNames) extends
    parentMembers <- Prelude.concat <$> mapM getParentMembers extends
    case body lam of
        DeclBlock childMembers -> do
            -- Only add parent members that child doesn't override
            let childNames = [lamName l | Function l <- childMembers]
                            ++ [lamName l | Law l _ <- childMembers]
                            ++ [name v | Value v _ <- childMembers]
            let newMembers = Prelude.filter (notOverridden childNames) parentMembers
            pure lam { body = DeclBlock (newMembers ++ childMembers) }
        _ -> pure lam
  where
    validateExtendsArgs childParamNames ref = do
        let refArgNames = extractRefArgNames ref
        let invalid = Prelude.filter (`Prelude.notElem` childParamNames) refArgNames
        unless (Prelude.null invalid) $
            logWarning (mkLogPayload SourceInteractive
                ("extends: unknown type params " ++ show invalid
                    ++ " in " ++ ppr ref ++ " for structure " ++ lamName lam ++ "\n"))
    extractRefArgNames (App _ args) = [n | Id n <- args]
    extractRefArgNames _ = []
    getParentMembers ref = do
        let parentName = extractStructRefName ref
        case lookupType parentName env of
            Just (Structure parentLam _) -> case body parentLam of
                DeclBlock exs -> pure exs
                _             -> pure []
            _ -> do
                logWarning (mkLogPayload SourceInteractive
                    ("extends: parent structure " ++ parentName ++ " not found in environment\n"))
                pure []
    notOverridden childNames (Function l) = lamName l `Prelude.notElem` childNames
    notOverridden childNames (Law l _)    = lamName l `Prelude.notElem` childNames
    notOverridden childNames (Value v _)  = name v `Prelude.notElem` childNames
    notOverridden _ _                     = True

-- When instance Child(T) is declared, also register functions for parent structures
propagateToParent :: Environment -> Name -> [Name] -> [Expr] -> SourceInfo -> IntState Environment
propagateToParent env structName typeNames impls si = do
    let allParents = getAllParents structName env
    -- filter out the structure itself from parents list
    let parents = Prelude.filter (/= structName) allParents
    if Prelude.null parents then pure env
    else do
        -- For each parent, for each impl function, check if function belongs to parent
        env' <- foldM (propagateOne impls) env parents
        -- Step 4: Propagate default methods from parent structures
        env'' <- foldM (propagateDefaults typeNames) env' parents
        -- Step 3: Validate parent instances are complete
        mapM_ (\pn -> validateParentInstance env'' typeNames pn si) parents
        pure env''
  where
    propagateOne impls' env1 parentName = do
        -- Look up the parent structure to find its function names
        case lookupType parentName env1 of
            Just (Structure parentLam _) -> case body parentLam of
                DeclBlock exs -> do
                    let parentFuncNames = [lamName l | Function l <- exs]
                                        ++ [name v | Value v _ <- exs]
                    -- For each impl that's a parent function, also add to parent
                    foldM (addIfParent parentFuncNames) env1 impls'
                _ -> pure env1
            _ -> pure env1
    addIfParent parentFuncNames env1 (Function lam)
      | lamName lam `Prelude.elem` parentFuncNames =
          pure $ addInstanceLambda (lamName lam) typeNames lam env1
      | otherwise = pure env1
    addIfParent _ env1 _ = pure env1

-- Propagate default methods from parent structure definitions to instances.
-- Uses fixpoint resolution: only propagate a default if all the algebra functions
-- it references are already resolved (provided by instance or by a previously resolved default).
-- This prevents infinite recursion from mutual defaults (e.g. == and != both defaulting to each other).
propagateDefaults :: [Name] -> Environment -> Name -> IntState Environment
propagateDefaults typeNames env1 parentName = do
    case lookupType parentName env1 of
        Just (Structure parentLam _) -> case body parentLam of
            DeclBlock exs -> do
                let allFuncs = [(lamName l, l) | Function l <- exs]
                        ++ [(name v, mkLambda (name v) [] ex (typ v)) | Value v ex <- exs]
                let allFuncNames = Set.fromList [n | (n, _) <- allFuncs]
                -- Functions with defaults: name -> (lambda, set of algebra functions referenced in default body)
                let withDefaults = [(n, l, Set.fromList (Prelude.filter (`Set.member` allFuncNames) (collectIdRefs (body l))))
                                   | (n, l) <- allFuncs, body l /= UNDEFINED]
                -- Start with functions already provided by the instance
                let provided = Set.fromList [n | (n, _) <- allFuncs,
                                             not (isNothing (lookupInstanceLambda n typeNames env1))]
                -- Fixpoint: repeatedly resolve defaults whose deps are all provided
                let (finalProvided, resolvedDefaults) = fixpointResolve provided withDefaults
                -- Register resolved defaults
                env2 <- foldM (\e (fn, defLam) ->
                    if isNothing (lookupInstanceLambda fn typeNames e)
                    then pure $ addInstanceLambda fn typeNames defLam e
                    else pure e
                    ) env1 resolvedDefaults
                pure env2
            _ -> pure env1
        _ -> pure env1

-- Fixpoint resolution for default methods.
-- Returns (set of all resolved names, list of (name, lambda) defaults that were resolved)
-- Phase 1: iteratively resolve defaults whose deps are all already resolved.
-- Phase 2: any remaining defaults that only depend on OTHER defaults (even cyclically)
--   are self-sufficient and get resolved together. Only truly unresolvable: functions
--   with no default that aren't provided by the instance.
fixpointResolve :: Set.HashSet Name -> [(Name, Lambda, Set.HashSet Name)] -> (Set.HashSet Name, [(Name, Lambda)])
fixpointResolve provided defaults =
    let (resolved1, acc1, remaining1) = iterResolve provided [] defaults
        -- Phase 2: remaining all have defaults (by construction). If they only depend
        -- on each other (all deps are within the remaining set), resolve them all.
        remainingNames = Set.fromList [n | (n, _, _) <- remaining1]
        selfSufficient = [item | item@(_, _, deps) <- remaining1,
                          Set.isSubsetOf deps (Set.union resolved1 remainingNames)]
        finalResolved = Prelude.foldl (\s (n, _, _) -> Set.insert n s) resolved1 selfSufficient
        finalAcc = acc1 ++ [(n, l) | (n, l, _) <- selfSufficient]
    in (finalResolved, finalAcc)
  where
    iterResolve resolved acc remaining =
        let (nowResolved, stillRemaining) = Data.List.partition
                (\(_, _, deps) -> Set.isSubsetOf deps resolved) remaining
        in if Prelude.null nowResolved
           then (resolved, acc, stillRemaining)
           else let newResolved = Prelude.foldl (\s (n, _, _) -> Set.insert n s) resolved nowResolved
                    newAcc = acc ++ [(n, l) | (n, l, _) <- nowResolved]
                in iterResolve newResolved newAcc stillRemaining

-- Validate that an instance provides a minimal complete definition.
-- Two categories of warnings:
--   1. Functions with NO default and not provided → error (definitely incomplete)
--   2. Mutual-default cycles where instance provides NONE from the cycle → warning
--      (might infinite-loop; provide at least one to be safe)
-- Functions with defaults that have all deps resolved (including self-sufficient groups) are fine.
validateMinimalDefinition :: Name -> [Name] -> Environment -> SourceInfo -> IntState ()
validateMinimalDefinition structName typeNames env si = do
    case lookupType structName env of
        Just (Structure structLam _) -> case body structLam of
            DeclBlock exs -> do
                let allFuncs = [(lamName l, l) | Function l <- exs]
                        ++ [(name v, mkLambda (name v) [] ex (typ v)) | Value v ex <- exs]
                let allFuncNames = Set.fromList [n | (n, _) <- allFuncs]
                -- Functions NOT provided by the instance (before default propagation)
                let instanceProvided = Set.fromList [n | (n, _) <- allFuncs,
                        not (isNothing (lookupInstanceLambda n typeNames env))]
                -- 1. Functions with no default that the instance didn't provide
                let noDefault = [n | (n, l) <- allFuncs,
                        body l == UNDEFINED, not (Set.member n instanceProvided)]
                -- 2. Find mutual-default cycles where instance provides NONE
                -- Build dep graph for defaults among non-provided functions
                let notProvided = [(n, l) | (n, l) <- allFuncs,
                        body l /= UNDEFINED, not (Set.member n instanceProvided)]
                let notProvidedNames = Set.fromList [n | (n, _) <- notProvided]
                -- For each not-provided default, find deps within not-provided set
                let deps = [(n, Set.intersection notProvidedNames
                            (Set.fromList (Prelude.filter (`Set.member` allFuncNames) (collectIdRefs (body l)))))
                           | (n, l) <- notProvided]
                -- Find pure mutual cycles: functions that ONLY depend on other not-provided defaults
                -- These are OK if they have base cases (we can't check), but warn if the cycle is "tight"
                -- (every member references at least one other member — classic Eq == / != pattern)
                let tightCycle = [n | (n, ds) <- deps,
                        not (Set.null (Set.intersection ds notProvidedNames)),
                        -- Only warn if deps are within the not-provided set (true cycle)
                        Set.isSubsetOf ds notProvidedNames,
                        -- Don't warn about self-recursion only (that's normal recursion)
                        not (Set.isSubsetOf ds (Set.singleton n))]
                let typeStr = intercalate ", " typeNames
                -- Only emit warnings if there are actual issues
                unless (Prelude.null noDefault) $
                    logWarning (mkLogPayload si $
                        "Instance " ++ structName ++ "(" ++ typeStr ++ ") is incomplete:\n"
                        ++ "  required (no default): " ++ intercalate ", " noDefault ++ "\n")
                unless (Prelude.null tightCycle) $
                    logWarning (mkLogPayload si $
                        "Instance " ++ structName ++ "(" ++ typeStr ++ "): mutual defaults — "
                        ++ "consider providing at least one of: " ++ intercalate ", " tightCycle ++ "\n")
            _ -> pure ()
        _ -> pure ()

-- Automated morphism composition: when instance M(A, B) is registered,
-- look for existing M(X, A) to derive M(X, B) and M(B, Y) to derive M(A, Y)
composeMorphismInstances :: Environment -> Name -> [Name] -> SourceInfo -> IntState Environment
composeMorphismInstances env structName typeNames si = do
    -- Only for 2-param morphisms
    case lookupType structName env of
        Just (Structure _ sinfo) | structKind sinfo == SMorphism && Prelude.length typeNames == 2 -> do
            let [tA, tB] = typeNames
            -- Find all function names in this morphism
            case lookupType structName env of
                Just (Structure structLam _) -> case body structLam of
                    DeclBlock exs -> do
                        let funcNames = [lamName l | Function l <- exs]
                        env' <- foldM (composeForFunc tA tB funcNames) env funcNames
                        pure env'
                    _ -> pure env
                _ -> pure env
        _ -> pure env
  where
    composeForFunc tA tB allFuncNames env1 funcNm = do
        let existing = findMorphismInstances funcNm env1
        -- For each existing M(X, A), derive M(X, B) via X->A->B
        env2 <- foldM (composeXAB funcNm tA tB) env1
            [(x, a) | (x, a) <- existing, a == tA, x /= tB]  -- M(X, A) exists, derive M(X, B)
        -- For each existing M(B, Y), derive M(A, Y) via A->B->Y
        env3 <- foldM (composeABY funcNm tA tB) env2
            [(b, y) | (b, y) <- existing, b == tB, y /= tA]  -- M(B, Y) exists, derive M(A, Y)
        pure env3
    -- Compose M(X, B) from M(X, A) + M(A, B)
    composeXAB funcNm tA tB env1 (tX, _) =
        -- Check no direct instance exists already and no self-loop
        case lookupInstanceLambda funcNm [tX, tB] env1 of
            Just _ -> pure env1  -- direct instance exists, prefer it
            Nothing | tX == tB -> pure env1  -- avoid identity loops
            Nothing ->
                -- Generate: function f(x:X) : B = f_AB(f_XA(x))
                let composedLam = mkLambda funcNm [Var "__x" (Id tX) UNDEFINED]
                        (App (Id funcNm) [App (Id funcNm) [Id "__x"]]) (Id tB)
                in pure $ addInstanceLambda funcNm [tX, tB] composedLam env1
    -- Compose M(A, Y) from M(A, B) + M(B, Y)
    composeABY funcNm tA tB env1 (_, tY) =
        case lookupInstanceLambda funcNm [tA, tY] env1 of
            Just _ -> pure env1
            Nothing | tA == tY -> pure env1
            Nothing ->
                let composedLam = mkLambda funcNm [Var "__x" (Id tA) UNDEFINED]
                        (App (Id funcNm) [App (Id funcNm) [Id "__x"]]) (Id tY)
                in pure $ addInstanceLambda funcNm [tA, tY] composedLam env1

-- Validate that a parent instance has all required functions
validateParentInstance :: Environment -> [Name] -> Name -> SourceInfo -> IntState ()
validateParentInstance env typeNames parentName si = do
    case lookupType parentName env of
        Just (Structure parentLam _) -> case body parentLam of
            DeclBlock exs -> do
                let funcNames = [lamName l | Function l <- exs]
                                ++ [name v | Value v _ <- exs]
                let missing = Prelude.filter (\fn -> isNothing (lookupInstanceLambda fn typeNames env)) funcNames
                unless (Prelude.null missing) $
                    logWarning (mkLogPayload si
                        ("instance " ++ parentName ++ "(" ++ intercalate ", " typeNames
                            ++ ") incomplete: missing " ++ show missing ++ "\n"))
            _ -> pure ()
        _ -> pure ()

-- Validate that required structures exist in the environment
validateRequires :: Environment -> [Expr] -> SourceInfo -> IntState ()
validateRequires _   []   _  = pure ()
validateRequires env reqs si = mapM_ checkReq reqs
  where
    checkReq ref = do
        let reqName = extractStructRefName ref
        case lookupType reqName env of
            Just (Structure _ _) -> pure ()
            _ -> logWarning (mkLogPayload si
                    ("requires: structure " ++ reqName ++ " not found in environment\n"))

-- Validate requires constraints for instance declarations.
-- Checks that required structure instances actually exist for the concrete types.
-- Also checks the parent structure's requires constraints (substituting type params).
validateRequiresForInstance :: Environment -> [Expr] -> [Name] -> String -> SourceInfo -> IntState ()
validateRequiresForInstance env reqs typeNames structName si = do
    -- 1. Check explicit requires on the instance itself
    mapM_ (checkRequiredInstance env typeNames) reqs
    -- 2. Check the parent structure's requires (with type param substitution)
    case lookupType structName env of
        Just (Structure structLam sinfo) -> do
            let structParamNames = [name v | v <- params structLam]
            let subst = Prelude.zip structParamNames typeNames
            mapM_ (checkRequiredInstanceSubst env subst) (structRequires sinfo)
        _ -> pure ()
  where
    -- Check a requires constraint that already has concrete types
    checkRequiredInstance env' tNames ref = do
        let reqStructName = extractStructRefName ref
        let reqTypeArgs = extractStructRefArgs ref
        -- Only check if all type args are concrete (start with uppercase)
        let concreteArgs = [nm | Id nm <- reqTypeArgs, not (Prelude.null nm), nm `headIsUpper` True]
                        ++ [nm | App (Id nm) _ <- reqTypeArgs, not (Prelude.null nm), nm `headIsUpper` True]
        -- Try to resolve type args: substitute from instance types if they're variables
        let resolvedArgs = if Prelude.null reqTypeArgs then tNames
                          else [resolveArg nm | nm <- [extractArgName a | a <- reqTypeArgs]]
        let concreteResolved = Prelude.filter (\nm -> not (Prelude.null nm) && headIsUpper' nm) resolvedArgs
        unless (Prelude.null concreteResolved) $
            checkInstanceExists env' reqStructName concreteResolved
    -- Check a requires constraint with type variable substitution
    checkRequiredInstanceSubst env' subst ref = do
        let reqStructName = extractStructRefName ref
        let reqTypeArgs = extractStructRefArgs ref
        let resolvedArgs = [substName subst (extractArgName a) | a <- reqTypeArgs]
        let concreteResolved = Prelude.filter (\nm -> not (Prelude.null nm) && headIsUpper' nm) resolvedArgs
        unless (Prelude.null concreteResolved) $
            checkInstanceExists env' reqStructName concreteResolved
    -- Look up the required structure, find a representative function, check instance exists
    checkInstanceExists env' reqStructName reqTypes = do
        case lookupType reqStructName env' of
            Just (Structure structLam _) -> case body structLam of
                DeclBlock exs -> do
                    let funcNames = [lamName l | Function l <- exs]
                                 ++ [name v | Value v _ <- exs]
                    case funcNames of
                        (fn:_) -> case lookupInstanceLambda fn reqTypes env' of
                            Just _ -> pure ()
                            Nothing -> logWarning (mkLogPayload si
                                ("requires: instance " ++ reqStructName ++ "("
                                    ++ intercalate ", " reqTypes
                                    ++ ") not found (needed by " ++ structName ++ ")\n"))
                        [] -> pure ()  -- structure has no functions, nothing to check
                _ -> pure ()
            _ -> pure ()
    -- Extract argument name from a requires expression
    extractArgName (Id nm) = nm
    extractArgName (App (Id nm) _) = nm
    extractArgName _ = ""
    -- Substitute type variable name using the param→concrete mapping
    substName subst nm = case Prelude.lookup nm subst of
        Just concrete -> concrete
        Nothing -> nm  -- not a type variable, keep as-is
    -- Resolve a requires arg name against instance types (for simple cases)
    resolveArg nm = nm
    -- Check if name starts with uppercase
    headIsUpper' nm = not (Prelude.null nm) && Prelude.head nm >= 'A' && Prelude.head nm <= 'Z'
    headIsUpper nm _ = headIsUpper' nm

-- Extract type arguments from a structure reference expression
extractStructRefArgs :: Expr -> [Expr]
extractStructRefArgs (App (Id _) args) = args
extractStructRefArgs _ = []

--------------------------------------------------------------------------------
-- PASS 1.5: Record Desugaring
-- Runs after environment building (Pass 1), before case optimization (Pass 2).
-- Converts RecordConstruct, RecordUpdate, RecordPattern into core forms.
--------------------------------------------------------------------------------

recordDesugarPass :: IntState ()
recordDesugarPass = do
    s <- get
    let env = currentEnvironment s
    -- Desugar in parsed module
    let mod' = Prelude.map (\(e, si) -> (desugarRecordExpr env e, si)) (parsedModule s)
    -- Desugar in topLambdas and instanceLambdas
    let env' = transformLambdaMaps (desugarRecordLambda env) env
    put s { parsedModule = mod', currentEnvironment = env' }
    verboseLog $ "  Pass 1.5: record desugaring complete"

desugarRecordLambda :: Environment -> Lambda -> Lambda
desugarRecordLambda env lam = lam { body = desugarRecordExpr env (body lam) }

desugarRecordExpr :: Environment -> Expr -> Expr
desugarRecordExpr env expr = f $ traverseExprDeep f expr
  where

    f (RecordConstruct consName fields) =
        case lookupConstructor consName env of
            Just (cons, _i) ->
                let paramNames = Prelude.map name (params cons)
                    reordered = Prelude.map (\pn -> case Prelude.lookup pn fields of
                        Just e  -> e
                        Nothing -> ERROR $ "Missing field " ++ pn ++ " in " ++ consName ++ " construction"
                        ) paramNames
                in App (Id consName) reordered
            Nothing -> ERROR $ "Constructor " ++ consName ++ " not found for record construction"

    f (RecordUpdate inner updFields) =
        case inferConstructorName env inner updFields of
            Just (consName, cons) ->
                let paramNames = Prelude.map name (params cons)
                    args = Prelude.map (\pn -> case Prelude.lookup pn updFields of
                        Just e  -> e
                        Nothing -> RecFieldAccess (pn, -1) inner
                        ) paramNames
                in App (Id consName) args
            Nothing -> ERROR "Cannot infer constructor for record update"

    f (RecordPattern consName fields) =
        case lookupConstructor consName env of
            Just (cons, _i) ->
                let paramNames = Prelude.map name (params cons)
                    pats = Prelude.map (\pn -> case Prelude.lookup pn fields of
                        Just p  -> p
                        Nothing -> Id "_"  -- wildcard for unspecified fields
                        ) paramNames
                in App (Id consName) pats
            Nothing -> ERROR $ "Constructor " ++ consName ++ " not found for record pattern"

    f e = e

-- Infer constructor name from an expression (for record update)
inferConstructorName :: Environment -> Expr -> [(Name, Expr)] -> Maybe (Name, Lambda)
inferConstructorName env (App (Id nm) _) _ =
    case lookupConstructor nm env of
        Just (cons, _) -> Just (nm, cons)
        Nothing -> Nothing
inferConstructorName env (Id nm) updFields =
    case lookupConstructor nm env of
        Just (cons, _) -> Just (nm, cons)
        Nothing ->
            -- nm is a variable, try to find constructor by update field names
            let fieldNames = Prelude.map fst updFields
            in findConstructorByFields env fieldNames
inferConstructorName env _ updFields =
    let fieldNames = Prelude.map fst updFields
    in findConstructorByFields env fieldNames

-- Find a constructor that has all the given field names
findConstructorByFields :: Environment -> [Name] -> Maybe (Name, Lambda)
findConstructorByFields env fieldNames =
    let allCons = Map.toList (constructors env)
        matches = [(consNm, lam) | (consNm, (lam, _)) <- allCons,
                    let pNames = Prelude.map name (params lam),
                    Prelude.all (`Prelude.elem` pNames) fieldNames]
    in case matches of
        [(consNm, lam)] -> Just (consNm, lam)  -- unique match
        _ -> Nothing  -- ambiguous or no match

-- Resolve a field name to its index in a constructor's params
-- Searches all constructors in the environment
resolveFieldIndex :: Environment -> Name -> Maybe (Name, Int)
resolveFieldIndex env fieldName =
    let allCons = Map.toList (constructors env)
        findField [] = Nothing
        findField ((consName, (lam, _)):rest) =
            case findFieldInParams 0 (params lam) of
                Just idx -> Just (consName, idx)
                Nothing  -> findField rest
        findFieldInParams _ [] = Nothing
        findFieldInParams i (v:vs)
            | name v == fieldName = Just i
            | otherwise = findFieldInParams (i+1) vs
    in findField allCons

-- Pass 2 (Case Optimization) is in src/CaseOptimization.hs

--------------------------------------------------------------------------------
-- PASS 3: Type checking
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 4: Further optimizations
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- PASS 5: Conversion to CLM
--------------------------------------------------------------------------------
lamToCLMPass :: IntState()
lamToCLMPass = do
    s <- get
    let env = currentEnvironment s
    let lambdas = topLambdas env
    let clms = Map.mapWithKey (\n l -> lambdaToCLMLambda env l) lambdas
    -- also convert instance lambdas to CLM
    let instLams = instanceLambdas env
    let clmInsts = Map.mapWithKey (\n l -> lambdaToCLMLambda env l) instLams
    -- Type-directed dispatch: wrap CLMIAP bodies with CLMTYPED for user functions
    -- that have a concrete return type. Only for topLambdas, not instanceLambdas.
    let clms' = Map.mapWithKey (\n clm -> maybeAddTypeHint env n clm) clms
    let env' = env { clmLambdas = clms', clmInstances = clmInsts }
    let s' = s {currentEnvironment = env'}
    put s'
    verboseLog $ "  Pass 4: converted " ++ show (Map.size clms') ++ " lambdas, "
        ++ show (Map.size clmInsts) ++ " instances to CLM"

-- | Wrap CLM body with CLMTYPED if the body is CLMIAP and the lambda has a
-- concrete return type. This enables type-directed dispatch at runtime.
-- Only applied to user functions (not structure/instance dispatch templates).
maybeAddTypeHint :: Environment -> Name -> CLMLam -> CLMLam
maybeAddTypeHint env lamName clm =
    case Map.lookup lamName (topLambdas env) of
        Just lam | not (hasImplicit lam) ->
            let retTy = lamType lam
                clmRetTy = exprToCLM env retTy
            in if isConcreteRetType clmRetTy
               then wrapClmBody clm clmRetTy
               else clm
        _ -> clm

-- | Check if a CLM return type expression is concrete (useful for dispatch)
isConcreteRetType :: CLMExpr -> Bool
isConcreteRetType (CLMID name) = not (Prelude.null name) && Prelude.head name >= 'A' && Prelude.head name <= 'Z'
isConcreteRetType (CLMAPP (CLMID name) _) = not (Prelude.null name) && Prelude.head name >= 'A' && Prelude.head name <= 'Z'
isConcreteRetType (CLMU _) = True
isConcreteRetType _ = False

-- | Wrap the body of a CLMLam with CLMTYPED if it's a CLMIAP
wrapClmBody :: CLMLam -> CLMExpr -> CLMLam
-- Wrap all CLMLam bodies that have CLMIAP
wrapClmBody (CLMLam vs bdy@(CLMIAP _ _)) retTy = CLMLam vs (CLMTYPED bdy retTy)
wrapClmBody clm _ = clm
    

varToCLMVar e v = (name v, exprToCLM e (val v))

varsToCLMVars e vs = Prelude.map (varToCLMVar e) vs

patternCheckToCLM :: Environment -> Expr -> CLMPatternCheck
patternCheckToCLM e (PatternGuard (PCheckTag ct) ex) = CLMCheckTag ct (exprToCLM e ex)
patternCheckToCLM e (PatternGuard (PCheckLit lit) ex) = CLMCheckLit lit (exprToCLM e ex)
patternCheckToCLM e ex = CLMCheckTag (ConsTag "ERROR" (-1)) (CLMERR ("Invalid pattern check: " ++ show ex) SourceInteractive)

exprToCLM :: Environment -> Expr -> CLMExpr
exprToCLM _ UNDEFINED = CLMEMPTY
exprToCLM env (Binding v) = CLMBIND (name v) (exprToCLM env $ val v)
exprToCLM env (Statements exs) = CLMPROG (Prelude.map (exprToCLM env) exs)
-- Class system: ClassName.new(args) → CLMNEW
exprToCLM env (App (RecFieldAccess ("new", _) (Id className)) args)
    | Map.member className (classDecls env) =
        CLMNEW className (Prelude.map (exprToCLM env) args)
-- Class system: super.method(args) → CLMSCALL
exprToCLM env (App (RecFieldAccess (methodName, _) (Id "super")) args) =
    CLMSCALL (CLMID "self") methodName (Prelude.map (exprToCLM env) args)
-- Class system: obj.method(args) → CLMMCALL when obj is known class type
-- We check if the receiver is an Id whose type is a class, or try field access lookup
exprToCLM env (App (RecFieldAccess (memberName, _) objExpr) args)
    | Just className <- inferClassFromExpr env objExpr
    , Just cm <- lookupClass className env
    , Map.member memberName (cmMethods cm) =
        CLMMCALL (exprToCLM env objExpr) memberName (Prelude.map (exprToCLM env) args)
    | Just className <- inferClassFromExpr env objExpr
    , Just cm <- lookupClass className env
    , Map.member memberName (cmStaticMethods cm) =
        CLMAPP (CLMID (className ++ "$" ++ memberName)) (Prelude.map (exprToCLM env) args)
-- Named field access: resolve name to index at compile time if possible
exprToCLM env (RecFieldAccess (nm, -1) e) | nm /= "" =
    let clmE = exprToCLM env e
    in  -- Class field access: resolve from class field indices
        case inferClassFromExpr env e of
            Just className | Just cm <- lookupClass className env
                           , Just idx <- Map.lookup nm (cmFieldIndices cm)
                -> CLMFieldAccess (nm, idx) clmE
            _ -> case resolveFieldIndex env nm of
                    Just (_, idx) -> CLMFieldAccess (nm, idx) clmE
                    Nothing       -> CLMFieldAccess (nm, -1) clmE  -- defer to interpreter
exprToCLM env (RecFieldAccess ac e) = CLMFieldAccess ac (exprToCLM env e)
exprToCLM env (ExpandedCase cases ex si) = CLMCASE (Prelude.map (patternCheckToCLM env) cases) (exprToCLM env ex)
exprToCLM env Intrinsic = CLMPRIMCALL
exprToCLM env Derive = CLMPRIMCALL
exprToCLM env (ConTuple cs exs) = CLMCON cs (Prelude.map (exprToCLM env) exs)
-- have to check for a case when Id in fact refers to an 0-arg constructor call
-- since we need to change it for a corresponding tuple
-- also check for nullary implicit-param functions (e.g., value declarations)
exprToCLM env (Id n) =
    case (lookupConstructor n env) of
        Just (cons, i) ->
            if ((params cons) == [] )
            then CLMCON (ConsTag n i) []
            else CLMID n
        Nothing -> case lookupLambda n env of
            Just fun | hasImplicit fun -> CLMIAP (CLMID n) []
            _ -> CLMID n
-- application of func or cons to an expression: doing a bunch of checks
-- while converting
exprToCLM env e@(App (Id nm) exs) = 
    let newArgs = Prelude.map (exprToCLM env) exs
        mcons = lookupConstructor nm env
    in  case mcons of
            Just (cons, i) -> 
                  if (Prelude.length (params cons) /= (Prelude.length newArgs) )
                  then CLMERR ("[CLM] wrong number of arguments in constructor application: " ++ show e) SourceInteractive
                  else CLMCON (ConsTag nm i) newArgs
            Nothing -> 
                let mfun = lookupLambda nm env
                in  case mfun of
                        Just fun -> 
                            if (hasImplicit fun)
                            then CLMIAP (CLMID nm) newArgs
                            else
                                if (Prelude.length (params fun) > (Prelude.length newArgs) )
                                then CLMPAP (CLMID nm) newArgs
                                else if (Prelude.length (params fun) == (Prelude.length newArgs) )
                                    then CLMAPP (CLMID nm) newArgs
                                    else CLMERR ("[CLM] function is given more arguments than it can handle: " ++ show e) SourceInteractive
                        Nothing -> CLMAPP (CLMID nm) newArgs
            
    
exprToCLM env (App ex exs) = CLMAPP (exprToCLM env ex) (Prelude.map (exprToCLM env) exs)
exprToCLM env (Function lam) = CLMLAM $ lambdaToCLMLambda env lam
exprToCLM env (Lit l) = CLMLIT l
exprToCLM _ (U l) = CLMU l
-- ReprCast: resolve direction based on repr map
-- If targetType is a user type (key in reprMap) → use fromRepr
-- If targetType is a repr type (value in reprMap) → use toRepr
-- Uses mkReprKey for parameterized type support (e.g., Array(Byte) as PackedBytes)
exprToCLM env (ReprCast e tp) =
    let reprKey = mkReprKey tp
        simpleName = case tp of { Id nm -> nm; App (Id nm) _ -> nm; _ -> "" }
        clmE = exprToCLM env e
    in if Map.member reprKey (reprMap env)
       then CLMIAP (CLMID "fromRepr") [clmE]  -- target is user type, convert from repr
       else if isReprTarget reprKey env || isReprTarget simpleName env
            then CLMIAP (CLMID "toRepr") [clmE]  -- target is repr type, convert to repr
            else if not (Prelude.null simpleName) && Map.member simpleName (classDecls env)
                 then CLMIAP (CLMID "__downcast") [CLMLIT (LString simpleName), clmE]
                 -- fallback: try simple name in reprMap (backward compat)
                 else if Map.member simpleName (reprMap env)
                      then CLMIAP (CLMID "fromRepr") [clmE]
                      else CLMERR ("[CLM] no repr or class found for cast to " ++ show tp) SourceInteractive
exprToCLM env (Typed e _) = exprToCLM env e  -- type annotation, erased at runtime
exprToCLM _ (Pi _ _ _) = CLMEMPTY  -- type-level Pi, erased at runtime
exprToCLM _ (Implicit _) = CLMEMPTY     -- type-level wrapper, erased at runtime
-- NTuple: unified tuple/record literal. Convert to CLMCON at CLM level.
-- Named tuples (records) get "__Record" tag; positional tuples get "__Tuple" tag.
exprToCLM env (NTuple fields) =
    let values = Prelude.map (\(_mn, e) -> exprToCLM env e) fields
        tag = if hasNamedFields fields then ConsTag "__Record" 0 else ConsTag "__Tuple" 0
    in CLMCON tag values
-- Record type: type-level, erased at runtime
exprToCLM _ (RecordType _ _) = CLMEMPTY
-- Module system nodes: erased at runtime
exprToCLM _ (ModuleDecl _) = CLMEMPTY
exprToCLM _ (Import _ _ _) = CLMEMPTY
exprToCLM _ (Open _) = CLMEMPTY
exprToCLM _ (Export _ _) = CLMEMPTY
exprToCLM env (PrivateDecl e) = exprToCLM env e
exprToCLM _ (OpaqueTy _ _) = CLMEMPTY
exprToCLM _ (TargetBlock _ _) = CLMEMPTY
exprToCLM _ (TargetSwitch _) = CLMEMPTY
-- Array literal
exprToCLM env (ArrayLit exs) = CLMARRAY (Prelude.map (exprToCLM env) exs)
-- Record system nodes: desugar inline if they reach CLM conversion
exprToCLM env e@(RecordConstruct _ _) = exprToCLM env (desugarRecordExpr env e)
exprToCLM env e@(RecordUpdate _ _) = exprToCLM env (desugarRecordExpr env e)
exprToCLM env e@(RecordPattern _ _) = exprToCLM env (desugarRecordExpr env e)
-- Effect system nodes: erased at runtime
exprToCLM _ (EffectDecl _ _ _) = CLMEMPTY
exprToCLM _ (HandlerDecl _ _ _ _ _) = CLMEMPTY
exprToCLM env (HandleWith bodyExpr handlerExpr) =
    case resolveHandlerExpr env handlerExpr of
        Just (effName, lets, ops) ->
            let clmBody = exprToCLM env bodyExpr
                clmOps = [handlerOpToCLM env l | Function l <- ops]
                clmLets = [(lamName l, exprToCLM env (body l)) | Function l <- lets, body l /= UNDEFINED]
            in CLMHANDLE clmBody effName clmLets clmOps
        Nothing -> exprToCLM env bodyExpr  -- unknown handler, just run body
-- Handler override at call site: func [SilentConsole, MockFileIO] (args)
-- Desugars to nested CLMHANDLE wrapping the inner call
exprToCLM env (OverrideApp func overrides args) =
    let -- Resolve each override to a handler expression
        handlerOverrides = resolveOverrides env overrides
        -- Build the inner call as a normal App
        innerCall = exprToCLM env (App func args)
    in Prelude.foldr (wrapWithHandler env) innerCall handlerOverrides
exprToCLM env (ActionBlock stmts) = exprToCLM env (desugarActionStmts stmts)  -- should be desugared by now
exprToCLM _ (EffType _ _) = CLMEMPTY  -- type-level, erased at runtime
exprToCLM _ (DeclBlock _) = CLMEMPTY  -- structure member list, should not reach CLM
-- Class system: ClassDecl is processed in Pass 1, erased at CLM level
exprToCLM _ (ClassDecl _ _) = CLMEMPTY
exprToCLM _ e = CLMERR ("[CLM] cannot convert expr to CLM: " ++ show e) SourceInteractive

-- | Resolve a handler expression to (effectName, letBindings, functionImpls)
-- Uses the effect declaration to distinguish ops from let bindings:
-- any function whose name is in the effect's op list is an op; everything else is a let.
resolveHandlerExpr :: Environment -> Expr -> Maybe (Name, [Expr], [Expr])
resolveHandlerExpr env (Id handlerName) =
    case Map.lookup handlerName (effectHandlers env) of
        Just (effName, _isDef, _hParams, impls) ->
            let opNames = effectOpNames env effName
                (ops, lets) = Data.List.partition (isEffectOp opNames) impls
            in Just (effName, lets, ops)
        Nothing -> Nothing
resolveHandlerExpr env (App (Id handlerName) args) =
    case Map.lookup handlerName (effectHandlers env) of
        Just (effName, _isDef, hParams, impls) ->
            -- Substitute handler params with provided args in all impl bodies
            let paramNames = Prelude.map Surface.name hParams
                substPairs = Prelude.zip paramNames args
                substFn = Prelude.foldl (\acc (pn, arg) -> traverseExpr (substId pn arg) . acc) id substPairs
                impls' = Prelude.map substFn impls
                opNames = effectOpNames env effName
                (ops, lets) = Data.List.partition (isEffectOp opNames) impls'
            in Just (effName, lets, ops)
        Nothing -> Nothing
resolveHandlerExpr _ _ = Nothing

-- | Get the operation names declared by an effect
effectOpNames :: Environment -> Name -> [Name]
effectOpNames env effName =
    case Map.lookup effName (effectDecls env) of
        Just (_params, ops) -> Prelude.map lamName ops
        Nothing -> []

-- | Check if an expression is an effect op (by name matching)
isEffectOp :: [Name] -> Expr -> Bool
isEffectOp opNames (Function lam) = lamName lam `Prelude.elem` opNames
isEffectOp _ _ = False

-- | Convert a handler op lambda to CLM. Intrinsic ops become CLMPRIMCALL (passthrough).
-- Non-intrinsic ops become full CLMLAMs.
handlerOpToCLM :: Environment -> Lambda -> (Name, CLMExpr)
handlerOpToCLM _ lam | body lam == Intrinsic = (lamName lam, CLMPRIMCALL)
handlerOpToCLM _ lam | body lam == UNDEFINED = (lamName lam, CLMPRIMCALL)
handlerOpToCLM env lam =
    let clmLam = lambdaToCLMLambda env lam
    in (lamName lam, CLMLAM clmLam)

-- | Resolve bracket overrides to handler expressions.
-- Named overrides (Just "Console", expr) are used directly.
-- Positional overrides (Nothing, expr) are resolved by looking up the handler name.
-- Returns list of handler expressions (each will be wrapped as CLMHANDLE).
resolveOverrides :: Environment -> [(Maybe Name, Expr)] -> [Expr]
resolveOverrides env = Prelude.concatMap resolve
  where
    resolve (Just _effName, handlerExpr) = [handlerExpr]  -- named: use directly
    resolve (Nothing, Id "_") = []  -- wildcard: skip (type inference placeholder)
    resolve (Nothing, handlerExpr) =
        -- Check if it's a known handler
        case handlerExpr of
            Id nm | Map.member nm (effectHandlers env) -> [handlerExpr]
            App (Id nm) _ | Map.member nm (effectHandlers env) -> [handlerExpr]
            _ -> []  -- not a handler — assume it's a type arg, skip

-- | Wrap a CLM expression in a CLMHANDLE for a handler override
wrapWithHandler :: Environment -> Expr -> CLMExpr -> CLMExpr
wrapWithHandler env handlerExpr clmBody =
    case resolveHandlerExpr env handlerExpr of
        Just (effName, lets, ops) ->
            let clmOps = [handlerOpToCLM env l | Function l <- ops]
                clmLets = [(lamName l, exprToCLM env (body l)) | Function l <- lets, body l /= UNDEFINED]
            in CLMHANDLE clmBody effName clmLets clmOps
        Nothing -> clmBody  -- couldn't resolve handler, just pass through

-- | Substitute an Id name with a replacement expression
substId :: Name -> Expr -> Expr -> Expr
substId nm replacement (Id n) | n == nm = replacement
substId _ _ e = e

-- | Try to infer the class name from an expression (for method/field resolution)
inferClassFromExpr :: Environment -> Expr -> Maybe Name
inferClassFromExpr env (Id nm) =
    -- Check if nm is a binding whose type is a class
    case lookupLambda nm env of
        Just lam -> case lamType lam of
            Id typNm | Map.member typNm (classDecls env) -> Just typNm
            _ -> Nothing
        Nothing -> Nothing  -- can't determine from a bare Id without type info
inferClassFromExpr env (App (Id nm) _)
    | Map.member nm (classDecls env) = Just nm  -- constructor call e.g. Dog.new(...)
inferClassFromExpr _ _ = Nothing

lambdaToCLMLambda :: Environment -> Lambda -> CLMLam
lambdaToCLMLambda env (Lambda nm params Intrinsic tp _ _) =
    CLMLam [] CLMPRIMCALL
lambdaToCLMLambda env (Lambda nm params Derive tp _ _) =
    CLMLam [] CLMPRIMCALL
lambdaToCLMLambda env (Lambda nm params (PatternMatches exs) tp _ _) =
    CLMLamCases (varsToCLMVars env params) (Prelude.map (exprToCLM env) exs)
lambdaToCLMLambda env (Lambda nm params body tp _ _) =
    CLMLam (varsToCLMVars env params) (exprToCLM env body)

--------------------------------------------------------------------------------
-- PASS 6: Compilation - JS
--------------------------------------------------------------------------------
compile2JSpass :: IntState()
compile2JSpass = do
    s <- get
    let lambdas = topLambdas $ currentEnvironment s
    let allTypes = types $ currentEnvironment s
    funcsJS <- (traverseWithKey g lambdas)
    typesJS <- (traverseWithKey f allTypes)
    let prog = typesJS `union` funcsJS
    put s{currentEnvironment = (currentEnvironment s) {outProgram = prog} }
    return ()
    where f k expr = pure $ compileExprToJS' expr
          g k lam  = pure $ (compileFunctionToJS "") lam


mkConsName :: String -> String -> String
mkConsName typName consName = "cons_" ++ typName ++ "_" ++ consName

compileExprToJS' :: Expr -> String
compileExprToJS' e = intercalate ("\n" :: String) (compileExprToJS e) 

compileExprToJS :: Expr -> [String]
compileExprToJS (SumType lam@(Lambda typName typArgs (Constructors cons) typTyp _ _)) =
    imap (compileConstructorToJS ("cons_" ++ typName ++ "_") ) cons
compileExprToJS (Function lam) = [compileFunctionToJS "" lam]
compileExprToJS e = ["/* NOT SUPPORTED:\n" ++ ppr e ++ "\n*/"]

argsToString :: Record -> String
argsToString args = showListRoBr f args
    where f (Var nm tp vl) = nm

argsToTupleFields :: Record -> String
argsToTupleFields args = showListPlainSep f ", " args
    where f (Var nm tp vl) = nm ++ ": " ++ nm

-- taking out prefix in names for now
compileConstructorToJS :: String -> Int -> Lambda -> String
compileConstructorToJS pref i (Lambda nm args ex tp _ _) = "function " ++nm ++
                         argsToString args 
                         ++ " { return { __consTag: " ++ (show i) ++ ", "
                         ++ argsToTupleFields args ++ " } } "                         
    
compileFunctionToJS :: String -> Lambda -> String
compileFunctionToJS pref lam@(Lambda nm args ex tp _ _) =
    if (isLambdaConstructor lam) then ""
    else "function " ++ pref++nm ++ argsToString args ++ funBodyToString ex

-- compiling pattern matches is the most complicated thing as we need to 
-- consult the environment about the order of constructors etc
funBodyToString :: Expr -> String
funBodyToString (Id x) = "{ return " ++ x ++ "; }"
funBodyToString (App ex exs) = "{ return " 
    ++ (exprOnlyToString ex) 
    ++ showListRoBr exprOnlyToString exs
    ++ "; }"
funBodyToString (PatternMatches cs) = "{\n" ++ 
    (showListPlainSep exprOnlyToString ";\n" cs) ++ ";\n}"
funBodyToString e = " { /* NOT IMPLEMENTED:\n" ++ ppr e ++ "\n*/ }" 

exprOnlyToString :: Expr -> String
exprOnlyToString (Id x) = x
exprOnlyToString (App ex exs) = exprOnlyToString ex ++ showListRoBr exprOnlyToString exs
exprOnlyToString (CaseOf args ex _) = caseToIf args ++ " return " 
    ++ exprOnlyToString ex
    where caseToIf vars = "if (" ++ (showListPlainSep ff " and " vars)
            ++ " )"
          ff (Var nm tp val) = "(" ++ nm ++ " == " ++ exprOnlyToString val ++ ")"
          
exprOnlyToString e = "/* NOT IMPLEMENTED: " ++ ppr e ++ "*/"

    
