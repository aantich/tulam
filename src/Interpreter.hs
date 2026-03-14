{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Interpreter
where

import Surface
import CLM
import Logs (SourceInfo(..))
import State
import Pipeline
import Util.PrettyPrinting

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.HashMap.Strict as Map

import Data.Traversable.WithIndex
import Data.Maybe (isJust, catMaybes)
import Data.List (intercalate, nub)
import Control.Monad (filterM, foldM)
import Data.Foldable (foldrM)
import Intrinsics (lookupIntrinsic, boolToCLM)
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef
import Data.Char (chr)
import Data.Word (Word8)
import Data.Bits ((.&.), (.|.), shiftL)



addBinding :: Expr -> IntState ()
addBinding ex@(Binding _) = do
    s <- get
    let env = currentEnvironment s
    -- ex' <- fixEmptyConstructors ex
    let clmex@(CLMBIND nm expr) = exprToCLM env ex
    let bnd = Map.insert nm (nm, expr) (clmBindings env)
    let s' = s { currentEnvironment = env { clmBindings = bnd } }
    liftIO $ putStrLn $ "Added binding " ++ ppr ex
    put s'
addBinding e = liftIO $ putStrLn $ "Cant add binding for expr " ++ ppr e

-- | Install default handlers for all effects that have a default handler declared.
-- Default handlers are appended to the BOTTOM of the handler stack so that
-- explicit handle...with blocks (pushed on top) override them.
installDefaultHandlers :: IntState ()
installDefaultHandlers = do
    s <- get
    let env = currentEnvironment s
    let defaults = Map.toList (defaultHandlers env)
    newEntries <- catMaybes <$> mapM (buildDefaultEntry env) defaults
    let stack = handlerStack s
    put $ s { handlerStack = stack ++ newEntries
            , defaultHandlerCount = Prelude.length newEntries }
  where
    buildDefaultEntry env (effName, handlerName) =
        case Map.lookup handlerName (effectHandlers env) of
            Just (_eff, _isDef, hParams, impls) | Prelude.null hParams -> do
                -- Parameterless handler: build op map from implementations
                let opNames = effectOpNames env effName
                let ops = [(lamName l, opToCLM env l) | Function l <- impls, lamName l `elem` opNames]
                -- Evaluate let bindings (for handlers like RefState — but default should be parameterless)
                let lets = [(lamName l, exprToCLM env (body l)) | Function l <- impls, lamName l `notElem` opNames, body l /= UNDEFINED]
                -- For now we skip let evaluation since default handlers should be simple
                let opMap = Map.fromList ops
                pure $ Just (effName, opMap)
            _ -> pure Nothing  -- parameterized or not found — skip
    opToCLM _env lam | body lam == Intrinsic = CLMPRIMCALL
    opToCLM _env lam | body lam == UNDEFINED = CLMPRIMCALL
    opToCLM env lam = CLMLAM (lambdaToCLMLambda env lam)

processInteractive :: Expr -> IntState ()
processInteractive ex0 = do
    s <- get
    -- apply afterparse desugaring (binary ops -> App calls etc)
    -- then string literal desugaring (if enabled)
    -- then action block desugaring (ActionBlock -> bind chains)
    let ns = newStrings (currentFlags s)
    let desugarStr = if ns then desugarStringLiterals else id
    let ex = desugarActions . desugarStr . afterparse $ traverseExpr afterparse ex0
    let env = currentEnvironment s
    let clmex = exprToCLM env ex
    liftIO $ putStrLn $ "CLM:\n" ++ ppr clmex
    case clmex of
        CLMID nm -> do
            liftIO $ putStrLn $ "Looking up id " ++ show nm
            let mid = Map.lookup nm (clmBindings env)
            case mid of
                Nothing -> liftIO $ putStrLn "Nothing found"
                Just v@(nm,ex)  -> liftIO $ putStrLn $ ppr (CLMBIND nm ex)
        ex1@(CLMAPP _ _) -> do
            ex1' <- evalCLM 0 ex1
            exf <- _contEval 1 ex1 ex1'
            case exf of
                CLMERR _ _ -> liftIO $ putStrLn $ ppr exf
                _ -> do
                    liftIO $ putStrLn "Final result:"
                    liftIO $ putStrLn $ ppr exf
        ex1@(CLMIAP _ _) -> do
            ex1' <- evalCLM 0 ex1
            exf <- _contEval 1 ex1 ex1'
            case exf of
                CLMERR _ _ -> liftIO $ putStrLn $ ppr exf
                _ -> do
                    liftIO $ putStrLn "Final result:"
                    liftIO $ putStrLn $ ppr exf
        ex1@(CLMCON _ _) -> do
            liftIO $ putStrLn $ ppr ex1
        ex1@(CLMLIT _) -> do
            liftIO $ putStrLn $ ppr ex1
        ex1@(CLMLAM _) -> do
            liftIO $ putStrLn $ ppr ex1
        _ -> liftIO $ putStrLn "Not implemented yet"

_contEval :: Int -> CLMExpr -> CLMExpr -> IntState CLMExpr
_contEval i e1 e2
    | e1 == e2 = pure e1
    | i >= maxEvalIterations = do
        trace $ "ERROR: eval did not converge after " ++ show maxEvalIterations ++ " iterations"
        pure $ CLMERR ("[RT] Evaluation did not converge after " ++ show maxEvalIterations
            ++ " iterations. Last: " ++ pprSummary 80 e2) SourceInteractive
    | otherwise = do
        e' <- evalCLM 0 e2
        _contEval (i+1) e2 e'

traceExpr :: Expr -> IntState()
traceExpr ex = do
    s <- get
    let env = currentEnvironment s
    let clmex = exprToCLM env ex
    liftIO $ putStrLn $ "CLM:\n" ++ show clmex

{-
CLMEMPTY
  | CLMERR String
  | CLMID Name
  | CLMBIND Name CLMExpr
  | CLMAPP CLMExpr [CLMExpr] -- saturated application first expr to the tuple of exprs
  | CLMPAP CLMExpr [CLMExpr] -- partial application (When we know the types!)
  | CLMCON ConsTag [CLMExpr] -- saturated constructor application, value in a sense
  | CLMFieldAccess (Name, Int) CLMExpr -- accessing a field of an expr by name or number
  | CLMCASE [CLMConsTagCheck] CLMExpr -- list of constructor checks that must all fold to True bound to an expr
  | CLMPROG [CLMExpr] -- list of expressions, for now used for Action but needs to change
  | CLMTYPED CLMExpr CLMExpr -- in case we want to give a type to an expression
  | CLMPRIMCALL -- body of the function that is a primitive call
-}

evalCLM :: Int -> CLMExpr -> IntState CLMExpr
-- depth limit guard
evalCLM i _ | i >= maxEvalDepth =
    pure $ CLMERR ("[RT] Max eval depth (" ++ show maxEvalDepth ++ ") exceeded") SourceInteractive
-- CLMERR propagation: errors are values
evalCLM _ e@(CLMERR _ _) = pure e
evalCLM i (CLMID nm) = do
    s <- get
    let env = currentEnvironment s
    trace $ "[depth=" ++ show i ++ "] CLMID " ++ nm
    case (lookupCLMBindingOrLambda env nm) of
        Nothing -> do
            trace $ "  not found, returning id"
            pure $ CLMID nm
        Just ex -> do
            trace $ "  => " ++ pprSummary 60 ex
            pure ex
evalCLM i e@(CLMCON (ConsTag nm k) exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMCON " ++ nm
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    -- propagate errors from constructor args
    case findCLMErr exs' of
        Just err -> pure err
        Nothing  -> pure $ CLMCON (ConsTag nm k) exs'
-- Class method dispatch: CLMAPP (CLMFieldAccess ("method",-1) obj) args
-- When field access fails on a CLMCON, try class method dispatch
evalCLM i e@(CLMAPP (CLMFieldAccess (nm, _) objExpr) argExprs) = do
    trace $ "[depth=" ++ show i ++ "] CLMAPP(field." ++ nm ++ ") " ++ pprSummary 60 e
    obj <- evalCLM (i+1) objExpr
    case obj of
        CLMCON (ConsTag className _) _ -> do
            s <- get
            let env = currentEnvironment s
            case lookupClass className env of
                Just cm -> case Map.lookup nm (cmMethods cm) of
                    Just methodLam -> do
                        args <- imapM (\j ex -> evalCLM (i+j+1) ex) argExprs
                        case findCLMErr args of
                            Just err -> pure err
                            Nothing -> do
                                trace $ "  class dispatch " ++ className ++ "." ++ nm
                                evalCLM (i+1) $ applyCLMLam methodLam (obj : args)
                    Nothing -> do
                        -- Not a method — try as field access then apply
                        let fieldResult = evalFieldAccess nm className obj env
                        case fieldResult of
                            Just val -> do
                                args <- imapM (\j ex -> evalCLM (i+j+1) ex) argExprs
                                case val of
                                    CLMLAM lam -> evalCLM (i+1) $ applyCLMLam lam args
                                    _ -> pure $ CLMAPP val args
                            Nothing -> pure $ CLMERR ("[RT] no field or method " ++ nm ++ " on " ++ className) SourceInteractive
                Nothing -> do
                    -- Not a class — fall through to normal field access + apply
                    let fa = CLMFieldAccess (nm, -1) obj
                    fa' <- evalCLM (i+1) fa
                    args <- imapM (\j ex -> evalCLM (i+j+1) ex) argExprs
                    case fa' of
                        CLMERR _ _ -> pure fa'
                        CLMLAM lam -> evalCLM (i+1) $ applyCLMLam lam args
                        _ -> pure $ CLMAPP fa' args
        _ -> do
            -- obj is not a CLMCON yet — evaluate field access normally
            let fa = CLMFieldAccess (nm, -1) obj
            fa' <- evalCLM (i+1) fa
            case fa' of
                CLMERR _ _ -> pure fa'
                _ -> do
                    args <- imapM (\j ex -> evalCLM (i+j+1) ex) argExprs
                    case fa' of
                        CLMLAM lam -> evalCLM (i+1) $ applyCLMLam lam args
                        _ -> if fa' == CLMFieldAccess (nm, -1) obj && args == argExprs
                             then pure e
                             else pure $ CLMAPP fa' args
evalCLM i e@(CLMAPP (CLMLAM lam) exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMAPP(lam) " ++ pprSummary 60 e
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    case findCLMErr exs' of
        Just err -> pure err
        Nothing  -> evalCLM (i+1) $ applyCLMLam lam exs'
evalCLM i e@(CLMAPP ex exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMAPP " ++ pprSummary 60 e
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    case findCLMErr exs' of
        Just err -> pure err
        Nothing -> do
            ex' <- evalCLM (i+(length exs)+1) ex
            case ex' of
                CLMERR _ _ -> pure ex'
                CLMLAM lam -> evalCLM (i+1) $ applyCLMLam lam exs'
                CLMID nm -> do
                    -- Try handler stack first, then IO intrinsic dispatch
                    mH <- dispatchHandlerOp nm exs'
                    case mH of
                        Just result -> pure result
                        Nothing -> do
                            mIO <- dispatchIOIntrinsic nm exs'
                            case mIO of
                                Just result -> pure result
                                Nothing -> do
                                    mRefl <- dispatchReflectionIntrinsic nm exs'
                                    case mRefl of
                                        Just result -> pure result
                                        Nothing -> if ex' == ex && exs' == exs then pure e
                                                   else pure $ CLMAPP ex' exs'
                _ -> if ex' == ex && exs' == exs then pure e
                     else pure $ CLMAPP ex' exs'
evalCLM i e@(CLMFieldAccess ("", fnum) (CLMCON ct tuple)) = do
    trace $ "[depth=" ++ show i ++ "] CLMFieldAccess ." ++ show fnum
    if (fnum >= Prelude.length tuple) then pure $ CLMERR ("[RT] tried to access field beyond tuple index in " ++ ppr e) SourceInteractive
    else pure (tuple Prelude.!! fnum)
-- named field access with resolved index
evalCLM i e@(CLMFieldAccess (nm, fnum) (CLMCON ct tuple)) | nm /= "" && fnum >= 0 = do
    trace $ "[depth=" ++ show i ++ "] CLMFieldAccess ." ++ nm ++ "(" ++ show fnum ++ ")"
    if (fnum >= Prelude.length tuple) then pure $ CLMERR ("[RT] field " ++ nm ++ " index out of bounds in " ++ ppr e) SourceInteractive
    else pure (tuple Prelude.!! fnum)
-- named field access with unresolved index: look up constructor from tag or class metadata
evalCLM i e@(CLMFieldAccess (nm, -1) (CLMCON (ConsTag consNm _) tuple)) | nm /= "" = do
    trace $ "[depth=" ++ show i ++ "] CLMFieldAccess(named) ." ++ nm ++ " on " ++ consNm
    s <- get
    let env = currentEnvironment s
    -- Try class field indices first (more reliable for class objects)
    case lookupClass consNm env of
        Just cm -> case Map.lookup nm (cmFieldIndices cm) of
            Just idx | idx < Prelude.length tuple -> pure (tuple Prelude.!! idx)
            Just _   -> pure $ CLMERR ("[RT] field " ++ nm ++ " index out of bounds in " ++ consNm) SourceInteractive
            Nothing  -> pure $ CLMERR ("[RT] field " ++ nm ++ " not found in class " ++ consNm) SourceInteractive
        Nothing ->
            -- Fall back to constructor lookup (for sum types/records)
            case lookupConstructor consNm env of
                Just (cons, _) ->
                    let paramNames = Prelude.map name (params cons)
                    in case Prelude.lookup nm (Prelude.zip paramNames [0..]) of
                        Just idx | idx < Prelude.length tuple -> pure (tuple Prelude.!! idx)
                        Just _   -> pure $ CLMERR ("[RT] field " ++ nm ++ " index out of bounds in " ++ consNm) SourceInteractive
                        Nothing  -> pure $ CLMERR ("[RT] field " ++ nm ++ " not found in constructor " ++ consNm) SourceInteractive
                Nothing -> pure $ CLMERR ("[RT] constructor " ++ consNm ++ " not found for field access") SourceInteractive
-- evaluate inner expression first, then retry field access
evalCLM i e@(CLMFieldAccess acc inner) = do
    trace $ "[depth=" ++ show i ++ "] CLMFieldAccess(eval) " ++ pprSummary 60 e
    inner' <- evalCLM (i+1) inner
    case inner' of
        CLMERR _ _ -> pure inner'
        _ -> if inner' == inner then pure e
             else evalCLM (i+1) (CLMFieldAccess acc inner')
-- literals are values
evalCLM _ e@(CLMLIT _) = pure e
-- arrays: evaluate each element
evalCLM i (CLMARRAY exs) = do
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    case findCLMErr exs' of
        Just err -> pure err
        Nothing  -> pure $ CLMARRAY exs'
-- CLMPROG: evaluate expressions in sequence, return last
evalCLM i (CLMPROG []) = pure CLMEMPTY
evalCLM i (CLMPROG [ex]) = evalCLM i ex
evalCLM i (CLMPROG (ex:exs)) = do
    r <- evalCLM i ex
    case r of
        CLMERR _ _ -> pure r
        _ -> evalCLM (i+1) (CLMPROG exs)
-- CLMCASE: evaluate constructor tag checks, return body if all match
evalCLM i e@(CLMCASE ctchecks body) = do
    trace $ "[depth=" ++ show i ++ "] CLMCASE " ++ pprSummary 60 e
    if evalPatternChecks ctchecks
    then evalCLM (i+1) body
    else pure e -- case didn't match, return as-is
-- CLMNEW: construct a class object (like CLMCON but with class validation)
evalCLM i (CLMNEW className argExprs) = do
    trace $ "[depth=" ++ show i ++ "] CLMNEW " ++ className
    args <- imapM (\j e -> evalCLM (i+j+1) e) argExprs
    case findCLMErr args of
        Just err -> pure err
        Nothing -> do
            s <- get
            let env = currentEnvironment s
            case lookupClass className env of
                Just cm
                    | cmModifier cm == ClassAbstract ->
                        pure $ CLMERR ("[RT] cannot instantiate abstract class " ++ className) SourceInteractive
                    | otherwise ->
                        pure $ CLMCON (ConsTag className (cmTag cm)) args
                Nothing ->
                    pure $ CLMERR ("[RT] class " ++ className ++ " not found") SourceInteractive
-- CLMMCALL: dynamic method dispatch via class hierarchy
evalCLM i (CLMMCALL objExpr methodName argExprs) = do
    trace $ "[depth=" ++ show i ++ "] CLMMCALL ." ++ methodName
    obj <- evalCLM (i+1) objExpr
    case obj of
        CLMERR _ _ -> pure obj
        _ -> do
            args <- imapM (\j e -> evalCLM (i+j+1) e) argExprs
            case findCLMErr args of
                Just err -> pure err
                Nothing -> case obj of
                    CLMCON (ConsTag className _) _ -> do
                        s <- get
                        let env = currentEnvironment s
                        case lookupClass className env of
                            Just cm -> case Map.lookup methodName (cmMethods cm) of
                                Just methodLam -> do
                                    trace $ "  dispatching " ++ className ++ "." ++ methodName
                                    let result = applyCLMLam methodLam (obj : args)
                                    evalCLM (i+1) result
                                Nothing ->
                                    pure $ CLMERR ("[RT] no method " ++ methodName ++ " on class " ++ className) SourceInteractive
                            Nothing ->
                                -- Not a class — fall back to field access + apply
                                pure $ CLMMCALL obj methodName args
                    _ -> pure $ CLMMCALL obj methodName args
-- CLMSCALL: super/parent method dispatch
evalCLM i (CLMSCALL objExpr methodName argExprs) = do
    trace $ "[depth=" ++ show i ++ "] CLMSCALL super." ++ methodName
    obj <- evalCLM (i+1) objExpr
    case obj of
        CLMERR _ _ -> pure obj
        _ -> do
            args <- imapM (\j e -> evalCLM (i+j+1) e) argExprs
            case findCLMErr args of
                Just err -> pure err
                Nothing -> case obj of
                    CLMCON (ConsTag className _) _ -> do
                        s <- get
                        let env = currentEnvironment s
                        case lookupParentMethod className methodName env of
                            Just parentLam -> do
                                trace $ "  super dispatching " ++ className ++ " -> parent." ++ methodName
                                evalCLM (i+1) $ applyCLMLam parentLam (obj : args)
                            Nothing ->
                                pure $ CLMERR ("[RT] no parent method " ++ methodName ++ " for class " ++ className) SourceInteractive
                    _ -> pure $ CLMERR ("[RT] super call on non-object: " ++ ppr obj) SourceInteractive
-- CLMTYPED wrapping CLMIAP: evaluate inner normally, then use type hint as FALLBACK
-- when normal dispatch fails. This handles return-type-directed dispatch
-- (e.g., toEnum(0) : Bool — arg type is Int, but we need Bool for dispatch).
evalCLM i (CLMTYPED inner@(CLMIAP (CLMID funcNm) _) typeHint) = do
    trace $ "[depth=" ++ show i ++ "] CLMTYPED-CLMIAP " ++ funcNm
    -- First, run normal dispatch
    result <- evalCLM i inner
    let hintType = extractTypeNameFromHint typeHint
    case result of
        -- Normal dispatch failed (returned CLMIAP unchanged with same function name)
        -- Only apply fallback when args are fully evaluated (all values)
        CLMIAP (CLMID fn) exs' | fn == funcNm && all isValue exs' ->
            case hintType of
                Just typeName -> do
                    trace $ "  type-directed fallback for " ++ funcNm ++ ", hint: " ++ typeName
                    s <- get
                    let env = currentEnvironment s
                    case lookupIntrinsic funcNm typeName of
                        Just intrinsicFn -> case intrinsicFn exs' of
                            Just r  -> pure r
                            Nothing -> dispatchInstance env funcNm [typeName] exs'
                        Nothing -> dispatchInstance env funcNm [typeName] exs'
                Nothing -> pure result
        -- Normal dispatch succeeded — check if result type matches hint
        _ -> do
            s2 <- get
            let env2 = currentEnvironment s2
            case hintType of
                Just typeName | isValue result && not (resultMatchesHint env2 result typeName) -> do
                    trace $ "  type-directed re-dispatch for " ++ funcNm ++ ", result type mismatch with " ++ typeName
                    -- Re-evaluate args and try hint-type dispatch
                    let rawExs = case inner of CLMIAP _ exs -> exs; _ -> []
                    exs' <- imapM (\j e -> evalCLM (i+j+1) e) rawExs
                    case findCLMErr exs' of
                        Just err -> pure err
                        Nothing -> do
                            s3 <- get
                            let env3 = currentEnvironment s3
                            case lookupIntrinsic funcNm typeName of
                                Just intrinsicFn -> case intrinsicFn exs' of
                                    Just r  -> pure r
                                    Nothing -> tryHintInstance env3 funcNm typeName exs' result
                                Nothing -> tryHintInstance env3 funcNm typeName exs' result
                _ -> pure result
-- CLMTYPED for non-CLMIAP: just evaluate inner, ignore type hint
evalCLM i (CLMTYPED inner _) = evalCLM i inner
-- CLMHANDLE: eval lets, substitute into ops, push onto stack, eval body, pop
evalCLM i (CLMHANDLE bdy effName lets ops) = do
    trace $ "[depth=" ++ show i ++ "] CLMHANDLE " ++ effName ++ " lets=" ++ show (Prelude.map fst lets)
    -- Evaluate let bindings first (e.g. let state = newRef(init))
    evalLets <- mapM (\(nm, expr) -> do
        val <- evalCLM (i+1) expr
        val' <- _contEval 1 expr val
        pure (nm, val')) lets
    -- Substitute let values into op implementations
    let substMap = Map.fromList evalLets
    let substOps = Prelude.map (\(nm, impl) -> (nm, simultBetaReduce substMap impl)) ops
    -- Also substitute into body in case it references let vars
    let substBody = simultBetaReduce substMap bdy
    -- Evaluate handler op implementations
    evalOps <- mapM (\(nm, impl) -> do
        impl' <- evalCLM (i+1) impl
        pure (nm, impl')) substOps
    let opMap = Map.fromList evalOps
    -- Push handler onto stack
    s <- get
    let oldStack = handlerStack s
    put $ s { handlerStack = (effName, opMap) : oldStack }
    -- Evaluate body with handler active
    result <- evalCLM (i+1) substBody
    result' <- _contEval 1 substBody result
    -- Pop handler (restore old stack)
    s2 <- get
    put $ s2 { handlerStack = oldStack }
    pure result'
-- CLMIAP: implicit param application (structure function dispatch)
evalCLM i e@(CLMIAP (CLMID funcNm) exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMIAP " ++ funcNm ++ " " ++ pprSummary 60 (CLMIAP (CLMID funcNm) exs)
    s <- get
    let env = currentEnvironment s
    -- evaluate arguments first
    exs' <- imapM (\j e1 -> evalCLM (i+j+1) e1) exs
    -- propagate errors from args
    case findCLMErr exs' of
        Just err -> pure err
        Nothing -> do
          -- CHECK HANDLER STACK first (dynamically-scoped effect handlers)
          mHandler <- dispatchHandlerOp funcNm exs'
          case mHandler of
            Just result -> pure result
            Nothing -> do
              -- CHECK EFFECT SEQUENCING (seq/bind fallback when handler stack active)
              mSeq <- dispatchEffectSequencing funcNm exs'
              case mSeq of
                Just result -> pure result
                Nothing -> do
                  -- CHECK IO INTRINSICS (effect operations: putStrLn, readLine, etc.)
                  mIO <- dispatchIOIntrinsic funcNm exs'
                  case mIO of
                    Just result -> pure result
                    Nothing -> do
                      -- TRY REF INTRINSICS (newRef, readRef, writeRef, modifyRef)
                      mRef <- dispatchRefIntrinsic funcNm exs'
                      case mRef of
                        Just result -> pure result
                        Nothing -> do
                          -- TRY MUTARRAY INTRINSICS (newMutArray, mutRead, mutWrite, etc.)
                          mMut <- dispatchMutArrayIntrinsic funcNm exs'
                          case mMut of
                            Just result -> pure result
                            Nothing -> do
                              -- TRY REFLECTION INTRINSICS (tag, tagName, arity, field, numConstructors, constructorByIndex)
                              mRefl <- dispatchReflectionIntrinsic funcNm exs'
                              case mRefl of
                                Just result -> pure result
                                Nothing -> do
                                  -- TRY DOWNCAST INTRINSIC (__downcast("ClassName", obj) -> Maybe)
                                  mDown <- dispatchDowncastIntrinsic funcNm exs'
                                  case mDown of
                                    Just result -> pure result
                                    Nothing -> do
                                      -- TRY ARRAY HOF DISPATCH (closure-aware)
                                      mHOF <- dispatchArrayHOF funcNm exs'
                                      case mHOF of
                                        Just result -> pure result
                                        Nothing ->
                                          -- For nullary implicit-param functions (e.g., value declarations),
                                          -- we can't infer type from args — try to find any instance
                                          if Prelude.null exs'
                                          then dispatchNullaryIAP env funcNm
                                          else do
                                            -- try multi-param key first (all arg types), then fall back to single-param
                                            let mTypeNames = inferTypeNames env exs'
                                            trace $ "  inferred types: " ++ show mTypeNames
                                            case mTypeNames of
                                                Just typeNames@(firstType:_) -> do
                                                    -- CHECK INTRINSICS FIRST (try all arg types, not just first)
                                                    let tryIntrinsic [] = Nothing
                                                        tryIntrinsic (t:ts) = case lookupIntrinsic funcNm t of
                                                            Just fn -> Just (fn, t)
                                                            Nothing -> tryIntrinsic ts
                                                    case tryIntrinsic (nub typeNames) of
                                                        Just (intrinsicFn, matchedType) -> case intrinsicFn exs' of
                                                            Just result -> do
                                                                trace $ "  intrinsic " ++ funcNm ++ " for " ++ matchedType ++ " => " ++ pprSummary 40 result
                                                                pure result
                                                            Nothing ->
                                                                if Prelude.length typeNames < Prelude.length exs'
                                                                then do
                                                                    trace $ "  intrinsic " ++ funcNm ++ " args not ready, deferring"
                                                                    pure $ CLMIAP (CLMID funcNm) exs'
                                                                else do
                                                                    trace $ "  intrinsic " ++ funcNm ++ " failed on args"
                                                                    pure $ CLMERR ("[RT] Intrinsic " ++ funcNm ++ " failed on args") SourceInteractive
                                                        Nothing -> dispatchInstance env funcNm typeNames exs'
                                                Nothing -> do
                                                    -- can't determine type, return with evaluated args
                                                    trace $ "  could not infer type for " ++ funcNm
                                                    pure $ CLMIAP (CLMID funcNm) exs'
-- CLMIAP with non-id function: evaluate the function first
evalCLM i e@(CLMIAP ex exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMIAP(eval func) " ++ pprSummary 60 e
    ex' <- evalCLM (i+1) ex
    case ex' of
        CLMERR _ _ -> pure ex'
        -- If the inner evaluated to a function ID, re-dispatch as proper CLMIAP
        CLMID nm -> evalCLM i (CLMIAP (CLMID nm) exs)
        _ -> do
            exs' <- imapM (\j e1 -> evalCLM (i+j+2) e1) exs
            case findCLMErr exs' of
                Just err -> pure err
                Nothing
                  -- Empty args: unwrap CLMIAP, return the value directly
                  | Prelude.null exs' -> pure ex'
                  -- Lambda: apply directly
                  | CLMLAM lam <- ex' -> pure $ applyCLMLam lam exs'
                  -- No change: return as-is to avoid loops
                  | ex' == ex && exs' == exs -> pure e
                  | otherwise -> pure $ CLMIAP ex' exs'
-- CLMPAP: partial application - evaluate and try to apply
evalCLM i e@(CLMPAP ex exs) = do
    trace $ "[depth=" ++ show i ++ "] CLMPAP " ++ pprSummary 60 e
    ex' <- evalCLM (i+1) ex
    case ex' of
        CLMERR _ _ -> pure ex'
        _ -> do
            exs' <- imapM (\j e1 -> evalCLM (i+j+2) e1) exs
            case findCLMErr exs' of
                Just err -> pure err
                Nothing -> case ex' of
                    CLMLAM lam -> pure $ applyCLMLam lam exs'
                    _ -> if ex' == ex && exs' == exs then pure e
                         else pure $ CLMPAP ex' exs'
evalCLM i e = do
    trace $ "[depth=" ++ show i ++ "] unhandled " ++ pprSummary 60 e
    pure e

-- | Resolve a field access by name on a class object using ClassMeta field indices
evalFieldAccess :: Name -> Name -> CLMExpr -> Environment -> Maybe CLMExpr
evalFieldAccess fieldName className (CLMCON _ fields) env =
    case lookupClass className env of
        Just cm -> case Map.lookup fieldName (cmFieldIndices cm) of
            Just idx | idx < length fields -> Just (fields !! idx)
            _ -> Nothing
        Nothing -> Nothing
evalFieldAccess _ _ _ _ = Nothing

-- | Find the first CLMERR in a list of expressions
findCLMErr :: [CLMExpr] -> Maybe CLMExpr
findCLMErr [] = Nothing
findCLMErr (e@(CLMERR _ _):_) = Just e
findCLMErr (_:rest) = findCLMErr rest

-- Instance dispatch: try multi-param key, prefix lookup, single-param fallback
dispatchInstance :: Environment -> Name -> [Name] -> [CLMExpr] -> IntState CLMExpr
dispatchInstance env funcNm typeNames exs' = do
    -- try full multi-param exact key first (untagged)
    case lookupCLMInstance funcNm typeNames Nothing env of
        Just clmLam -> do
            trace $ "Found instance " ++ funcNm ++ " for " ++ show typeNames
            evalCLM 0 $ applyCLMLam clmLam exs'
        Nothing -> do
            -- try any-tag fallback for multi-param key
            case lookupCLMInstanceAnyTag funcNm typeNames env of
                Just clmLam -> do
                    trace $ "Found tagged instance " ++ funcNm ++ " for " ++ show typeNames
                    evalCLM 0 $ applyCLMLam clmLam exs'
                Nothing -> do
                    -- try prefix-based lookup (for morphisms where arg types < all type params)
                    case lookupCLMInstancePrefix funcNm typeNames Nothing env of
                        Just clmLam -> do
                            trace $ "Found prefix instance " ++ funcNm ++ " for " ++ show typeNames
                            evalCLM 0 $ applyCLMLam clmLam exs'
                        Nothing -> do
                            -- fall back to single-param: try each arg type until one matches
                            let tryKeys [] = Nothing
                                tryKeys (t:ts) = case lookupCLMInstance funcNm [t] Nothing env of
                                    Just clmLam -> Just (clmLam, t)
                                    Nothing -> case lookupCLMInstanceAnyTag funcNm [t] env of
                                        Just clmLam -> Just (clmLam, t)
                                        Nothing -> tryKeys ts
                            case tryKeys (nub typeNames) of
                                Just (clmLam, matchedType) -> do
                                    trace $ "Found single-param instance " ++ funcNm ++ " for [\"" ++ matchedType ++ "\"]"
                                    evalCLM 0 $ applyCLMLam clmLam exs'
                                Nothing -> do
                                    -- UNIVERSAL SHOW FALLBACK: for "show" on CLMCON / CLMARRAY values
                                    case (funcNm, exs') of
                                        ("show", [CLMCON (ConsTag nm _) []]) ->
                                            pure $ CLMLIT (LString nm)
                                        ("show", [CLMCON (ConsTag nm _) fields]) -> do
                                            fieldStrs <- mapM (\f -> do
                                                r <- evalCLM 0 (CLMIAP (CLMID "show") [f])
                                                _contEval 1 (CLMIAP (CLMID "show") [f]) r) fields
                                            let showField (CLMLIT (LString s)) = s
                                                showField other = ppr other
                                            let fieldStr = intercalate ", " (Prelude.map showField fieldStrs)
                                            pure $ CLMLIT (LString (nm ++ "(" ++ fieldStr ++ ")"))
                                        ("show", [CLMARRAY xs]) -> do
                                            elemStrs <- mapM (\x -> do
                                                r <- evalCLM 0 (CLMIAP (CLMID "show") [x])
                                                _contEval 1 (CLMIAP (CLMID "show") [x]) r) xs
                                            let showField (CLMLIT (LString s)) = s
                                                showField other = ppr other
                                            let inner = intercalate ", " (Prelude.map showField elemStrs)
                                            pure $ CLMLIT (LString ("[" ++ inner ++ "]"))
                                        _ -> do
                                            trace $ "No instance found for " ++ funcNm ++ ", using default"
                                            case Map.lookup funcNm (clmLambdas env) of
                                                Just clmLam -> case typeNames of
                                                    (t:_) -> pure $ applyCLMLam clmLam [CLMID t]
                                                    []    -> pure $ CLMERR ("[RT] No function or instance found for " ++ funcNm) SourceInteractive
                                                Nothing -> pure $ CLMERR ("[RT] No function or instance found for " ++ funcNm) SourceInteractive

-- Infer the type name from a CLM expression by looking at constructor tags
inferTypeFromExpr :: Environment -> CLMExpr -> Maybe Name
inferTypeFromExpr env (CLMCON (ConsTag consNm _) _) = lookupTypeOfConstructor consNm env
inferTypeFromExpr _ (CLMLIT lit) = litTypeName lit
inferTypeFromExpr _ (CLMARRAY _) = Just "Array"
inferTypeFromExpr _ _ = Nothing

-- Infer type names from all arguments (for multi-param morphism dispatch)
inferTypeNames :: Environment -> [CLMExpr] -> Maybe [Name]
inferTypeNames env exs =
    let types = catMaybes $ Prelude.map (inferTypeFromExpr env) exs
    in if Prelude.null types then Nothing else Just types

-- Extract a concrete type name from a CLM type hint expression
extractTypeNameFromHint :: CLMExpr -> Maybe Name
extractTypeNameFromHint (CLMID name) = Just name          -- simple type: "Char", "Int"
extractTypeNameFromHint (CLMAPP (CLMID name) _) = Just name  -- parameterized: "Maybe(Int)" → "Maybe"
extractTypeNameFromHint (CLMCON (ConsTag name _) _) = Just name  -- constructor as type
extractTypeNameFromHint _ = Nothing

-- | Check if a CLMLam is an intrinsic skeleton (CLMPRIMCALL body)
isPrimCallLam :: CLMLam -> Bool
isPrimCallLam (CLMLam _ CLMPRIMCALL) = True
isPrimCallLam _ = False

-- | Check if a CLM expression is a fully evaluated value (not an unevaluated call)
isValue :: CLMExpr -> Bool
isValue (CLMLIT _) = True
isValue (CLMCON _ _) = True
isValue (CLMLAM _) = True
isValue (CLMARRAY _) = True
isValue CLMEMPTY = True
isValue (CLMU _) = True
isValue (CLMREF _) = True
isValue (CLMMUTARRAY _) = True
isValue _ = False

-- | Check if a result's type matches the expected hint type name.
-- Uses the environment to check constructor-to-type mapping.
resultMatchesHint :: Environment -> CLMExpr -> Name -> Bool
resultMatchesHint env expr typeName =
    case inferTypeFromExpr env expr of
        Just actualType -> actualType == typeName
        Nothing -> case expr of
            CLMLIT _ -> False  -- literal with unknown type, don't assume match
            _ -> True  -- for other expressions, assume match (don't re-dispatch)

-- | Try hint-type instance dispatch with error recovery
tryHintInstance :: Environment -> Name -> Name -> [CLMExpr] -> CLMExpr -> IntState CLMExpr
tryHintInstance env funcNm typeName exs' fallbackResult = do
    let hintKey = mkInstanceKey funcNm [typeName] Nothing
    case Map.lookup hintKey (clmInstances env) of
        Just clmLam | not (isPrimCallLam clmLam) ->
            let r = applyCLMLam clmLam exs'
            in case r of
                CLMERR _ _ -> pure fallbackResult  -- instance failed, keep original result
                _ -> pure r
        _ -> pure fallbackResult  -- no instance found, keep original result

lookupCLMBindingOrLambda :: Environment -> Name -> Maybe CLMExpr
lookupCLMBindingOrLambda env nm =
    let bnd = Map.lookup nm (clmBindings env)
    in  case bnd of
            Just (nm1,expr) -> Just expr
            Nothing -> let mlam = Map.lookup nm (clmLambdas env) in
                case mlam of
                    Nothing -> Nothing
                    Just lm -> Just $ CLMLAM lm

-- | Dispatch for nullary implicit-param functions (e.g., value declarations like pi, maxBound).
-- Finds any matching instance and tries intrinsic first, then CLM lambda application.
dispatchNullaryIAP :: Environment -> Name -> IntState CLMExpr
dispatchNullaryIAP env funcNm =
    case findAnyInstanceWithType funcNm env of
        Just (typNm, clmLam) ->
            case lookupIntrinsic funcNm typNm of
                Just intrinsicFn -> case intrinsicFn [] of
                    Just result -> do
                        trace $ "  intrinsic nullary " ++ funcNm ++ " for " ++ typNm ++ " => " ++ pprSummary 40 result
                        pure result
                    Nothing -> do
                        trace $ "  nullary instance " ++ funcNm
                        pure $ applyCLMLam clmLam []
                Nothing -> do
                    trace $ "  nullary instance " ++ funcNm
                    pure $ applyCLMLam clmLam []
        Nothing -> do
            trace $ "  no instance for nullary " ++ funcNm
            pure $ CLMIAP (CLMID funcNm) []

-- Effect handler dispatch: check the dynamic handler stack for an op implementation
-- Returns Nothing if no handler provides this op (falls through to IO intrinsics)
dispatchHandlerOp :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchHandlerOp funcNm args = do
    s <- get
    let stack = handlerStack s
    case lookupHandlerOp funcNm stack of
        Nothing -> pure Nothing
        Just impl -> case impl of
            CLMPRIMCALL -> pure Nothing  -- intrinsic marker, fall through to IO dispatch
            CLMLAM lam -> do
                result <- evalCLM 0 (applyCLMLam lam args)
                result' <- _contEval 1 (applyCLMLam lam args) result
                pure (Just result')
            _ -> do
                -- Handler op is a value (e.g. nullary like get())
                result <- evalCLM 0 impl
                result' <- _contEval 1 impl result
                pure (Just result')

-- When an explicit (non-default) handler is on the stack, seq/bind work as simple sequencing
-- even when no Monad instance is available for the result type.
-- seq(a, f) → f(a)   where f is a lambda
-- bind(a, f) → f(a)  where f is a lambda
-- NOTE: Only fires when there are explicit handlers above the default handlers.
-- Default handlers alone (Console/FileIO/etc.) should NOT trigger this.
dispatchEffectSequencing :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchEffectSequencing funcNm args = do
    s <- get
    let stack = handlerStack s
    let hasExplicitHandlers = Prelude.length stack > defaultHandlerCount s
    case (hasExplicitHandlers, funcNm, args) of
        (True, nm, [a, CLMLAM lam]) | nm `elem` ["effectSeq", "effectBind", "seq", "bind"] -> do
            result <- evalCLM 0 (applyCLMLam lam [a])
            result' <- _contEval 1 (applyCLMLam lam [a]) result
            pure (Just result')
        _ -> pure Nothing

-- IO intrinsic dispatch for effect operations (Console, FileIO, etc.)
-- These run in IntState (which has IO) rather than being pure functions.
-- Returns Nothing if the function name is not a known IO intrinsic.
dispatchIOIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
-- Console effect operations
dispatchIOIntrinsic "putStrLn" [arg] = do
    let str = clmExprToString arg
    liftIO $ putStrLn str
    pure $ Just $ CLMCON (ConsTag "Unit" 0) []
dispatchIOIntrinsic "putStr" [arg] = do
    let str = clmExprToString arg
    liftIO $ putStr str
    pure $ Just $ CLMCON (ConsTag "Unit" 0) []
dispatchIOIntrinsic "readLine" [] = do
    line <- liftIO getLine
    pure $ Just $ CLMLIT (LString line)
-- error: always produces CLMERR
dispatchIOIntrinsic "error" [arg] =
    pure $ Just $ CLMERR (clmExprToString arg) SourceInteractive
-- FileIO operations
dispatchIOIntrinsic "readFile" [arg] = do
    let path = clmExprToString arg
    content <- liftIO $ readFile path
    pure $ Just $ CLMLIT (LString content)
dispatchIOIntrinsic "writeFile" [arg1, arg2] = do
    let path = clmExprToString arg1
    let content = clmExprToString arg2
    liftIO $ writeFile path content
    pure $ Just $ CLMCON (ConsTag "Unit" 0) []
dispatchIOIntrinsic "appendFile" [arg1, arg2] = do
    liftIO $ appendFile (clmExprToString arg1) (clmExprToString arg2)
    pure $ Just $ CLMCON (ConsTag "Unit" 0) []
dispatchIOIntrinsic "fileExists" [arg] = do
    exists <- liftIO $ doesFileExist (clmExprToString arg)
    pure $ Just $ boolToCLM exists
-- Clock effect: monotonic nanosecond clock
dispatchIOIntrinsic "clockNanos" [] = do
    t <- liftIO getPOSIXTime
    let nanos = floor (t * 1e9) :: Int
    pure $ Just $ CLMLIT (LInt nanos)
-- Note: bind, seq, pure are NOT intercepted here — they go through normal
-- CLMIAP instance dispatch (Maybe.bind, List.bind, etc.). Effect-specific
-- bind/seq will be handled via effect handler instances when registered.
dispatchIOIntrinsic _ _ = pure Nothing

-- | Unit value helper
unitCLM :: CLMExpr
unitCLM = CLMCON (ConsTag "Unit" 0) []

-- Mutable reference intrinsic dispatch
-- newRef/readRef/writeRef/modifyRef operate on IORef-backed CLMREF handles
dispatchRefIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
-- newRef(val) → allocate IORef, return CLMREF handle
dispatchRefIntrinsic "newRef" [val] = do
    st <- get
    let rid = nextRefId st
    ref <- liftIO $ newIORef val
    put $ st { refTable = Map.insert rid ref (refTable st), nextRefId = rid + 1 }
    pure $ Just $ CLMREF rid
-- readRef(CLMREF id) → read IORef value
dispatchRefIntrinsic "readRef" [CLMREF rid] = do
    st <- get
    case Map.lookup rid (refTable st) of
        Just ref -> do
            val <- liftIO $ readIORef ref
            pure $ Just val
        Nothing -> pure $ Just $ CLMERR ("[RT] readRef: invalid ref handle " ++ show rid) SourceInteractive
-- writeRef(CLMREF id, val) → write to IORef, return Unit
dispatchRefIntrinsic "writeRef" [CLMREF rid, val] = do
    st <- get
    case Map.lookup rid (refTable st) of
        Just ref -> do
            liftIO $ writeIORef ref val
            pure $ Just unitCLM
        Nothing -> pure $ Just $ CLMERR ("[RT] writeRef: invalid ref handle " ++ show rid) SourceInteractive
-- modifyRef(CLMREF id, fn) → read, apply fn, eval, write, return Unit
dispatchRefIntrinsic "modifyRef" [CLMREF rid, CLMLAM fn] = do
    st <- get
    case Map.lookup rid (refTable st) of
        Just ref -> do
            oldVal <- liftIO $ readIORef ref
            let applied = applyCLMLam fn [oldVal]
            newVal <- evalCLM 0 applied
            newVal' <- _contEval 1 applied newVal
            liftIO $ writeIORef ref newVal'
            pure $ Just unitCLM
        Nothing -> pure $ Just $ CLMERR ("[RT] modifyRef: invalid ref handle " ++ show rid) SourceInteractive
dispatchRefIntrinsic _ _ = pure Nothing

-- Mutable array intrinsic dispatch
-- newMutArray/mutRead/mutWrite/mutLength/freeze/thaw/mutPush operate on IORef-backed CLMMUTARRAY handles
dispatchMutArrayIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
-- newMutArray(n, val) → allocate IORef (replicate n val), return CLMMUTARRAY handle
dispatchMutArrayIntrinsic "newMutArray" [CLMLIT (LInt n), val] = do
    st <- get
    let mid = nextMutArrayId st
    ref <- liftIO $ newIORef (Prelude.replicate n val)
    put $ st { mutArrayTable = Map.insert mid ref (mutArrayTable st), nextMutArrayId = mid + 1 }
    pure $ Just $ CLMMUTARRAY mid
-- mutRead(CLMMUTARRAY id, i) → bounds-checked index
dispatchMutArrayIntrinsic "mutRead" [CLMMUTARRAY mid, CLMLIT (LInt i)] = do
    st <- get
    case Map.lookup mid (mutArrayTable st) of
        Just ref -> do
            arr <- liftIO $ readIORef ref
            if i >= 0 && i < Prelude.length arr
                then pure $ Just $ arr Prelude.!! i
                else pure $ Just $ CLMERR ("[RT] mutRead: index " ++ show i ++ " out of bounds (length " ++ show (Prelude.length arr) ++ ")") SourceInteractive
        Nothing -> pure $ Just $ CLMERR ("[RT] mutRead: invalid mutarray handle " ++ show mid) SourceInteractive
-- mutWrite(CLMMUTARRAY id, i, val) → update at index
dispatchMutArrayIntrinsic "mutWrite" [CLMMUTARRAY mid, CLMLIT (LInt i), val] = do
    st <- get
    case Map.lookup mid (mutArrayTable st) of
        Just ref -> do
            arr <- liftIO $ readIORef ref
            if i >= 0 && i < Prelude.length arr
                then do
                    let arr' = Prelude.take i arr ++ [val] ++ Prelude.drop (i + 1) arr
                    liftIO $ writeIORef ref arr'
                    pure $ Just unitCLM
                else pure $ Just $ CLMERR ("[RT] mutWrite: index " ++ show i ++ " out of bounds (length " ++ show (Prelude.length arr) ++ ")") SourceInteractive
        Nothing -> pure $ Just $ CLMERR ("[RT] mutWrite: invalid mutarray handle " ++ show mid) SourceInteractive
-- mutLength(CLMMUTARRAY id) → return length as Int
dispatchMutArrayIntrinsic "mutLength" [CLMMUTARRAY mid] = do
    st <- get
    case Map.lookup mid (mutArrayTable st) of
        Just ref -> do
            arr <- liftIO $ readIORef ref
            pure $ Just $ CLMLIT (LInt (Prelude.length arr))
        Nothing -> pure $ Just $ CLMERR ("[RT] mutLength: invalid mutarray handle " ++ show mid) SourceInteractive
-- freeze(CLMMUTARRAY id) → read list, return CLMARRAY
dispatchMutArrayIntrinsic "freeze" [CLMMUTARRAY mid] = do
    st <- get
    case Map.lookup mid (mutArrayTable st) of
        Just ref -> do
            arr <- liftIO $ readIORef ref
            pure $ Just $ CLMARRAY arr
        Nothing -> pure $ Just $ CLMERR ("[RT] freeze: invalid mutarray handle " ++ show mid) SourceInteractive
-- thaw(CLMARRAY elems) → allocate new mutable array from elements
dispatchMutArrayIntrinsic "thaw" [CLMARRAY elems] = do
    st <- get
    let mid = nextMutArrayId st
    ref <- liftIO $ newIORef elems
    put $ st { mutArrayTable = Map.insert mid ref (mutArrayTable st), nextMutArrayId = mid + 1 }
    pure $ Just $ CLMMUTARRAY mid
-- mutPush(CLMMUTARRAY id, val) → append element
dispatchMutArrayIntrinsic "mutPush" [CLMMUTARRAY mid, val] = do
    st <- get
    case Map.lookup mid (mutArrayTable st) of
        Just ref -> do
            arr <- liftIO $ readIORef ref
            liftIO $ writeIORef ref (arr ++ [val])
            pure $ Just unitCLM
        Nothing -> pure $ Just $ CLMERR ("[RT] mutPush: invalid mutarray handle " ++ show mid) SourceInteractive
dispatchMutArrayIntrinsic _ _ = pure Nothing

-- Reflection intrinsic dispatch for tag/tagName/arity/field/numConstructors/constructorByIndex
-- Returns Nothing if the function name is not a known reflection intrinsic.
dispatchReflectionIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
-- tag(x): constructor tag index (0-based), primitives return 0
dispatchReflectionIntrinsic "tag" [CLMCON (ConsTag _ idx) _] =
    pure $ Just $ CLMLIT (LInt (fromIntegral idx))
dispatchReflectionIntrinsic "tag" [CLMLIT _] =
    pure $ Just $ CLMLIT (LInt 0)
-- tagName(x): constructor name as string, primitives return type name
dispatchReflectionIntrinsic "tagName" [CLMCON (ConsTag nm _) _] =
    pure $ Just $ CLMLIT (LString nm)
dispatchReflectionIntrinsic "tagName" [CLMLIT lit] =
    pure $ Just $ CLMLIT (LString (maybe "Unknown" id (litTypeName lit)))
-- arity(x): number of fields in constructor, primitives return 0
dispatchReflectionIntrinsic "arity" [CLMCON _ fields] =
    pure $ Just $ CLMLIT (LInt (fromIntegral $ Prelude.length fields))
dispatchReflectionIntrinsic "arity" [CLMLIT _] =
    pure $ Just $ CLMLIT (LInt 0)
-- field(x, i): get i-th field (0-indexed) with bounds check
dispatchReflectionIntrinsic "field" [CLMCON (ConsTag nm _) fields, CLMLIT (LInt i)] =
    let idx = fromIntegral i
    in if idx >= 0 && idx < Prelude.length fields
       then pure $ Just $ fields Prelude.!! idx
       else pure $ Just $ CLMERR ("field: index " ++ show i ++ " out of bounds for " ++ nm
            ++ " (arity " ++ show (Prelude.length fields) ++ ")") SourceInteractive
-- numConstructors(typeName): count constructors in a sum type
dispatchReflectionIntrinsic "numConstructors" [CLMLIT (LString typeName)] = do
    s <- get
    let env = currentEnvironment s
    case Map.lookup typeName (types env) of
        Just (SumType lam) -> case body lam of
            Constructors cons -> pure $ Just $ CLMLIT (LInt (fromIntegral $ Prelude.length cons))
            _ -> pure $ Just $ CLMLIT (LInt 1)
        Just _ -> pure $ Just $ CLMLIT (LInt 1) -- primitive or structure, 1 "constructor"
        Nothing -> pure $ Just $ CLMERR ("numConstructors: type " ++ typeName ++ " not found") SourceInteractive
-- constructorByIndex(typeName, i): create nullary constructor by tag index
dispatchReflectionIntrinsic "constructorByIndex" [CLMLIT (LString typeName), CLMLIT (LInt i)] = do
    s <- get
    let env = currentEnvironment s
    case Map.lookup typeName (types env) of
        Just (SumType lam) -> case body lam of
            Constructors cons ->
                let idx = fromIntegral i
                in if idx >= 0 && idx < Prelude.length cons
                   then let con = cons Prelude.!! idx
                            conName = lamName con
                            conArity = Prelude.length (params con)
                        in if conArity == 0
                           then pure $ Just $ CLMCON (ConsTag conName idx) []
                           else pure $ Just $ CLMERR ("constructorByIndex: " ++ conName
                                ++ " has " ++ show conArity ++ " fields (not nullary)") SourceInteractive
                   else pure $ Just $ CLMERR ("constructorByIndex: index " ++ show i
                        ++ " out of bounds for " ++ typeName) SourceInteractive
            _ -> pure $ Just $ CLMERR ("constructorByIndex: " ++ typeName ++ " has no constructors") SourceInteractive
        Nothing -> pure $ Just $ CLMERR ("constructorByIndex: type " ++ typeName ++ " not found") SourceInteractive
-- intToByte(n): convert Int to Byte (truncating to low 8 bits)
dispatchReflectionIntrinsic "intToByte" [CLMLIT (LInt n)] =
    pure $ Just $ CLMLIT (LWord8 (fromIntegral n))
-- byteToInt(b): convert Byte to Int (widening)
dispatchReflectionIntrinsic "byteToInt" [CLMLIT (LWord8 w)] =
    pure $ Just $ CLMLIT (LInt (fromIntegral w))
dispatchReflectionIntrinsic _ _ = pure Nothing

-- | Safe downcast intrinsic: __downcast("TargetClass", obj) -> Maybe(obj)
-- Checks if obj's concrete class is targetClass or a subclass of it.
-- Returns Just(obj) on success, Nothing on failure.
dispatchDowncastIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchDowncastIntrinsic "__downcast" [CLMLIT (LString targetClass), obj@(CLMCON (ConsTag srcClass _) _)] = do
    s <- get
    let env = currentEnvironment s
    if isSubclassOf srcClass targetClass env
        then pure $ Just $ CLMCON (ConsTag "Just" 1) [obj]
        else pure $ Just $ CLMCON (ConsTag "Nothing" 0) []
dispatchDowncastIntrinsic "__downcast" [CLMLIT (LString _), _] =
    pure $ Just $ CLMCON (ConsTag "Nothing" 0) []  -- non-object value → downcast fails
dispatchDowncastIntrinsic _ _ = pure Nothing

-- Closure-aware dispatch for Array higher-order functions.
-- Called from CLMIAP eval path when args contain closures + arrays.
dispatchArrayHOF :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
-- fmap(f, arr) — map function over array elements
dispatchArrayHOF "fmap" [CLMLAM lam, CLMARRAY xs] = do
    results <- mapM (\x -> do
        let app = applyCLMLam lam [x]
        r <- evalCLM 0 app
        _contEval 1 app r) xs
    case findCLMErr results of
        Just err -> pure $ Just err
        Nothing  -> pure $ Just $ CLMARRAY results
-- filter(f, arr) — keep elements where predicate returns True
dispatchArrayHOF "filter" [CLMLAM lam, CLMARRAY xs] = do
    results <- filterM (\x -> do
        let app = applyCLMLam lam [x]
        r <- evalCLM 0 app
        r' <- _contEval 1 app r
        pure (r' == CLMCON (ConsTag "True" 0) [])) xs
    pure $ Just $ CLMARRAY results
-- foldl(f, acc, arr) — left fold over array
dispatchArrayHOF "foldl" [CLMLAM lam, acc, CLMARRAY xs] = do
    result <- foldM (\a x -> do
        let app = applyCLMLam lam [a, x]
        r <- evalCLM 0 app
        _contEval 1 app r) acc xs
    pure $ Just result
-- foldr(f, acc, arr) — right fold over array
dispatchArrayHOF "foldr" [CLMLAM lam, acc, CLMARRAY xs] = do
    result <- foldrM (\x a -> do
        let app = applyCLMLam lam [x, a]
        r <- evalCLM 0 app
        _contEval 1 app r) acc xs
    pure $ Just result
-- generate(n, f) — create array by applying f to each index
dispatchArrayHOF "generate" [CLMLIT (LInt n), CLMLAM lam] = do
    results <- mapM (\i -> do
        let app = applyCLMLam lam [CLMLIT (LInt i)]
        r <- evalCLM 0 app
        _contEval 1 app r) [0..n-1]
    case findCLMErr results of
        Just err -> pure $ Just err
        Nothing  -> pure $ Just $ CLMARRAY results
dispatchArrayHOF _ _ = pure Nothing

-- Convert CLM expression to string for IO output
clmExprToString :: CLMExpr -> String
clmExprToString (CLMLIT (LString s)) = s
clmExprToString (CLMLIT (LInt n)) = show n
clmExprToString (CLMLIT (LFloat f)) = show f
clmExprToString (CLMLIT (LChar c)) = [c]
clmExprToString (CLMCON (ConsTag "Str" _) [CLMARRAY bytes, _]) = decodeUtf8CLM bytes
clmExprToString (CLMCON (ConsTag nm _) []) = nm
clmExprToString e = ppr e

-- | Decode a CLM byte array (list of CLMLIT LWord8/LInt) to a Haskell String (UTF-8)
decodeUtf8CLM :: [CLMExpr] -> String
decodeUtf8CLM exprs =
    let bytes = Prelude.map toByte exprs
    in decodeUtf8Bytes bytes
  where
    toByte (CLMLIT (LWord8 w)) = w
    toByte (CLMLIT (LInt n))   = fromIntegral n
    toByte _                   = 0

-- | Decode a list of UTF-8 bytes to a Haskell String
decodeUtf8Bytes :: [Word8] -> String
decodeUtf8Bytes [] = []
decodeUtf8Bytes (b:bs)
    | b <= 0x7F = chr (fromIntegral b) : decodeUtf8Bytes bs
    | b .&. 0xE0 == 0xC0 = case bs of
        (b1:rest) -> chr (((fromIntegral b .&. 0x1F) `shiftL` 6) .|. (fromIntegral b1 .&. 0x3F))
                     : decodeUtf8Bytes rest
        _ -> '?' : decodeUtf8Bytes bs
    | b .&. 0xF0 == 0xE0 = case bs of
        (b1:b2:rest) -> chr (((fromIntegral b .&. 0x0F) `shiftL` 12) .|. ((fromIntegral b1 .&. 0x3F) `shiftL` 6)
                             .|. (fromIntegral b2 .&. 0x3F))
                        : decodeUtf8Bytes rest
        _ -> '?' : decodeUtf8Bytes bs
    | b .&. 0xF8 == 0xF0 = case bs of
        (b1:b2:b3:rest) -> chr (((fromIntegral b .&. 0x07) `shiftL` 18) .|. ((fromIntegral b1 .&. 0x3F) `shiftL` 12)
                                .|. ((fromIntegral b2 .&. 0x3F) `shiftL` 6) .|. (fromIntegral b3 .&. 0x3F))
                           : decodeUtf8Bytes rest
        _ -> '?' : decodeUtf8Bytes bs
    | otherwise = '?' : decodeUtf8Bytes bs
