{-# LANGUAGE OverloadedStrings #-}
-- | Lower CLM (Core List Machine) to LIR (Low-Level IR).
--
-- Phase A.1 subset: CLMLIT, CLMID, CLMAPP, CLMCON, CLMCASE,
-- CLMLam/CLMLamCases, CLMFieldAccess, CLMPROG, CLMBIND, CLMERR, CLMEMPTY, CLMU.
module Backends.LLVM.CLMToLIR
  ( lowerModule
  , lowerFunction
  , LowerError(..)
  , sanitizeName
  -- * Declarative extern mapping (replaces hardcoded intrinsicToLIR)
  , ExternInfo(..)
  , ExternKind(..)
  , ExternMap
  , buildExternMap
  , resolveExtern
  , llvmInstrBuilder
  , surfaceTypeToLType
  , surfaceRetTypeToLType
  , externDeclarations
  ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Control.Monad (forM, forM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Surface (Name, Var(..), Expr(..), ConsTag(..), Literal(..))
import Logs (SourceInfo(..))
import CLM
import Backends.LLVM.LIR

-- ============================================================================
-- Lowering Errors
-- ============================================================================

data LowerError = LowerError String
  deriving (Show, Eq)

-- ============================================================================
-- Declarative extern mapping
-- ============================================================================

-- | How a primitive extern function maps to LIR.
data ExternInfo = ExternInfo
  { eiParamTypes :: [LType]     -- ^ Parameter types (for function signatures)
  , eiRetType    :: LType       -- ^ Return type
  , eiKind       :: ExternKind  -- ^ How to lower calls to this extern
  } deriving (Show, Eq)

-- | The three kinds of extern functions (from Native.tl declarations).
data ExternKind
  = EKInline String           -- ^ Inline LLVM instruction (e.g. "add", "icmp eq")
  | EKLLVMIntrinsic String    -- ^ LLVM intrinsic call (e.g. "llvm.sqrt.f64")
  | EKRuntimeCall             -- ^ C runtime function call (extern name = symbol name)
  deriving (Show, Eq)

-- | Map from extern function name to its specification.
type ExternMap = HashMap Name ExternInfo

-- | Convert a Surface type expression to an LIR value type.
-- Unit maps to LTPtr (it's a zero-field heap object — a valid value).
-- Use 'surfaceRetTypeToLType' for function return types where Unit → LTVoid.
surfaceTypeToLType :: Expr -> LType
surfaceTypeToLType (Id "Int")     = LTInt64
surfaceTypeToLType (Id "Int64")   = LTInt64
surfaceTypeToLType (Id "Int32")   = LTInt32
surfaceTypeToLType (Id "Int16")   = LTInt16
surfaceTypeToLType (Id "Int8")    = LTInt8
surfaceTypeToLType (Id "UInt64")  = LTWord64
surfaceTypeToLType (Id "UInt32")  = LTWord32
surfaceTypeToLType (Id "UInt16")  = LTWord16
surfaceTypeToLType (Id "UInt8")   = LTWord8
surfaceTypeToLType (Id "Byte")    = LTWord8
surfaceTypeToLType (Id "Float64") = LTFloat64
surfaceTypeToLType (Id "Float32") = LTFloat32
surfaceTypeToLType (Id "Bool")    = LTBool
surfaceTypeToLType (Id "Char")    = LTChar
surfaceTypeToLType (Id "String")  = LTPtr
surfaceTypeToLType (Id "Unit")    = LTPtr   -- Unit is a value (zero-field heap object)
surfaceTypeToLType (Id "Ordering") = LTPtr  -- ADT = heap object
surfaceTypeToLType (App _ _)      = LTPtr   -- parameterized type = heap object
surfaceTypeToLType _              = LTPtr   -- default: heap-allocated object

-- | Convert a Surface type to an LIR return type.
-- Like 'surfaceTypeToLType' but maps Unit → LTVoid for function returns.
-- This is correct for extern C functions where Unit means C void.
surfaceRetTypeToLType :: Expr -> LType
surfaceRetTypeToLType (Id "Unit") = LTVoid
surfaceRetTypeToLType other       = surfaceTypeToLType other

-- | Build an ExternMap from target extern declarations.
-- Input: the NameMap from Environment.targetExterns for a specific target.
-- Each entry: (funcName → (params, returnTypeExpr, Maybe (kind, spec)))
buildExternMap :: HashMap Name ([Var], Expr, Maybe (Name, String)) -> ExternMap
buildExternMap = Map.map $ \(params, retTy, spec) ->
  ExternInfo
    { eiParamTypes = map (surfaceTypeToLType . typ) params  -- params: Unit → LTPtr
    , eiRetType    = surfaceRetTypeToLType retTy            -- returns: Unit → LTVoid
    , eiKind       = case spec of
        Just ("inline", instr) -> EKInline instr
        Just ("llvm", name)    -> EKLLVMIntrinsic name
        _                      -> EKRuntimeCall
    }

-- | Resolve an extern function name to an LIR instruction builder.
-- Uses the declarative ExternMap built from Native.tl declarations.
resolveExtern :: ExternMap -> Name -> Maybe ([LOperand] -> LInstr)
resolveExtern externMap funcName =
  case Map.lookup funcName externMap of
    Nothing -> Nothing
    Just info -> case eiKind info of
      EKInline instr    -> llvmInstrBuilder instr (length (eiParamTypes info))
      EKLLVMIntrinsic n -> Just $ \ops -> LCall n ops (eiRetType info)
      EKRuntimeCall     -> Just $ \ops -> LCall funcName ops (eiRetType info)

-- | Map LLVM instruction name + arity to LIR constructor.
-- This is GENERIC LLVM knowledge — not tulam-specific.
-- The tulam-specific mappings live in Native.tl extern declarations.
llvmInstrBuilder :: String -> Int -> Maybe ([LOperand] -> LInstr)
-- Integer arithmetic
llvmInstrBuilder "add" 2       = Just $ \[a,b] -> LAdd a b
llvmInstrBuilder "sub" 2       = Just $ \[a,b] -> LSub a b
llvmInstrBuilder "mul" 2       = Just $ \[a,b] -> LMul a b
llvmInstrBuilder "sdiv" 2      = Just $ \[a,b] -> LDiv a b
llvmInstrBuilder "srem" 2      = Just $ \[a,b] -> LRem a b
llvmInstrBuilder "udiv" 2      = Just $ \[a,b] -> LUDiv a b
llvmInstrBuilder "urem" 2      = Just $ \[a,b] -> LURem a b
llvmInstrBuilder "neg" 1       = Just $ \[a]   -> LNeg a
-- Float arithmetic
llvmInstrBuilder "fadd" 2      = Just $ \[a,b] -> LFAdd a b
llvmInstrBuilder "fsub" 2      = Just $ \[a,b] -> LFSub a b
llvmInstrBuilder "fmul" 2      = Just $ \[a,b] -> LFMul a b
llvmInstrBuilder "fdiv" 2      = Just $ \[a,b] -> LFDiv a b
llvmInstrBuilder "fneg" 1      = Just $ \[a]   -> LFNeg a
-- Bitwise
llvmInstrBuilder "and" 2       = Just $ \[a,b] -> LAnd a b
llvmInstrBuilder "or" 2        = Just $ \[a,b] -> LOr a b
llvmInstrBuilder "xor" 2       = Just $ \[a,b] -> LXor a b
llvmInstrBuilder "shl" 2       = Just $ \[a,b] -> LShl a b
llvmInstrBuilder "ashr" 2      = Just $ \[a,b] -> LAShr a b
llvmInstrBuilder "lshr" 2      = Just $ \[a,b] -> LLShr a b
-- Integer comparison
llvmInstrBuilder "icmp eq" 2   = Just $ \[a,b] -> LICmpEq a b
llvmInstrBuilder "icmp ne" 2   = Just $ \[a,b] -> LICmpNe a b
llvmInstrBuilder "icmp slt" 2  = Just $ \[a,b] -> LICmpLt a b
llvmInstrBuilder "icmp sle" 2  = Just $ \[a,b] -> LICmpLe a b
llvmInstrBuilder "icmp sgt" 2  = Just $ \[a,b] -> LICmpGt a b
llvmInstrBuilder "icmp sge" 2  = Just $ \[a,b] -> LICmpGe a b
llvmInstrBuilder "icmp ult" 2  = Just $ \[a,b] -> LICmpULt a b
llvmInstrBuilder "icmp ule" 2  = Just $ \[a,b] -> LICmpULe a b
llvmInstrBuilder "icmp ugt" 2  = Just $ \[a,b] -> LICmpUGt a b
llvmInstrBuilder "icmp uge" 2  = Just $ \[a,b] -> LICmpUGe a b
-- Float comparison
llvmInstrBuilder "fcmp oeq" 2  = Just $ \[a,b] -> LFCmpEq a b
llvmInstrBuilder "fcmp one" 2  = Just $ \[a,b] -> LFCmpNe a b
llvmInstrBuilder "fcmp olt" 2  = Just $ \[a,b] -> LFCmpLt a b
llvmInstrBuilder "fcmp ole" 2  = Just $ \[a,b] -> LFCmpLe a b
llvmInstrBuilder "fcmp ogt" 2  = Just $ \[a,b] -> LFCmpGt a b
llvmInstrBuilder "fcmp oge" 2  = Just $ \[a,b] -> LFCmpGe a b
llvmInstrBuilder _ _           = Nothing

-- | Generate LLVM extern declarations for non-inline externs.
-- Inline externs don't need declarations (they're expanded in place).
-- LLVM intrinsics and runtime calls need `declare` statements.
externDeclarations :: ExternMap -> [LFunction]
externDeclarations = map mkExtern . filter needsDecl . Map.toList
  where
    needsDecl (_, info) = case eiKind info of
      EKInline _        -> False   -- inlined at call site
      EKLLVMIntrinsic _ -> True    -- needs declare @llvm.sqrt.f64(...)
      EKRuntimeCall     -> True    -- needs declare @__show_i64(...)
    mkExtern (name, info) =
      let symName = case eiKind info of
            EKLLVMIntrinsic n -> n   -- use the LLVM intrinsic name
            _                 -> name -- use the extern function name
          params = [("", ty) | ty <- eiParamTypes info]
      in LFunction symName params (eiRetType info) [] True

-- ============================================================================
-- Lowering Monad
-- ============================================================================

data LowerState = LowerState
  { lsNextVar       :: !Int
  , lsNextBlock     :: !Int
  , lsBlocks        :: [LBlock]              -- Accumulated blocks (reversed)
  , lsCurrentInstrs :: [(Name, LInstr)]      -- Current block instrs (reversed)
  , lsCurrentBlock  :: Name                  -- Current block label
  , lsGlobals       :: [LGlobal]             -- Accumulated globals (reversed)
  , lsStringMap     :: HashMap String Name   -- Dedup string constants
  , lsEnv           :: HashMap Name LOperand -- Variable bindings
  , lsFuncMap       :: HashMap Name (Name, [LType], LType) -- func name → (llvm name, param types, ret type)
  , lsExternMap     :: ExternMap             -- Declarative extern specs from Native.tl
  }

type LowerM = ExceptT LowerError (StateT LowerState IO)

runLowerM :: LowerState -> LowerM a -> IO (Either LowerError (a, LowerState))
runLowerM st action = do
  (result, st') <- runStateT (runExceptT action) st
  case result of
    Left err -> return (Left err)
    Right a  -> return (Right (a, st'))

throwL :: String -> LowerM a
throwL = throwE . LowerError

-- Lifted state operations
getS :: LowerM LowerState
getS = lift get

getsS :: (LowerState -> a) -> LowerM a
getsS = lift . gets

putS :: LowerState -> LowerM ()
putS = lift . put

modifyS :: (LowerState -> LowerState) -> LowerM ()
modifyS = lift . modify'

-- ============================================================================
-- Fresh name/block generation
-- ============================================================================

freshVar :: String -> LowerM Name
freshVar prefix = do
  n <- getsS lsNextVar
  modifyS $ \s -> s { lsNextVar = n + 1 }
  return (prefix ++ show n)

freshBlock :: String -> LowerM Name
freshBlock prefix = do
  n <- getsS lsNextBlock
  modifyS $ \s -> s { lsNextBlock = n + 1 }
  return (prefix ++ show n)

-- ============================================================================
-- Instruction emission
-- ============================================================================

-- | Emit an instruction, returning the result name.
emit :: Name -> LInstr -> LowerM Name
emit name instr = do
  modifyS $ \s -> s { lsCurrentInstrs = (name, instr) : lsCurrentInstrs s }
  return name

-- | Emit with a fresh name, returning a typed operand.
emitFresh :: String -> LType -> LInstr -> LowerM LOperand
emitFresh prefix ty instr = do
  name <- freshVar prefix
  _ <- emit name instr
  return (LVar name ty)

-- | Seal current block with terminator, start new block.
sealBlock :: LTerminator -> Name -> LowerM ()
sealBlock term newLabel = do
  st <- getS
  let block = LBlock (lsCurrentBlock st) (reverse (lsCurrentInstrs st)) term
  putS st
    { lsBlocks = block : lsBlocks st
    , lsCurrentInstrs = []
    , lsCurrentBlock = newLabel
    }

-- | Bind a variable in the local environment.
bindVar :: Name -> LOperand -> LowerM ()
bindVar name op = modifyS $ \s -> s { lsEnv = Map.insert name op (lsEnv s) }

-- | Look up a variable.
lookupVar :: Name -> LowerM (Maybe LOperand)
lookupVar name = getsS (Map.lookup name . lsEnv)

-- ============================================================================
-- String constant management
-- ============================================================================

getOrCreateString :: String -> LowerM Name
getOrCreateString str = do
  mname <- getsS (Map.lookup str . lsStringMap)
  case mname of
    Just name -> return name
    Nothing -> do
      gname <- freshVar "str_"
      let global = LGlobalString gname str
      modifyS $ \s -> s
        { lsGlobals = global : lsGlobals s
        , lsStringMap = Map.insert str gname (lsStringMap s)
        }
      return gname

-- ============================================================================
-- Core lowering: CLMExpr → LOperand
-- ============================================================================

lowerExpr :: CLMExpr -> LowerM LOperand

-- Literals
lowerExpr (CLMLIT (LInt n))     = return $ LLitInt (fromIntegral n) LTInt64
lowerExpr (CLMLIT (LFloat d))   = return $ LLitFloat d LTFloat64
lowerExpr (CLMLIT (LChar c))    = return $ LLitInt (fromIntegral (fromEnum c)) LTChar
lowerExpr (CLMLIT (LInt8 n))    = return $ LLitInt (fromIntegral n) LTInt8
lowerExpr (CLMLIT (LInt16 n))   = return $ LLitInt (fromIntegral n) LTInt16
lowerExpr (CLMLIT (LInt32 n))   = return $ LLitInt (fromIntegral n) LTInt32
lowerExpr (CLMLIT (LInt64 n))   = return $ LLitInt (fromIntegral n) LTInt64
lowerExpr (CLMLIT (LWord8 n))   = return $ LLitInt (fromIntegral n) LTWord8
lowerExpr (CLMLIT (LWord16 n))  = return $ LLitInt (fromIntegral n) LTWord16
lowerExpr (CLMLIT (LWord32 n))  = return $ LLitInt (fromIntegral n) LTWord32
lowerExpr (CLMLIT (LWord64 n))  = return $ LLitInt (fromIntegral n) LTWord64
lowerExpr (CLMLIT (LFloat32 f)) = return $ LLitFloat (realToFrac f) LTFloat32

lowerExpr (CLMLIT (LString s)) = do
  gname <- getOrCreateString s
  return $ LLitString gname s

lowerExpr (CLMLIT (LList _))   = throwL "LList not supported in Phase A.1"
lowerExpr (CLMLIT (LVec _))    = throwL "LVec not supported in Phase A.1"
lowerExpr (CLMLIT (LNTuple _)) = throwL "LNTuple literals not supported in Phase A.1"

-- Variable reference
lowerExpr (CLMID name) = do
  mop <- lookupVar name
  case mop of
    Just op -> return op
    Nothing -> return $ LVar name LTPtr  -- assume global function ref

-- Direct function application: CLMAPP (CLMID funcName) args
-- Resolution order: (1) declarative ExternMap, (2) legacy intrinsicToLIR, (3) function call
lowerExpr (CLMAPP (CLMID funcName) args) = do
  extMap <- getsS lsExternMap
  case resolveExtern extMap funcName of
    Just mkInstr -> do
      ops <- mapM lowerExpr args
      let instr = mkInstr ops
      emitFresh "t_" (instrResultType instr) instr

    Nothing -> case intrinsicToLIR funcName of
      Just mkInstr -> do
        ops <- mapM lowerExpr args
        let instr = mkInstr ops
        emitFresh "t_" (instrResultType instr) instr

      Nothing -> do
        ops <- mapM lowerExpr args
        fmap' <- getsS lsFuncMap
        let llvmName = "tulam_" ++ sanitizeName funcName
            (_, _, retTy) = case Map.lookup funcName fmap' of
              Just info -> info
              Nothing   -> (llvmName, map operandType ops, LTInt64)
        emitFresh "r_" retTy (LCall llvmName ops retTy)

-- Immediately-applied lambda: beta-reduce (let bindings, if/then/else)
-- CLMAPP (CLMLAM (CLMLam [vars] body)) [args] → bind vars=args, lower body
lowerExpr (CLMAPP (CLMLAM (CLMLam vars body)) args) = do
  ops <- mapM lowerExpr args
  -- Bind each param to its argument value
  mapM_ (\((name, _), op) -> bindVar name op) (zip vars ops)
  lowerExpr body

-- Immediately-applied case lambda: bind params, then lower case chain
-- CLMAPP (CLMLAM (CLMLamCases [vars] cases)) [args] → bind vars=args, match cases
lowerExpr (CLMAPP (CLMLAM (CLMLamCases vars cases)) args) = do
  ops <- mapM lowerExpr args
  mapM_ (\((name, _), op) -> bindVar name op) (zip vars ops)
  -- Lower cases as a chain of checks (same as lowerCaseChain but inline)
  lowerCaseExprChain cases

-- Application where func is not a simple name
lowerExpr (CLMAPP func args) = do
  funcOp <- lowerExpr func
  ops <- mapM lowerExpr args
  emitFresh "r_" LTInt64 (LCallPtr funcOp ops LTInt64)

-- Constructor allocation
lowerExpr (CLMCON (ConsTag _cname tag) fields) = do
  ops <- mapM lowerExpr fields
  obj <- emitFresh "obj_" LTPtr (LAlloc tag (length ops))
  forM_ (zip [0..] ops) $ \(i, op) -> do
    val64 <- coerceToI64ForStore op
    emitFresh "_s_" LTVoid (LStore val64 obj i)
  return obj

-- Field access
lowerExpr (CLMFieldAccess (_name, idx) obj) = do
  objOp <- lowerExpr obj
  emitFresh "fld_" LTInt64 (LLoad objOp idx LTInt64)

-- Sequential statements
lowerExpr (CLMPROG [])     = return LLitNull
lowerExpr (CLMPROG [e])    = lowerExpr e
lowerExpr (CLMPROG (e:es)) = lowerExpr e >> lowerExpr (CLMPROG es)

-- Let binding
lowerExpr (CLMBIND name expr) = do
  op <- lowerExpr expr
  bindVar name op
  return op

-- Error
lowerExpr (CLMERR msg _srcInfo) = do
  gname <- getOrCreateString msg
  emitFresh "_err_" LTVoid (LCall "tlm_error" [LLitString gname msg] LTVoid)
  return LLitNull

-- Erased types
lowerExpr CLMEMPTY = return LLitNull
lowerExpr (CLMU _) = return LLitNull

-- Inline pattern match expression
lowerExpr (CLMCASE checks body)
  | null checks = lowerExpr body
  | otherwise = do
      cond <- lowerPatternChecks checks
      thenLabel  <- freshBlock "case_body_"
      elseLabel  <- freshBlock "case_skip_"
      mergeLabel <- freshBlock "case_merge_"

      sealBlock (LCondBr cond thenLabel elseLabel) thenLabel

      bodyOp <- lowerExpr body
      thenBlockName <- getsS lsCurrentBlock
      sealBlock (LBr mergeLabel) elseLabel

      elseBlockName <- getsS lsCurrentBlock
      sealBlock (LBr mergeLabel) mergeLabel

      let retTy = operandType bodyOp
      emitFresh "phi_" retTy
        (LPhi [(bodyOp, thenBlockName), (LLitNull, elseBlockName)] retTy)

-- Typed expression — just lower the inner
lowerExpr (CLMTYPED inner _ty) = lowerExpr inner

-- Lambda value — closures not supported in Phase A.1
lowerExpr (CLMLAM clm)        = throwL $ "Inline lambda values (closures) not supported in Phase A.1: " ++ take 200 (show clm)

-- Unsupported nodes
lowerExpr (CLMIAP f args)     = throwL $ "CLMIAP not supported in Phase A.1 (needs monomorphization): " ++ take 200 (show f) ++ " applied to " ++ show (length args) ++ " args"
lowerExpr (CLMPAP _ _)        = throwL "CLMPAP not supported in Phase A.1 (needs closures)"
lowerExpr (CLMMCALL _ _ _)    = throwL "CLMMCALL not supported in Phase A.1"
lowerExpr (CLMSCALL _ _ _)    = throwL "CLMSCALL not supported in Phase A.1"
lowerExpr (CLMNEW _ _)        = throwL "CLMNEW not supported in Phase A.1"
lowerExpr (CLMHANDLE _ _ _ _) = throwL "CLMHANDLE not supported in Phase A.1"
lowerExpr (CLMREF _)          = throwL "CLMREF not supported in Phase A.1"
lowerExpr (CLMMUTARRAY _)     = throwL "CLMMUTARRAY not supported in Phase A.1"
lowerExpr (CLMARRAY _)        = throwL "CLMARRAY not supported in Phase A.1"
lowerExpr CLMPRIMCALL         = throwL "Bare CLMPRIMCALL — should be wrapped in CLMAPP"

-- ============================================================================
-- Pattern check lowering
-- ============================================================================

lowerPatternChecks :: [CLMPatternCheck] -> LowerM LOperand
lowerPatternChecks []  = return $ LLitBool True
lowerPatternChecks [c] = lowerOneCheck c
lowerPatternChecks (c:cs) = do
  first <- lowerOneCheck c
  rest  <- lowerPatternChecks cs
  emitFresh "chk_" LTBool (LAnd first rest)

lowerOneCheck :: CLMPatternCheck -> LowerM LOperand
lowerOneCheck (CLMCheckTag (ConsTag _ tagVal) scrutExpr) = do
  scrut <- lowerExpr scrutExpr
  tagOp <- emitFresh "tag_" LTInt16 (LGetTag scrut)
  emitFresh "eq_" LTBool (LICmpEq tagOp (LLitInt (fromIntegral tagVal) LTInt16))

lowerOneCheck (CLMCheckLit lit scrutExpr) = do
  scrut <- lowerExpr scrutExpr
  litOp <- lowerExpr (CLMLIT lit)
  let ty = operandType litOp
  if isFloatType ty
    then emitFresh "eq_" LTBool (LFCmpEq scrut litOp)
    else emitFresh "eq_" LTBool (LICmpEq scrut litOp)

-- ============================================================================
-- Function lowering
-- ============================================================================

-- | Lower a named CLMLam to an LFunction + any generated globals.
-- The ExternMap provides declarative extern specs from Native.tl.
-- Pass Map.empty to use only the legacy intrinsicToLIR mapping.
lowerFunction :: Name -> CLMLam -> HashMap Name (Name, [LType], LType)
              -> ExternMap
              -> IO (Either LowerError (LFunction, [LGlobal]))
lowerFunction name clmLam funcMap extMap = do
  let llvmName = "tulam_" ++ sanitizeName name
      vars = clmLamParams clmLam
      params = [(sanitizeName n, inferParamType n) | (n, _) <- vars]
      retTy = case Map.lookup name funcMap of
                Just (_, _, rt) -> rt
                Nothing         -> LTInt64
      initEnv = Map.fromList [(n, LVar (sanitizeName n) ty) | (n, ty) <- params]
      initState = emptyLowerState
        { lsEnv = initEnv
        , lsFuncMap = funcMap
        , lsExternMap = extMap
        , lsCurrentBlock = "entry"
        }
  result <- runLowerM initState $ case clmLam of
    CLMLam _vars body -> do
      resultOp <- lowerExpr body
      st <- getS
      let terminator = if operandType resultOp == LTVoid then LRetVoid else LRet resultOp
          finalBlock = LBlock (lsCurrentBlock st)
                        (reverse (lsCurrentInstrs st))
                        terminator
          allBlocks = reverse (finalBlock : lsBlocks st)
      return (allBlocks, reverse (lsGlobals st))

    CLMLamCases _vars cases -> do
      (blocks, globals) <- lowerCaseChain cases retTy
      return (blocks, globals)

  case result of
    Left err -> return (Left err)
    Right ((blocks, globals), _) ->
      return $ Right (LFunction llvmName params retTy blocks False, globals)

-- | Lower a chain of CLMCASE alternatives into a linear check chain.
lowerCaseChain :: [CLMExpr] -> LType -> LowerM ([LBlock], [LGlobal])
lowerCaseChain cases _retTy = do
  -- Generate labels for each case
  labels <- forM (zip [0..] cases) $ \(i, _) -> do
    chk  <- freshBlock ("case" ++ show (i :: Int) ++ "_check_")
    body <- freshBlock ("case" ++ show (i :: Int) ++ "_body_")
    return (chk, body)

  failLabel <- freshBlock "match_fail_"

  -- Branch from entry to first case check
  let firstCheck = fst (head labels)
  sealBlock (LBr firstCheck) firstCheck

  -- For each case: check guards → body or next
  let nextChecks = map fst (tail labels) ++ [failLabel]
  forM_ (zip3 cases labels nextChecks) $ \(caseExpr, (_chkLabel, bodyLabel), nextCheck) ->
    case caseExpr of
      CLMCASE checks body -> do
        if null checks
          then sealBlock (LBr bodyLabel) bodyLabel
          else do
            cond <- lowerPatternChecks checks
            sealBlock (LCondBr cond bodyLabel nextCheck) bodyLabel
        resultOp <- lowerExpr body
        sealBlock (LRet resultOp) nextCheck
      _ -> do
        resultOp <- lowerExpr caseExpr
        sealBlock (LRet resultOp) nextCheck

  -- Failure block
  errStr <- getOrCreateString "pattern match failure"
  _ <- emitFresh "_err_" LTVoid
    (LCall "tlm_error" [LLitString errStr "pattern match failure"] LTVoid)
  st <- getS
  let failBlock = LBlock (lsCurrentBlock st)
                    (reverse (lsCurrentInstrs st))
                    LUnreachable
      allBlocks = reverse (failBlock : lsBlocks st)
  return (allBlocks, reverse (lsGlobals st))

-- | Lower an inline case chain (from immediately-applied CLMLamCases).
-- Unlike lowerCaseChain (which uses LRet for each branch), this produces
-- a phi node to merge results — for use inside expressions.
lowerCaseExprChain :: [CLMExpr] -> LowerM LOperand
lowerCaseExprChain [] = return LLitNull
lowerCaseExprChain cases = do
  mergeLabel <- freshBlock "match_merge_"
  failLabel  <- freshBlock "match_fail_"

  -- Generate labels for each case
  labels <- forM (zip [0..] cases) $ \(i, _) -> do
    chk  <- freshBlock ("mc" ++ show (i :: Int) ++ "_chk_")
    bd   <- freshBlock ("mc" ++ show (i :: Int) ++ "_bd_")
    return (chk, bd)

  -- Branch from current block to first case check
  let firstCheck = fst (head labels)
  sealBlock (LBr firstCheck) firstCheck

  -- For each case: check guards → body or next, branch to merge
  let nextChecks = map fst (tail labels) ++ [failLabel]
  results <- forM (zip3 cases labels nextChecks) $ \(caseExpr, (_chkLabel, bodyLabel), nextCheck) ->
    case caseExpr of
      CLMCASE checks caseBody -> do
        if null checks
          then sealBlock (LBr bodyLabel) bodyLabel
          else do
            cond <- lowerPatternChecks checks
            sealBlock (LCondBr cond bodyLabel nextCheck) bodyLabel
        resultOp <- lowerExpr caseBody
        fromBlock <- getsS lsCurrentBlock
        sealBlock (LBr mergeLabel) nextCheck
        return (resultOp, fromBlock)
      _ -> do
        resultOp <- lowerExpr caseExpr
        fromBlock <- getsS lsCurrentBlock
        sealBlock (LBr mergeLabel) nextCheck
        return (resultOp, fromBlock)

  -- Failure block → error + unreachable, then merge
  errStr <- getOrCreateString "pattern match failure"
  _ <- emitFresh "_err_" LTVoid
    (LCall "tlm_error" [LLitString errStr "pattern match failure"] LTVoid)
  sealBlock (LBr mergeLabel) mergeLabel  -- unreachable path, but need valid block

  -- Merge with phi
  let retTy = case results of
        ((op, _):_) -> operandType op
        []          -> LTInt64
  emitFresh "phi_" retTy (LPhi results retTy)

-- ============================================================================
-- Module lowering
-- ============================================================================

-- | Lower CLM functions to an LIR module.
-- ExternMap provides declarative extern specs; pass Map.empty for legacy mode.
lowerModule :: Name                    -- Module name
            -> HashMap Name CLMLam     -- clmLambdas (top-level)
            -> HashMap Name CLMLam     -- clmInstances
            -> [Name]                  -- Functions to compile (empty = all top-level)
            -> ExternMap               -- Declarative extern specs from Native.tl
            -> IO (Either LowerError LModule)
lowerModule modName clmLams clmInsts requested extMap = do
  let allFuncs = Map.union clmLams clmInsts
      funcMap = Map.mapWithKey (\k lam ->
        let llName = "tulam_" ++ sanitizeName k
            ptys   = [inferParamType n | (n, _) <- clmLamParams lam]
        in (llName, ptys, LTInt64)) allFuncs

      toCompile
        | null requested = Map.toList clmLams
        | otherwise = [(n, lam) | (n, lam) <- Map.toList allFuncs, n `elem` requested]

  results <- mapM (\(n, lam) -> lowerFunction n lam funcMap extMap) toCompile

  case sequence results of
    Left err -> return (Left err)
    Right funcsAndGlobals -> do
      let (funcs, globalss) = unzip funcsAndGlobals
          -- Add extern declarations for non-inline externs used by the module
          externs = externDeclarations extMap
      return $ Right $ LModule modName (concat globalss) funcs externs

-- ============================================================================
-- Intrinsic mapping
-- ============================================================================

-- | Map intrinsic function names to LIR instruction builders.
-- Key format: "funcName\0TypeName" for instance-resolved intrinsics.
intrinsicToLIR :: Name -> Maybe ([LOperand] -> LInstr)
-- Arithmetic
intrinsicToLIR "add\0Int"      = Just $ \[a,b] -> LAdd a b
intrinsicToLIR "sub\0Int"      = Just $ \[a,b] -> LSub a b
intrinsicToLIR "mul\0Int"      = Just $ \[a,b] -> LMul a b
intrinsicToLIR "div\0Int"      = Just $ \[a,b] -> LDiv a b
intrinsicToLIR "rem\0Int"      = Just $ \[a,b] -> LRem a b
intrinsicToLIR "negate\0Int"   = Just $ \[a]   -> LNeg a

intrinsicToLIR "add\0Float64"  = Just $ \[a,b] -> LFAdd a b
intrinsicToLIR "sub\0Float64"  = Just $ \[a,b] -> LFSub a b
intrinsicToLIR "mul\0Float64"  = Just $ \[a,b] -> LFMul a b
intrinsicToLIR "div\0Float64"  = Just $ \[a,b] -> LFDiv a b
intrinsicToLIR "negate\0Float64" = Just $ \[a] -> LFNeg a

-- Comparison
intrinsicToLIR "eq\0Int"       = Just $ \[a,b] -> LICmpEq a b
intrinsicToLIR "neq\0Int"      = Just $ \[a,b] -> LICmpNe a b
intrinsicToLIR "lt\0Int"       = Just $ \[a,b] -> LICmpLt a b
intrinsicToLIR "le\0Int"       = Just $ \[a,b] -> LICmpLe a b
intrinsicToLIR "gt\0Int"       = Just $ \[a,b] -> LICmpGt a b
intrinsicToLIR "ge\0Int"       = Just $ \[a,b] -> LICmpGe a b

intrinsicToLIR "eq\0Float64"   = Just $ \[a,b] -> LFCmpEq a b
intrinsicToLIR "lt\0Float64"   = Just $ \[a,b] -> LFCmpLt a b
intrinsicToLIR "le\0Float64"   = Just $ \[a,b] -> LFCmpLe a b
intrinsicToLIR "gt\0Float64"   = Just $ \[a,b] -> LFCmpGt a b
intrinsicToLIR "ge\0Float64"   = Just $ \[a,b] -> LFCmpGe a b

-- Bitwise
intrinsicToLIR "band\0Int"     = Just $ \[a,b] -> LAnd a b
intrinsicToLIR "bor\0Int"      = Just $ \[a,b] -> LOr a b
intrinsicToLIR "bxor\0Int"     = Just $ \[a,b] -> LXor a b
intrinsicToLIR "shl\0Int"      = Just $ \[a,b] -> LShl a b
intrinsicToLIR "shr\0Int"      = Just $ \[a,b] -> LAShr a b

-- Print intrinsics (runtime calls)
intrinsicToLIR "print_int"     = Just $ \[a] -> LCall "tlm_print_int" [a] LTVoid
intrinsicToLIR "print_float"   = Just $ \[a] -> LCall "tlm_print_float" [a] LTVoid
intrinsicToLIR "print_string"  = Just $ \[a] -> LCall "tlm_print_string" [a] LTVoid
intrinsicToLIR "print_char"    = Just $ \[a] -> LCall "tlm_print_char" [a] LTVoid
intrinsicToLIR "print_bool"    = Just $ \[a] -> LCall "tlm_print_bool" [a] LTVoid
intrinsicToLIR "print_newline" = Just $ \[]  -> LCall "tlm_print_newline" [] LTVoid

intrinsicToLIR _ = Nothing

-- ============================================================================
-- Helpers
-- ============================================================================

emptyLowerState :: LowerState
emptyLowerState = LowerState 0 0 [] [] "entry" [] Map.empty Map.empty Map.empty Map.empty

clmLamParams :: CLMLam -> [CLMVar]
clmLamParams (CLMLam vars _)      = vars
clmLamParams (CLMLamCases vars _) = vars

-- | Sanitize a tulam name to be a valid LLVM IR identifier.
sanitizeName :: Name -> Name
sanitizeName = concatMap go
  where
    go c | c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") = [c]
    go '\0' = "_"
    go '+' = "_plus_"
    go '-' = "_minus_"
    go '*' = "_star_"
    go '/' = "_slash_"
    go '=' = "_eq_"
    go '<' = "_lt_"
    go '>' = "_gt_"
    go '!' = "_bang_"
    go '&' = "_amp_"
    go '|' = "_pipe_"
    go '^' = "_caret_"
    go '~' = "_tilde_"
    go '#' = "_hash_"
    go '.' = "_dot_"
    go _   = "_"

-- | Phase A.1: all params default to i64.
inferParamType :: Name -> LType
inferParamType _ = LTInt64

-- | Coerce an operand to i64 for storing in a heap object field.
coerceToI64ForStore :: LOperand -> LowerM LOperand
coerceToI64ForStore op = case operandType op of
  LTInt64   -> return op
  LTWord64  -> return op
  LTPtr     -> emitFresh "ptoi_" LTInt64 (LPtrToInt op LTInt64)
  LTInt32   -> emitFresh "sext_" LTInt64 (LSext op LTInt64)
  LTInt16   -> emitFresh "sext_" LTInt64 (LSext op LTInt64)
  LTInt8    -> emitFresh "sext_" LTInt64 (LSext op LTInt64)
  LTWord32  -> emitFresh "zext_" LTInt64 (LZext op LTInt64)
  LTWord16  -> emitFresh "zext_" LTInt64 (LZext op LTInt64)
  LTWord8   -> emitFresh "zext_" LTInt64 (LZext op LTInt64)
  LTBool    -> emitFresh "zext_" LTInt64 (LZext op LTInt64)
  LTChar    -> emitFresh "sext_" LTInt64 (LSext op LTInt64)
  LTFloat64 -> emitFresh "f2i_"  LTInt64 (LBitcast op LTInt64)
  LTFloat32 -> do
    ext <- emitFresh "fext_" LTFloat64 (LFPExt op LTFloat64)
    emitFresh "f2i_" LTInt64 (LBitcast ext LTInt64)
  _         -> return op
