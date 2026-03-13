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
  , detectNullaryAsNull
  ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HSet
import Control.Monad (forM, forM_, zipWithM)
import Data.List (partition)
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
-- Type conversions
llvmInstrBuilder "sitofp" 1    = Just $ \[a] -> LSIToFP a LTFloat64
llvmInstrBuilder "fptosi" 1    = Just $ \[a] -> LFPToSI a LTInt64
llvmInstrBuilder "fpext" 1     = Just $ \[a] -> LFPExt a LTFloat64
llvmInstrBuilder "fptrunc" 1   = Just $ \[a] -> LFPTrunc a LTFloat32
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
      in LFunction symName params (eiRetType info) [] True []

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
  , lsGlobalPrefix  :: !Name                 -- Prefix for global names (function-scoped uniqueness)
  , lsLiftedFuncs   :: [LFunction]           -- Lambda-lifted anonymous functions
  , lsLiftedCount   :: !Int                  -- Counter for unique lifted function names
  , lsSingletonMap  :: HashMap Int Name      -- Zero-field constructor tag → global name
  , lsNullaryAsNull :: HSet.HashSet Int     -- Tags represented as null pointers (Phase 2)
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
      prefix <- getsS lsGlobalPrefix
      n <- getsS lsNextVar
      modifyS $ \s -> s { lsNextVar = n + 1 }
      let gname = prefix ++ "str_" ++ show n
          global = LGlobalString gname str
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

-- Special case: modifyRef(ref, fn(x) = body) → inline as writeref(ref, body[x := readref(ref)])
lowerExpr (CLMAPP (CLMID "__modifyref") [refExpr, CLMLAM (CLMLam [(varName, _)] body)]) = do
  refOp <- lowerExpr refExpr
  -- Read current value
  curVal <- emitFresh "refval_" LTInt64 (LCall "__readref" [refOp] LTInt64)
  -- Bind lambda param to current value and evaluate body
  bindVar varName curVal
  newVal <- lowerExpr body
  -- Write back (coerce to i64 — __writeref uses i64 for all value types)
  coercedVal <- coerceArg newVal LTInt64
  emitFresh "_wref_" LTVoid (LCall "__writeref" [refOp, coercedVal] LTVoid)

-- Direct function application: CLMAPP (CLMID funcName) args
-- Resolution order: (1) declarative ExternMap, (2) legacy intrinsicToLIR, (3) function call
lowerExpr (CLMAPP (CLMID funcName) args) = do
  extMap <- getsS lsExternMap
  case resolveExtern extMap funcName of
    Just mkInstr -> do
      ops <- mapM lowerExpr args
      -- For runtime calls, coerce args to expected param types
      let ei = Map.lookup funcName extMap
      coercedOps <- case ei of
        Just info | not (null (eiParamTypes info)) ->
          zipWithM coerceArg ops (eiParamTypes info ++ repeat LTInt64)
        _ -> return ops
      let instr = mkInstr coercedOps
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
            (_, expectedPtys, retTy) = case Map.lookup funcName fmap' of
              Just info -> info
              Nothing   -> (llvmName, map operandType ops, LTInt64)
        -- Coerce arguments to match expected parameter types
        coercedOps <- zipWithM coerceArg ops (expectedPtys ++ repeat LTInt64)
        emitFresh "r_" retTy (LCall llvmName coercedOps retTy)

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

-- Bool constructors: unboxed i1 values (no heap allocation)
lowerExpr (CLMCON (ConsTag "True" _) [])  = return $ LLitBool True
lowerExpr (CLMCON (ConsTag "False" _) []) = return $ LLitBool False

-- Nullary constructor: use null pointer (Phase 2 optimization)
-- Null is safe because arena never returns null (exits on OOM).
-- Singletons are kept as fallback for pure enum types where all constructors are nullary.
lowerExpr (CLMCON (ConsTag _cname tag) []) = do
  nullSet <- getsS lsNullaryAsNull
  if HSet.member tag nullSet
    then return LLitNull
    else do
      -- Allocate every time. Singleton optimization is unsound in SSA because
      -- a value defined in one branch can't be used in a sibling branch (LLVM
      -- dominance rules). Zero-field allocs are cheap (~one function call).
      emitFresh "obj_" LTPtr (LAlloc tag 0)

-- Constructor with fields: inline bump allocation (Phase 1)
lowerExpr (CLMCON (ConsTag _cname tag) fields) = do
  ops <- mapM lowerExpr fields
  obj <- emitFresh "obj_" LTPtr (LAllocInline tag (length ops))
  forM_ (zip [0..] ops) $ \(i, op) -> do
    val64 <- coerceToI64ForStore op
    emitFresh "_s_" LTVoid (LStore val64 obj i)
  return obj

-- Field access
lowerExpr (CLMFieldAccess (_name, idx) obj) = do
  objOp <- lowerExpr obj
  -- Coerce to ptr if needed (e.g., i64 from __mutread containing a heap pointer)
  objPtr <- coerceArg objOp LTPtr
  emitFresh "fld_" LTInt64 (LLoad objPtr idx LTInt64)

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

-- Lambda value — lift to top-level function, return function pointer
lowerExpr (CLMLAM (CLMLam vars body)) = liftLambda vars body
lowerExpr (CLMLAM (CLMLamCases vars cases)) =
    -- Wrap cases in CLMPROG for lowering
    liftLambda vars (CLMPROG cases)
lowerExpr (CLMLAM clm)        = throwL $ "Unsupported CLMLam shape: " ++ take 200 (show clm)

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
  case operandType scrut of
    -- Bool is unboxed i1: True=tag0 maps to i1=1, False=tag1 maps to i1=0
    LTBool ->
      if tagVal == 0  -- True constructor
        then return scrut
        else emitFresh "not_" LTBool (LXor scrut (LLitBool True))
    -- Normal ADT: get tag from heap object (coerce to ptr if needed)
    _ -> do
      scrutPtr <- coerceArg scrut LTPtr
      tagOp <- emitFresh "tag_" LTInt16 (LGetTag scrutPtr)
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

-- | Lower a named CLMLam to LFunctions + any generated globals.
-- Returns a list of functions (main + any lambda-lifted helpers) and globals.
-- The ExternMap provides declarative extern specs from Native.tl.
-- Pass Map.empty to use only the legacy intrinsicToLIR mapping.
lowerFunction :: Name -> CLMLam -> HashMap Name (Name, [LType], LType)
              -> ExternMap
              -> HSet.HashSet Int    -- ^ Nullary tags represented as null (from compilation plan)
              -> IO (Either LowerError ([LFunction], [LGlobal]))
lowerFunction name clmLam funcMap extMap nullaryTags = do
  let llvmName = "tulam_" ++ sanitizeName name
      vars = clmLamParams clmLam
      -- Use types from funcTypeMap if available (correct ADT types from Lambda annotations)
      paramTypes = case Map.lookup name funcMap of
                     Just (_, ptys, _) -> ptys
                     Nothing -> map (const LTInt64) vars
      params = zipWith (\(n, _) ty -> (sanitizeName n, ty)) vars
                       (paramTypes ++ repeat LTInt64)  -- pad with i64 if mismatch
      retTy = case Map.lookup name funcMap of
                Just (_, _, rt) -> rt
                Nothing         -> LTInt64
      initEnv = Map.fromList [(n, LVar (sanitizeName n) ty) | (n, ty) <- params]
      initState = emptyLowerState
        { lsEnv = initEnv
        , lsFuncMap = funcMap
        , lsExternMap = extMap
        , lsCurrentBlock = "entry"
        , lsGlobalPrefix = sanitizeName name ++ "_"
        , lsNullaryAsNull = nullaryTags
        }
  result <- runLowerM initState $ case clmLam of
    CLMLam _vars body -> do
      resultOp <- lowerExpr body
      -- Coerce result to match function return type
      finalOp <- if retTy == LTVoid || operandType resultOp == LTVoid
                  then return resultOp
                  else coerceArg resultOp retTy
      st <- getS
      let instrs = reverse (lsCurrentInstrs st)
          -- Tail call optimization: if the last instruction is a call whose
          -- result is directly returned, replace call+ret with musttail call.
          (instrs', terminator) = optimizeTailCall instrs finalOp retTy
          finalBlock = LBlock (lsCurrentBlock st) instrs' terminator
          -- Optimize tail calls across all blocks (handles phi+trampoline chains)
          allBlocks = optimizeTailCalls (reverse (finalBlock : lsBlocks st))
      return (allBlocks, reverse (lsGlobals st))

    CLMLamCases _vars cases -> do
      (blocks, globals) <- lowerCaseChain cases retTy
      return (blocks, globals)

  case result of
    Left err -> return (Left err)
    Right ((blocks, globals), finalSt) ->
      let attrs = computeFuncAttrs blocks
          mainFunc = LFunction llvmName params retTy blocks False attrs
          lifted = lsLiftedFuncs finalSt
      in return $ Right (mainFunc : lifted, globals)

-- | Try to extract a simple tag-switch pattern from case arms.
-- Returns (scrutinee name, [(tag, body)], Maybe defaultBody) if all arms
-- are single CLMCheckTag checks on the same CLMID scrutinee.
extractTagSwitch :: [CLMExpr] -> Maybe (Name, [(Int, CLMExpr)], Maybe CLMExpr)
extractTagSwitch cases = go cases [] Nothing
  where
    go [] tagBodies defBody = case tagBodies of
      [] -> Nothing
      _  -> Just (scrutName (head tagBodies), map snd (reverse tagBodies), defBody)
    go (CLMCASE [] body : rest) tagBodies _ =
      -- Default arm (no checks) — must be last or near-last
      go rest tagBodies (Just body)
    go (CLMCASE [CLMCheckTag (ConsTag _ tag) (CLMID scrut)] body : rest) tagBodies defBody =
      case tagBodies of
        [] -> go rest ((scrut, (tag, body)) : tagBodies) defBody
        ((s, _) : _) | s == scrut -> go rest ((scrut, (tag, body)) : tagBodies) defBody
        _ -> Nothing  -- different scrutinees
    go _ _ _ = Nothing  -- non-simple pattern

    scrutName (n, _) = n

-- | Lower a chain of CLMCASE alternatives into a linear check chain.
lowerCaseChain :: [CLMExpr] -> LType -> LowerM ([LBlock], [LGlobal])
lowerCaseChain cases retTy' =
  -- Try switch optimization first (skip for boolean scrutinees — they're unboxed i1)
  case extractTagSwitch cases of
    Just (scrutName, tagCases, mDefault) -> do
      mOp <- lookupVar scrutName
      let isBool = maybe False (\op -> operandType op == LTBool) mOp
      if isBool
        then lowerCaseChainLinear cases retTy'
        else lowerTagSwitchRet scrutName tagCases mDefault retTy'
    Nothing -> lowerCaseChainLinear cases retTy'

-- | Switch-based case chain (return version: each arm returns).
lowerTagSwitchRet :: Name -> [(Int, CLMExpr)] -> Maybe CLMExpr -> LType -> LowerM ([LBlock], [LGlobal])
lowerTagSwitchRet scrutName tagCases mDefault retTy' = do
  -- Lower the scrutinee and get its tag
  mScrutOp <- lookupVar scrutName
  scrutOp <- case mScrutOp of
    Just op -> return op
    Nothing -> throwL $ "switch: unbound scrutinee " ++ scrutName
  scrutPtr <- coerceArg scrutOp LTPtr
  tagOp <- emitFresh "sw_tag_" LTInt16 (LGetTag scrutPtr)

  -- Generate body labels
  bodyLabels <- forM (zip [0..] tagCases) $ \(i, _) ->
    freshBlock ("sw_body_" ++ show (i :: Int) ++ "_")
  defLabel <- freshBlock "sw_default_"
  failLabel <- freshBlock "match_fail_"

  -- Emit switch
  let switchCases = [ (fromIntegral tag, lbl) | ((tag, _), lbl) <- zip tagCases bodyLabels ]
      defTarget = case mDefault of { Just _ -> defLabel; Nothing -> failLabel }
  sealBlock (LSwitch tagOp defTarget switchCases) (head bodyLabels)

  -- Emit each body
  let nextTargets = tail bodyLabels ++ [defLabel]
  forM_ (zip3 tagCases bodyLabels nextTargets) $ \((_tag, body), _bodyLabel, nextLabel) -> do
    savedEnv <- getsS lsEnv
    resultOp <- lowerExpr body
    coercedOp <- coerceArg resultOp retTy'
    modifyS $ \s -> s { lsEnv = savedEnv }
    sealBlock (LRet coercedOp) nextLabel

  -- Default body or failure
  case mDefault of
    Just defBody -> do
      savedEnv <- getsS lsEnv
      resultOp <- lowerExpr defBody
      coercedOp <- coerceArg resultOp retTy'
      modifyS $ \s -> s { lsEnv = savedEnv }
      sealBlock (LRet coercedOp) failLabel
    Nothing -> return ()

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

-- | Linear case chain fallback (original implementation).
lowerCaseChainLinear :: [CLMExpr] -> LType -> LowerM ([LBlock], [LGlobal])
lowerCaseChainLinear cases retTy' = do
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
  -- IMPORTANT: save/restore env to prevent nested matches from clobbering bindings.
  let nextChecks = map fst (tail labels) ++ [failLabel]
  forM_ (zip3 cases labels nextChecks) $ \(caseExpr, (_chkLabel, bodyLabel), nextCheck) -> do
    savedEnv <- getsS lsEnv
    case caseExpr of
      CLMCASE checks body -> do
        if null checks
          then sealBlock (LBr bodyLabel) bodyLabel
          else do
            cond <- lowerPatternChecks checks
            sealBlock (LCondBr cond bodyLabel nextCheck) bodyLabel
        resultOp <- lowerExpr body
        coercedOp <- coerceArg resultOp retTy'
        modifyS $ \s -> s { lsEnv = savedEnv }
        sealBlock (LRet coercedOp) nextCheck
      _ -> do
        resultOp <- lowerExpr caseExpr
        coercedOp <- coerceArg resultOp retTy'
        modifyS $ \s -> s { lsEnv = savedEnv }
        sealBlock (LRet coercedOp) nextCheck

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
--
-- Uses trampoline blocks to handle mixed-type results: each case body
-- branches to its own trampoline block where type coercion happens,
-- then all trampolines branch to the merge block with uniform types.
lowerCaseExprChain :: [CLMExpr] -> LowerM LOperand
lowerCaseExprChain [] = return LLitNull
lowerCaseExprChain cases =
  -- Try switch optimization first
  case extractTagSwitch cases of
    Just (scrutName, tagCases, mDefault) -> do
      mOp <- lookupVar scrutName
      let isBool = maybe False (\op -> operandType op == LTBool) mOp
      if isBool
        then lowerCaseExprChainLinear cases
        else lowerTagSwitchExpr scrutName tagCases mDefault
    Nothing -> lowerCaseExprChainLinear cases

-- | Switch-based case chain (expression version: produces phi).
lowerTagSwitchExpr :: Name -> [(Int, CLMExpr)] -> Maybe CLMExpr -> LowerM LOperand
lowerTagSwitchExpr scrutName tagCases mDefault = do
  mergeLabel <- freshBlock "sw_merge_"
  failLabel  <- freshBlock "sw_fail_"

  -- Lower scrutinee and extract tag
  mScrutOp <- lookupVar scrutName
  scrutOp <- case mScrutOp of
    Just op -> return op
    Nothing -> throwL $ "switch: unbound scrutinee " ++ scrutName
  scrutPtr <- coerceArg scrutOp LTPtr
  tagOp <- emitFresh "sw_tag_" LTInt16 (LGetTag scrutPtr)

  -- Generate body + trampoline labels
  labels <- forM (zip [0..] tagCases) $ \(i, _) -> do
    bd   <- freshBlock ("swb_" ++ show (i :: Int) ++ "_")
    trmp <- freshBlock ("swt_" ++ show (i :: Int) ++ "_")
    return (bd, trmp)
  defLabel <- freshBlock "sw_def_"

  -- Emit switch
  let switchCases = [ (fromIntegral tag, fst lbl) | ((tag, _), lbl) <- zip tagCases labels ]
      defTarget = case mDefault of { Just _ -> defLabel; Nothing -> failLabel }
  sealBlock (LSwitch tagOp defTarget switchCases) (fst (head labels))

  -- Emit each body → trampoline
  let nextBlocks = map fst (tail labels) ++ [defLabel]
  bodyResults <- forM (zip3 tagCases labels nextBlocks) $
    \((_tag, body), (_bdLabel, trmpLabel), nextBlock) -> do
      savedEnv <- getsS lsEnv
      resultOp <- lowerExpr body
      modifyS $ \s -> s { lsEnv = savedEnv }
      sealBlock (LBr trmpLabel) nextBlock
      return (resultOp, trmpLabel)

  -- Default body
  defResult <- case mDefault of
    Just defBody -> do
      defTrmp <- freshBlock "swt_def_"
      savedEnv <- getsS lsEnv
      resultOp <- lowerExpr defBody
      modifyS $ \s -> s { lsEnv = savedEnv }
      sealBlock (LBr defTrmp) failLabel
      return [(resultOp, defTrmp)]
    Nothing -> do
      sealBlock (LBr failLabel) failLabel
      return []

  -- Failure block
  errStr <- getOrCreateString "pattern match failure"
  _ <- emitFresh "_err_" LTVoid
    (LCall "tlm_error" [LLitString errStr "pattern match failure"] LTVoid)
  let allResults = bodyResults ++ defResult
      allTrmps = map snd allResults
  sealBlock LUnreachable (head allTrmps)

  -- Determine common type
  let commonTy = case allResults of
        [] -> LTInt64
        _  -> if any (\(op, _) -> operandType op == LTVoid) allResults
              then LTVoid
              else if any (\(op, _) -> operandType op == LTPtr) allResults
              then LTPtr
              else operandType (fst (head allResults))

  -- Process trampolines: coerce, branch to merge
  let nextTrmps = tail allTrmps ++ [mergeLabel]
  coercedResults <- forM (zip allResults nextTrmps) $
    \((resultOp, _trmpLabel), nextTrmp) -> do
      coercedOp <- if commonTy == LTVoid
                   then return resultOp
                   else coerceArg resultOp commonTy
      curBlock <- getsS lsCurrentBlock
      sealBlock (LBr mergeLabel) nextTrmp
      return (coercedOp, curBlock)

  -- Merge with phi
  if commonTy == LTVoid
    then return LLitNull
    else emitFresh "phi_" commonTy (LPhi coercedResults commonTy)

-- | Linear case chain fallback for expressions.
lowerCaseExprChainLinear :: [CLMExpr] -> LowerM LOperand
lowerCaseExprChainLinear cases = do
  mergeLabel <- freshBlock "match_merge_"
  failLabel  <- freshBlock "match_fail_"

  -- Generate labels for each case: check + body + trampoline
  labels <- forM (zip [0..] cases) $ \(i, _) -> do
    chk  <- freshBlock ("mc" ++ show (i :: Int) ++ "_chk_")
    bd   <- freshBlock ("mc" ++ show (i :: Int) ++ "_bd_")
    trmp <- freshBlock ("mc" ++ show (i :: Int) ++ "_trmp_")
    return (chk, bd, trmp)

  -- Branch from current block to first case check
  let firstCheck = (\(c,_,_) -> c) (head labels)
  sealBlock (LBr firstCheck) firstCheck

  -- For each case: check guards → body or next, body → trampoline
  -- IMPORTANT: save/restore env around each case body to prevent
  -- nested pattern matches from clobbering outer scrutinee bindings.
  let nextChecks = map (\(c,_,_) -> c) (tail labels) ++ [failLabel]
  bodyResults <- forM (zip3 cases labels nextChecks) $
    \(caseExpr, (_chkLabel, bodyLabel, trmpLabel), nextCheck) -> do
    savedEnv <- getsS lsEnv
    case caseExpr of
      CLMCASE checks caseBody -> do
        if null checks
          then sealBlock (LBr bodyLabel) bodyLabel
          else do
            cond <- lowerPatternChecks checks
            sealBlock (LCondBr cond bodyLabel nextCheck) bodyLabel
        resultOp <- lowerExpr caseBody
        modifyS $ \s -> s { lsEnv = savedEnv }
        -- Seal body → trampoline, then start next check block
        sealBlock (LBr trmpLabel) nextCheck
        return (resultOp, trmpLabel)
      _ -> do
        resultOp <- lowerExpr caseExpr
        modifyS $ \s -> s { lsEnv = savedEnv }
        sealBlock (LBr trmpLabel) nextCheck
        return (resultOp, trmpLabel)

  -- Failure block: error + unreachable, then start first trampoline
  errStr <- getOrCreateString "pattern match failure"
  _ <- emitFresh "_err_" LTVoid
    (LCall "tlm_error" [LLitString errStr "pattern match failure"] LTVoid)
  let trmpLabels = map (\(_,_,t) -> t) labels
  sealBlock LUnreachable (head trmpLabels)

  -- Determine common type: void takes priority (can't coerce void), then ptr
  let commonTy = case bodyResults of
        [] -> LTInt64
        _  -> if any (\(op, _) -> operandType op == LTVoid) bodyResults
              then LTVoid
              else if any (\(op, _) -> operandType op == LTPtr) bodyResults
              then LTPtr
              else operandType (fst (head bodyResults))

  -- Process trampolines: coerce each result to commonTy, branch to merge
  -- Each trampoline block is visited sequentially via sealBlock chaining.
  let nextTrmps = tail trmpLabels ++ [mergeLabel]
  coercedResults <- forM (zip bodyResults nextTrmps) $
    \((resultOp, trmpLabel), nextTrmp) -> do
      coercedOp <- if commonTy == LTVoid
                   then return resultOp
                   else coerceArg resultOp commonTy
      sealBlock (LBr mergeLabel) nextTrmp
      return (coercedOp, trmpLabel)

  -- Merge with phi
  if commonTy == LTVoid
    then return LLitNull
    else emitFresh "phi_" commonTy (LPhi coercedResults commonTy)

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

  -- Detect nullary-as-null tags globally across all functions (Phase 2)
  let globalNullary = mconcat [detectNullaryAsNull lam | (_, lam) <- toCompile]
  results <- mapM (\(n, lam) -> lowerFunction n lam funcMap extMap globalNullary) toCompile

  case sequence results of
    Left err -> return (Left err)
    Right funcsAndGlobals -> do
      let (funcLists, globalss) = unzip funcsAndGlobals
          funcs = concat funcLists
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
emptyLowerState = LowerState 0 0 [] [] "entry" [] Map.empty Map.empty Map.empty Map.empty "" [] 0 Map.empty HSet.empty

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

-- | Coerce an operand to match an expected type at a call site.
-- Inserts inttoptr/ptrtoint/zext/trunc as needed.
coerceArg :: LOperand -> LType -> LowerM LOperand
coerceArg op expected
  | operandType op == expected = return op
  | operandType op == LTInt64 && expected == LTPtr =
      emitFresh "i2p_" LTPtr (LIntToPtr op)
  | operandType op == LTPtr && expected == LTInt64 =
      emitFresh "p2i_" LTInt64 (LPtrToInt op LTInt64)
  | operandType op == LTBool && expected == LTInt64 =
      emitFresh "zext_" LTInt64 (LZext op LTInt64)
  | operandType op == LTInt64 && expected == LTBool =
      emitFresh "trunc_" LTBool (LTrunc op LTBool)
  | operandType op == LTBool && expected == LTPtr =
      -- Bool → i64 → ptr (two-step)
      do ext <- emitFresh "zext_" LTInt64 (LZext op LTInt64)
         emitFresh "i2p_" LTPtr (LIntToPtr ext)
  | operandType op == LTInt64 && expected == LTFloat64 =
      emitFresh "i2f_" LTFloat64 (LBitcast op LTFloat64)
  | operandType op == LTFloat64 && expected == LTInt64 =
      emitFresh "f2i_" LTInt64 (LBitcast op LTInt64)
  | operandType op == LTFloat64 && expected == LTPtr =
      do bc <- emitFresh "f2i_" LTInt64 (LBitcast op LTInt64)
         emitFresh "i2p_" LTPtr (LIntToPtr bc)
  | operandType op == LTPtr && expected == LTFloat64 =
      do bc <- emitFresh "p2i_" LTInt64 (LPtrToInt op LTInt64)
         emitFresh "i2f_" LTFloat64 (LBitcast bc LTFloat64)
  | otherwise = return op  -- trust the types match close enough

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

-- ============================================================================
-- Nullary-as-null detection (Phase 2)
-- ============================================================================

-- | Detect nullary constructors that can be represented as null pointers.
-- Currently disabled: returns empty set. The null-pointer optimization requires
-- null guards at ALL LGetTag sites, which adds complexity to the switch-based
-- and linear pattern matching paths. Phase 1 (inline alloc) and Phase 3
-- (function attributes) provide the main performance wins.
-- TODO: Enable when all pattern match paths handle null correctly.
detectNullaryAsNull :: CLMLam -> HSet.HashSet Int
detectNullaryAsNull _clmLam = HSet.empty

-- | Collect (nullary tags, non-nullary tags) from all CLMCON nodes in a CLM expr.
collectConTags :: CLMExpr -> (HSet.HashSet Int, HSet.HashSet Int)
collectConTags (CLMCON (ConsTag _ tag) [])     = (HSet.singleton tag, HSet.empty)
collectConTags (CLMCON (ConsTag "True" _) _)   = (HSet.empty, HSet.empty)  -- Bool: unboxed
collectConTags (CLMCON (ConsTag "False" _) _)  = (HSet.empty, HSet.empty)
collectConTags (CLMCON (ConsTag _ tag) (_:_))  = (HSet.empty, HSet.singleton tag)
collectConTags (CLMAPP f args) = mconcat (map collectConTags (f : args))
collectConTags (CLMPROG es)    = mconcat (map collectConTags es)
collectConTags (CLMBIND _ e)   = collectConTags e
collectConTags (CLMCASE _ b)   = collectConTags b
collectConTags (CLMTYPED e _)  = collectConTags e
collectConTags (CLMFieldAccess _ e) = collectConTags e
collectConTags (CLMLAM (CLMLam _ b))       = collectConTags b
collectConTags (CLMLAM (CLMLamCases _ cs)) = mconcat (map collectConTags cs)
collectConTags (CLMIAP f args) = mconcat (map collectConTags (f : args))
collectConTags _ = (HSet.empty, HSet.empty)

-- ============================================================================
-- Function attributes (Phase 3)
-- ============================================================================

-- | Compute LLVM function attributes based on function body.
-- Small functions get alwaysinline, all get nounwind.
computeFuncAttrs :: [LBlock] -> [String]
computeFuncAttrs blocks =
  let instrCount = sum [length (lblockInstrs b) | b <- blocks]
      -- Count only non-trivial instructions (skip phi, copy, trampoline branches).
      -- Pure functions with nested if/else create many blocks but few real ops.
      isSmall = instrCount <= 20
      hasAlloc = any blockHasAlloc blocks
      hasStore = any blockHasStore blocks
      hasCall = any blockHasCall blocks
      base = ["nounwind"]
      inline = if isSmall && not hasCall then ["alwaysinline"] else []
      readonly = if not hasAlloc && not hasStore then ["readonly"] else []
  in base ++ inline ++ readonly
  where
    blockHasAlloc b = any isAllocInstr (map snd (lblockInstrs b))
    blockHasStore b = any isStoreInstr (map snd (lblockInstrs b))
    isAllocInstr (LAlloc _ _)        = True
    isAllocInstr (LAllocInline _ _)  = True
    isAllocInstr (LCall "tlm_alloc" _ _) = True
    isAllocInstr (LCall "tlm_alloc_slow" _ _) = True
    isAllocInstr _ = False
    isStoreInstr (LStore _ _ _) = True
    isStoreInstr _ = False
    blockHasCall b = any isCallInstr (map snd (lblockInstrs b))
    isCallInstr (LCall "tlm_error" _ _) = False  -- always in unreachable blocks
    isCallInstr (LCall _ _ _) = True
    isCallInstr (LCallPtr _ _ _) = True
    isCallInstr _ = False

-- ============================================================================
-- Lambda lifting (non-capturing lambdas → top-level functions)
-- ============================================================================

-- | Collect free variables from a CLM expression (names not bound locally).
freeVarsCLM :: CLMExpr -> HSet.HashSet Name
freeVarsCLM (CLMID n)           = HSet.singleton n
freeVarsCLM (CLMLIT _)          = HSet.empty
freeVarsCLM CLMEMPTY            = HSet.empty
freeVarsCLM (CLMU _)            = HSet.empty
freeVarsCLM CLMPRIMCALL         = HSet.empty
freeVarsCLM (CLMERR _ _)        = HSet.empty
freeVarsCLM (CLMAPP f args)     = HSet.unions (freeVarsCLM f : map freeVarsCLM args)
freeVarsCLM (CLMCON _ fields)   = HSet.unions (map freeVarsCLM fields)
freeVarsCLM (CLMFieldAccess _ e)= freeVarsCLM e
freeVarsCLM (CLMTYPED e _)     = freeVarsCLM e
freeVarsCLM (CLMPROG es)        = HSet.unions (map freeVarsCLM es)
freeVarsCLM (CLMBIND n e)       = HSet.delete n (freeVarsCLM e)
freeVarsCLM (CLMCASE _ body)    = freeVarsCLM body  -- simplified: checks bind vars but we don't track
freeVarsCLM (CLMIAP f args)     = HSet.unions (freeVarsCLM f : map freeVarsCLM args)
freeVarsCLM (CLMLAM (CLMLam vars body)) =
    HSet.difference (freeVarsCLM body) (HSet.fromList (map fst vars))
freeVarsCLM (CLMLAM (CLMLamCases vars cases)) =
    HSet.difference (HSet.unions (map freeVarsCLM cases)) (HSet.fromList (map fst vars))
freeVarsCLM _ = HSet.empty  -- conservative: other nodes treated as no free vars

-- | Lift a non-capturing lambda to a top-level function and return a function pointer.
-- For capturing lambdas, the captured values are passed as extra leading parameters
-- and a wrapper is emitted.
liftLambda :: [CLMVar] -> CLMExpr -> LowerM LOperand
liftLambda vars body = do
  -- Determine free variables (captures)
  let paramNames = HSet.fromList (map fst vars)
      bodyFVs = freeVarsCLM body
      -- Free vars that aren't lambda params and aren't known functions/externs
  funcMap <- getsS lsFuncMap
  extMap <- getsS lsExternMap
  let isKnownFunc n = Map.member n funcMap || Map.member n extMap
                       || "__" `isPrefixOfStr` n  -- runtime/extern functions
      captures = HSet.toList $ HSet.filter
        (\n -> not (HSet.member n paramNames) && not (isKnownFunc n)) bodyFVs

  -- Look up types of captured variables from current env
  env <- getsS lsEnv
  let captureOps = [(n, case Map.lookup n env of
                          Just op -> op
                          Nothing -> LVar n LTInt64  -- fallback
                       ) | n <- captures]

  -- Generate unique name for lifted function
  cnt <- getsS lsLiftedCount
  prefix <- getsS lsGlobalPrefix
  modifyS $ \s -> s { lsLiftedCount = cnt + 1 }
  let liftedName = "tulam_" ++ prefix ++ "lambda_" ++ show cnt

  -- Build param list: captures first, then lambda params
  let captureParams = [(n, operandType op) | (n, op) <- captureOps]
      lamParams = [(fst v, LTInt64) | v <- vars]  -- default to i64 for lambda params
      allParams = captureParams ++ lamParams

  -- Lower the body in a fresh sub-lowering context
  let bodyEnv = Map.fromList [(n, LVar n ty) | (n, ty) <- allParams]
  savedState <- getS
  let subState = emptyLowerState
        { lsEnv = bodyEnv
        , lsFuncMap = lsFuncMap savedState
        , lsExternMap = lsExternMap savedState
        , lsCurrentBlock = "entry"
        , lsGlobalPrefix = prefix ++ "lam" ++ show cnt ++ "_"
        , lsLiftedCount = lsLiftedCount savedState  -- share counter
        }

  result <- liftIO $ runLowerM subState $ do
    resultOp <- lowerExpr body
    st <- getS
    let terminator = if operandType resultOp == LTVoid then LRetVoid else LRet resultOp
        finalBlock = LBlock (lsCurrentBlock st) (reverse (lsCurrentInstrs st)) terminator
        allBlocks = reverse (finalBlock : lsBlocks st)
        retTy = operandType resultOp
    return (allBlocks, retTy, reverse (lsGlobals st), lsLiftedFuncs st, lsLiftedCount st)

  case result of
    Left (LowerError err) -> throwL $ "in lifted lambda: " ++ err
    Right ((blocks, retTy, globals, subLifted, newCount), _) -> do
      let liftedAttrs = computeFuncAttrs blocks
          liftedFunc = LFunction liftedName allParams retTy blocks False liftedAttrs
      -- Add lifted function and any sub-lifted functions + globals to state
      modifyS $ \s -> s
        { lsLiftedFuncs = liftedFunc : subLifted ++ lsLiftedFuncs s
        , lsGlobals = globals ++ lsGlobals s
        , lsLiftedCount = newCount
        }

      -- If no captures, return a simple function pointer reference
      if null captures
        then return $ LVar liftedName (LTFunPtr (map snd lamParams) retTy)
        else do
          -- With captures: emit a call-site wrapper that partially applies captures
          -- For now, error on true closures — we'd need PAP/closure objects
          throwL $ "Closures with captures not yet supported: captures=" ++ show captures
  where
    isPrefixOfStr prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Tail Call Optimization
-- ============================================================================

-- | Check if the last instruction in a block is a call whose result is
-- directly returned. If so, replace call+ret with musttail call terminator.
-- This converts O(n) stack recursive functions to O(1) stack.
optimizeTailCall :: [(Name, LInstr)] -> LOperand -> LType
                 -> ([(Name, LInstr)], LTerminator)
optimizeTailCall instrs retOp retTy =
    case (reverse instrs, retOp) of
        ((callName, LCall fn args callRetTy) : restRev, LVar varName _)
            | callName == varName ->
                if callRetTy == LTVoid || retTy == LTVoid
                then (reverse restRev, LTailCallVoid fn args)
                else (reverse restRev, LTailCall fn args callRetTy)
        _ -> (instrs, if retTy == LTVoid || operandType retOp == LTVoid
                       then LRetVoid
                       else LRet retOp)

-- | Optimize tail calls across all blocks in a function.
-- Handles both simple cases (call immediately before ret) and
-- complex cases (call → trampoline chain → merge block with phi+ret).
--
-- After converting calls to tail calls, cleans up dead phi sources and
-- removes unreachable blocks.
optimizeTailCalls :: [LBlock] -> [LBlock]
optimizeTailCalls blocks =
    let -- Phase 1: Find variables that flow to return via phi nodes
        tailVars = findTailReturnVars blocks
        -- Phase 2: Convert eligible calls to tail calls
        optimized = map (optimizeBlock tailVars) blocks
        -- Phase 3: Find blocks that now end with tail calls (no longer branch out)
        tailCallBlocks = HSet.fromList [lblockName b | b <- optimized, isTailCallTerm (lblockTerm b)]
        -- Phase 4: Clean up phi nodes and remove dead blocks
        cleaned = removeDeadPhiSources tailCallBlocks optimized
    in cleaned
  where
    isTailCallTerm (LTailCall _ _ _) = True
    isTailCallTerm (LTailCallVoid _ _) = True
    isTailCallTerm _ = False

    -- Find variables that flow directly to a return (possibly through phi nodes).
    findTailReturnVars :: [LBlock] -> HSet.HashSet Name
    findTailReturnVars bs =
        HSet.fromList $ concatMap findInBlock bs
      where
        findInBlock (LBlock _ instrs (LRet (LVar retName _))) =
            case [sources | (n, LPhi sources _) <- instrs, n == retName] of
                [sources] -> [vn | (LVar vn _, _) <- sources]
                _ -> [retName]
        findInBlock _ = []

    -- Optimize a single block
    optimizeBlock :: HSet.HashSet Name -> LBlock -> LBlock
    optimizeBlock _ block@(LBlock bname instrs (LRet (LVar retName _))) =
        case reverse instrs of
            (cn, LCall fn args crt) : restRev | cn == retName ->
                LBlock bname (reverse restRev) (LTailCall fn args crt)
            _ -> block
    optimizeBlock _ (LBlock bname instrs LRetVoid) =
        case reverse instrs of
            (_, LCall fn args LTVoid) : restRev ->
                LBlock bname (reverse restRev) (LTailCallVoid fn args)
            _ -> LBlock bname instrs LRetVoid
    optimizeBlock tailVars (LBlock bname instrs (LBr target)) =
        case reverse instrs of
            -- Non-void call whose result flows to return
            (cn, LCall fn args crt) : restRev | HSet.member cn tailVars ->
                LBlock bname (reverse restRev) (LTailCall fn args crt)
            -- Void call as last instruction before branch to merge/trampoline
            (_, LCall fn args LTVoid) : restRev | mergesIntoVoidRet target ->
                LBlock bname (reverse restRev) (LTailCallVoid fn args)
            _ -> LBlock bname instrs (LBr target)
    optimizeBlock _ block = block

    -- Check if a target label ultimately reaches ret void (through trampoline chains)
    mergesIntoVoidRet :: Name -> Bool
    mergesIntoVoidRet target =
        any (\b -> lblockName b == target &&
                   case (lblockInstrs b, lblockTerm b) of
                       ([], LBr next) -> mergesIntoVoidRet next
                       (_, LRetVoid)  -> True
                       _ -> False) blocks

    -- Remove phi sources that come from blocks that now do tail calls
    -- (since those blocks no longer branch to the merge block).
    -- Also remove trampoline blocks that are now unreachable.
    removeDeadPhiSources :: HSet.HashSet Name -> [LBlock] -> [LBlock]
    removeDeadPhiSources tcBlocks bs =
        let -- Trampoline blocks whose predecessors are now tail-calling
            -- A trampoline is: empty instrs + unconditional branch
            deadTrampolines = HSet.fromList
                [lblockName b | b <- bs
                , null (lblockInstrs b)
                , case lblockTerm b of LBr _ -> True; _ -> False
                -- Trampoline is dead if it has no live predecessors
                -- (predecessors either became tail calls or don't exist)
                , let preds = findPredecessors (lblockName b) bs
                , all (\p -> HSet.member p tcBlocks) preds
                ]
            allDead = HSet.union tcBlocks deadTrampolines
            -- Clean phi nodes: remove sources from dead blocks
            cleanedBlocks = map (cleanPhis allDead) bs
            -- Remove dead trampoline blocks (but keep tail-call blocks!)
            filtered = filter (\b -> not (HSet.member (lblockName b) deadTrampolines)) cleanedBlocks
        in filtered

    -- Find predecessor block names that branch to a given label
    findPredecessors :: Name -> [LBlock] -> [Name]
    findPredecessors target = concatMap (\b ->
        if branchesTo (lblockTerm b) target then [lblockName b] else [])
      where
        branchesTo (LBr t) tgt = t == tgt
        branchesTo (LCondBr _ t f) tgt = t == tgt || f == tgt
        branchesTo (LSwitch _ def cases) tgt = def == tgt || any (\(_, l) -> l == tgt) cases
        branchesTo _ _ = False

    -- Remove phi sources from dead/tail-calling blocks
    cleanPhis :: HSet.HashSet Name -> LBlock -> LBlock
    cleanPhis deadBlocks (LBlock bname instrs term) =
        LBlock bname (map cleanInstr instrs) term
      where
        cleanInstr (n, LPhi sources ty) =
            let sources' = filter (\(_, fromLabel) -> not (HSet.member fromLabel deadBlocks)) sources
            in case sources' of
                [(op, _)] -> (n, LCopy op)  -- Single source: degenerate phi → copy
                _         -> (n, LPhi sources' ty)
        cleanInstr x = x
