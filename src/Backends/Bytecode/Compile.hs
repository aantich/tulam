{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | CLM to bytecode compiler.
--
-- Compiles CLM expressions into register-based bytecode for the tulam VM.
-- Uses a simple linear register allocator with a compile-time environment
-- mapping names to registers.
module Backends.Bytecode.Compile
    ( compileCLMModule
    , compileCLMExpr
    , CompileError(..)
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word
import Data.IORef
import Data.List (foldl')

import Surface (Name, ConsTag(..), Literal(..), Level(..))
import CLM
import Logs (SourceInfo(..))
import Backends.Bytecode.Instruction
import Backends.Bytecode.Module
import Backends.Bytecode.Builtins

-- | Compilation errors.
data CompileError
    = UnboundVariable Name
    | UnsupportedCLM String
    | TooManyRegisters
    | TooManyConstants
    deriving (Show, Eq)

-- | Compiler state.
data CompState = CompState
    { csNextReg    :: !Int                -- next available register
    , csMaxReg     :: !Int                -- high water mark for registers
    , csCode       :: ![Instruction]      -- instructions (reversed, for efficient cons)
    , csConstants  :: ![Constant]         -- constant pool (reversed)
    , csConstMap   :: !(HashMap Constant Int)  -- dedup map: constant -> index
    , csFunctions  :: ![(Text, CLMLam, [Name])]  -- pending sub-functions: (name, body, captures)
    , csCompiledFuncs :: ![FuncInfo]      -- compiled function infos (reversed)
    , csAllCode    :: ![Instruction]      -- all emitted code across functions (reversed)
    , csEnv        :: !(HashMap Name Int) -- name -> register mapping
    , csFuncMap    :: !(HashMap Name Int) -- name -> function index mapping
    , csJumpTables :: ![JumpTable]        -- jump tables (reversed)
    , csNextFunc   :: !Int                -- next function index
    , csTailPos    :: !Bool               -- are we in tail position?
    , csConsNames  :: ![(Text, Int)]      -- constructor name -> tag mapping
    , csInstrCount :: !Int                -- monotonic instruction counter (for jump offset calculation)
    } deriving (Show)

-- Initial compiler state.
initCompState :: HashMap Name Int -> CompState
initCompState funcMap = CompState
    { csNextReg    = 0
    , csMaxReg     = 0
    , csCode       = []
    , csConstants  = []
    , csConstMap   = Map.empty
    , csFunctions  = []
    , csCompiledFuncs = []
    , csAllCode    = []
    , csEnv        = Map.empty
    , csFuncMap    = funcMap
    , csJumpTables = []
    , csNextFunc   = Map.size funcMap
    , csTailPos    = True
    , csConsNames  = []
    , csInstrCount = 0
    }

-- | Allocate a fresh register.
allocReg :: CompState -> (Int, CompState)
allocReg cs =
    let r = csNextReg cs
    in (r, cs { csNextReg = r + 1, csMaxReg = max (csMaxReg cs) (r + 1) })

-- | Allocate N consecutive registers, return the first one.
allocRegs :: Int -> CompState -> (Int, CompState)
allocRegs n cs =
    let r = csNextReg cs
    in (r, cs { csNextReg = r + n, csMaxReg = max (csMaxReg cs) (r + n) })

-- | Emit an instruction.
emit :: Instruction -> CompState -> CompState
emit instr cs = cs { csCode = instr : csCode cs
                   , csInstrCount = csInstrCount cs + 1 }

-- | Add a constant to the pool, deduplicating.
addConstant :: Constant -> CompState -> (Int, CompState)
addConstant k cs = case Map.lookup k (csConstMap cs) of
    Just idx -> (idx, cs)
    Nothing ->
        let idx = length (csConstants cs)
        in (idx, cs { csConstants = k : csConstants cs
                     , csConstMap = Map.insert k idx (csConstMap cs) })

-- | Bind a name to a register.
bindName :: Name -> Int -> CompState -> CompState
bindName nm r cs = cs { csEnv = Map.insert nm r (csEnv cs) }

-- | Look up a name's register.
lookupName :: Name -> CompState -> Maybe Int
lookupName nm cs = Map.lookup nm (csEnv cs)

-- | Save and restore register state for scoped compilation.
withScope :: CompState -> (CompState -> (a, CompState)) -> (a, CompState)
withScope cs f =
    let savedEnv = csEnv cs
        savedNextReg = csNextReg cs
        (result, cs') = f cs
    in (result, cs' { csEnv = savedEnv })
    -- Note: we do NOT restore csNextReg to allow sub-scopes to use higher regs

-- | Set tail position flag.
inTailPos :: Bool -> CompState -> CompState
inTailPos b cs = cs { csTailPos = b }

-- | Compile a complete CLM module.
-- Takes a map of function names to their CLM lambdas.
compileCLMModule :: Text -> HashMap Name CLMLam -> Either CompileError BytecodeModule
compileCLMModule modName clmFuncs =
    let -- Assign function indices
        funcNames = Map.keys clmFuncs
        funcMap = Map.fromList (zip funcNames [0..])
        cs0 = initCompState funcMap

        -- Compile each function
        cs1 = foldl' (\cs nm ->
            case Map.lookup nm clmFuncs of
                Just lam -> snd (compileFunction nm lam cs)
                Nothing  -> cs
            ) cs0 funcNames

        -- Build the module — encode instructions to Word32
        allCode = V.fromList (map encodeInstr (reverse (csAllCode cs1)))
        constants = V.fromList (reverse (csConstants cs1))
        functions = V.fromList (reverse (csCompiledFuncs cs1))
        jumpTables = V.fromList (reverse (csJumpTables cs1))
        consNames = V.fromList (csConsNames cs1)

    in Right BytecodeModule
        { bmName       = modName
        , bmConstants  = constants
        , bmFunctions  = functions
        , bmCode       = allCode
        , bmJumpTables = jumpTables
        , bmConsNames  = consNames
        }

-- | Compile a single function into the module.
compileFunction :: Name -> CLMLam -> CompState -> (Int, CompState)
compileFunction name lam cs0 =
    let funcIdx = case Map.lookup name (csFuncMap cs0) of
            Just idx -> idx
            Nothing  -> csNextFunc cs0
        cs1 = cs0 { csCode = [], csNextReg = 0, csMaxReg = 0
                   , csEnv = Map.empty, csTailPos = True
                   , csFuncMap = Map.insert name funcIdx (csFuncMap cs0)
                   , csNextFunc = max (csNextFunc cs0) (funcIdx + 1)
                   , csInstrCount = 0 }
        entryOffset = length (csAllCode cs1)
    in case lam of
        CLMLam vars body ->
            let -- Bind parameters to registers
                (cs2, _paramRegs) = foldl' (\(s, regs) (nm, _ty) ->
                    let (r, s') = allocReg s
                        s'' = bindName nm r s'
                    in (s'', regs ++ [r])
                    ) (cs1, []) vars
                -- Compile the body
                (resultReg, cs3) = compileCLMExpr body cs2
                -- Emit return (if not already a tail call)
                cs4 = if lastIsTailCall (csCode cs3)
                       then cs3
                       else emit (IRet resultReg) cs3
                codeLen = length (csCode cs4)
                fi = FuncInfo
                    { fiName       = T.pack name
                    , fiArity      = length vars
                    , fiNumRegs    = csMaxReg cs4
                    , fiEntry      = entryOffset
                    , fiCodeLen    = codeLen
                    , fiUpvalCount = 0
                    , fiDebug      = DebugInfo V.empty ""
                    }
                cs5 = cs4 { csCompiledFuncs = fi : csCompiledFuncs cs4
                           , csAllCode = csCode cs4 ++ csAllCode cs4 }
            in (funcIdx, cs5)

        CLMLamCases vars cases ->
            let -- Bind parameters to registers
                (cs2, _paramRegs) = foldl' (\(s, regs) (nm, _ty) ->
                    let (r, s') = allocReg s
                        s'' = bindName nm r s'
                    in (s'', regs ++ [r])
                    ) (cs1, []) vars
                -- Compile the case chain
                cs3 = compileCaseChain cases cs2
                -- Add error fallback
                (errIdx, cs4) = addConstant (KString "Pattern match failure") cs3
                cs5 = emit (IError errIdx) cs4
                codeLen = length (csCode cs5)
                fi = FuncInfo
                    { fiName       = T.pack name
                    , fiArity      = length vars
                    , fiNumRegs    = csMaxReg cs5
                    , fiEntry      = entryOffset
                    , fiCodeLen    = codeLen
                    , fiUpvalCount = 0
                    , fiDebug      = DebugInfo V.empty ""
                    }
                cs6 = cs5 { csCompiledFuncs = fi : csCompiledFuncs cs5
                           , csAllCode = csCode cs5 ++ csAllCode cs5 }
            in (funcIdx, cs6)

-- | Compile a chain of CLMCASE expressions (from CLMLamCases).
--
-- For each case arm, emits:
--   check instructions → JMPF (skip past body) → body → RET
-- The JMPF offset is computed by compiling the body first to measure its size,
-- then patching the placeholder.
compileCaseChain :: [CLMExpr] -> CompState -> CompState
compileCaseChain [] cs = cs
compileCaseChain (CLMCASE checks body : rest) cs =
    let -- Step 1: Compile pattern checks with JMPF placeholders (offset=0).
        -- Record the absolute instruction position of each JMPF.
        (cs1, jmpfPositions) = compilePatternChecks checks cs
        -- Step 2: Compile the body.
        (resultReg, cs2) = compileCLMExpr body cs1
        cs3 = if lastIsTailCall (csCode cs2)
               then cs2
               else emit (IRet resultReg) cs2
        -- Step 3: The target for all JMPFs is the instruction AFTER the body.
        -- targetPos is the absolute position of the next instruction to be emitted.
        targetPos = csInstrCount cs3
        -- Step 4: Patch each JMPF instruction in csCode.
        -- For a JMPF at absolute position P, we need offset = targetPos - P
        -- so that: PC_after_fetch + offset - 1 = (P+1) + offset - 1 = P + offset = targetPos.
        cs4 = patchJmpFs cs3 jmpfPositions targetPos
        -- Continue with remaining cases
    in compileCaseChain rest cs4
compileCaseChain (_:rest) cs = compileCaseChain rest cs

-- | Patch JMPF instructions in csCode (reversed instruction list).
-- Each JMPF at absolute position P gets offset = targetPos - P.
patchJmpFs :: CompState -> [Int] -> Int -> CompState
patchJmpFs cs [] _ = cs
patchJmpFs cs positions targetPos =
    let totalInstrs = csInstrCount cs
        -- In the reversed csCode list, an instruction at absolute position P
        -- is at list index (totalInstrs - 1 - P).
        patchOne code (absPos, listIdx) = patchAt listIdx (targetPos - absPos) code
        indexedPositions = [(p, totalInstrs - 1 - p) | p <- positions]
    in cs { csCode = foldl' patchOne (csCode cs) indexedPositions }

-- | Patch the JMPF instruction at the given index in a reversed instruction list.
patchAt :: Int -> Int -> [Instruction] -> [Instruction]
patchAt idx offset code = go 0 code
  where
    go _ [] = []
    go i (instr:rest)
        | i == idx = case instr of
            IJmpF reg _ -> IJmpF reg offset : rest
            other       -> other : rest  -- shouldn't happen
        | otherwise = instr : go (i + 1) rest

-- | Compile a case chain inline (not at function top level).
-- Returns result in a designated register. Uses JMP to a merge point
-- instead of RET after each arm.
compileInlineCaseChain :: [CLMExpr] -> CompState -> (Int, CompState)
compileInlineCaseChain [] cs =
    let (errIdx, cs1) = addConstant (KString "Pattern match failure") cs
        (r, cs2) = allocReg cs1
    in (r, emit (IError errIdx) cs2)
compileInlineCaseChain cases cs =
    let (dstReg, cs1) = allocReg cs
        -- Compile all arms, collecting JMP-to-merge positions
        (cs2, jmpToMerge) = foldl' (\(s, mergeJmps) caseExpr ->
            case caseExpr of
                CLMCASE checks body ->
                    let -- Compile checks, get JMPF positions
                        (s1, jmpfPositions) = compilePatternChecks checks s
                        -- Compile body
                        (resultReg, s2) = compileCLMExpr body (inTailPos False s1)
                        -- Move result to dstReg
                        s3 = if resultReg /= dstReg
                             then emit (IMov dstReg resultReg) s2
                             else s2
                        -- JMP to merge (placeholder offset=0)
                        jmpPos = csInstrCount s3
                        s4 = emit (IJmp 0) s3
                        -- Patch JMPFs to jump past this arm
                        targetPos = csInstrCount s4
                        s5 = patchJmpFs s4 jmpfPositions targetPos
                    in (s5, jmpPos : mergeJmps)
                _ ->
                    -- Default body (no checks)
                    let (resultReg, s1) = compileCLMExpr caseExpr (inTailPos False s)
                        s2 = if resultReg /= dstReg
                             then emit (IMov dstReg resultReg) s1
                             else s1
                    in (s2, mergeJmps)
            ) (cs1, []) cases
        -- Add error fallback
        (errIdx, cs3) = addConstant (KString "Pattern match failure") cs2
        cs4 = emit (IError errIdx) cs3
        -- Patch all JMP-to-merge instructions
        mergePos = csInstrCount cs4
        cs5 = patchJmps cs4 jmpToMerge mergePos
    in (dstReg, cs5)

-- | Patch JMP instructions (unconditional jumps to merge point).
patchJmps :: CompState -> [Int] -> Int -> CompState
patchJmps cs [] _ = cs
patchJmps cs positions targetPos =
    let totalInstrs = csInstrCount cs
        patchOne code (absPos, listIdx) = patchJmpAt listIdx (targetPos - absPos) code
        indexedPositions = [(p, totalInstrs - 1 - p) | p <- positions]
    in cs { csCode = foldl' patchOne (csCode cs) indexedPositions }

-- | Patch a JMP instruction at the given index.
patchJmpAt :: Int -> Int -> [Instruction] -> [Instruction]
patchJmpAt idx offset code = go 0 code
  where
    go _ [] = []
    go i (instr:rest)
        | i == idx = case instr of
            IJmp _ -> IJmp offset : rest
            other  -> other : rest
        | otherwise = instr : go (i + 1) rest

-- | Compile pattern checks. Returns the state and the absolute positions
-- of emitted JMPF instructions (for later patching).
compilePatternChecks :: [CLMPatternCheck] -> CompState -> (CompState, [Int])
compilePatternChecks checks cs0 = foldl' compileOneCheck (cs0, []) checks

compileOneCheck :: (CompState, [Int]) -> CLMPatternCheck -> (CompState, [Int])
compileOneCheck (cs, jmpfPositions) check = case check of
    CLMCheckTag (ConsTag _ expectedTag) scrutinee ->
        let -- Compile scrutinee
            (scrReg, cs1) = compileCLMExpr scrutinee (inTailPos False cs)
            -- Get tag
            (tagReg, cs2) = allocReg cs1
            cs3 = emit (IGetTag tagReg scrReg) cs2
            -- Compare with expected tag
            (expectedReg, cs4) = allocReg cs3
            cs5 = emit (ILoadInt expectedReg expectedTag) cs4
            (cmpReg, cs6) = allocReg cs5
            cs7 = emit (IEqI cmpReg tagReg expectedReg) cs6
            -- Record the absolute position BEFORE emitting JMPF
            jmpfAbsPos = csInstrCount cs7
            -- Emit JMPF with placeholder offset (will be patched)
            cs8 = emit (IJmpF cmpReg 0) cs7
        in (cs8, jmpfAbsPos : jmpfPositions)

    CLMCheckLit lit scrutinee ->
        let (scrReg, cs1) = compileCLMExpr scrutinee (inTailPos False cs)
            (litReg, cs2) = compileLiteral lit cs1
            (cmpReg, cs3) = allocReg cs2
            cs4 = case lit of
                LFloat _ -> emit (IEqF cmpReg scrReg litReg) cs3
                _        -> emit (IEqI cmpReg scrReg litReg) cs3
            -- Record absolute position BEFORE emitting JMPF
            jmpfAbsPos = csInstrCount cs4
            cs5 = emit (IJmpF cmpReg 0) cs4
        in (cs5, jmpfAbsPos : jmpfPositions)

-- | Compile a literal value, returning the register it's in.
compileLiteral :: Literal -> CompState -> (Int, CompState)
compileLiteral lit cs = case lit of
    LInt n ->
        if n >= -32768 && n < 32768
        then let (r, cs1) = allocReg cs
             in (r, emit (ILoadInt r n) cs1)
        else let (kidx, cs1) = addConstant (KInt n) cs
                 (r, cs2) = allocReg cs1
             in (r, emit (ILoadK r kidx) cs2)
    LFloat d ->
        let (kidx, cs1) = addConstant (KFloat d) cs
            (r, cs2) = allocReg cs1
        in (r, emit (ILoadK r kidx) cs2)
    LString s ->
        let (kidx, cs1) = addConstant (KString (T.pack s)) cs
            (r, cs2) = allocReg cs1
        in (r, emit (ILoadK r kidx) cs2)
    LChar c ->
        let (kidx, cs1) = addConstant (KInt (fromEnum c)) cs
            (r, cs2) = allocReg cs1
        in (r, emit (ILoadK r kidx) cs2)
    LWord8 w ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadInt r (fromIntegral w)) cs1)
    _ ->
        -- LList, LVec, LNTuple, fixed-width int types — emit as constant
        let (kidx, cs1) = addConstant (KInt 0) cs
            (r, cs2) = allocReg cs1
        in (r, emit (ILoadK r kidx) cs2)

-- | Compile a CLM expression, returning the register holding the result.
compileCLMExpr :: CLMExpr -> CompState -> (Int, CompState)
compileCLMExpr expr cs = case expr of
    -- Literals
    CLMLIT lit -> compileLiteral lit cs

    -- Variables
    CLMID nm -> case lookupName nm cs of
        Just r  -> (r, cs)
        Nothing -> case Map.lookup nm (csFuncMap cs) of
            -- Reference to a top-level function (might need closure later)
            Just fidx ->
                let (r, cs1) = allocReg cs
                in (r, emit (ILoadK r fidx) cs1)  -- TODO: proper func ref
            Nothing ->
                -- Unknown variable - emit as constant name for later dispatch
                let (kidx, cs1) = addConstant (KName (T.pack nm)) cs
                    (r, cs2) = allocReg cs1
                in (r, emit (ILoadK r kidx) cs2)

    -- Empty / Unit
    CLMEMPTY ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)

    CLMU _ ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)

    -- Error
    CLMERR msg _si ->
        let (kidx, cs1) = addConstant (KString (T.pack msg)) cs
            (r, cs2) = allocReg cs1
        in (r, emit (IError kidx) cs2)

    -- Let binding
    CLMBIND nm val ->
        let (valReg, cs1) = compileCLMExpr val (inTailPos False cs)
            cs2 = bindName nm valReg cs1
        in (valReg, cs2)

    -- Sequential program
    CLMPROG [] ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)

    CLMPROG [e] -> compileCLMExpr e cs

    CLMPROG (e:es) ->
        let (_r, cs1) = compileCLMExpr e (inTailPos False cs)
        in compileCLMExpr (CLMPROG es) cs1

    -- Direct function application
    CLMAPP (CLMID funcName) args ->
        compileCall funcName args cs

    CLMAPP (CLMLAM lam) args -> case lam of
        -- For CLMLamCases: compile args, bind params, compile case chain
        -- (cannot beta-reduce at compile time because case resolution is runtime)
        CLMLamCases vars cases ->
            let -- Compile arguments
                (cs1, argRegs) = foldl' (\(s, regs) arg ->
                    let (r, s') = compileCLMExpr arg (inTailPos False s)
                    in (s', regs ++ [r])
                    ) (cs, []) args
                -- Bind parameters to argument registers
                cs2 = foldl' (\s ((nm, _ty), r) -> bindName nm r s) cs1 (zip vars argRegs)
            in compileInlineCaseChain cases cs2
        -- For simple CLMLam: compile as let-binding (sequential evaluation).
        -- We must NOT beta-reduce because it would lose side effects
        -- (e.g., `let u = mutWrite(r, 42) in readRef(r)` would drop the write).
        CLMLam vars body ->
            let -- Compile arguments
                (cs1, argRegs) = foldl' (\(s, regs) arg ->
                    let (r, s') = compileCLMExpr arg (inTailPos False s)
                    in (s', regs ++ [r])
                    ) (cs, []) args
                -- Bind parameters to argument registers
                cs2 = foldl' (\s ((nm, _ty), r) -> bindName nm r s) cs1 (zip vars argRegs)
            in compileCLMExpr body cs2

    CLMAPP func args ->
        -- General application: compile func, then call as closure
        let (funcReg, cs1) = compileCLMExpr func (inTailPos False cs)
            (baseReg, cs2) = allocRegs (length args) cs1
            cs3 = foldl' (\s (i, arg) ->
                let (r, s') = compileCLMExpr arg (inTailPos False s)
                    s'' = if r /= baseReg + i
                          then emit (IMov (baseReg + i) r) s'
                          else s'
                in s''
                ) cs2 (zip [0..] args)
            isTail = csTailPos cs
            (dstReg, cs4) = allocReg cs3
        in if isTail
           then (dstReg, emit (ITailCallCls funcReg (length args)) cs4)
           else (dstReg, emit (ICallCls dstReg funcReg (length args)) cs4)

    -- Constructor
    CLMCON (ConsTag cname tag) fields ->
        let nfields = length fields
            (baseReg, cs1) = allocRegs (nfields + 1) cs  -- +1 for result
            dstReg = baseReg
            cs2 = foldl' (\s (i, fld) ->
                let (r, s') = compileCLMExpr fld (inTailPos False s)
                    s'' = if r /= baseReg + 1 + i
                          then emit (IMov (baseReg + 1 + i) r) s'
                          else s'
                in s''
                ) cs1 (zip [0..] fields)
            cs3 = emit (INewCon dstReg tag nfields) cs2
            cs4 = cs3 { csConsNames = (T.pack cname, tag) : csConsNames cs3 }
        in (dstReg, cs4)

    -- Field access
    CLMFieldAccess ("", idx) obj ->
        let (objReg, cs1) = compileCLMExpr obj (inTailPos False cs)
            (r, cs2) = allocReg cs1
        in (r, emit (IGetField r objReg idx) cs2)

    CLMFieldAccess (nm, idx) obj | idx >= 0 ->
        let (objReg, cs1) = compileCLMExpr obj (inTailPos False cs)
            (r, cs2) = allocReg cs1
        in (r, emit (IGetField r objReg idx) cs2)

    CLMFieldAccess (nm, _) obj ->
        -- Unresolved field: use name lookup (fall back to index 0 for now)
        let (objReg, cs1) = compileCLMExpr obj (inTailPos False cs)
            (r, cs2) = allocReg cs1
        in (r, emit (IGetField r objReg 0) cs2)

    -- Pattern match case
    CLMCASE checks body ->
        let (cs1, _patches) = compilePatternChecks checks (inTailPos False cs)
            (resultReg, cs2) = compileCLMExpr body cs1
        in (resultReg, cs2)

    -- Lambda (creates a closure)
    CLMLAM lam ->
        let captures = collectFreeVars lam cs
            -- Register a new sub-function
            funcName = "_lambda_" ++ show (csNextFunc cs)
            funcIdx = csNextFunc cs
            cs1 = cs { csNextFunc = funcIdx + 1
                     , csFuncMap = Map.insert funcName funcIdx (csFuncMap cs) }
            -- Compile the sub-function (deferred)
            (fidx, cs2) = compileFunction funcName lam cs1
            -- Emit CLOSURE instruction
            nCaptures = length captures
            (baseReg, cs3) = allocRegs (nCaptures + 1) cs2
            dstReg = baseReg
            -- Copy captures into consecutive registers
            cs4 = foldl' (\s (i, capName) ->
                case lookupName capName s of
                    Just r  -> emit (IMov (baseReg + 1 + i) r) s
                    Nothing -> emit (ILoadNil (baseReg + 1 + i)) s
                ) cs3 (zip [0..] captures)
            cs5 = emit (IClosure dstReg fidx nCaptures) cs4
        in (dstReg, cs5)

    -- Implicit-param application (should be monomorphized away; fallback to dispatch)
    CLMIAP (CLMID funcName) args ->
        -- Try as a direct call first (monomorphized case)
        compileCall funcName args cs

    CLMIAP func args ->
        -- General dispatch
        let (funcReg, cs1) = compileCLMExpr func (inTailPos False cs)
            (baseReg, cs2) = allocRegs (length args) cs1
            cs3 = foldl' (\s (i, arg) ->
                let (r, s') = compileCLMExpr arg (inTailPos False s)
                in if r /= baseReg + i
                   then emit (IMov (baseReg + i) r) s'
                   else s'
                ) cs2 (zip [0..] args)
            (dstReg, cs4) = allocReg cs3
        in (dstReg, emit (ICallCls dstReg funcReg (length args)) cs4)

    -- Type hint (erased at runtime)
    CLMTYPED inner _ -> compileCLMExpr inner cs

    -- Array literal
    CLMARRAY elems ->
        let n = length elems
            (arrReg, cs1) = allocReg cs
            cs2 = emit (INewArray arrReg n) cs1
            cs3 = foldl' (\s (i, el) ->
                let (r, s') = compileCLMExpr el (inTailPos False s)
                    (idxReg, s'') = allocReg s'
                    s3 = emit (ILoadInt idxReg i) s''
                in emit (IArrSet arrReg idxReg r) s3
                ) cs2 (zip [0..] elems)
        in (arrReg, cs3)

    -- Partial application
    CLMPAP func args ->
        compileCLMExpr (CLMAPP func args) cs  -- handled in CLMAPP

    -- Primitive call marker (should be resolved)
    CLMPRIMCALL ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)

    -- Mutable ref/array handles (created at runtime, not at compile time)
    CLMREF _ ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)
    CLMMUTARRAY _ ->
        let (r, cs1) = allocReg cs
        in (r, emit (ILoadUnit r) cs1)

    -- Class system nodes
    CLMNEW className args ->
        -- Treat as constructor with class name
        let (kidx, cs1) = addConstant (KName (T.pack className)) cs
            nargs = length args
            (baseReg, cs2) = allocRegs (nargs + 1) cs1
            dstReg = baseReg
            cs3 = foldl' (\s (i, arg) ->
                let (r, s') = compileCLMExpr arg (inTailPos False s)
                in if r /= baseReg + 1 + i
                   then emit (IMov (baseReg + 1 + i) r) s'
                   else s'
                ) cs2 (zip [0..] args)
            cs4 = emit (INewCon dstReg kidx nargs) cs3
        in (dstReg, cs4)

    CLMMCALL objExpr methodName args ->
        -- Compile as: get obj, dispatch method
        let (objReg, cs1) = compileCLMExpr objExpr (inTailPos False cs)
            (nameIdx, cs2) = addConstant (KName (T.pack methodName)) cs1
            nargs = length args + 1  -- obj + args
            (baseReg, cs3) = allocRegs (nargs + 1) cs2
            dstReg = baseReg
            cs4 = emit (IMov (baseReg + 1) objReg) cs3
            cs5 = foldl' (\s (i, arg) ->
                let (r, s') = compileCLMExpr arg (inTailPos False s)
                in if r /= baseReg + 2 + i
                   then emit (IMov (baseReg + 2 + i) r) s'
                   else s'
                ) cs4 (zip [0..] args)
            cs6 = emit (IDispatch dstReg nameIdx nargs) cs5
        in (dstReg, cs6)

    CLMSCALL objExpr methodName args ->
        -- Same as CLMMCALL for now (proper super dispatch later)
        compileCLMExpr (CLMMCALL objExpr methodName args) cs

    -- Effect handler
    CLMHANDLE body effName lets ops ->
        -- Simplified: compile as sequential for now
        let cs1 = foldl' (\s (nm, e) ->
                let (r, s') = compileCLMExpr e (inTailPos False s)
                in bindName nm r s'
                ) cs ops
            (resultReg, cs2) = compileCLMExpr body cs1
        in (resultReg, cs2)

-- | Compile a direct function call.
compileCall :: Name -> [CLMExpr] -> CompState -> (Int, CompState)
compileCall funcName args cs =
    let isTail = csTailPos cs
        nargs = length args
        (baseReg, cs1) = allocRegs (nargs + 1) (inTailPos False cs)
        dstReg = baseReg
        -- Compile arguments into consecutive registers after dst
        cs2 = foldl' (\s (i, arg) ->
            let (r, s') = compileCLMExpr arg s
                targetReg = baseReg + 1 + i
            in if r /= targetReg
               then emit (IMov targetReg r) s'
               else s'
            ) cs1 (zip [0..] args)
    in case Map.lookup funcName (csFuncMap cs2) of
        Just funcIdx ->
            if isTail
            then -- For TAILCALL: move args to r0..rN-1 (callee's param positions)
                let cs3 = foldl' (\s i ->
                        let srcReg = baseReg + 1 + i
                        in if srcReg /= i
                           then emit (IMov i srcReg) s
                           else s
                        ) cs2 [0..nargs-1]
                in (dstReg, emit (ITailCall funcIdx nargs) cs3)
            else (dstReg, emit (ICall dstReg funcIdx nargs) cs2)
        Nothing ->
            -- Check if it's a known builtin before falling through to dispatch
            let arg1Reg = baseReg + 1
                arg2Reg = baseReg + 2
                arg3Reg = baseReg + 3
            in case resolveBuiltin funcName of
                Just (BInline2 instr) ->
                    (dstReg, emit (instr dstReg arg1Reg arg2Reg) cs2)
                Just (BInline1 instr) ->
                    (dstReg, emit (instr dstReg arg1Reg) cs2)
                Just BNewRef ->
                    (dstReg, emit (INewRef dstReg arg1Reg) cs2)
                Just BReadRef ->
                    (dstReg, emit (IReadRef dstReg arg1Reg) cs2)
                Just BWriteRef ->
                    (dstReg, emit (ILoadUnit dstReg) $ emit (IWriteRef arg1Reg arg2Reg) cs2)
                Just BNewMutArr ->
                    (dstReg, emit (INewMutArr dstReg arg1Reg arg2Reg) cs2)
                Just BMutRead ->
                    (dstReg, emit (IMutRead dstReg arg1Reg arg2Reg) cs2)
                Just BMutWrite ->
                    (dstReg, emit (ILoadUnit dstReg) $ emit (IMutWrite arg1Reg arg2Reg arg3Reg) cs2)
                Just BPrint ->
                    (dstReg, emit (ILoadUnit dstReg) $ emit (IPrint arg1Reg) cs2)
                Just BPrintLn ->
                    (dstReg, emit (ILoadUnit dstReg) $ emit (IPrintLn arg1Reg) cs2)
                Just BReadLine ->
                    (dstReg, emit (IReadLine dstReg) cs2)
                Just BStrCat ->
                    (dstReg, emit (IStrCat dstReg arg1Reg arg2Reg) cs2)
                Just BStrLen ->
                    (dstReg, emit (IStrLen dstReg arg1Reg) cs2)
                Just BPrintNewline ->
                    -- Complex: fall through to DISPATCH (VM handles it)
                    let (nameIdx, cs3) = addConstant (KName (T.pack funcName)) cs2
                    in (dstReg, emit (IDispatch dstReg nameIdx nargs) cs3)
                Just BDispatch ->
                    let (nameIdx, cs3) = addConstant (KName (T.pack funcName)) cs2
                    in (dstReg, emit (IDispatch dstReg nameIdx nargs) cs3)
                Nothing ->
                    -- Unknown function: fall through to dispatch
                    let (nameIdx, cs3) = addConstant (KName (T.pack funcName)) cs2
                    in (dstReg, emit (IDispatch dstReg nameIdx nargs) cs3)

-- | Collect free variables from a CLM lambda (for closure captures).
collectFreeVars :: CLMLam -> CompState -> [Name]
collectFreeVars lam cs =
    let bound = case lam of
            CLMLam vars _      -> map fst vars
            CLMLamCases vars _ -> map fst vars
        freeInBody = case lam of
            CLMLam _ body      -> freeVarsExpr body
            CLMLamCases _ cases -> concatMap freeVarsExpr cases
        free = filter (\n -> n `notElem` bound && Map.member n (csEnv cs)) freeInBody
    in nub' free
  where
    nub' = map head . groupBy (==) . sort'
    sort' [] = []
    sort' (x:xs) = sort' [y | y <- xs, y < x] ++ [x] ++ sort' [y | y <- xs, y >= x]
    groupBy _ [] = []
    groupBy f (x:xs) = let (same, rest) = span (f x) xs in (x:same) : groupBy f rest

-- | Collect free variable names in a CLM expression.
freeVarsExpr :: CLMExpr -> [Name]
freeVarsExpr (CLMID nm) = [nm]
freeVarsExpr (CLMAPP f args) = freeVarsExpr f ++ concatMap freeVarsExpr args
freeVarsExpr (CLMIAP f args) = freeVarsExpr f ++ concatMap freeVarsExpr args
freeVarsExpr (CLMPAP f args) = freeVarsExpr f ++ concatMap freeVarsExpr args
freeVarsExpr (CLMCON _ fields) = concatMap freeVarsExpr fields
freeVarsExpr (CLMFieldAccess _ e) = freeVarsExpr e
freeVarsExpr (CLMBIND nm e) = filter (/= nm) (freeVarsExpr e)
freeVarsExpr (CLMPROG es) = concatMap freeVarsExpr es
freeVarsExpr (CLMCASE checks body) = concatMap freeVarsCheck checks ++ freeVarsExpr body
freeVarsExpr (CLMTYPED e _) = freeVarsExpr e
freeVarsExpr (CLMARRAY es) = concatMap freeVarsExpr es
freeVarsExpr (CLMLAM lam) = freeVarsLam lam
freeVarsExpr (CLMMCALL obj _ args) = freeVarsExpr obj ++ concatMap freeVarsExpr args
freeVarsExpr (CLMSCALL obj _ args) = freeVarsExpr obj ++ concatMap freeVarsExpr args
freeVarsExpr (CLMNEW _ args) = concatMap freeVarsExpr args
freeVarsExpr (CLMHANDLE body _ lets ops) =
    freeVarsExpr body ++ concatMap (freeVarsExpr . snd) lets ++ concatMap (freeVarsExpr . snd) ops
freeVarsExpr _ = []

freeVarsCheck :: CLMPatternCheck -> [Name]
freeVarsCheck (CLMCheckTag _ e) = freeVarsExpr e
freeVarsCheck (CLMCheckLit _ e) = freeVarsExpr e

freeVarsLam :: CLMLam -> [Name]
freeVarsLam (CLMLam vars body) =
    let bound = map fst vars
    in filter (`notElem` bound) (freeVarsExpr body)
freeVarsLam (CLMLamCases vars cases) =
    let bound = map fst vars
    in filter (`notElem` bound) (concatMap freeVarsExpr cases)

-- | Check if the last emitted instruction is a tail call.
lastIsTailCall :: [Instruction] -> Bool
lastIsTailCall (ITailCall _ _ : _) = True
lastIsTailCall (ITailCallCls _ _ : _) = True
lastIsTailCall _ = False
