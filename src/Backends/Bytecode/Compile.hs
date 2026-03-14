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
import Data.List (foldl', sortOn)

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
        functions = V.fromList (sortOn fiFuncIdx (csCompiledFuncs cs1))
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
compileFunction = compileFunctionWithCaptures []

-- | Compile a function with captured variables.
-- Captures occupy registers 0..k-1, parameters occupy k..k+n-1.
-- This matches the VM's CALLCLS convention (upvalues first, then args).
compileFunctionWithCaptures :: [Name] -> Name -> CLMLam -> CompState -> (Int, CompState)
compileFunctionWithCaptures captures name lam cs0 =
    let funcIdx = case Map.lookup name (csFuncMap cs0) of
            Just idx -> idx
            Nothing  -> csNextFunc cs0
        cs1 = cs0 { csCode = [], csNextReg = 0, csMaxReg = 0
                   , csEnv = Map.empty, csTailPos = True
                   , csFuncMap = Map.insert name funcIdx (csFuncMap cs0)
                   , csNextFunc = max (csNextFunc cs0) (funcIdx + 1)
                   , csInstrCount = 0 }
        -- Allocate registers for captures first (r0..r(k-1))
        (cs1', _capRegs) = foldl' (\(s, regs) capName ->
            let (r, s') = allocReg s
                s'' = bindName capName r s'
            in (s'', regs ++ [r])
            ) (cs1, []) captures
    in case lam of
        CLMLam vars body ->
            let -- Bind parameters to registers (after captures)
                (cs2, _paramRegs) = foldl' (\(s, regs) (nm, _ty) ->
                    let (r, s') = allocReg s
                        s'' = bindName nm r s'
                    in (s'', regs ++ [r])
                    ) (cs1', []) vars
                -- Compile the body (may compile nested functions into csAllCode)
                (resultReg, cs3) = compileCLMExpr body cs2
                -- Emit return (if not already a tail call)
                cs4 = if lastIsTailCall (csCode cs3)
                       then cs3
                       else emit (IRet resultReg) cs3
                codeLen = length (csCode cs4)
                -- Entry offset is calculated AFTER body compilation, so nested
                -- functions' code (already in csAllCode) comes before this function.
                entryOffset = length (csAllCode cs4)
                nCaps = length captures
                fi = FuncInfo
                    { fiName       = T.pack name
                    , fiFuncIdx    = funcIdx
                    , fiArity      = length vars
                    , fiNumRegs    = csMaxReg cs4
                    , fiEntry      = entryOffset
                    , fiCodeLen    = codeLen
                    , fiUpvalCount = nCaps
                    , fiDebug      = DebugInfo V.empty ""
                    }
                cs5 = cs4 { csCompiledFuncs = fi : csCompiledFuncs cs4
                           , csAllCode = csCode cs4 ++ csAllCode cs4 }
            in (funcIdx, cs5)

        CLMLamCases vars cases ->
            let -- Bind parameters to registers (after captures)
                (cs2, _paramRegs) = foldl' (\(s, regs) (nm, _ty) ->
                    let (r, s') = allocReg s
                        s'' = bindName nm r s'
                    in (s'', regs ++ [r])
                    ) (cs1', []) vars
                -- Compile the case chain (may compile nested functions)
                cs3 = compileCaseChain cases cs2
                -- Add error fallback
                (errIdx, cs4) = addConstant (KString "Pattern match failure") cs3
                cs5 = emit (IError errIdx) cs4
                codeLen = length (csCode cs5)
                entryOffset = length (csAllCode cs5)
                nCaps = length captures
                fi = FuncInfo
                    { fiName       = T.pack name
                    , fiFuncIdx    = funcIdx
                    , fiArity      = length vars
                    , fiNumRegs    = csMaxReg cs5
                    , fiEntry      = entryOffset
                    , fiCodeLen    = codeLen
                    , fiUpvalCount = nCaps
                    , fiDebug      = DebugInfo V.empty ""
                    }
                cs6 = cs5 { csCompiledFuncs = fi : csCompiledFuncs cs5
                           , csAllCode = csCode cs5 ++ csAllCode cs5 }
            in (funcIdx, cs6)

-- | Try to extract a tag-switch pattern from case arms.
-- Returns (scrutinee register, [(tag, body)], Maybe defaultBody) if all arms
-- are single CLMCheckTag checks on the same CLMID scrutinee that's bound.
extractBCTagSwitch :: [CLMExpr] -> CompState -> Maybe (Int, [(Int, CLMExpr)], Maybe CLMExpr)
extractBCTagSwitch cases cs = go cases [] Nothing Nothing
  where
    go [] _ Nothing _ = Nothing
    go [] tagBodies _ (Just r) =
      Just (r, [(t, b) | (t, b) <- reverse tagBodies], Nothing)
    go (CLMCASE [] body : rest) tagBodies _ reg =
      case tagBodies of
        [] -> Nothing
        _  -> go rest tagBodies (Just body) reg
    go (CLMCASE [CLMCheckTag (ConsTag _ tag) (CLMID scrut)] body : rest) tagBodies defBody mReg =
      case lookupName scrut cs of
        Nothing -> Nothing
        Just r -> case mReg of
          Nothing -> go rest ((tag, body) : tagBodies) defBody (Just r)
          Just r' | r' == r -> go rest ((tag, body) : tagBodies) defBody mReg
          _ -> Nothing
    go _ _ _ _ = Nothing

-- | Compile a chain of CLMCASE expressions (from CLMLamCases).
--
-- For each case arm, emits:
--   check instructions → JMPF (skip past body) → body → RET
-- The JMPF offset is computed by compiling the body first to measure its size,
-- then patching the placeholder.
compileCaseChain :: [CLMExpr] -> CompState -> CompState
compileCaseChain [] cs = cs
compileCaseChain cases cs | Just (scrReg, tagCases, mDefault) <- extractBCTagSwitch cases cs =
    compileCaseChainSwitch scrReg tagCases mDefault cs
compileCaseChain (CLMCASE checks body : rest) cs =
    let -- Save csEnv before compiling the arm body. Nested case chains in the
        -- body may shadow the scrutinee parameter binding; we must restore env
        -- so the next arm's check sees the correct scrutinee register.
        savedEnv = csEnv cs
        savedNextReg = csNextReg cs
        -- Step 1: Compile pattern checks with JMPF placeholders (offset=0).
        -- Record the absolute instruction position of each JMPF.
        (cs1, jmpfPositions) = compilePatternChecks checks cs
        -- Step 2: Compile the body in tail position (each arm returns).
        (resultReg, cs2) = compileCLMExpr body (inTailPos True cs1)
        cs3 = if lastIsTailCall (csCode cs2)
               then cs2
               else emit (IRet resultReg) cs2
        -- Step 3: The target for all JMPFs is the instruction AFTER the body.
        -- targetPos is the absolute position of the next instruction to be emitted.
        targetPos = csInstrCount cs3
        -- Step 4: Patch each JMPF instruction in csCode.
        cs4 = patchJmpFs cs3 jmpfPositions targetPos
        -- Restore env and nextReg for next arm (keep code, constants, etc.)
        cs5 = cs4 { csEnv = savedEnv, csNextReg = savedNextReg }
        -- Continue with remaining cases
    in compileCaseChain rest cs5
compileCaseChain (_:rest) cs = compileCaseChain rest cs

-- | Compile a case chain using SWITCH instruction (O(1) tag dispatch).
-- Emits: GETTAG dst, scrut; SWITCH dst, tableIdx
-- Each case body ends with RET. Jump table entries are patched post-compilation.
compileCaseChainSwitch :: Int -> [(Int, CLMExpr)] -> Maybe CLMExpr -> CompState -> CompState
compileCaseChainSwitch scrReg tagCases mDefault cs0 =
    let -- Emit GETTAG
        (tagReg, cs1) = allocReg cs0
        cs2 = emit (IGetTag tagReg scrReg) cs1
        -- Reserve jump table index
        jtIdx = length (csJumpTables cs2)
        -- Emit SWITCH with placeholder table index
        cs3 = emit (ISwitch tagReg jtIdx) cs2
        -- The absolute position right after SWITCH — this is where bodies start
        afterSwitch = csInstrCount cs3
        -- Compile each case body, recording entry points
        (cs4, bodyEntries) = foldl' (\(s, entries) (tag, body) ->
            let savedEnv = csEnv s
                savedNextReg = csNextReg s
                bodyStart = csInstrCount s
                (resultReg, s1) = compileCLMExpr body (inTailPos True s)
                s2 = if lastIsTailCall (csCode s1)
                     then s1
                     else emit (IRet resultReg) s1
                s3 = s2 { csEnv = savedEnv, csNextReg = savedNextReg }
            in (s3, (tag, bodyStart) : entries)
            ) (cs3, []) tagCases
        -- Compile default body (if any)
        (cs5, defaultOffset) = case mDefault of
            Just defBody ->
                let savedEnv = csEnv cs4
                    savedNextReg = csNextReg cs4
                    defStart = csInstrCount cs4
                    (resultReg, s1) = compileCLMExpr defBody (inTailPos True cs4)
                    s2 = if lastIsTailCall (csCode s1)
                         then s1
                         else emit (IRet resultReg) s1
                    s3 = s2 { csEnv = savedEnv, csNextReg = savedNextReg }
                in (s3, defStart)
            Nothing ->
                let errStart = csInstrCount cs4
                    (errIdx, s1) = addConstant (KString "Pattern match failure") cs4
                    s2 = emit (IError errIdx) s1
                in (s2, errStart)
        -- Build jump table
        jt = JumpTable
            { jtDefault = defaultOffset
            , jtEntries = V.fromList (reverse bodyEntries)
            }
        cs6 = cs5 { csJumpTables = jt : csJumpTables cs5 }
    in cs6

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
compileInlineCaseChain cases cs | Just (scrReg, tagCases, mDefault) <- extractBCTagSwitch cases cs =
    compileInlineCaseChainSwitch scrReg tagCases mDefault cs
compileInlineCaseChain cases cs =
    let (dstReg, cs1) = allocReg cs
        -- Compile all arms, collecting JMP-to-merge positions.
        -- Save/restore csEnv between arms to prevent inner match bindings
        -- from shadowing the scrutinee parameter in subsequent arms.
        (cs2, jmpToMerge) = foldl' (\(s, mergeJmps) caseExpr ->
            case caseExpr of
                CLMCASE checks body ->
                    let savedEnv = csEnv s
                        savedNextReg = csNextReg s
                        -- Compile checks, get JMPF positions
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
                        -- Restore env for next arm
                        s6 = s5 { csEnv = savedEnv, csNextReg = savedNextReg }
                    in (s6, jmpPos : mergeJmps)
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

-- | Compile an inline case chain using SWITCH (expression version).
-- Uses SWITCH + JMP-to-merge instead of JMPF chains.
compileInlineCaseChainSwitch :: Int -> [(Int, CLMExpr)] -> Maybe CLMExpr -> CompState -> (Int, CompState)
compileInlineCaseChainSwitch scrReg tagCases mDefault cs0 =
    let (dstReg, cs1) = allocReg cs0
        -- Emit GETTAG + SWITCH
        (tagReg, cs2) = allocReg cs1
        cs3 = emit (IGetTag tagReg scrReg) cs2
        jtIdx = length (csJumpTables cs3)
        cs4 = emit (ISwitch tagReg jtIdx) cs3
        -- Compile each body, collecting JMP-to-merge positions
        (cs5, bodyEntries, jmpToMerge) = foldl' (\(s, entries, mergeJmps) (tag, body) ->
            let savedEnv = csEnv s
                savedNextReg = csNextReg s
                bodyStart = csInstrCount s
                (resultReg, s1) = compileCLMExpr body (inTailPos False s)
                s2 = if resultReg /= dstReg
                     then emit (IMov dstReg resultReg) s1
                     else s1
                jmpPos = csInstrCount s2
                s3 = emit (IJmp 0) s2  -- placeholder
                s4 = s3 { csEnv = savedEnv, csNextReg = savedNextReg }
            in (s4, (tag, bodyStart) : entries, jmpPos : mergeJmps)
            ) (cs4, [], []) tagCases
        -- Default body
        (cs6, defaultOffset, jmpToMerge2) = case mDefault of
            Just defBody ->
                let savedEnv = csEnv cs5
                    savedNextReg = csNextReg cs5
                    defStart = csInstrCount cs5
                    (resultReg, s1) = compileCLMExpr defBody (inTailPos False cs5)
                    s2 = if resultReg /= dstReg
                         then emit (IMov dstReg resultReg) s1
                         else s1
                    jmpPos = csInstrCount s2
                    s3 = emit (IJmp 0) s2
                    s4 = s3 { csEnv = savedEnv, csNextReg = savedNextReg }
                in (s4, defStart, jmpPos : jmpToMerge)
            Nothing ->
                let errStart = csInstrCount cs5
                    (errIdx, s1) = addConstant (KString "Pattern match failure") cs5
                    s2 = emit (IError errIdx) s1
                in (s2, errStart, jmpToMerge)
        -- Build jump table
        jt = JumpTable
            { jtDefault = defaultOffset
            , jtEntries = V.fromList (reverse bodyEntries)
            }
        cs7 = cs6 { csJumpTables = jt : csJumpTables cs6 }
        -- Patch JMP-to-merge
        mergePos = csInstrCount cs7
        cs8 = patchJmps cs7 jmpToMerge2 mergePos
    in (dstReg, cs8)

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
            in if csTailPos cs
               then -- In tail position: use case chain with RETs (enables tail calls in arms)
                    let cs3 = compileCaseChain cases cs2
                        (errIdx, cs4) = addConstant (KString "Pattern match failure") cs3
                        cs5 = emit (IError errIdx) cs4
                    in (0, cs5)  -- register doesn't matter, all arms RET
               else compileInlineCaseChain cases cs2
        -- For simple CLMLam: compile as sequential let-binding.
        -- Each arg is compiled and its param bound BEFORE compiling the next arg,
        -- because later args may reference earlier params (desugared let-blocks).
        -- e.g., `let { x = 1; y = x + 1 } in y` → args[1] references params[0].
        CLMLam vars body ->
            let parentTailPos = csTailPos cs
                -- Compile args sequentially, binding each param as we go.
                -- After each binding, reclaim dead temp registers by resetting
                -- csNextReg to one past the highest live register. This prevents
                -- register overflow (8-bit register fields = max 256) in functions
                -- with many sequential let bindings (e.g., createBodies with 30+ bodySet calls).
                cs1 = foldl' (\s ((nm, _ty), arg) ->
                    let (r, s') = compileCLMExpr arg (inTailPos False s)
                        s'' = bindName nm r s'
                        -- Reclaim dead temps: set csNextReg to max live register + 1
                        maxLive = if Map.null (csEnv s'') then 0
                                  else maximum (Map.elems (csEnv s''))
                        s''' = s'' { csNextReg = maxLive + 1 }
                    in s'''
                    ) cs (zip vars args)
                -- Restore parent's tail position for body compilation
            in compileCLMExpr body (inTailPos parentTailPos cs1)

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
    CLMCON (ConsTag cname tag) fields
        -- Special-case Bool constructors to native VBool values
        | cname == "True"  && null fields ->
            let (r, cs1) = allocReg cs
            in (r, emit (ILoadTrue r) cs1)
        | cname == "False" && null fields ->
            let (r, cs1) = allocReg cs
            in (r, emit (ILoadFalse r) cs1)
        | cname == "Unit"  && null fields ->
            let (r, cs1) = allocReg cs
            in (r, emit (ILoadUnit r) cs1)
        | otherwise ->
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
            -- Save parent's compilation state before compiling nested function.
            -- compileFunction resets csCode/csEnv/csNextReg/etc., so we must preserve
            -- the parent's partial code and register state.
            savedCode = csCode cs1
            savedEnv = csEnv cs1
            savedNextReg = csNextReg cs1
            savedMaxReg = csMaxReg cs1
            savedTailPos = csTailPos cs1
            savedInstrCount = csInstrCount cs1
            -- Compile the sub-function (with captures bound to initial registers)
            (fidx, cs2) = compileFunctionWithCaptures captures funcName lam cs1
            -- Restore parent's state
            cs2' = cs2 { csCode = savedCode
                       , csEnv = savedEnv
                       , csNextReg = savedNextReg
                       , csMaxReg = savedMaxReg
                       , csTailPos = savedTailPos
                       , csInstrCount = savedInstrCount }
            -- Emit CLOSURE instruction
            nCaptures = length captures
            (baseReg, cs3) = allocRegs (nCaptures + 1) cs2'
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
        -- If name is a local variable with 0 args, just read the register.
        -- This handles cases like `CLMIAP (CLMID "count") []` where "count"
        -- is a local variable that got wrapped in CLMIAP by the monomorphizer
        -- because a global function with the same name has implicit params.
        case (null args, lookupName funcName cs) of
            (True, Just r) -> (r, cs)
            _ -> compileCall funcName args cs

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
            cs6 = emit (IMCall dstReg nameIdx nargs) cs5
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
                Just BClockNanos ->
                    (dstReg, emit (IClock dstReg) cs2)
                Just BPrintNewline ->
                    (dstReg, emit (ILoadUnit dstReg) $ emit IPrintNl cs2)
                Just BModifyRef ->
                    (dstReg, emit (IModRef dstReg arg1Reg arg2Reg) cs2)
                Just BMutLen ->
                    (dstReg, emit (IMutLen dstReg arg1Reg) cs2)
                Just BShowI ->
                    (dstReg, emit (IShowI dstReg arg1Reg) cs2)
                Just BShowF ->
                    (dstReg, emit (IShowF dstReg arg1Reg) cs2)
                Just BShowC ->
                    (dstReg, emit (IShowC dstReg arg1Reg) cs2)
                Just BShowS ->
                    (dstReg, emit (IShowS dstReg arg1Reg) cs2)
                Just BError ->
                    -- Error with runtime string argument
                    (dstReg, emit (IErrorReg arg1Reg) cs2)
                Just (BConstInt n) ->
                    let (kIdx, cs3) = addConstant (KInt (fromIntegral n)) cs2
                    in (dstReg, emit (ILoadK dstReg kIdx) cs3)
                Nothing ->
                    -- Check if it's a local variable (closure/function parameter)
                    case lookupName funcName cs2 of
                        Just closureReg ->
                            -- Call as closure: move closure reg, then CALLCLS
                            let cs3 = emit (IMov dstReg closureReg) cs2
                            in if isTail
                               then (dstReg, emit (ITailCallCls dstReg nargs) cs3)
                               else (dstReg, emit (ICallCls dstReg dstReg nargs) cs3)
                        Nothing ->
                            -- Truly unknown function: compile error
                            let (msgIdx, cs3) = addConstant (KString (T.pack ("unresolved function: " ++ funcName))) cs2
                            in (dstReg, emit (IError msgIdx) cs3)

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
