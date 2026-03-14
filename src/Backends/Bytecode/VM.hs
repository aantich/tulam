{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Bytecode VM execution engine (NaN-boxed representation).
--
-- Register-based virtual machine with proper tail call optimization.
-- All registers store raw Word64 values using NaN-boxing from Value.hs.
-- Complex objects (constructors, closures, arrays, refs) live on the heap
-- table; strings live on the string table. Hot numeric paths operate
-- directly on Word64 without any allocation.
module Backends.Bytecode.VM
    ( VMState(..)
    , Frame(..)
    , VMError(..)
    , initVM
    , runFunction
    , runFunctionByName
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.Word
import Data.Bits
import Data.IORef
import Control.Monad (when, forM_)
import System.IO (hFlush, stdout)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Backends.Bytecode.Value
import Backends.Bytecode.Instruction ()
import Backends.Bytecode.Module

-- | VM error types.
data VMError
    = VMRuntimeError !Text
    | VMStackOverflow
    | VMInvalidOpcode !Word8
    | VMInvalidFunction !Int
    | VMInvalidRegister !Int
    | VMPatternMatchFailure
    | VMDivisionByZero
    deriving (Show, Eq)

-- | Call stack frame.
data Frame = Frame
    { frRetPC    :: !Int    -- return program counter
    , frRetReg   :: !Int    -- destination register in caller's frame
    , frBaseFP   :: !Int    -- caller's frame pointer (register base)
    , frFrameSize :: !Int   -- caller's frame size (for restoring on return)
    , frFuncIdx  :: !Int    -- function index (for debug)
    } deriving (Show, Eq)

-- | Maximum call stack depth (for non-tail calls only).
maxCallDepth :: Int
maxCallDepth = 100000

-- | Register file size (grows as needed).
initialRegFileSize :: Int
initialRegFileSize = 65536  -- initial; grows dynamically

-- | VM state. All mutable fields are IORef/MVector for O(1) access.
data VMState = VMState
    { vmModule     :: !BytecodeModule              -- loaded module
    , vmRegisters  :: !(IORef (MV.IOVector Word64)) -- growable register file (NaN-boxed)
    , vmPC         :: !(IORef Int)                 -- program counter
    , vmFP         :: !(IORef Int)                 -- frame pointer (register base offset)
    , vmFrameSize  :: !(IORef Int)                 -- current frame size (caller's maxRegs)
    , vmCallStack  :: !(IORef [Frame])             -- call stack (LIFO)
    , vmCallDepth  :: !(IORef Int)                 -- current call depth
    , vmHeap       :: !HeapTable                   -- heap-allocated objects
    , vmStrings    :: !StringTable                 -- interned strings
    }

-- | Initialize a VM from a bytecode module.
initVM :: BytecodeModule -> IO VMState
initVM bm = do
    regs <- MV.replicate initialRegFileSize mkEmptyW
    regsRef <- newIORef regs
    pc <- newIORef 0
    fp <- newIORef 0
    frameSize <- newIORef 256  -- default; set properly by runFunction
    stack <- newIORef []
    depth <- newIORef 0
    heap <- newHeapTable
    strings <- newStringTable
    return VMState
        { vmModule    = bm
        , vmRegisters = regsRef
        , vmPC        = pc
        , vmFP        = fp
        , vmFrameSize = frameSize
        , vmCallStack = stack
        , vmCallDepth = depth
        , vmHeap      = heap
        , vmStrings   = strings
        }

-- | Read a register (absolute index = FP + local index). Returns raw Word64.
{-# INLINE readReg #-}
readReg :: VMState -> Int -> IO Word64
readReg vm r = do
    fp <- readIORef (vmFP vm)
    regs <- readIORef (vmRegisters vm)
    let idx = fp + r
    if idx < MV.length regs
        then MV.read regs idx
        else return mkEmptyW

-- | Write a register with a raw Word64.
{-# INLINE writeReg #-}
writeReg :: VMState -> Int -> Word64 -> IO ()
writeReg vm r v = do
    fp <- readIORef (vmFP vm)
    let idx = fp + r
    ensureRegSize vm (idx + 1)
    regs <- readIORef (vmRegisters vm)
    MV.write regs idx v

-- | Ensure register file is at least the given size.
-- Grows by doubling when needed (amortized O(1)).
ensureRegSize :: VMState -> Int -> IO ()
ensureRegSize vm needed = do
    regs <- readIORef (vmRegisters vm)
    let currentSize = MV.length regs
    when (needed > currentSize) $ do
        let newSize = max needed (currentSize * 2)
        newRegs <- MV.replicate newSize mkEmptyW
        -- Copy old data
        forM_ [0..currentSize-1] $ \i -> do
            v <- MV.read regs i
            MV.write newRegs i v
        writeIORef (vmRegisters vm) newRegs

-- | Read register at absolute position.
{-# INLINE readRegAbs #-}
readRegAbs :: VMState -> Int -> IO Word64
readRegAbs vm idx = do
    regs <- readIORef (vmRegisters vm)
    if idx < MV.length regs
        then MV.read regs idx
        else return mkEmptyW

-- | Write register at absolute position.
{-# INLINE writeRegAbs #-}
writeRegAbs :: VMState -> Int -> Word64 -> IO ()
writeRegAbs vm idx v = do
    ensureRegSize vm (idx + 1)
    regs <- readIORef (vmRegisters vm)
    MV.write regs idx v

-- | Run a function by index with the given arguments.
-- Public API returns Val (wrapping the internal Word64).
runFunction :: VMState -> Int -> [Val] -> IO (Either VMError Val)
runFunction vm funcIdx args = do
    let bm = vmModule vm
    case lookupFunction bm funcIdx of
        Nothing -> return $ Left (VMInvalidFunction funcIdx)
        Just fi -> do
            -- Set up initial frame
            writeIORef (vmPC vm) (fiEntry fi)
            writeIORef (vmFP vm) 0
            writeIORef (vmFrameSize vm) (csMaxRegs fi)
            writeIORef (vmCallStack vm) []
            writeIORef (vmCallDepth vm) 0
            -- Write arguments to parameter registers (unwrap Val to Word64)
            forM_ (zip [0..] args) $ \(i, Val w) ->
                writeReg vm i w
            -- Run the dispatch loop
            result <- execLoop vm
            return $ case result of
                Left err -> Left err
                Right w  -> Right (Val w)

-- | Run a function by name.
runFunctionByName :: VMState -> Text -> [Val] -> IO (Either VMError Val)
runFunctionByName vm name args =
    case lookupFunctionByName (vmModule vm) name of
        Nothing -> return $ Left (VMRuntimeError $ "Function not found: " <> name)
        Just (idx, _) -> runFunction vm idx args

-- | The main execution loop. Returns raw Word64 internally.
-- This is the hot path — must be a tight tail-recursive loop.
execLoop :: VMState -> IO (Either VMError Word64)
execLoop !vm = do
    pc <- readIORef (vmPC vm)
    let code = bmCode (vmModule vm)
    if pc >= V.length code
        then return $ Left (VMRuntimeError "PC out of bounds")
        else do
            let !w = code V.! pc
                !opByte = fromIntegral (w `shiftR` 24) :: Word8
                !a = fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Int
                !b = fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Int
                !c = fromIntegral (w .&. 0xFF) :: Int
                !imm16 = fromIntegral (w .&. 0xFFFF) :: Int
                !simm16 = if imm16 >= 0x8000 then imm16 - 0x10000 else imm16
                !imm24 = fromIntegral (w .&. 0xFFFFFF) :: Int
                !simm24 = if imm24 >= 0x800000 then imm24 - 0x1000000 else imm24
            writeIORef (vmPC vm) (pc + 1)
            dispatch vm opByte a b c imm16 simm16 simm24

-- | Dispatch a single instruction. Returns raw Word64.
dispatch :: VMState -> Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Either VMError Word64)

-- LOADK: r = constants[k]
dispatch vm 0x01 a _ _ imm16 _ _ = do
    let k = bmConstants (vmModule vm) V.! imm16
    w <- constToW vm k
    writeReg vm a w
    execLoop vm

-- LOADINT: r = imm16 (sign-extended)
dispatch vm 0x02 a _ _ _ simm16 _ = do
    writeReg vm a (mkIntW simm16)
    execLoop vm

-- LOADTRUE
dispatch vm 0x03 a _ _ _ _ _ = do
    writeReg vm a (mkBoolW True)
    execLoop vm

-- LOADFALSE
dispatch vm 0x04 a _ _ _ _ _ = do
    writeReg vm a (mkBoolW False)
    execLoop vm

-- LOADUNIT
dispatch vm 0x05 a _ _ _ _ _ = do
    writeReg vm a mkUnitW
    execLoop vm

-- LOADNIL
dispatch vm 0x06 a _ _ _ _ _ = do
    writeReg vm a mkEmptyW
    execLoop vm

-- MOV: r1 = r2
dispatch vm 0x07 a b _ _ _ _ = do
    v <- readReg vm b
    writeReg vm a v
    execLoop vm

-- ADDI: r = a + b (int)
dispatch vm 0x10 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (addIntW va vb)
    execLoop vm

-- SUBI
dispatch vm 0x11 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (subIntW va vb)
    execLoop vm

-- MULI
dispatch vm 0x12 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (mulIntW va vb)
    execLoop vm

-- DIVI
dispatch vm 0x13 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case divIntW va vb of
        Nothing -> return $ Left VMDivisionByZero
        Just r  -> do writeReg vm a r; execLoop vm

-- REMI
dispatch vm 0x14 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case remIntW va vb of
        Nothing -> return $ Left VMDivisionByZero
        Just r  -> do writeReg vm a r; execLoop vm

-- NEGI
dispatch vm 0x15 a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (negIntW va)
    execLoop vm

-- ABSI
dispatch vm 0x16 a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (absIntW va)
    execLoop vm

-- ADDF
dispatch vm 0x18 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (addFloatW va vb)
    execLoop vm

-- SUBF
dispatch vm 0x19 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (subFloatW va vb)
    execLoop vm

-- MULF
dispatch vm 0x1A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (mulFloatW va vb)
    execLoop vm

-- DIVF
dispatch vm 0x1B a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case divFloatW va vb of
        Nothing -> return $ Left VMDivisionByZero
        Just r  -> do writeReg vm a r; execLoop vm

-- NEGF
dispatch vm 0x1C a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (negFloatW va)
    execLoop vm

-- SQRTF
dispatch vm 0x1D a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (sqrtFloatW va)
    execLoop vm

-- ABSF
dispatch vm 0x1E a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (absFloatW va)
    execLoop vm

-- ITOF
dispatch vm 0x1F a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (itofW va)
    execLoop vm

-- EQI / EQP: bitwise equality
dispatch vm 0x20 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (eqW va vb)
    execLoop vm

-- LTI
dispatch vm 0x21 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (intBinCmpW (<) va vb)
    execLoop vm

-- LEI
dispatch vm 0x22 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (intBinCmpW (<=) va vb)
    execLoop vm

-- GTI
dispatch vm 0x23 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (intBinCmpW (>) va vb)
    execLoop vm

-- GEI
dispatch vm 0x24 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (intBinCmpW (>=) va vb)
    execLoop vm

-- NEQI: bitwise inequality
dispatch vm 0x09 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (mkBoolW (va /= vb))
    execLoop vm

-- NEQF
dispatch vm 0x0A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (/=) va vb)
    execLoop vm

-- FTOI
dispatch vm 0x25 a b _ _ _ _ = do
    va <- readReg vm b
    writeReg vm a (ftoiW va)
    execLoop vm

-- GTF
dispatch vm 0x26 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (>) va vb)
    execLoop vm

-- GEF
dispatch vm 0x27 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (>=) va vb)
    execLoop vm

-- EQF
dispatch vm 0x28 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (==) va vb)
    execLoop vm

-- LTF
dispatch vm 0x29 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (<) va vb)
    execLoop vm

-- EQP (pointer/value equality — same as EQI for NaN-boxed)
dispatch vm 0x2A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (eqW va vb)
    execLoop vm

-- LEF
dispatch vm 0x2B a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (floatBinCmpW (<=) va vb)
    execLoop vm

-- BAND
dispatch vm 0x2C a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (bandW va vb)
    execLoop vm

-- BOR
dispatch vm 0x2D a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (borW va vb)
    execLoop vm

-- BXOR
dispatch vm 0x2E a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (bxorW va vb)
    execLoop vm

-- BSHL
dispatch vm 0x2F a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (bshlW va vb)
    execLoop vm

-- BSHR
dispatch vm 0x30 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (bshrW va vb)
    execLoop vm

-- JMP: PC += offset
dispatch vm 0x38 _ _ _ _ _ simm24 = do
    pc <- readIORef (vmPC vm)
    writeIORef (vmPC vm) (pc + simm24 - 1)  -- -1 because we already incremented
    execLoop vm

-- JMPT: if r then PC += offset
dispatch vm 0x39 a _ _ _ simm16 _ = do
    va <- readReg vm a
    when (isValTrueW va) $ do
        pc <- readIORef (vmPC vm)
        writeIORef (vmPC vm) (pc + simm16 - 1)
    execLoop vm

-- JMPF: if !r then PC += offset
dispatch vm 0x3A a _ _ _ simm16 _ = do
    va <- readReg vm a
    when (not (isValTrueW va)) $ do
        pc <- readIORef (vmPC vm)
        writeIORef (vmPC vm) (pc + simm16 - 1)
    execLoop vm

-- SWITCH: dispatch on tag via jump table
dispatch vm 0x3B a _ _ imm16 _ _ = do
    va <- readReg vm a
    tag <- getTagForSwitch vm va
    let jts = bmJumpTables (vmModule vm)
    if imm16 < V.length jts
        then do
            let jt = jts V.! imm16
                entries = jtEntries jt
                -- Linear scan for matching tag (jump tables are small)
                target = V.foldl' (\acc (t, off) ->
                    case acc of
                        Just _  -> acc
                        Nothing -> if t == tag then Just off else Nothing
                    ) Nothing entries
            pc <- readIORef (vmPC vm)
            case target of
                Just off -> writeIORef (vmPC vm) off
                Nothing  -> writeIORef (vmPC vm) (jtDefault jt)
            execLoop vm
        else return $ Left (VMRuntimeError "SWITCH: invalid jump table index")

-- RET: return value in register a
dispatch vm 0x3C a _ _ _ _ _ = do
    val <- readReg vm a
    stack <- readIORef (vmCallStack vm)
    case stack of
        [] -> return $ Right val  -- top-level return
        (Frame retPC retReg baseFP callerFS _funcIdx : rest) -> do
            writeIORef (vmCallStack vm) rest
            writeIORef (vmPC vm) retPC
            writeIORef (vmFP vm) baseFP
            writeIORef (vmFrameSize vm) callerFS
            depth <- readIORef (vmCallDepth vm)
            writeIORef (vmCallDepth vm) (depth - 1)
            writeReg vm retReg val
            execLoop vm

-- CALL: r = func(args in r+1..r+nargs)
dispatch vm 0x40 a b c _ _ _ = do
    let funcIdx = b
        nargs = c
        dstReg = a
    let bm = vmModule vm
    case lookupFunction bm funcIdx of
        Nothing -> return $ Left (VMInvalidFunction funcIdx)
        Just fi -> do
            depth <- readIORef (vmCallDepth vm)
            when (depth >= maxCallDepth) $
                return () -- TODO: proper error
            -- Save return info
            pc <- readIORef (vmPC vm)
            fp <- readIORef (vmFP vm)
            callerFrameSize <- readIORef (vmFrameSize vm)
            let frame = Frame pc dstReg fp callerFrameSize funcIdx
            modifyIORef' (vmCallStack vm) (frame :)
            writeIORef (vmCallDepth vm) (depth + 1)
            -- New frame starts AFTER caller's register space
            let newFP = fp + callerFrameSize
            writeIORef (vmFP vm) newFP
            writeIORef (vmFrameSize vm) (csMaxRegs fi)
            forM_ [0..nargs-1] $ \i -> do
                v <- readRegAbs vm (fp + dstReg + 1 + i)
                writeRegAbs vm (newFP + i) v
            -- Jump to function entry
            writeIORef (vmPC vm) (fiEntry fi)
            execLoop vm

-- TAILCALL: reuse frame, jump to func
dispatch vm 0x41 _ b c _ _ _ = do
    let funcIdx = b
        nargs = c
    let bm = vmModule vm
    case lookupFunction bm funcIdx of
        Nothing -> return $ Left (VMInvalidFunction funcIdx)
        Just fi -> do
            fp <- readIORef (vmFP vm)
            -- Read all args first to handle overlapping reads/writes
            args <- mapM (\i -> readReg vm i) [0..nargs-1]
            forM_ (zip [0..] args) $ \(i, v) ->
                writeRegAbs vm (fp + i) v
            writeIORef (vmFrameSize vm) (csMaxRegs fi)
            writeIORef (vmPC vm) (fiEntry fi)
            execLoop vm

-- CALLCLS: r = closure(args)
dispatch vm 0x42 a b c _ _ _ = do
    closureW <- readReg vm b
    let nargs = c
        dstReg = a
    if isHeapW closureW
        then do
            obj <- heapRead (vmHeap vm) closureW
            case obj of
                HClosure funcIdx upvals -> do
                    let bm = vmModule vm
                    case lookupFunction bm funcIdx of
                        Nothing -> return $ Left (VMInvalidFunction funcIdx)
                        Just fi -> do
                            depth <- readIORef (vmCallDepth vm)
                            pc <- readIORef (vmPC vm)
                            fp <- readIORef (vmFP vm)
                            callerFS <- readIORef (vmFrameSize vm)
                            let frame = Frame pc dstReg fp callerFS funcIdx
                            modifyIORef' (vmCallStack vm) (frame :)
                            writeIORef (vmCallDepth vm) (depth + 1)
                            -- Set up new frame: upvalues first, then args
                            let newFP = fp + callerFS
                            writeIORef (vmFP vm) newFP
                            writeIORef (vmFrameSize vm) (csMaxRegs fi)
                            -- Write upvalues (stored as Val in heap)
                            forM_ [0..V.length upvals - 1] $ \i ->
                                writeRegAbs vm (newFP + i) (unVal (upvals V.! i))
                            -- Write args after upvalues
                            let argBase = V.length upvals
                            forM_ [0..nargs-1] $ \i -> do
                                v <- readRegAbs vm (fp + dstReg + 1 + i)
                                writeRegAbs vm (newFP + argBase + i) v
                            writeIORef (vmPC vm) (fiEntry fi)
                            execLoop vm
                HPAP funcIdx expected appliedArgs -> do
                    -- Saturate partial application
                    newArgs <- mapM (\i -> readReg vm (dstReg + 1 + i)) [0..nargs-1]
                    let newArgsVals = map Val newArgs
                        allArgs = V.toList appliedArgs ++ newArgsVals
                    if length allArgs >= expected
                        then do
                            -- Fully saturated: call
                            let bm = vmModule vm
                            case lookupFunction bm funcIdx of
                                Nothing -> return $ Left (VMInvalidFunction funcIdx)
                                Just fi -> do
                                    pc <- readIORef (vmPC vm)
                                    fp <- readIORef (vmFP vm)
                                    callerFS <- readIORef (vmFrameSize vm)
                                    depth <- readIORef (vmCallDepth vm)
                                    let frame = Frame pc dstReg fp callerFS funcIdx
                                    modifyIORef' (vmCallStack vm) (frame :)
                                    writeIORef (vmCallDepth vm) (depth + 1)
                                    let newFP = fp + callerFS
                                    writeIORef (vmFP vm) newFP
                                    writeIORef (vmFrameSize vm) (csMaxRegs fi)
                                    forM_ (zip [0..] allArgs) $ \(i, Val w) ->
                                        writeRegAbs vm (newFP + i) w
                                    writeIORef (vmPC vm) (fiEntry fi)
                                    execLoop vm
                        else do
                            -- Still partial: build new PAP on heap
                            papW <- heapAlloc (vmHeap vm) (HPAP funcIdx expected (V.fromList allArgs))
                            writeReg vm dstReg papW
                            execLoop vm
                _ -> return $ Left (VMRuntimeError "CALLCLS: not a closure or PAP")
        else return $ Left (VMRuntimeError "CALLCLS: not a heap object")

-- TAILCALLCLS: tail call closure
dispatch vm 0x43 _ b c _ _ _ = do
    closureW <- readReg vm b
    let nargs = c
    if isHeapW closureW
        then do
            obj <- heapRead (vmHeap vm) closureW
            case obj of
                HClosure funcIdx upvals -> do
                    let bm = vmModule vm
                    case lookupFunction bm funcIdx of
                        Nothing -> return $ Left (VMInvalidFunction funcIdx)
                        Just fi -> do
                            fp <- readIORef (vmFP vm)
                            -- Read args before overwriting
                            args <- mapM (\i -> readReg vm i) [0..nargs-1]
                            -- Write upvalues
                            forM_ [0..V.length upvals - 1] $ \i ->
                                writeRegAbs vm (fp + i) (unVal (upvals V.! i))
                            -- Write args
                            let argBase = V.length upvals
                            forM_ (zip [0..] args) $ \(i, v) ->
                                writeRegAbs vm (fp + argBase + i) v
                            writeIORef (vmFrameSize vm) (csMaxRegs fi)
                            writeIORef (vmPC vm) (fiEntry fi)
                            execLoop vm
                _ -> return $ Left (VMRuntimeError "TAILCALLCLS: not a closure")
        else return $ Left (VMRuntimeError "TAILCALLCLS: not a heap object")

-- NEWCON: r = Con(tag, fields in r+1..r+nfields)
dispatch vm 0x50 a b c _ _ _ = do
    let tag = b
        nfields = c
    fields <- V.generateM nfields $ \i -> Val <$> readReg vm (a + 1 + i)
    w <- heapAlloc (vmHeap vm) (HCon tag nfields fields)
    writeReg vm a w
    execLoop vm

-- GETFIELD: r = obj.fields[idx]
dispatch vm 0x51 a b c _ _ _ = do
    objW <- readReg vm b
    let idx = c
    if isHeapW objW
        then do
            obj <- heapRead (vmHeap vm) objW
            case obj of
                HCon _ _ fields | idx < V.length fields ->
                    writeReg vm a (unVal (fields V.! idx))
                _ -> writeReg vm a mkEmptyW
        else writeReg vm a mkEmptyW
    execLoop vm

-- GETTAG: r = obj.tag
-- Also handles Bool (True=0, False=1) for pattern matching
dispatch vm 0x52 a b _ _ _ _ = do
    objW <- readReg vm b
    if isHeapW objW
        then do
            obj <- heapRead (vmHeap vm) objW
            case obj of
                HCon tag _ _ -> writeReg vm a (mkIntW tag)
                _            -> writeReg vm a (mkIntW (-1))
        else if isBoolW objW
            then writeReg vm a (mkIntW (if getBoolW objW then 0 else 1))
            else writeReg vm a (mkIntW (-1))
    execLoop vm

-- NEWARRAY: r = array(len)
dispatch vm 0x58 a _ _ imm16 _ _ = do
    let fields = V.replicate imm16 (Val mkUnitW)
    w <- heapAlloc (vmHeap vm) (HArray fields)
    writeReg vm a w
    execLoop vm

-- ARRGET: r = arr[idx]
dispatch vm 0x59 a b c _ _ _ = do
    arrW <- readReg vm b
    idxW <- readReg vm c
    if isHeapW arrW && isIntW idxW
        then do
            obj <- heapRead (vmHeap vm) arrW
            let i = getIntW idxW
            case obj of
                HArray xs | i >= 0 && i < V.length xs ->
                    writeReg vm a (unVal (xs V.! i))
                _ -> writeReg vm a mkEmptyW
        else writeReg vm a mkEmptyW
    execLoop vm

-- ARRSET: arr[idx] = val (produces new immutable array)
dispatch vm 0x5A a b c _ _ _ = do
    arrW <- readReg vm a
    idxW <- readReg vm b
    val  <- readReg vm c
    if isHeapW arrW && isIntW idxW
        then do
            obj <- heapRead (vmHeap vm) arrW
            let i = getIntW idxW
            case obj of
                HArray xs | i >= 0 && i < V.length xs -> do
                    let xs' = xs V.// [(i, Val val)]
                    newW <- heapAlloc (vmHeap vm) (HArray xs')
                    writeReg vm a newW
                _ -> return ()
        else return ()
    execLoop vm

-- ARRLEN: r = length(arr)
dispatch vm 0x5B a b _ _ _ _ = do
    arrW <- readReg vm b
    if isHeapW arrW
        then do
            obj <- heapRead (vmHeap vm) arrW
            case obj of
                HArray xs -> writeReg vm a (mkIntW (V.length xs))
                _         -> writeReg vm a (mkIntW 0)
        else writeReg vm a (mkIntW 0)
    execLoop vm

-- NEWREF: r = newRef(val)
dispatch vm 0x60 a b _ _ _ _ = do
    val <- readReg vm b
    ref <- newIORef (Val val)
    w <- heapAlloc (vmHeap vm) (HRef ref)
    writeReg vm a w
    execLoop vm

-- READREF: r = readRef(ref)
dispatch vm 0x61 a b _ _ _ _ = do
    refW <- readReg vm b
    if isHeapW refW
        then do
            obj <- heapRead (vmHeap vm) refW
            case obj of
                HRef ref -> do
                    Val v <- readIORef ref
                    writeReg vm a v
                _ -> writeReg vm a mkEmptyW
        else writeReg vm a mkEmptyW
    execLoop vm

-- WRITEREF: writeRef(ref, val)
dispatch vm 0x62 a b _ _ _ _ = do
    refW <- readReg vm a
    val  <- readReg vm b
    if isHeapW refW
        then do
            obj <- heapRead (vmHeap vm) refW
            case obj of
                HRef ref -> writeIORef ref (Val val)
                _        -> return ()
        else return ()
    execLoop vm

-- NEWMUTARR: r = MutArray(len, default)
dispatch vm 0x63 a b c _ _ _ = do
    lenW <- readReg vm b
    defW <- readReg vm c
    if isIntW lenW
        then do
            let len = getIntW lenW
            vec <- MV.replicate len (Val defW)
            w <- heapAlloc (vmHeap vm) (HMutArray vec)
            writeReg vm a w
        else writeReg vm a mkEmptyW
    execLoop vm

-- MUTREAD: r = mutarr[idx]
dispatch vm 0x64 a b c _ _ _ = do
    arrW <- readReg vm b
    idxW <- readReg vm c
    if isHeapW arrW && isIntW idxW
        then do
            obj <- heapRead (vmHeap vm) arrW
            let i = getIntW idxW
            case obj of
                HMutArray vec | i >= 0 && i < MV.length vec -> do
                    Val v <- MV.read vec i
                    writeReg vm a v
                _ -> writeReg vm a mkEmptyW
        else writeReg vm a mkEmptyW
    execLoop vm

-- MUTWRITE: mutarr[idx] = val
dispatch vm 0x65 a b c _ _ _ = do
    arrW <- readReg vm a
    idxW <- readReg vm b
    val  <- readReg vm c
    if isHeapW arrW && isIntW idxW
        then do
            obj <- heapRead (vmHeap vm) arrW
            let i = getIntW idxW
            case obj of
                HMutArray vec | i >= 0 && i < MV.length vec ->
                    MV.write vec i (Val val)
                _ -> return ()
        else return ()
    execLoop vm

-- MUTLEN: r = length(mutarr)
dispatch vm 0x66 a b _ _ _ _ = do
    arrW <- readReg vm b
    if isHeapW arrW
        then do
            obj <- heapRead (vmHeap vm) arrW
            case obj of
                HMutArray vec -> writeReg vm a (mkIntW (MV.length vec))
                _             -> writeReg vm a (mkIntW 0)
        else writeReg vm a (mkIntW 0)
    execLoop vm

-- PRINT
dispatch vm 0x70 a _ _ _ _ _ = do
    val <- readReg vm a
    str <- valToStringW vm val
    TIO.putStr str
    hFlush stdout
    execLoop vm

-- PRINTLN
dispatch vm 0x71 a _ _ _ _ _ = do
    val <- readReg vm a
    str <- valToStringW vm val
    TIO.putStrLn str
    execLoop vm

-- READLINE
dispatch vm 0x72 a _ _ _ _ _ = do
    line <- TIO.getLine
    w <- strAlloc (vmStrings vm) line
    writeReg vm a w
    execLoop vm

-- STRCAT
dispatch vm 0x78 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    sa <- valToStringW vm va
    sb <- valToStringW vm vb
    w <- strAlloc (vmStrings vm) (sa <> sb)
    writeReg vm a w
    execLoop vm

-- STRLEN
dispatch vm 0x79 a b _ _ _ _ = do
    va <- readReg vm b
    if isStrW va
        then do
            s <- strRead (vmStrings vm) va
            writeReg vm a (mkIntW (T.length s))
        else writeReg vm a (mkIntW 0)
    execLoop vm

-- CLOSURE: r = Closure(funcIdx, upvalues from r+1..r+nupvals)
dispatch vm 0x48 a b c _ _ _ = do
    let funcIdx = b
        nupvals = c
    upvals <- V.generateM nupvals $ \i -> Val <$> readReg vm (a + 1 + i)
    w <- heapAlloc (vmHeap vm) (HClosure funcIdx upvals)
    writeReg vm a w
    execLoop vm

-- GETUPVAL: r = currentClosure.upvalues[idx]
-- Note: upvalues are at the beginning of the current frame (set during CALLCLS)
dispatch vm 0x49 a _ _ imm16 _ _ = do
    val <- readReg vm imm16  -- upvalue index maps directly to register
    writeReg vm a val
    execLoop vm

-- SHOWI: r = show(int) — allocates string in string table
dispatch vm 0x7A a b _ _ _ _ = do
    val <- readReg vm b
    let str = if isIntW val
              then T.pack (show (getIntW val))
              else T.pack "<non-int>"
    w <- strAlloc (vmStrings vm) str
    writeReg vm a w
    execLoop vm

-- SHOWF: r = show(float)
dispatch vm 0x7B a b _ _ _ _ = do
    val <- readReg vm b
    let str = if isFloatW val
              then T.pack (show (getFloatW val))
              else T.pack "<non-float>"
    w <- strAlloc (vmStrings vm) str
    writeReg vm a w
    execLoop vm

-- SHOWC: r = show(char)
dispatch vm 0x7C a b _ _ _ _ = do
    val <- readReg vm b
    let str = if isCharW val
              then T.singleton (getCharW val)
              else T.pack "<non-char>"
    w <- strAlloc (vmStrings vm) str
    writeReg vm a w
    execLoop vm

-- SHOWS: r = show(string) — quoted
dispatch vm 0x7D a b _ _ _ _ = do
    val <- readReg vm b
    str <- if isStrW val
           then do s <- strRead (vmStrings vm) val; return ("\"" <> s <> "\"")
           else return $ T.pack "<non-string>"
    w <- strAlloc (vmStrings vm) str
    writeReg vm a w
    execLoop vm

-- CLOCK: r = monotonic nanoseconds
dispatch vm 0x73 a _ _ _ _ _ = do
    t <- getPOSIXTime
    let nanos = floor (t * 1e9) :: Int
    writeReg vm a (mkIntW nanos)
    execLoop vm

-- PRINTNL: print newline to stdout
dispatch vm 0x7E _ _ _ _ _ _ = do
    TIO.putStr "\n"
    hFlush stdout
    execLoop vm

-- MODREF: dst = modifyRef(refReg, closureReg)
dispatch vm 0x7F a b c _ _ _ = do
    refW     <- readReg vm b
    closureW <- readReg vm c
    if isHeapW refW
        then do
            obj <- heapRead (vmHeap vm) refW
            case obj of
                HRef ref -> do
                    Val oldVal <- readIORef ref
                    result <- callClosureSync vm closureW [oldVal]
                    case result of
                        Left err -> return $ Left err
                        Right newVal -> do
                            writeIORef ref (Val newVal)
                            writeReg vm a mkUnitW
                            execLoop vm
                _ -> return $ Left (VMRuntimeError "modifyRef: not a ref")
        else return $ Left (VMRuntimeError "modifyRef: not a heap object")

-- MCALL: r = obj.method(args) — OOP dynamic method dispatch
-- For now: not yet implemented (no AWFY programs use classes)
dispatch _vm 0x80 _ _ _ _ _ _ = do
    return $ Left (VMRuntimeError "MCALL: OOP method dispatch not yet implemented in bytecode VM")

-- DEBUGLOC (no-op for now; stores source location for debugger)
dispatch vm 0xF0 _ _ _ _ _ _ = execLoop vm

-- ERROR (constant message)
dispatch _vm 0xF1 _ _ _ imm16 _ _ = do
    let k = bmConstants (vmModule _vm) V.! imm16
    return $ Left (VMRuntimeError (constToText k))

-- ERRORREG (register message)
dispatch vm 0xF2 a _ _ _ _ _ = do
    val <- readReg vm a
    msg <- if isStrW val
           then strRead (vmStrings vm) val
           else return $ valToString (Val val)
    return $ Left (VMRuntimeError msg)

-- NOP
dispatch vm 0xFE _ _ _ _ _ _ = execLoop vm

-- HALT
dispatch _vm 0xFF _ _ _ _ _ _ = return $ Right mkUnitW

-- Unknown opcode
dispatch _vm op _ _ _ _ _ _ = return $ Left (VMInvalidOpcode op)

-- ============================================================================
-- Helper functions
-- ============================================================================

-- | Max registers hint for a function (use fiNumRegs + small padding).
csMaxRegs :: FuncInfo -> Int
csMaxRegs fi = fiNumRegs fi + 4  -- small padding for safety

-- | Convert constant to Word64, allocating in string table if needed.
constToW :: VMState -> Constant -> IO Word64
constToW _  (KInt n)    = return (mkIntW n)
constToW _  (KFloat d)  = return (mkFloatW d)
constToW vm (KString s) = strAlloc (vmStrings vm) s
constToW vm (KName n)   = strAlloc (vmStrings vm) n

-- | Convert constant to Text (for error messages).
constToText :: Constant -> Text
constToText (KInt n)    = T.pack (show n)
constToText (KFloat d)  = T.pack (show d)
constToText (KString s) = s
constToText (KName n)   = n

-- | Extract tag from a NaN-boxed value for SWITCH dispatch.
-- Handles heap objects (HCon tag), bools, and ints.
getTagForSwitch :: VMState -> Word64 -> IO Int
getTagForSwitch vm w
    | isHeapW w = do
        obj <- heapRead (vmHeap vm) w
        case obj of
            HCon tag _ _ -> return tag
            _            -> return (-1)
    | isBoolW w  = return (if getBoolW w then 0 else 1)
    | isIntW w   = return (getIntW w)
    | otherwise  = return (-1)

-- | Convert a Word64 to a displayable Text string.
-- Handles all NaN-boxed types including heap objects and strings.
valToStringW :: VMState -> Word64 -> IO Text
valToStringW vm w
    | isIntW w    = return $ T.pack (show (getIntW w))
    | isFloatW w  = return $ T.pack (show (getFloatW w))
    | isBoolW w   = return $ if getBoolW w then "True" else "False"
    | isCharW w   = return $ T.singleton (getCharW w)
    | isUnitW w   = return "()"
    | isEmptyW w  = return ""
    | isStrW w    = strRead (vmStrings vm) w
    | isHeapW w   = do
        obj <- heapRead (vmHeap vm) w
        case obj of
            HCon tag _ fields -> do
                fieldStrs <- mapM (\(Val fw) -> valToStringW vm fw) (V.toList fields)
                return $ T.pack $ "Con(" ++ show tag ++ ", [" ++
                    concatMap (\s -> T.unpack s ++ ", ") fieldStrs ++ "])"
            HArray xs -> do
                elemStrs <- mapM (\(Val fw) -> valToStringW vm fw) (V.toList xs)
                return $ "[" <> T.intercalate ", " elemStrs <> "]"
            HClosure fi _ -> return $ T.pack $ "<closure:" ++ show fi ++ ">"
            HPAP fi _ _   -> return $ T.pack $ "<pap:" ++ show fi ++ ">"
            HRef _        -> return "<ref>"
            HMutArray _   -> return "<mutarray>"
    | otherwise   = return $ T.pack "<unknown>"

-- | Call a closure synchronously by saving/restoring VM state.
-- Used by MODREF for higher-order builtins like modifyRef.
-- Takes and returns raw Word64.
callClosureSync :: VMState -> Word64 -> [Word64] -> IO (Either VMError Word64)
callClosureSync vm closureW args = do
    if isHeapW closureW
        then do
            obj <- heapRead (vmHeap vm) closureW
            case obj of
                HClosure funcIdx upvals -> do
                    let bm = vmModule vm
                    case lookupFunction bm funcIdx of
                        Nothing -> return $ Left (VMInvalidFunction funcIdx)
                        Just fi -> do
                            -- Save current state
                            savedPC    <- readIORef (vmPC vm)
                            savedFP    <- readIORef (vmFP vm)
                            savedFS    <- readIORef (vmFrameSize vm)
                            savedStack <- readIORef (vmCallStack vm)
                            savedDepth <- readIORef (vmCallDepth vm)
                            -- Set up a new frame at a safe offset
                            let newFP = savedFP + savedFS
                            writeIORef (vmFP vm) newFP
                            writeIORef (vmFrameSize vm) (csMaxRegs fi)
                            writeIORef (vmCallStack vm) []
                            writeIORef (vmCallDepth vm) 0
                            -- Write upvalues (stored as Val in heap)
                            forM_ [0..V.length upvals - 1] $ \i ->
                                writeRegAbs vm (newFP + i) (unVal (upvals V.! i))
                            -- Write args after upvalues
                            let argBase = V.length upvals
                            forM_ (zip [0..] args) $ \(i, v) ->
                                writeRegAbs vm (newFP + argBase + i) v
                            -- Jump to function entry
                            writeIORef (vmPC vm) (fiEntry fi)
                            -- Run the closure
                            result <- execLoop vm
                            -- Restore saved state
                            writeIORef (vmPC vm) savedPC
                            writeIORef (vmFP vm) savedFP
                            writeIORef (vmFrameSize vm) savedFS
                            writeIORef (vmCallStack vm) savedStack
                            writeIORef (vmCallDepth vm) savedDepth
                            return result
                _ -> return $ Left (VMRuntimeError "callClosureSync: not a closure")
        else return $ Left (VMRuntimeError "callClosureSync: not a heap object")
