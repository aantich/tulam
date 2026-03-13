{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Bytecode VM execution engine.
--
-- Register-based virtual machine with proper tail call optimization.
-- The core dispatch loop is a tight tail-recursive function that GHC
-- compiles to efficient machine code with minimal allocation.
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
import Backends.Bytecode.Instruction
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
    { vmModule     :: !BytecodeModule       -- loaded module
    , vmRegisters  :: !(IORef (MV.IOVector Val))  -- growable register file
    , vmPC         :: !(IORef Int)          -- program counter
    , vmFP         :: !(IORef Int)          -- frame pointer (register base offset)
    , vmFrameSize  :: !(IORef Int)          -- current frame size (caller's maxRegs)
    , vmCallStack  :: !(IORef [Frame])      -- call stack (LIFO)
    , vmCallDepth  :: !(IORef Int)          -- current call depth
    }

-- | Initialize a VM from a bytecode module.
initVM :: BytecodeModule -> IO VMState
initVM bm = do
    regs <- MV.replicate initialRegFileSize VEmpty
    regsRef <- newIORef regs
    pc <- newIORef 0
    fp <- newIORef 0
    frameSize <- newIORef 256  -- default; set properly by runFunction
    stack <- newIORef []
    depth <- newIORef 0
    return VMState
        { vmModule    = bm
        , vmRegisters = regsRef
        , vmPC        = pc
        , vmFP        = fp
        , vmFrameSize = frameSize
        , vmCallStack = stack
        , vmCallDepth = depth
        }

-- | Read a register (absolute index = FP + local index).
readReg :: VMState -> Int -> IO Val
readReg vm r = do
    fp <- readIORef (vmFP vm)
    regs <- readIORef (vmRegisters vm)
    let idx = fp + r
    if idx < MV.length regs
        then MV.read regs idx
        else return VEmpty

-- | Write a register.
writeReg :: VMState -> Int -> Val -> IO ()
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
        newRegs <- MV.replicate newSize VEmpty
        -- Copy old data
        forM_ [0..currentSize-1] $ \i -> do
            v <- MV.read regs i
            MV.write newRegs i v
        writeIORef (vmRegisters vm) newRegs

-- | Run a function by index with the given arguments.
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
            -- Write arguments to parameter registers
            forM_ (zip [0..] args) $ \(i, arg) ->
                writeReg vm i arg
            -- Run the dispatch loop
            execLoop vm

-- | Run a function by name.
runFunctionByName :: VMState -> Text -> [Val] -> IO (Either VMError Val)
runFunctionByName vm name args =
    case lookupFunctionByName (vmModule vm) name of
        Nothing -> return $ Left (VMRuntimeError $ "Function not found: " <> name)
        Just (idx, _) -> runFunction vm idx args

-- | The main execution loop.
-- This is the hot path — must be a tight tail-recursive loop.
execLoop :: VMState -> IO (Either VMError Val)
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

-- | Dispatch a single instruction. Inlined by GHC for the common opcodes.
dispatch :: VMState -> Word8 -> Int -> Int -> Int -> Int -> Int -> Int -> IO (Either VMError Val)

-- LOADK: r = constants[k]
dispatch vm 0x01 a _ _ imm16 _ _ = do
    let k = bmConstants (vmModule vm) V.! imm16
    writeReg vm a (constToVal k)
    execLoop vm

-- LOADINT: r = imm16 (sign-extended)
dispatch vm 0x02 a _ _ _ simm16 _ = do
    writeReg vm a (VInt simm16)
    execLoop vm

-- LOADTRUE
dispatch vm 0x03 a _ _ _ _ _ = do
    writeReg vm a (VBool True)
    execLoop vm

-- LOADFALSE
dispatch vm 0x04 a _ _ _ _ _ = do
    writeReg vm a (VBool False)
    execLoop vm

-- LOADUNIT
dispatch vm 0x05 a _ _ _ _ _ = do
    writeReg vm a VUnit
    execLoop vm

-- LOADNIL
dispatch vm 0x06 a _ _ _ _ _ = do
    writeReg vm a VEmpty
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
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x + y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- SUBI
dispatch vm 0x11 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x - y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- MULI
dispatch vm 0x12 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x * y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- DIVI
dispatch vm 0x13 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt _, VInt 0) -> return $ Left VMDivisionByZero
        (VInt x, VInt y) -> do writeReg vm a (VInt (x `div` y)); execLoop vm
        _ -> do writeReg vm a VEmpty; execLoop vm

-- REMI
dispatch vm 0x14 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt _, VInt 0) -> return $ Left VMDivisionByZero
        (VInt x, VInt y) -> do writeReg vm a (VInt (x `rem` y)); execLoop vm
        _ -> do writeReg vm a VEmpty; execLoop vm

-- NEGI
dispatch vm 0x15 a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VInt x -> writeReg vm a (VInt (negate x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ADDF
dispatch vm 0x18 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VFloat (x + y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- SUBF
dispatch vm 0x19 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VFloat (x - y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- MULF
dispatch vm 0x1A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VFloat (x * y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- DIVF
dispatch vm 0x1B a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) | y /= 0.0 -> do writeReg vm a (VFloat (x / y)); execLoop vm
        (VFloat _, VFloat _) -> return $ Left VMDivisionByZero
        _ -> return $ Left VMDivisionByZero

-- NEGF
dispatch vm 0x1C a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VFloat (negate x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ABSI
dispatch vm 0x16 a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VInt x -> writeReg vm a (VInt (abs x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- SQRTF
dispatch vm 0x1D a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VFloat (sqrt x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ABSF
dispatch vm 0x1E a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VFloat (abs x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- SQRTF
dispatch vm 0x1D a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VFloat (sqrt x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ABSF
dispatch vm 0x1E a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VFloat (abs x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ITOF
dispatch vm 0x1F a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VInt x -> writeReg vm a (VFloat (fromIntegral x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- EQI
dispatch vm 0x20 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (VBool (va == vb))
    execLoop vm

-- LTI
dispatch vm 0x21 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VBool (x < y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- LEI
dispatch vm 0x22 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VBool (x <= y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- GTI
dispatch vm 0x23 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VBool (x > y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- GEI
dispatch vm 0x24 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VBool (x >= y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- NEQI
dispatch vm 0x09 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (VBool (va /= vb))
    execLoop vm

-- NEQF
dispatch vm 0x0A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x /= y))
        _ -> writeReg vm a (VBool True)
    execLoop vm

-- FTOI
dispatch vm 0x25 a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VFloat x -> writeReg vm a (VInt (truncate x))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- GTF
dispatch vm 0x26 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x > y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- GEF
dispatch vm 0x27 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x >= y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- EQF
dispatch vm 0x28 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x == y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- LTF
dispatch vm 0x29 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x < y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- EQP (pointer/value equality)
dispatch vm 0x2A a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (VBool (va == vb))
    execLoop vm

-- LEF
dispatch vm 0x2B a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VFloat x, VFloat y) -> writeReg vm a (VBool (x <= y))
        _ -> writeReg vm a (VBool False)
    execLoop vm

-- BAND
dispatch vm 0x2C a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x .&. y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- BOR
dispatch vm 0x2D a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x .|. y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- BXOR
dispatch vm 0x2E a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x `xor` y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- BSHL
dispatch vm 0x2F a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x `shiftL` y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- BSHR
dispatch vm 0x30 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    case (va, vb) of
        (VInt x, VInt y) -> writeReg vm a (VInt (x `shiftR` y))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- JMP: PC += offset
dispatch vm 0x38 _ _ _ _ _ simm24 = do
    pc <- readIORef (vmPC vm)
    writeIORef (vmPC vm) (pc + simm24 - 1)  -- -1 because we already incremented
    execLoop vm

-- JMPT: if r then PC += offset
dispatch vm 0x39 a _ _ _ simm16 _ = do
    va <- readReg vm a
    when (isValTrue va) $ do
        pc <- readIORef (vmPC vm)
        writeIORef (vmPC vm) (pc + simm16 - 1)
    execLoop vm

-- JMPF: if !r then PC += offset
dispatch vm 0x3A a _ _ _ simm16 _ = do
    va <- readReg vm a
    when (not (isValTrue va)) $ do
        pc <- readIORef (vmPC vm)
        writeIORef (vmPC vm) (pc + simm16 - 1)
    execLoop vm

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
            -- Copy args to parameter positions (in-place)
            -- Args are in current frame at some registers; move them to r0..rN-1
            -- We need to be careful about overlapping reads/writes
            -- For self-tail-call: args might already be at the right positions
            -- For general tail call: read all args first, then write
            args <- mapM (\i -> readReg vm i) [0..nargs-1]
            forM_ (zip [0..] args) $ \(i, v) ->
                writeRegAbs vm (fp + i) v
            writeIORef (vmFrameSize vm) (csMaxRegs fi)
            writeIORef (vmPC vm) (fiEntry fi)
            execLoop vm

-- CALLCLS: r = closure(args)
dispatch vm 0x42 a b c _ _ _ = do
    closureVal' <- readReg vm b
    let nargs = c
        dstReg = a
    case closureVal' of
        VObj (HClosure funcIdx upvals) -> do
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
                    -- Write upvalues
                    forM_ [0..V.length upvals - 1] $ \i ->
                        writeRegAbs vm (newFP + i) (upvals V.! i)
                    -- Write args after upvalues
                    let argBase = V.length upvals
                    forM_ [0..nargs-1] $ \i -> do
                        v <- readRegAbs vm (fp + dstReg + 1 + i)
                        writeRegAbs vm (newFP + argBase + i) v
                    writeIORef (vmPC vm) (fiEntry fi)
                    execLoop vm
        VObj (HPAP funcIdx expected appliedArgs) -> do
            -- Saturate partial application
            newArgs <- mapM (\i -> readReg vm (dstReg + 1 + i)) [0..nargs-1]
            let allArgs = V.toList appliedArgs ++ newArgs
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
                            forM_ (zip [0..] allArgs) $ \(i, v) ->
                                writeRegAbs vm (newFP + i) v
                            writeIORef (vmPC vm) (fiEntry fi)
                            execLoop vm
                else do
                    -- Still partial: build new PAP
                    writeReg vm dstReg (VObj (HPAP funcIdx expected (V.fromList allArgs)))
                    execLoop vm
        _ -> return $ Left (VMRuntimeError "CALLCLS: not a closure or PAP")

-- TAILCALLCLS: tail call closure
dispatch vm 0x43 _ b c _ _ _ = do
    closureVal' <- readReg vm b
    let nargs = c
    case closureVal' of
        VObj (HClosure funcIdx upvals) -> do
            let bm = vmModule vm
            case lookupFunction bm funcIdx of
                Nothing -> return $ Left (VMInvalidFunction funcIdx)
                Just fi -> do
                    fp <- readIORef (vmFP vm)
                    -- Read args before overwriting
                    args <- mapM (\i -> readReg vm i) [0..nargs-1]
                    -- Write upvalues
                    forM_ [0..V.length upvals - 1] $ \i ->
                        writeRegAbs vm (fp + i) (upvals V.! i)
                    -- Write args
                    let argBase = V.length upvals
                    forM_ (zip [0..] args) $ \(i, v) ->
                        writeRegAbs vm (fp + argBase + i) v
                    writeIORef (vmFrameSize vm) (csMaxRegs fi)
                    writeIORef (vmPC vm) (fiEntry fi)
                    execLoop vm
        _ -> return $ Left (VMRuntimeError "TAILCALLCLS: not a closure")

-- NEWCON: r = Con(tag, fields in r+1..r+nfields)
dispatch vm 0x50 a b c _ _ _ = do
    let tag = b
        nfields = c
    fields <- V.generateM nfields $ \i -> readReg vm (a + 1 + i)
    writeReg vm a (VObj (HCon tag nfields fields))
    execLoop vm

-- GETFIELD: r = obj.fields[idx]
dispatch vm 0x51 a b c _ _ _ = do
    obj <- readReg vm b
    let idx = c
    case obj of
        VObj (HCon _ _ fields) | idx < V.length fields ->
            writeReg vm a (fields V.! idx)
        _ -> writeReg vm a VEmpty
    execLoop vm

-- GETTAG: r = obj.tag
-- Also handles VBool (True=0, False=1) for pattern matching on Bool
dispatch vm 0x52 a b _ _ _ _ = do
    obj <- readReg vm b
    case obj of
        VObj (HCon tag _ _) -> writeReg vm a (VInt tag)
        VBool True  -> writeReg vm a (VInt 0)   -- True has tag 0
        VBool False -> writeReg vm a (VInt 1)   -- False has tag 1
        _ -> writeReg vm a (VInt (-1))
    execLoop vm

-- NEWARRAY: r = array(len)
dispatch vm 0x58 a _ _ imm16 _ _ = do
    writeReg vm a (VObj (HArray (V.replicate imm16 VUnit)))
    execLoop vm

-- ARRGET: r = arr[idx]
dispatch vm 0x59 a b c _ _ _ = do
    arr <- readReg vm b
    idx <- readReg vm c
    case (arr, idx) of
        (VObj (HArray xs), VInt i) | i >= 0 && i < V.length xs ->
            writeReg vm a (xs V.! i)
        _ -> writeReg vm a VEmpty
    execLoop vm

-- ARRSET: arr[idx] = val
dispatch vm 0x5A a b c _ _ _ = do
    arr <- readReg vm a
    idx <- readReg vm b
    val <- readReg vm c
    case (arr, idx) of
        (VObj (HArray xs), VInt i) | i >= 0 && i < V.length xs ->
            writeReg vm a (VObj (HArray (xs V.// [(i, val)])))
        _ -> return ()
    execLoop vm

-- ARRLEN: r = length(arr)
dispatch vm 0x5B a b _ _ _ _ = do
    arr <- readReg vm b
    case arr of
        VObj (HArray xs) -> writeReg vm a (VInt (V.length xs))
        _ -> writeReg vm a (VInt 0)
    execLoop vm

-- NEWREF: r = newRef(val)
dispatch vm 0x60 a b _ _ _ _ = do
    val <- readReg vm b
    ref <- newIORef val
    writeReg vm a (VObj (HRef ref))
    execLoop vm

-- READREF: r = readRef(ref)
dispatch vm 0x61 a b _ _ _ _ = do
    refVal' <- readReg vm b
    case refVal' of
        VObj (HRef ref) -> do
            val <- readIORef ref
            writeReg vm a val
        _ -> writeReg vm a VEmpty
    execLoop vm

-- WRITEREF: writeRef(ref, val)
dispatch vm 0x62 a b _ _ _ _ = do
    refVal' <- readReg vm a
    val <- readReg vm b
    case refVal' of
        VObj (HRef ref) -> writeIORef ref val
        _ -> return ()
    execLoop vm

-- NEWMUTARR: r = MutArray(len, default)
dispatch vm 0x63 a b c _ _ _ = do
    lenVal <- readReg vm b
    defVal <- readReg vm c
    case lenVal of
        VInt len -> do
            vec <- MV.replicate len defVal
            writeReg vm a (VObj (HMutArray vec))
        _ -> writeReg vm a VEmpty
    execLoop vm

-- MUTREAD: r = mutarr[idx]
dispatch vm 0x64 a b c _ _ _ = do
    arrVal <- readReg vm b
    idxVal <- readReg vm c
    case (arrVal, idxVal) of
        (VObj (HMutArray vec), VInt i) | i >= 0 && i < MV.length vec -> do
            val <- MV.read vec i
            writeReg vm a val
        _ -> writeReg vm a VEmpty
    execLoop vm

-- MUTWRITE: mutarr[idx] = val
dispatch vm 0x65 a b c _ _ _ = do
    arrVal <- readReg vm a
    idxVal <- readReg vm b
    val <- readReg vm c
    case (arrVal, idxVal) of
        (VObj (HMutArray vec), VInt i) | i >= 0 && i < MV.length vec ->
            MV.write vec i val
        _ -> return ()
    execLoop vm

-- MUTLEN: r = length(mutarr)
dispatch vm 0x66 a b _ _ _ _ = do
    arrVal <- readReg vm b
    case arrVal of
        VObj (HMutArray vec) -> writeReg vm a (VInt (MV.length vec))
        _ -> writeReg vm a (VInt 0)
    execLoop vm

-- PRINT
dispatch vm 0x70 a _ _ _ _ _ = do
    val <- readReg vm a
    TIO.putStr (valToString val)
    hFlush stdout
    execLoop vm

-- PRINTLN
dispatch vm 0x71 a _ _ _ _ _ = do
    val <- readReg vm a
    TIO.putStrLn (valToString val)
    execLoop vm

-- READLINE
dispatch vm 0x72 a _ _ _ _ _ = do
    line <- TIO.getLine
    writeReg vm a (VString line)
    execLoop vm

-- STRCAT
dispatch vm 0x78 a b c _ _ _ = do
    va <- readReg vm b
    vb <- readReg vm c
    writeReg vm a (VString (valToString va <> valToString vb))
    execLoop vm

-- STRLEN
dispatch vm 0x79 a b _ _ _ _ = do
    va <- readReg vm b
    case va of
        VString s -> writeReg vm a (VInt (T.length s))
        _ -> writeReg vm a (VInt 0)
    execLoop vm

-- CLOSURE: r = Closure(funcIdx, upvalues from r+1..r+nupvals)
dispatch vm 0x48 a b c _ _ _ = do
    let funcIdx = b
        nupvals = c
    upvals <- V.generateM nupvals $ \i -> readReg vm (a + 1 + i)
    writeReg vm a (VObj (HClosure funcIdx upvals))
    execLoop vm

-- GETUPVAL: r = currentClosure.upvalues[idx]
-- Note: upvalues are at the beginning of the current frame (set during CALLCLS)
dispatch vm 0x49 a _ _ imm16 _ _ = do
    val <- readReg vm imm16  -- upvalue index maps directly to register
    writeReg vm a val
    execLoop vm

-- SHOWI: r = show(int)
dispatch vm 0x7A a b _ _ _ _ = do
    val <- readReg vm b
    let str = case val of
            VInt n  -> T.pack (show n)
            _       -> T.pack (show val)
    writeReg vm a (VString str)
    execLoop vm

-- SHOWF: r = show(float)
dispatch vm 0x7B a b _ _ _ _ = do
    val <- readReg vm b
    let str = case val of
            VFloat d -> T.pack (show d)
            _        -> T.pack (show val)
    writeReg vm a (VString str)
    execLoop vm

-- SHOWC: r = show(char)
dispatch vm 0x7C a b _ _ _ _ = do
    val <- readReg vm b
    let str = case val of
            VChar c  -> T.singleton c
            _        -> T.pack (show val)
    writeReg vm a (VString str)
    execLoop vm

-- SHOWS: r = show(string) — quoted
dispatch vm 0x7D a b _ _ _ _ = do
    val <- readReg vm b
    let str = case val of
            VString s -> "\"" <> s <> "\""
            _         -> T.pack (show val)
    writeReg vm a (VString str)
    execLoop vm

-- CLOCK: r = monotonic nanoseconds
dispatch vm 0x73 a _ _ _ _ _ = do
    t <- getPOSIXTime
    let nanos = floor (t * 1e9) :: Int
    writeReg vm a (VInt nanos)
    execLoop vm

-- PRINTNL: print newline to stdout
dispatch vm 0x7E _ _ _ _ _ _ = do
    TIO.putStr "\n"
    hFlush stdout
    execLoop vm

-- MODREF: dst = modifyRef(refReg, closureReg)
dispatch vm 0x7F a b c _ _ _ = do
    refVal <- readReg vm b
    closureVal' <- readReg vm c
    case refVal of
        VObj (HRef ref) -> do
            oldVal <- readIORef ref
            result <- callClosureSync vm closureVal' [oldVal]
            case result of
                Left err -> return $ Left err
                Right newVal -> do
                    writeIORef ref newVal
                    writeReg vm a VUnit
                    execLoop vm
        _ -> return $ Left (VMRuntimeError "modifyRef: not a ref")

-- MCALL: r = obj.method(args) — OOP dynamic method dispatch
-- For now: not yet implemented (no AWFY programs use classes)
dispatch _vm 0x80 _ b _ _ _ _ = do
    return $ Left (VMRuntimeError "MCALL: OOP method dispatch not yet implemented in bytecode VM")

-- DEBUGLOC (no-op for now; stores source location for debugger)
dispatch vm 0xF0 _ _ _ _ _ _ = execLoop vm

-- ERROR (constant message)
dispatch vm 0xF1 _ _ _ imm16 _ _ = do
    let k = bmConstants (vmModule vm) V.! imm16
    return $ Left (VMRuntimeError (constToText k))

-- ERRORREG (register message)
dispatch vm 0xF2 a _ _ _ _ _ = do
    val <- readReg vm a
    let msg = case val of
            VString s -> s
            _         -> T.pack (show val)
    return $ Left (VMRuntimeError msg)

-- NOP
dispatch vm 0xFE _ _ _ _ _ _ = execLoop vm

-- HALT
dispatch _vm 0xFF _ _ _ _ _ _ = return $ Right VUnit

-- Unknown opcode
dispatch _vm op _ _ _ _ _ _ = return $ Left (VMInvalidOpcode op)

-- Helper: max registers hint for a function (use fiNumRegs + small padding)
csMaxRegs :: FuncInfo -> Int
csMaxRegs fi = fiNumRegs fi + 4  -- small padding for safety

-- Helper: read register at absolute position
readRegAbs :: VMState -> Int -> IO Val
readRegAbs vm idx = do
    regs <- readIORef (vmRegisters vm)
    if idx < MV.length regs
        then MV.read regs idx
        else return VEmpty

-- Helper: write register at absolute position
writeRegAbs :: VMState -> Int -> Val -> IO ()
writeRegAbs vm idx v = do
    ensureRegSize vm (idx + 1)
    regs <- readIORef (vmRegisters vm)
    MV.write regs idx v

-- Helper: convert constant to Val
constToVal :: Constant -> Val
constToVal (KInt n)    = VInt n
constToVal (KFloat d)  = VFloat d
constToVal (KString s) = VString s
constToVal (KName n)   = VString n  -- names as strings for dispatch

-- Helper: convert constant to Text (for error messages)
constToText :: Constant -> Text
constToText (KInt n)    = T.pack (show n)
constToText (KFloat d)  = T.pack (show d)
constToText (KString s) = s
constToText (KName n)   = n


-- | Call a closure synchronously by saving/restoring VM state.
-- Used by DISPATCH for higher-order builtins like modifyRef.
callClosureSync :: VMState -> Val -> [Val] -> IO (Either VMError Val)
callClosureSync vm closureVal' args = do
    case closureVal' of
        VObj (HClosure funcIdx upvals) -> do
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
                    -- Set up a new frame at a safe offset (after caller's frame)
                    let newFP = savedFP + savedFS
                    writeIORef (vmFP vm) newFP
                    writeIORef (vmFrameSize vm) (csMaxRegs fi)
                    writeIORef (vmCallStack vm) []
                    writeIORef (vmCallDepth vm) 0
                    -- Write upvalues
                    forM_ [0..V.length upvals - 1] $ \i ->
                        writeRegAbs vm (newFP + i) (upvals V.! i)
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
