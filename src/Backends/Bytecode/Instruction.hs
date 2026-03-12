{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Bytecode instruction set for the tulam VM.
--
-- 32-bit fixed-width instructions, register-addressed (8-bit index).
-- Encoding formats:
--   A: [op:8][dst:8][src1:8][src2:8]
--   B: [op:8][dst:8][imm16:16]
--   C: [op:8][dst:8][src:8][idx:8]
--   D: [op:8][operand:24]
module Backends.Bytecode.Instruction
    ( OpCode(..)
    , Instruction(..)
    , encodeInstr
    , decodeInstr
    , disassemble
    , disassembleOne
    ) where

import Data.Word
import Data.Bits
import Data.Int (Int16)

-- | Opcode enumeration. Values are the 8-bit opcode byte.
data OpCode
    -- Constants & moves
    = OP_LOADK       -- 0x01  B: r = constants[k]
    | OP_LOADINT     -- 0x02  B: r = signExtend(imm16)
    | OP_LOADTRUE    -- 0x03  B: r = True
    | OP_LOADFALSE   -- 0x04  B: r = False
    | OP_LOADUNIT    -- 0x05  B: r = Unit
    | OP_LOADNIL     -- 0x06  B: r = Empty
    | OP_MOV         -- 0x07  A: r1 = r2
    -- Int arithmetic
    | OP_ADDI        -- 0x10  A: r = a + b
    | OP_SUBI        -- 0x11  A: r = a - b
    | OP_MULI        -- 0x12  A: r = a * b
    | OP_DIVI        -- 0x13  A: r = a / b
    | OP_REMI        -- 0x14  A: r = a % b
    | OP_NEGI        -- 0x15  A: r = -a
    | OP_ABSI        -- 0x16  A: r = |a|
    -- Float arithmetic
    | OP_ADDF        -- 0x18  A: r = a +. b
    | OP_SUBF        -- 0x19  A: r = a -. b
    | OP_MULF        -- 0x1A  A: r = a *. b
    | OP_DIVF        -- 0x1B  A: r = a /. b
    | OP_NEGF        -- 0x1C  A: r = -.a
    | OP_SQRTF       -- 0x1D  A: r = sqrt(a)
    | OP_ABSF        -- 0x1E  A: r = |a|
    | OP_ITOF        -- 0x1F  A: r = toFloat(a) (int→float64)
    -- Comparison
    | OP_EQI         -- 0x20  A: r = (a == b) int
    | OP_LTI         -- 0x21  A: r = (a < b) int
    | OP_LEI         -- 0x22  A: r = (a <= b) int
    | OP_GTI         -- 0x23  A: r = (a > b) int
    | OP_GEI         -- 0x24  A: r = (a >= b) int
    | OP_FTOI        -- 0x25  A: r = truncate(a) (float64→int)
    | OP_GTF         -- 0x26  A: r = (a > b) float
    | OP_GEF         -- 0x27  A: r = (a >= b) float
    | OP_EQF         -- 0x28  A: r = (a == b) float
    | OP_LTF         -- 0x29  A: r = (a < b) float
    | OP_EQP         -- 0x2A  A: r = (a == b) pointer
    | OP_LEF         -- 0x2B  A: r = (a <= b) float
    | OP_NEQI        -- 0x09  A: r = (a != b) int
    | OP_NEQF        -- 0x0A  A: r = (a != b) float
    -- Bitwise
    | OP_BAND        -- 0x2C  A: r = a .&. b
    | OP_BOR         -- 0x2D  A: r = a .|. b
    | OP_BXOR        -- 0x2E  A: r = a xor b
    | OP_BSHL        -- 0x2F  A: r = a << b
    | OP_BSHR        -- 0x30  A: r = a >> b
    -- Control flow
    | OP_JMP         -- 0x38  D: PC += offset (signed 24-bit)
    | OP_JMPT        -- 0x39  B+ext: if r then PC += offset
    | OP_JMPF        -- 0x3A  B+ext: if !r then PC += offset
    | OP_SWITCH      -- 0x3B  B: dispatch on tag to jump table
    | OP_RET         -- 0x3C  B: return r
    -- Function calls
    | OP_CALL        -- 0x40  special: r = func(args)
    | OP_TAILCALL    -- 0x41  special: tail call (reuse frame)
    | OP_CALLCLS     -- 0x42  special: r = closure(args)
    | OP_TAILCALLCLS -- 0x43  special: tail call closure
    | OP_CALLEXT     -- 0x44  special: r = extern(args)
    -- Closures
    | OP_CLOSURE     -- 0x48  special: create closure
    | OP_GETUPVAL    -- 0x49  B: r = upvalues[idx]
    | OP_PAP         -- 0x4A  special: create partial application
    -- Constructors
    | OP_NEWCON      -- 0x50  special: r = Con(tag, fields)
    | OP_GETFIELD    -- 0x51  C: r = obj.fields[idx]
    | OP_GETTAG      -- 0x52  A: r = obj.tag
    -- Arrays
    | OP_NEWARRAY    -- 0x58  B: r = array(len)
    | OP_ARRGET      -- 0x59  A: r = arr[idx]
    | OP_ARRSET      -- 0x5A  A: arr[idx] = val
    | OP_ARRLEN      -- 0x5B  A: r = length(arr)
    -- Mutable refs
    | OP_NEWREF      -- 0x60  A: r = newRef(val)
    | OP_READREF     -- 0x61  A: r = readRef(ref)
    | OP_WRITEREF    -- 0x62  A: writeRef(ref, val)
    -- Mutable arrays
    | OP_NEWMUTARR  -- 0x63  A: r = newMutArray(len, default)
    | OP_MUTREAD    -- 0x64  A: r = mutRead(arr, idx)
    | OP_MUTWRITE   -- 0x65  A: mutWrite(arr, idx, val)
    | OP_MUTLEN     -- 0x66  A: r = mutLength(arr)
    -- Effects
    | OP_PUSHHANDLER -- 0x68  special: push handler frame
    | OP_POPHANDLER  -- 0x69  D: pop handler frame
    | OP_PERFORM     -- 0x6A  special: perform effect op
    -- IO
    | OP_PRINT       -- 0x70  B: print(r)
    | OP_PRINTLN     -- 0x71  B: println(r)
    | OP_READLINE    -- 0x72  B: r = readline()
    -- Strings
    | OP_STRCAT      -- 0x78  A: r = a ++ b
    | OP_STRLEN      -- 0x79  A: r = length(a)
    -- Dispatch (REPL fallback)
    | OP_DISPATCH    -- 0x80  special: runtime dispatch
    -- Debug
    | OP_DEBUGLOC    -- 0xF0  B: set source location
    | OP_ERROR       -- 0xF1  B: raise error
    | OP_NOP         -- 0xFE  D: no-op
    | OP_HALT        -- 0xFF  D: stop
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Map opcode to its byte value.
opToByte :: OpCode -> Word8
opToByte OP_LOADK       = 0x01
opToByte OP_LOADINT     = 0x02
opToByte OP_LOADTRUE    = 0x03
opToByte OP_LOADFALSE   = 0x04
opToByte OP_LOADUNIT    = 0x05
opToByte OP_LOADNIL     = 0x06
opToByte OP_MOV         = 0x07
opToByte OP_ADDI        = 0x10
opToByte OP_SUBI        = 0x11
opToByte OP_MULI        = 0x12
opToByte OP_DIVI        = 0x13
opToByte OP_REMI        = 0x14
opToByte OP_NEGI        = 0x15
opToByte OP_ABSI        = 0x16
opToByte OP_ADDF        = 0x18
opToByte OP_SUBF        = 0x19
opToByte OP_MULF        = 0x1A
opToByte OP_DIVF        = 0x1B
opToByte OP_NEGF        = 0x1C
opToByte OP_SQRTF       = 0x1D
opToByte OP_ABSF        = 0x1E
opToByte OP_ITOF        = 0x1F
opToByte OP_EQI         = 0x20
opToByte OP_LTI         = 0x21
opToByte OP_LEI         = 0x22
opToByte OP_GTI         = 0x23
opToByte OP_GEI         = 0x24
opToByte OP_FTOI        = 0x25
opToByte OP_GTF         = 0x26
opToByte OP_GEF         = 0x27
opToByte OP_EQF         = 0x28
opToByte OP_LTF         = 0x29
opToByte OP_EQP         = 0x2A
opToByte OP_LEF         = 0x2B
opToByte OP_NEQI        = 0x09
opToByte OP_NEQF        = 0x0A
opToByte OP_BAND        = 0x2C
opToByte OP_BOR         = 0x2D
opToByte OP_BXOR        = 0x2E
opToByte OP_BSHL        = 0x2F
opToByte OP_BSHR        = 0x30
opToByte OP_JMP         = 0x38
opToByte OP_JMPT        = 0x39
opToByte OP_JMPF        = 0x3A
opToByte OP_SWITCH      = 0x3B
opToByte OP_RET         = 0x3C
opToByte OP_CALL        = 0x40
opToByte OP_TAILCALL    = 0x41
opToByte OP_CALLCLS     = 0x42
opToByte OP_TAILCALLCLS = 0x43
opToByte OP_CALLEXT     = 0x44
opToByte OP_CLOSURE     = 0x48
opToByte OP_GETUPVAL    = 0x49
opToByte OP_PAP         = 0x4A
opToByte OP_NEWCON      = 0x50
opToByte OP_GETFIELD    = 0x51
opToByte OP_GETTAG      = 0x52
opToByte OP_NEWARRAY    = 0x58
opToByte OP_ARRGET      = 0x59
opToByte OP_ARRSET      = 0x5A
opToByte OP_ARRLEN      = 0x5B
opToByte OP_NEWREF      = 0x60
opToByte OP_READREF     = 0x61
opToByte OP_WRITEREF    = 0x62
opToByte OP_NEWMUTARR  = 0x63
opToByte OP_MUTREAD    = 0x64
opToByte OP_MUTWRITE   = 0x65
opToByte OP_MUTLEN     = 0x66
opToByte OP_PUSHHANDLER = 0x68
opToByte OP_POPHANDLER  = 0x69
opToByte OP_PERFORM     = 0x6A
opToByte OP_PRINT       = 0x70
opToByte OP_PRINTLN     = 0x71
opToByte OP_READLINE    = 0x72
opToByte OP_STRCAT      = 0x78
opToByte OP_STRLEN      = 0x79
opToByte OP_DISPATCH    = 0x80
opToByte OP_DEBUGLOC    = 0xF0
opToByte OP_ERROR       = 0xF1
opToByte OP_NOP         = 0xFE
opToByte OP_HALT        = 0xFF

-- | Map byte to opcode.
byteToOp :: Word8 -> Maybe OpCode
byteToOp 0x01 = Just OP_LOADK
byteToOp 0x02 = Just OP_LOADINT
byteToOp 0x03 = Just OP_LOADTRUE
byteToOp 0x04 = Just OP_LOADFALSE
byteToOp 0x05 = Just OP_LOADUNIT
byteToOp 0x06 = Just OP_LOADNIL
byteToOp 0x07 = Just OP_MOV
byteToOp 0x10 = Just OP_ADDI
byteToOp 0x11 = Just OP_SUBI
byteToOp 0x12 = Just OP_MULI
byteToOp 0x13 = Just OP_DIVI
byteToOp 0x14 = Just OP_REMI
byteToOp 0x15 = Just OP_NEGI
byteToOp 0x16 = Just OP_ABSI
byteToOp 0x18 = Just OP_ADDF
byteToOp 0x19 = Just OP_SUBF
byteToOp 0x1A = Just OP_MULF
byteToOp 0x1B = Just OP_DIVF
byteToOp 0x1C = Just OP_NEGF
byteToOp 0x1D = Just OP_SQRTF
byteToOp 0x1E = Just OP_ABSF
byteToOp 0x1F = Just OP_ITOF
byteToOp 0x20 = Just OP_EQI
byteToOp 0x21 = Just OP_LTI
byteToOp 0x22 = Just OP_LEI
byteToOp 0x23 = Just OP_GTI
byteToOp 0x24 = Just OP_GEI
byteToOp 0x25 = Just OP_FTOI
byteToOp 0x26 = Just OP_GTF
byteToOp 0x27 = Just OP_GEF
byteToOp 0x28 = Just OP_EQF
byteToOp 0x29 = Just OP_LTF
byteToOp 0x2A = Just OP_EQP
byteToOp 0x2B = Just OP_LEF
byteToOp 0x09 = Just OP_NEQI
byteToOp 0x0A = Just OP_NEQF
byteToOp 0x2C = Just OP_BAND
byteToOp 0x2D = Just OP_BOR
byteToOp 0x2E = Just OP_BXOR
byteToOp 0x2F = Just OP_BSHL
byteToOp 0x30 = Just OP_BSHR
byteToOp 0x38 = Just OP_JMP
byteToOp 0x39 = Just OP_JMPT
byteToOp 0x3A = Just OP_JMPF
byteToOp 0x3B = Just OP_SWITCH
byteToOp 0x3C = Just OP_RET
byteToOp 0x40 = Just OP_CALL
byteToOp 0x41 = Just OP_TAILCALL
byteToOp 0x42 = Just OP_CALLCLS
byteToOp 0x43 = Just OP_TAILCALLCLS
byteToOp 0x44 = Just OP_CALLEXT
byteToOp 0x48 = Just OP_CLOSURE
byteToOp 0x49 = Just OP_GETUPVAL
byteToOp 0x4A = Just OP_PAP
byteToOp 0x50 = Just OP_NEWCON
byteToOp 0x51 = Just OP_GETFIELD
byteToOp 0x52 = Just OP_GETTAG
byteToOp 0x58 = Just OP_NEWARRAY
byteToOp 0x59 = Just OP_ARRGET
byteToOp 0x5A = Just OP_ARRSET
byteToOp 0x5B = Just OP_ARRLEN
byteToOp 0x60 = Just OP_NEWREF
byteToOp 0x61 = Just OP_READREF
byteToOp 0x62 = Just OP_WRITEREF
byteToOp 0x63 = Just OP_NEWMUTARR
byteToOp 0x64 = Just OP_MUTREAD
byteToOp 0x65 = Just OP_MUTWRITE
byteToOp 0x66 = Just OP_MUTLEN
byteToOp 0x68 = Just OP_PUSHHANDLER
byteToOp 0x69 = Just OP_POPHANDLER
byteToOp 0x6A = Just OP_PERFORM
byteToOp 0x70 = Just OP_PRINT
byteToOp 0x71 = Just OP_PRINTLN
byteToOp 0x72 = Just OP_READLINE
byteToOp 0x78 = Just OP_STRCAT
byteToOp 0x79 = Just OP_STRLEN
byteToOp 0x80 = Just OP_DISPATCH
byteToOp 0xF0 = Just OP_DEBUGLOC
byteToOp 0xF1 = Just OP_ERROR
byteToOp 0xFE = Just OP_NOP
byteToOp 0xFF = Just OP_HALT
byteToOp _    = Nothing

-- | High-level instruction representation (for compiler output / disassembler).
data Instruction
    -- Constants
    = ILoadK     !Int !Int        -- dst, constIdx
    | ILoadInt   !Int !Int        -- dst, immediate
    | ILoadTrue  !Int
    | ILoadFalse !Int
    | ILoadUnit  !Int
    | ILoadNil   !Int
    | IMov       !Int !Int        -- dst, src
    -- Int arithmetic
    | IAddI      !Int !Int !Int   -- dst, src1, src2
    | ISubI      !Int !Int !Int
    | IMulI      !Int !Int !Int
    | IDivI      !Int !Int !Int
    | IRemI      !Int !Int !Int
    | INegI      !Int !Int        -- dst, src
    | IAbsI      !Int !Int        -- dst, src
    -- Float arithmetic
    | IAddF      !Int !Int !Int
    | ISubF      !Int !Int !Int
    | IMulF      !Int !Int !Int
    | IDivF      !Int !Int !Int
    | INegF      !Int !Int
    | ISqrtF     !Int !Int        -- dst, src
    | IAbsF      !Int !Int        -- dst, src
    | IItoF      !Int !Int        -- dst, src (int→float64)
    -- Comparison
    | IEqI       !Int !Int !Int
    | INeqI      !Int !Int !Int
    | ILtI       !Int !Int !Int
    | ILeI       !Int !Int !Int
    | IGtI       !Int !Int !Int
    | IGeI       !Int !Int !Int
    | IFtoI      !Int !Int        -- dst, src (float64→int)
    | IGtF       !Int !Int !Int
    | IGeF       !Int !Int !Int
    | IEqF       !Int !Int !Int
    | INeqF      !Int !Int !Int
    | ILtF       !Int !Int !Int
    | IEqP       !Int !Int !Int
    | ILeF       !Int !Int !Int
    -- Bitwise
    | IBand      !Int !Int !Int
    | IBor       !Int !Int !Int
    | IBxor      !Int !Int !Int
    | IBshl      !Int !Int !Int
    | IBshr      !Int !Int !Int
    -- Control flow
    | IJmp       !Int             -- offset (signed)
    | IJmpT      !Int !Int        -- src, offset
    | IJmpF      !Int !Int        -- src, offset
    | ISwitch    !Int !Int        -- src, jumpTableIdx
    | IRet       !Int             -- src
    -- Calls
    | ICall      !Int !Int !Int   -- dst, funcIdx, nargs (args in dst+1..dst+nargs)
    | ITailCall  !Int !Int        -- funcIdx, nargs
    | ICallCls   !Int !Int !Int   -- dst, closureReg, nargs
    | ITailCallCls !Int !Int      -- closureReg, nargs
    | ICallExt   !Int !Int !Int   -- dst, externIdx, nargs
    -- Closures
    | IClosure   !Int !Int !Int   -- dst, funcIdx, nupvals
    | IGetUpval  !Int !Int        -- dst, upvalIdx
    | IPap       !Int !Int !Int   -- dst, funcIdx, nargs
    -- Constructors
    | INewCon    !Int !Int !Int   -- dst, tag, nfields (fields in dst+1..dst+nfields)
    | IGetField  !Int !Int !Int   -- dst, obj, fieldIdx
    | IGetTag    !Int !Int        -- dst, obj
    -- Arrays
    | INewArray  !Int !Int        -- dst, length
    | IArrGet    !Int !Int !Int   -- dst, arr, idx
    | IArrSet    !Int !Int !Int   -- arr, idx, val
    | IArrLen    !Int !Int        -- dst, arr
    -- Refs
    | INewRef    !Int !Int        -- dst, val
    | IReadRef   !Int !Int        -- dst, ref
    | IWriteRef  !Int !Int        -- ref, val
    -- Mutable arrays
    | INewMutArr !Int !Int !Int   -- dst, lenReg, defaultReg
    | IMutRead   !Int !Int !Int   -- dst, arrReg, idxReg
    | IMutWrite  !Int !Int !Int   -- arrReg, idxReg, valReg
    | IMutLen    !Int !Int        -- dst, arrReg
    -- Effects
    | IPushHandler !Int           -- handlerIdx
    | IPopHandler
    | IPerform   !Int !Int !Int   -- dst, opIdx, nargs
    -- IO
    | IPrint     !Int             -- src
    | IPrintLn   !Int             -- src
    | IReadLine  !Int             -- dst
    -- Strings
    | IStrCat    !Int !Int !Int   -- dst, src1, src2
    | IStrLen    !Int !Int        -- dst, src
    -- Dispatch
    | IDispatch  !Int !Int !Int   -- dst, nameIdx, nargs
    -- Debug
    | IDebugLoc  !Int             -- srcIdx
    | IError     !Int             -- msgIdx
    | INop
    | IHalt
    deriving (Show, Eq)

-- | Encode a high-level instruction to a 32-bit word.
encodeInstr :: Instruction -> Word32
encodeInstr instr = case instr of
    ILoadK d k      -> mkB OP_LOADK d k
    ILoadInt d i    -> mkB OP_LOADINT d i
    ILoadTrue d     -> mkB OP_LOADTRUE d 0
    ILoadFalse d    -> mkB OP_LOADFALSE d 0
    ILoadUnit d     -> mkB OP_LOADUNIT d 0
    ILoadNil d      -> mkB OP_LOADNIL d 0
    IMov d s        -> mkA OP_MOV d s 0
    IAddI d a b     -> mkA OP_ADDI d a b
    ISubI d a b     -> mkA OP_SUBI d a b
    IMulI d a b     -> mkA OP_MULI d a b
    IDivI d a b     -> mkA OP_DIVI d a b
    IRemI d a b     -> mkA OP_REMI d a b
    INegI d a       -> mkA OP_NEGI d a 0
    IAbsI d a       -> mkA OP_ABSI d a 0
    IAddF d a b     -> mkA OP_ADDF d a b
    ISubF d a b     -> mkA OP_SUBF d a b
    IMulF d a b     -> mkA OP_MULF d a b
    IDivF d a b     -> mkA OP_DIVF d a b
    INegF d a       -> mkA OP_NEGF d a 0
    ISqrtF d a      -> mkA OP_SQRTF d a 0
    IAbsF d a       -> mkA OP_ABSF d a 0
    IItoF d a       -> mkA OP_ITOF d a 0
    IEqI d a b      -> mkA OP_EQI d a b
    INeqI d a b     -> mkA OP_NEQI d a b
    ILtI d a b      -> mkA OP_LTI d a b
    ILeI d a b      -> mkA OP_LEI d a b
    IGtI d a b      -> mkA OP_GTI d a b
    IGeI d a b      -> mkA OP_GEI d a b
    IFtoI d a       -> mkA OP_FTOI d a 0
    IGtF d a b      -> mkA OP_GTF d a b
    IGeF d a b      -> mkA OP_GEF d a b
    IEqF d a b      -> mkA OP_EQF d a b
    INeqF d a b     -> mkA OP_NEQF d a b
    ILtF d a b      -> mkA OP_LTF d a b
    IEqP d a b      -> mkA OP_EQP d a b
    ILeF d a b      -> mkA OP_LEF d a b
    IBand d a b     -> mkA OP_BAND d a b
    IBor d a b      -> mkA OP_BOR d a b
    IBxor d a b     -> mkA OP_BXOR d a b
    IBshl d a b     -> mkA OP_BSHL d a b
    IBshr d a b     -> mkA OP_BSHR d a b
    IJmp off        -> mkD OP_JMP off
    IJmpT s off     -> mkB OP_JMPT s off
    IJmpF s off     -> mkB OP_JMPF s off
    ISwitch s t     -> mkB OP_SWITCH s t
    IRet s          -> mkB OP_RET s 0
    ICall d f n     -> mkA OP_CALL d f n
    ITailCall f n   -> mkA OP_TAILCALL 0 f n
    ICallCls d c n  -> mkA OP_CALLCLS d c n
    ITailCallCls c n -> mkA OP_TAILCALLCLS 0 c n
    ICallExt d e n  -> mkA OP_CALLEXT d e n
    IClosure d f n  -> mkA OP_CLOSURE d f n
    IGetUpval d i   -> mkB OP_GETUPVAL d i
    IPap d f n      -> mkA OP_PAP d f n
    INewCon d t n   -> mkA OP_NEWCON d t n
    IGetField d o i -> mkA OP_GETFIELD d o i
    IGetTag d o     -> mkA OP_GETTAG d o 0
    INewArray d l   -> mkB OP_NEWARRAY d l
    IArrGet d a i   -> mkA OP_ARRGET d a i
    IArrSet a i v   -> mkA OP_ARRSET a i v
    IArrLen d a     -> mkA OP_ARRLEN d a 0
    INewRef d v     -> mkA OP_NEWREF d v 0
    IReadRef d r    -> mkA OP_READREF d r 0
    IWriteRef r v   -> mkA OP_WRITEREF r v 0
    INewMutArr d l v -> mkA OP_NEWMUTARR d l v
    IMutRead d a i  -> mkA OP_MUTREAD d a i
    IMutWrite a i v -> mkA OP_MUTWRITE a i v
    IMutLen d a     -> mkA OP_MUTLEN d a 0
    IPushHandler h  -> mkD OP_PUSHHANDLER h
    IPopHandler     -> mkD OP_POPHANDLER 0
    IPerform d o n  -> mkA OP_PERFORM d o n
    IPrint s        -> mkB OP_PRINT s 0
    IPrintLn s      -> mkB OP_PRINTLN s 0
    IReadLine d     -> mkB OP_READLINE d 0
    IStrCat d a b   -> mkA OP_STRCAT d a b
    IStrLen d a     -> mkA OP_STRLEN d a 0
    IDispatch d n a -> mkA OP_DISPATCH d n a
    IDebugLoc s     -> mkB OP_DEBUGLOC 0 s
    IError m        -> mkB OP_ERROR 0 m
    INop            -> mkD OP_NOP 0
    IHalt           -> mkD OP_HALT 0

-- Format A: [op:8][a:8][b:8][c:8]
mkA :: OpCode -> Int -> Int -> Int -> Word32
mkA op a b c = (fromIntegral (opToByte op) `shiftL` 24)
    .|. (fromIntegral (a .&. 0xFF) `shiftL` 16)
    .|. (fromIntegral (b .&. 0xFF) `shiftL` 8)
    .|. fromIntegral (c .&. 0xFF)

-- Format B: [op:8][a:8][imm16:16]
mkB :: OpCode -> Int -> Int -> Word32
mkB op a imm = (fromIntegral (opToByte op) `shiftL` 24)
    .|. (fromIntegral (a .&. 0xFF) `shiftL` 16)
    .|. (fromIntegral (imm .&. 0xFFFF))

-- Format D: [op:8][imm24:24]
mkD :: OpCode -> Int -> Word32
mkD op imm = (fromIntegral (opToByte op) `shiftL` 24)
    .|. (fromIntegral (imm .&. 0xFFFFFF))

-- | Decode a 32-bit word into a high-level instruction.
decodeInstr :: Word32 -> Maybe Instruction
decodeInstr w =
    let opByte = fromIntegral (w `shiftR` 24) :: Word8
        a      = fromIntegral ((w `shiftR` 16) .&. 0xFF) :: Int
        b      = fromIntegral ((w `shiftR` 8) .&. 0xFF) :: Int
        c      = fromIntegral (w .&. 0xFF) :: Int
        imm16  = fromIntegral (w .&. 0xFFFF) :: Int
        -- Sign-extend 16-bit immediate
        simm16 = if imm16 >= 0x8000 then imm16 - 0x10000 else imm16
        imm24  = fromIntegral (w .&. 0xFFFFFF) :: Int
        simm24 = if imm24 >= 0x800000 then imm24 - 0x1000000 else imm24
    in case byteToOp opByte of
        Just OP_LOADK       -> Just $ ILoadK a imm16
        Just OP_LOADINT     -> Just $ ILoadInt a simm16
        Just OP_LOADTRUE    -> Just $ ILoadTrue a
        Just OP_LOADFALSE   -> Just $ ILoadFalse a
        Just OP_LOADUNIT    -> Just $ ILoadUnit a
        Just OP_LOADNIL     -> Just $ ILoadNil a
        Just OP_MOV         -> Just $ IMov a b
        Just OP_ADDI        -> Just $ IAddI a b c
        Just OP_SUBI        -> Just $ ISubI a b c
        Just OP_MULI        -> Just $ IMulI a b c
        Just OP_DIVI        -> Just $ IDivI a b c
        Just OP_REMI        -> Just $ IRemI a b c
        Just OP_NEGI        -> Just $ INegI a b
        Just OP_ABSI        -> Just $ IAbsI a b
        Just OP_ADDF        -> Just $ IAddF a b c
        Just OP_SUBF        -> Just $ ISubF a b c
        Just OP_MULF        -> Just $ IMulF a b c
        Just OP_DIVF        -> Just $ IDivF a b c
        Just OP_NEGF        -> Just $ INegF a b
        Just OP_SQRTF       -> Just $ ISqrtF a b
        Just OP_ABSF        -> Just $ IAbsF a b
        Just OP_ITOF        -> Just $ IItoF a b
        Just OP_EQI         -> Just $ IEqI a b c
        Just OP_NEQI        -> Just $ INeqI a b c
        Just OP_LTI         -> Just $ ILtI a b c
        Just OP_LEI         -> Just $ ILeI a b c
        Just OP_GTI         -> Just $ IGtI a b c
        Just OP_GEI         -> Just $ IGeI a b c
        Just OP_FTOI        -> Just $ IFtoI a b
        Just OP_GTF         -> Just $ IGtF a b c
        Just OP_GEF         -> Just $ IGeF a b c
        Just OP_EQF         -> Just $ IEqF a b c
        Just OP_NEQF        -> Just $ INeqF a b c
        Just OP_LTF         -> Just $ ILtF a b c
        Just OP_EQP         -> Just $ IEqP a b c
        Just OP_LEF         -> Just $ ILeF a b c
        Just OP_BAND        -> Just $ IBand a b c
        Just OP_BOR         -> Just $ IBor a b c
        Just OP_BXOR        -> Just $ IBxor a b c
        Just OP_BSHL        -> Just $ IBshl a b c
        Just OP_BSHR        -> Just $ IBshr a b c
        Just OP_JMP         -> Just $ IJmp simm24
        Just OP_JMPT        -> Just $ IJmpT a simm16
        Just OP_JMPF        -> Just $ IJmpF a simm16
        Just OP_SWITCH      -> Just $ ISwitch a imm16
        Just OP_RET         -> Just $ IRet a
        Just OP_CALL        -> Just $ ICall a b c
        Just OP_TAILCALL    -> Just $ ITailCall b c
        Just OP_CALLCLS     -> Just $ ICallCls a b c
        Just OP_TAILCALLCLS -> Just $ ITailCallCls b c
        Just OP_CALLEXT     -> Just $ ICallExt a b c
        Just OP_CLOSURE     -> Just $ IClosure a b c
        Just OP_GETUPVAL    -> Just $ IGetUpval a imm16
        Just OP_PAP         -> Just $ IPap a b c
        Just OP_NEWCON      -> Just $ INewCon a b c
        Just OP_GETFIELD    -> Just $ IGetField a b c
        Just OP_GETTAG      -> Just $ IGetTag a b
        Just OP_NEWARRAY    -> Just $ INewArray a imm16
        Just OP_ARRGET      -> Just $ IArrGet a b c
        Just OP_ARRSET      -> Just $ IArrSet a b c
        Just OP_ARRLEN      -> Just $ IArrLen a b
        Just OP_NEWREF      -> Just $ INewRef a b
        Just OP_READREF     -> Just $ IReadRef a b
        Just OP_WRITEREF    -> Just $ IWriteRef a b
        Just OP_NEWMUTARR  -> Just $ INewMutArr a b c
        Just OP_MUTREAD    -> Just $ IMutRead a b c
        Just OP_MUTWRITE   -> Just $ IMutWrite a b c
        Just OP_MUTLEN     -> Just $ IMutLen a b
        Just OP_PUSHHANDLER -> Just $ IPushHandler imm24
        Just OP_POPHANDLER  -> Just $ IPopHandler
        Just OP_PERFORM     -> Just $ IPerform a b c
        Just OP_PRINT       -> Just $ IPrint a
        Just OP_PRINTLN     -> Just $ IPrintLn a
        Just OP_READLINE    -> Just $ IReadLine a
        Just OP_STRCAT      -> Just $ IStrCat a b c
        Just OP_STRLEN      -> Just $ IStrLen a b
        Just OP_DISPATCH    -> Just $ IDispatch a b c
        Just OP_DEBUGLOC    -> Just $ IDebugLoc imm16
        Just OP_ERROR       -> Just $ IError imm16
        Just OP_NOP         -> Just INop
        Just OP_HALT        -> Just IHalt
        Nothing             -> Nothing

-- | Disassemble a single instruction to a readable string.
disassembleOne :: Int -> Instruction -> String
disassembleOne pc instr = padRight 6 (show pc) ++ "  " ++ case instr of
    ILoadK d k      -> "LOADK     r" ++ show d ++ ", k" ++ show k
    ILoadInt d i    -> "LOADINT   r" ++ show d ++ ", " ++ show i
    ILoadTrue d     -> "LOADTRUE  r" ++ show d
    ILoadFalse d    -> "LOADFALSE r" ++ show d
    ILoadUnit d     -> "LOADUNIT  r" ++ show d
    ILoadNil d      -> "LOADNIL   r" ++ show d
    IMov d s        -> "MOV       r" ++ show d ++ ", r" ++ show s
    IAddI d a b     -> "ADDI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ISubI d a b     -> "SUBI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IMulI d a b     -> "MULI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IDivI d a b     -> "DIVI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IRemI d a b     -> "REMI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    INegI d a       -> "NEGI      r" ++ show d ++ ", r" ++ show a
    IAbsI d a       -> "ABSI      r" ++ show d ++ ", r" ++ show a
    IAddF d a b     -> "ADDF      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ISubF d a b     -> "SUBF      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IMulF d a b     -> "MULF      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IDivF d a b     -> "DIVF      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    INegF d a       -> "NEGF      r" ++ show d ++ ", r" ++ show a
    ISqrtF d a      -> "SQRTF     r" ++ show d ++ ", r" ++ show a
    IAbsF d a       -> "ABSF      r" ++ show d ++ ", r" ++ show a
    IItoF d a       -> "ITOF      r" ++ show d ++ ", r" ++ show a
    IEqI d a b      -> "EQI       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    INeqI d a b     -> "NEQI      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ILtI d a b      -> "LTI       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ILeI d a b      -> "LEI       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IGtI d a b      -> "GTI       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IGeI d a b      -> "GEI       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IFtoI d a       -> "FTOI      r" ++ show d ++ ", r" ++ show a
    IGtF d a b      -> "GTF       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IGeF d a b      -> "GEF       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IEqF d a b      -> "EQF       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    INeqF d a b     -> "NEQF      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ILtF d a b      -> "LTF       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IEqP d a b      -> "EQP       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    ILeF d a b      -> "LEF       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IBand d a b     -> "BAND      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IBor d a b      -> "BOR       r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IBxor d a b     -> "BXOR      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IBshl d a b     -> "BSHL      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IBshr d a b     -> "BSHR      r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IJmp off        -> "JMP       " ++ showOff off
    IJmpT s off     -> "JMPT      r" ++ show s ++ ", " ++ showOff off
    IJmpF s off     -> "JMPF      r" ++ show s ++ ", " ++ showOff off
    ISwitch s t     -> "SWITCH    r" ++ show s ++ ", table" ++ show t
    IRet s          -> "RET       r" ++ show s
    ICall d f n     -> "CALL      r" ++ show d ++ ", f" ++ show f ++ ", " ++ show n
    ITailCall f n   -> "TAILCALL  f" ++ show f ++ ", " ++ show n
    ICallCls d c n  -> "CALLCLS   r" ++ show d ++ ", r" ++ show c ++ ", " ++ show n
    ITailCallCls c n -> "TAILCALLCLS r" ++ show c ++ ", " ++ show n
    ICallExt d e n  -> "CALLEXT   r" ++ show d ++ ", ext" ++ show e ++ ", " ++ show n
    IClosure d f n  -> "CLOSURE   r" ++ show d ++ ", f" ++ show f ++ ", " ++ show n
    IGetUpval d i   -> "GETUPVAL  r" ++ show d ++ ", " ++ show i
    IPap d f n      -> "PAP       r" ++ show d ++ ", f" ++ show f ++ ", " ++ show n
    INewCon d t n   -> "NEWCON    r" ++ show d ++ ", tag" ++ show t ++ ", " ++ show n
    IGetField d o i -> "GETFIELD  r" ++ show d ++ ", r" ++ show o ++ ", " ++ show i
    IGetTag d o     -> "GETTAG    r" ++ show d ++ ", r" ++ show o
    INewArray d l   -> "NEWARRAY  r" ++ show d ++ ", " ++ show l
    IArrGet d a i   -> "ARRGET    r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show i
    IArrSet a i v   -> "ARRSET    r" ++ show a ++ "[r" ++ show i ++ "], r" ++ show v
    IArrLen d a     -> "ARRLEN    r" ++ show d ++ ", r" ++ show a
    INewRef d v     -> "NEWREF    r" ++ show d ++ ", r" ++ show v
    IReadRef d r    -> "READREF   r" ++ show d ++ ", r" ++ show r
    IWriteRef r v   -> "WRITEREF  r" ++ show r ++ ", r" ++ show v
    INewMutArr d l v -> "NEWMUTARR r" ++ show d ++ ", r" ++ show l ++ ", r" ++ show v
    IMutRead d a i  -> "MUTREAD   r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show i
    IMutWrite a i v -> "MUTWRITE  r" ++ show a ++ "[r" ++ show i ++ "], r" ++ show v
    IMutLen d a     -> "MUTLEN    r" ++ show d ++ ", r" ++ show a
    IPushHandler h  -> "PUSHHANDLER " ++ show h
    IPopHandler     -> "POPHANDLER"
    IPerform d o n  -> "PERFORM   r" ++ show d ++ ", op" ++ show o ++ ", " ++ show n
    IPrint s        -> "PRINT     r" ++ show s
    IPrintLn s      -> "PRINTLN   r" ++ show s
    IReadLine d     -> "READLINE  r" ++ show d
    IStrCat d a b   -> "STRCAT    r" ++ show d ++ ", r" ++ show a ++ ", r" ++ show b
    IStrLen d a     -> "STRLEN    r" ++ show d ++ ", r" ++ show a
    IDispatch d n a -> "DISPATCH  r" ++ show d ++ ", name" ++ show n ++ ", " ++ show a
    IDebugLoc s     -> "DEBUGLOC  " ++ show s
    IError m        -> "ERROR     msg" ++ show m
    INop            -> "NOP"
    IHalt           -> "HALT"
  where
    showOff n = if n >= 0 then "+" ++ show n else show n
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- | Disassemble a list of encoded instructions.
disassemble :: [Word32] -> String
disassemble ws = unlines
    [ case decodeInstr w of
        Just instr -> disassembleOne pc instr
        Nothing    -> padRight 6 (show pc) ++ "  ???"
    | (pc, w) <- zip [0..] ws
    ]
  where
    padRight n s = s ++ replicate (max 0 (n - length s)) ' '
