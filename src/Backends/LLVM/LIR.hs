{-# LANGUAGE DeriveGeneric #-}
-- | Low-Level Intermediate Representation (LIR) for the LLVM backend.
--
-- LIR is a flat, first-order, explicitly-typed IR between CLM and LLVM IR.
-- All sub-expressions are named via let bindings (A-normal form).
-- Heap allocation is explicit. Pattern matching compiles to branches.
module Backends.LLVM.LIR
  ( -- * Types
    LType(..)
  , isFloatType
  , isIntType
  , isSignedType
  , isUnsignedType
  , typeSize
    -- * Operands
  , LOperand(..)
  , operandType
    -- * Instructions
  , LInstr(..)
  , instrResultType
    -- * Terminators
  , LTerminator(..)
    -- * Blocks, Functions, Modules
  , LBlock(..)
  , LFunction(..)
  , LGlobal(..)
  , LModule(..)
  ) where

import Surface (Name)

-- ============================================================================
-- Types
-- ============================================================================

-- | Concrete LIR types — no polymorphism.
data LType
  = LTInt64                    -- ^ i64 (tulam Int, default integer)
  | LTInt32                    -- ^ i32
  | LTInt16                    -- ^ i16
  | LTInt8                     -- ^ i8
  | LTWord64                   -- ^ i64 unsigned (UInt64, UInt)
  | LTWord32                   -- ^ i32 unsigned
  | LTWord16                   -- ^ i16 unsigned
  | LTWord8                    -- ^ i8 unsigned (Byte, UInt8)
  | LTFloat64                  -- ^ double
  | LTFloat32                  -- ^ float
  | LTBool                     -- ^ i1
  | LTChar                     -- ^ i32 (Unicode code point)
  | LTPtr                      -- ^ ptr (opaque pointer — heap objects, strings)
  | LTVoid                     -- ^ void (for statements with no return value)
  | LTFunPtr [LType] LType    -- ^ Function pointer type
  deriving (Show, Eq)

isFloatType :: LType -> Bool
isFloatType LTFloat64 = True
isFloatType LTFloat32 = True
isFloatType _ = False

isIntType :: LType -> Bool
isIntType LTFloat64 = False
isIntType LTFloat32 = False
isIntType LTPtr = False
isIntType LTVoid = False
isIntType (LTFunPtr _ _) = False
isIntType _ = True

isSignedType :: LType -> Bool
isSignedType LTInt64 = True
isSignedType LTInt32 = True
isSignedType LTInt16 = True
isSignedType LTInt8  = True
isSignedType _ = False

isUnsignedType :: LType -> Bool
isUnsignedType LTWord64 = True
isUnsignedType LTWord32 = True
isUnsignedType LTWord16 = True
isUnsignedType LTWord8  = True
isUnsignedType _ = False

-- | Size of a type in bytes.
typeSize :: LType -> Int
typeSize LTInt64     = 8
typeSize LTInt32     = 4
typeSize LTInt16     = 2
typeSize LTInt8      = 1
typeSize LTWord64    = 8
typeSize LTWord32    = 4
typeSize LTWord16    = 2
typeSize LTWord8     = 1
typeSize LTFloat64   = 8
typeSize LTFloat32   = 4
typeSize LTBool      = 1
typeSize LTChar      = 4
typeSize LTPtr       = 8
typeSize LTVoid      = 0
typeSize (LTFunPtr _ _) = 8

-- ============================================================================
-- Operands
-- ============================================================================

-- | An operand is either a named variable or an immediate constant.
data LOperand
  = LVar Name LType            -- ^ Named variable with type
  | LLitInt Integer LType      -- ^ Integer constant (with specific int type)
  | LLitFloat Double LType     -- ^ Float constant
  | LLitBool Bool              -- ^ Boolean constant (i1)
  | LLitNull                   -- ^ Null pointer
  | LLitString Name String     -- ^ Reference to a global string constant (@name)
  deriving (Show, Eq)

-- | Get the type of an operand.
operandType :: LOperand -> LType
operandType (LVar _ ty)       = ty
operandType (LLitInt _ ty)    = ty
operandType (LLitFloat _ ty)  = ty
operandType (LLitBool _)      = LTBool
operandType LLitNull          = LTPtr
operandType (LLitString _ _)  = LTPtr

-- ============================================================================
-- Instructions (A-normal form — one operation per instruction)
-- ============================================================================

-- | A single LIR instruction. Each produces one named result.
data LInstr
  -- Arithmetic (integer)
  = LAdd LOperand LOperand
  | LSub LOperand LOperand
  | LMul LOperand LOperand
  | LDiv LOperand LOperand          -- ^ Signed integer division
  | LRem LOperand LOperand          -- ^ Signed integer remainder
  | LUDiv LOperand LOperand         -- ^ Unsigned division
  | LURem LOperand LOperand         -- ^ Unsigned remainder
  | LNeg LOperand                   -- ^ Integer negate (0 - x)

  -- Arithmetic (float)
  | LFAdd LOperand LOperand
  | LFSub LOperand LOperand
  | LFMul LOperand LOperand
  | LFDiv LOperand LOperand
  | LFNeg LOperand

  -- Comparison (integer)
  | LICmpEq LOperand LOperand
  | LICmpNe LOperand LOperand
  | LICmpLt LOperand LOperand       -- ^ Signed <
  | LICmpLe LOperand LOperand       -- ^ Signed <=
  | LICmpGt LOperand LOperand       -- ^ Signed >
  | LICmpGe LOperand LOperand       -- ^ Signed >=
  | LICmpULt LOperand LOperand      -- ^ Unsigned <
  | LICmpULe LOperand LOperand      -- ^ Unsigned <=
  | LICmpUGt LOperand LOperand      -- ^ Unsigned >
  | LICmpUGe LOperand LOperand      -- ^ Unsigned >=

  -- Comparison (float)
  | LFCmpEq LOperand LOperand
  | LFCmpNe LOperand LOperand
  | LFCmpLt LOperand LOperand
  | LFCmpLe LOperand LOperand
  | LFCmpGt LOperand LOperand
  | LFCmpGe LOperand LOperand

  -- Bitwise
  | LAnd LOperand LOperand
  | LOr  LOperand LOperand
  | LXor LOperand LOperand
  | LShl LOperand LOperand
  | LAShr LOperand LOperand         -- ^ Arithmetic shift right
  | LLShr LOperand LOperand         -- ^ Logical shift right

  -- Memory / Heap
  | LAlloc Int Int                   -- ^ alloc(tag, numFields) → ptr
  | LStore LOperand LOperand Int     -- ^ store value into object at field index
  | LLoad LOperand Int LType         -- ^ load field from object at index
  | LGetTag LOperand                 -- ^ extract tag (i16) from heap object header

  -- Function calls
  | LCall Name [LOperand] LType     -- ^ Call named function, return type
  | LCallPtr LOperand [LOperand] LType  -- ^ Call via function pointer

  -- Conversions
  | LSext LOperand LType             -- ^ Sign-extend
  | LZext LOperand LType             -- ^ Zero-extend
  | LTrunc LOperand LType            -- ^ Truncate
  | LSIToFP LOperand LType           -- ^ Signed int to float
  | LFPToSI LOperand LType           -- ^ Float to signed int
  | LFPExt LOperand LType            -- ^ Float extend (f32 → f64)
  | LFPTrunc LOperand LType          -- ^ Float truncate (f64 → f32)
  | LBitcast LOperand LType          -- ^ Bitcast (reinterpret)
  | LIntToPtr LOperand               -- ^ Int to pointer
  | LPtrToInt LOperand LType         -- ^ Pointer to int

  -- Misc
  | LPhi [(LOperand, Name)] LType   -- ^ Phi node: [(value, from-block)]
  | LCopy LOperand                   -- ^ Copy (for trivial let bindings)
  deriving (Show, Eq)

-- | Get the result type of an instruction.
instrResultType :: LInstr -> LType
instrResultType (LAdd a _)      = operandType a
instrResultType (LSub a _)      = operandType a
instrResultType (LMul a _)      = operandType a
instrResultType (LDiv a _)      = operandType a
instrResultType (LRem a _)      = operandType a
instrResultType (LUDiv a _)     = operandType a
instrResultType (LURem a _)     = operandType a
instrResultType (LNeg a)        = operandType a
instrResultType (LFAdd a _)     = operandType a
instrResultType (LFSub a _)     = operandType a
instrResultType (LFMul a _)     = operandType a
instrResultType (LFDiv a _)     = operandType a
instrResultType (LFNeg a)       = operandType a
instrResultType (LICmpEq _ _)   = LTBool
instrResultType (LICmpNe _ _)   = LTBool
instrResultType (LICmpLt _ _)   = LTBool
instrResultType (LICmpLe _ _)   = LTBool
instrResultType (LICmpGt _ _)   = LTBool
instrResultType (LICmpGe _ _)   = LTBool
instrResultType (LICmpULt _ _)  = LTBool
instrResultType (LICmpULe _ _)  = LTBool
instrResultType (LICmpUGt _ _)  = LTBool
instrResultType (LICmpUGe _ _)  = LTBool
instrResultType (LFCmpEq _ _)   = LTBool
instrResultType (LFCmpNe _ _)   = LTBool
instrResultType (LFCmpLt _ _)   = LTBool
instrResultType (LFCmpLe _ _)   = LTBool
instrResultType (LFCmpGt _ _)   = LTBool
instrResultType (LFCmpGe _ _)   = LTBool
instrResultType (LAnd a _)      = operandType a
instrResultType (LOr a _)       = operandType a
instrResultType (LXor a _)      = operandType a
instrResultType (LShl a _)      = operandType a
instrResultType (LAShr a _)     = operandType a
instrResultType (LLShr a _)     = operandType a
instrResultType (LAlloc _ _)    = LTPtr
instrResultType (LStore _ _ _)  = LTVoid
instrResultType (LLoad _ _ ty)  = ty
instrResultType (LGetTag _)     = LTInt16
instrResultType (LCall _ _ ty)  = ty
instrResultType (LCallPtr _ _ ty) = ty
instrResultType (LSext _ ty)    = ty
instrResultType (LZext _ ty)    = ty
instrResultType (LTrunc _ ty)   = ty
instrResultType (LSIToFP _ ty)  = ty
instrResultType (LFPToSI _ ty)  = ty
instrResultType (LFPExt _ ty)   = ty
instrResultType (LFPTrunc _ ty) = ty
instrResultType (LBitcast _ ty) = ty
instrResultType (LIntToPtr _)   = LTPtr
instrResultType (LPtrToInt _ ty)= ty
instrResultType (LPhi _ ty)     = ty
instrResultType (LCopy op)      = operandType op

-- ============================================================================
-- Terminators
-- ============================================================================

-- | Block terminators — control flow at end of block.
data LTerminator
  = LRet LOperand                    -- ^ Return value
  | LRetVoid                         -- ^ Return void
  | LBr Name                        -- ^ Unconditional branch
  | LCondBr LOperand Name Name      -- ^ Conditional branch (cond, true, false)
  | LSwitch LOperand Name [(Integer, Name)]
    -- ^ Switch on integer value (val, default, [(case, block)])
  | LUnreachable                     -- ^ Unreachable (after noreturn calls)
  deriving (Show, Eq)

-- ============================================================================
-- Blocks, Functions, Modules
-- ============================================================================

-- | A basic block: label + instructions + terminator.
data LBlock = LBlock
  { lblockName   :: Name
  , lblockInstrs :: [(Name, LInstr)]    -- ^ [(result_name, instruction)]
  , lblockTerm   :: LTerminator
  } deriving (Show, Eq)

-- | A function definition (or extern declaration).
data LFunction = LFunction
  { lfuncName    :: Name
  , lfuncParams  :: [(Name, LType)]
  , lfuncRetType :: LType
  , lfuncBlocks  :: [LBlock]            -- ^ First block = entry. Empty for externs.
  , lfuncExtern  :: Bool                -- ^ True = declaration only (extern)
  } deriving (Show, Eq)

-- | A global constant.
data LGlobal
  = LGlobalString Name String          -- ^ @name = private constant [N x i8] c"...\00"
  | LGlobalInt Name Integer LType      -- ^ @name = global iN value
  deriving (Show, Eq)

-- | A complete LIR module.
data LModule = LModule
  { lmodName    :: Name
  , lmodGlobals :: [LGlobal]
  , lmodFuncs   :: [LFunction]
  , lmodExterns :: [LFunction]          -- ^ External function declarations
  } deriving (Show, Eq)
