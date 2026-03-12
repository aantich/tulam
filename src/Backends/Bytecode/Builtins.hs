{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- | Bytecode builtin resolution.
--
-- Maps extern function names (from target declarations, e.g. __add_i64)
-- and built-in function names (newRef, mutRead, etc.) to bytecode emission
-- strategies. This is the bytecode backend's equivalent of CLMToLIR's ExternMap.
--
-- Shared infrastructure: uses the SAME extern function names as the LLVM native
-- backend (from lib/Backend/LLVM/Native.tl). The difference is that the native
-- backend maps these to LLVM instructions while we map them to bytecode opcodes.
module Backends.Bytecode.Builtins
    ( BuiltinOp(..)
    , resolveBuiltin
    , allBuiltinNames
    ) where

import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)

import Backends.Bytecode.Instruction

-- | How a builtin function maps to bytecode.
data BuiltinOp
    = BInline1 (Int -> Int -> Instruction)
      -- ^ Unary inline: takes (dst, src) → instruction
    | BInline2 (Int -> Int -> Int -> Instruction)
      -- ^ Binary inline: takes (dst, src1, src2) → instruction
    | BNewRef
      -- ^ newRef(val) → NEWREF dst, val
    | BReadRef
      -- ^ readRef(ref) → READREF dst, ref
    | BWriteRef
      -- ^ writeRef(ref, val) → WRITEREF ref, val
    | BNewMutArr
      -- ^ newMutArray(len, default) → NEWMUTARR dst, len, default
    | BMutRead
      -- ^ mutRead(arr, idx) → MUTREAD dst, arr, idx
    | BMutWrite
      -- ^ mutWrite(arr, idx, val) → MUTWRITE arr, idx, val
    | BPrint
      -- ^ __print_string(s) → PRINT s
    | BPrintLn
      -- ^ putStrLn(s) → PRINTLN s (convenience for handler-less path)
    | BPrintNewline
      -- ^ __print_newline() → special (emit newline)
    | BReadLine
      -- ^ __read_line() / readLine() → READLINE dst
    | BStrCat
      -- ^ __string_concat(a, b) → STRCAT dst, a, b (also used for ++)
    | BStrLen
      -- ^ __string_length(s) → STRLEN dst, s
    | BDispatch
      -- ^ Fall through to VM DISPATCH instruction (for complex builtins
      --   like __show_i64, modifyRef, etc. that need runtime logic)

-- | Resolve a function name to its bytecode emission strategy.
-- Returns Nothing if the function is not a known builtin.
resolveBuiltin :: String -> Maybe BuiltinOp
resolveBuiltin name = Map.lookup name builtinTable

-- | All known builtin function names (for documentation/debugging).
allBuiltinNames :: [String]
allBuiltinNames = Map.keys builtinTable

-- | The builtin table: maps function names to emission strategies.
--
-- This covers:
-- 1. Extern functions from target "native" declarations (__add_i64, etc.)
-- 2. Built-in runtime functions (newRef, mutRead, etc.)
-- 3. Effect handler implementations (__print_string, etc.)
builtinTable :: HashMap String BuiltinOp
builtinTable = Map.fromList $
    -- ====================================================================
    -- Int (i64) arithmetic
    -- ====================================================================
    [ ("__add_i64",  BInline2 IAddI)
    , ("__sub_i64",  BInline2 ISubI)
    , ("__mul_i64",  BInline2 IMulI)
    , ("__div_i64",  BInline2 IDivI)
    , ("__rem_i64",  BInline2 IRemI)
    , ("__neg_i64",  BInline1 INegI)

    -- ====================================================================
    -- Int (i64) comparison
    -- ====================================================================
    , ("__eq_i64",   BInline2 IEqI)
    , ("__neq_i64",  BInline2 INeqI)
    , ("__lt_i64",   BInline2 ILtI)
    , ("__le_i64",   BInline2 ILeI)
    , ("__gt_i64",   BInline2 IGtI)
    , ("__ge_i64",   BInline2 IGeI)

    -- ====================================================================
    -- Int (i64) bitwise
    -- ====================================================================
    , ("__band_i64", BInline2 IBand)
    , ("__bor_i64",  BInline2 IBor)
    , ("__bxor_i64", BInline2 IBxor)
    , ("__shl_i64",  BInline2 IBshl)
    , ("__shr_i64",  BInline2 IBshr)

    -- ====================================================================
    -- Float64 (f64) arithmetic
    -- ====================================================================
    , ("__add_f64",  BInline2 IAddF)
    , ("__sub_f64",  BInline2 ISubF)
    , ("__mul_f64",  BInline2 IMulF)
    , ("__div_f64",  BInline2 IDivF)
    , ("__neg_f64",  BInline1 INegF)

    -- ====================================================================
    -- Float64 (f64) comparison
    -- ====================================================================
    , ("__feq_f64",  BInline2 IEqF)
    , ("__fneq_f64", BInline2 INeqF)
    , ("__flt_f64",  BInline2 ILtF)
    , ("__fle_f64",  BInline2 ILeF)
    , ("__fgt_f64",  BInline2 IGtF)
    , ("__fge_f64",  BInline2 IGeF)

    -- ====================================================================
    -- Float64 (f64) math
    -- ====================================================================
    , ("__sqrt_f64", BInline1 ISqrtF)
    , ("__fabs_f64", BInline1 IAbsF)

    -- ====================================================================
    -- Type conversion
    -- ====================================================================
    , ("__itof",     BInline1 IItoF)
    , ("toFloat",    BInline1 IItoF)   -- user-facing name

    -- ====================================================================
    -- String operations
    -- ====================================================================
    , ("__string_concat", BStrCat)
    , ("__string_length", BStrLen)

    -- ====================================================================
    -- IO / Console
    -- ====================================================================
    , ("__print_string",  BPrint)
    , ("__print_newline", BPrintNewline)
    , ("__read_line",     BReadLine)
    , ("putStrLn",        BPrintLn)    -- direct call fallback
    , ("putStr",          BPrint)      -- direct call fallback

    -- ====================================================================
    -- Mutable references — primitive ops (from Native.tl handler resolution)
    -- ====================================================================
    , ("__newref",    BNewRef)
    , ("__readref",   BReadRef)
    , ("__writeref",  BWriteRef)
    , ("__modifyref", BDispatch)  -- complex: needs closure call, handled by VM DISPATCH

    -- Legacy direct names (fallback if handler resolution didn't run)
    , ("newRef",    BNewRef)
    , ("readRef",   BReadRef)
    , ("writeRef",  BWriteRef)
    , ("modifyRef", BDispatch)

    -- ====================================================================
    -- Mutable arrays — primitive ops (from Native.tl handler resolution)
    -- ====================================================================
    , ("__newmutarray", BNewMutArr)
    , ("__mutread",     BMutRead)
    , ("__mutwrite",    BMutWrite)
    , ("__mutlength",   BDispatch)
    , ("__freeze",      BDispatch)
    , ("__thaw",        BDispatch)
    , ("__mutpush",     BDispatch)

    -- Legacy direct names (fallback if handler resolution didn't run)
    , ("newMutArray", BNewMutArr)
    , ("mutRead",    BMutRead)
    , ("mutWrite",   BMutWrite)

    -- ====================================================================
    -- Show / display (complex: needs runtime type dispatch)
    -- ====================================================================
    , ("__show_i64", BDispatch)
    , ("__show_f64", BDispatch)
    , ("__show_f32", BDispatch)
    , ("__show_char", BDispatch)
    , ("__string_show", BDispatch)

    -- ====================================================================
    -- Bounded / Enum
    -- ====================================================================
    , ("__int_min_i64", BDispatch)
    , ("__int_max_i64", BDispatch)

    -- ====================================================================
    -- Error handling
    -- ====================================================================
    , ("__error", BDispatch)

    -- ====================================================================
    -- Char operations
    -- ====================================================================
    , ("__eq_i32",   BInline2 IEqI)   -- Char is Int internally
    , ("__neq_i32",  BInline2 INeqI)
    , ("__lt_i32",   BInline2 ILtI)
    , ("__le_i32",   BInline2 ILeI)
    , ("__gt_i32",   BInline2 IGtI)
    , ("__ge_i32",   BInline2 IGeI)
    , ("__char_to_int", BInline1 IMov)  -- identity (Char is Int in bytecode)
    , ("__int_to_char", BInline1 IMov)  -- identity

    -- ====================================================================
    -- Float32 arithmetic (mapped to Float64 in bytecode VM for simplicity)
    -- ====================================================================
    , ("__add_f32",  BInline2 IAddF)
    , ("__sub_f32",  BInline2 ISubF)
    , ("__mul_f32",  BInline2 IMulF)
    , ("__div_f32",  BInline2 IDivF)
    , ("__neg_f32",  BInline1 INegF)
    , ("__feq_f32",  BInline2 IEqF)
    , ("__fneq_f32", BInline2 INeqF)
    , ("__flt_f32",  BInline2 ILtF)
    , ("__fle_f32",  BInline2 ILeF)
    , ("__fgt_f32",  BInline2 IGtF)
    , ("__fge_f32",  BInline2 IGeF)
    , ("__sqrt_f32", BInline1 ISqrtF)
    , ("__fabs_f32", BInline1 IAbsF)
    ]
