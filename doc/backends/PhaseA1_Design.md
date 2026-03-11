# Phase A.1: LLVM Backend Foundation — Detailed Implementation Design

## Table of Contents

1. [Scope & Goals](#1-scope--goals)
2. [Architecture Overview](#2-architecture-overview)
3. [LIR: Low-Level Intermediate Representation](#3-lir-low-level-intermediate-representation)
4. [CLM to LIR Lowering](#4-clm-to-lir-lowering)
5. [LIR to LLVM IR Emission](#5-lir-to-llvm-ir-emission)
6. [Runtime Library (C++)](#6-runtime-library-c)
7. [Memory Representation](#7-memory-representation)
8. [Build & Link Pipeline](#8-build--link-pipeline)
9. [Test Strategy](#9-test-strategy)
10. [File Plan](#10-file-plan)
11. [Implementation Order](#11-implementation-order)
12. [What's In / What's Out](#12-whats-in--whats-out)
13. [Detailed Module Specifications](#13-detailed-module-specifications)

---

## 1. Scope & Goals

### 1.1 What Phase A.1 Delivers

A working end-to-end pipeline that compiles a **subset of tulam** to native executables via LLVM:

```
tulam source → Parser → Pipeline (Passes 0-4.5) → CLM
  → NEW: CLM → LIR → LLVM IR text → llc → .o → clang++ link (with RTS) → executable
```

### 1.2 Supported CLM Subset

| CLM Node | Supported | Notes |
|----------|-----------|-------|
| `CLMLIT` (LInt, LFloat64, LString, LChar, LBool-like) | Yes | Core literals |
| `CLMID` | Yes | Variable references |
| `CLMAPP` | Yes | Direct function application |
| `CLMCON` | Yes | Constructor (tagged tuple) allocation |
| `CLMCASE` | Yes | Pattern matching (tag checks + literal checks) |
| `CLMLAM` / `CLMLamCases` | Yes | Lambda values, closure allocation |
| `CLMFieldAccess` | Yes | Field access by index |
| `CLMPROG` | Yes | Sequential statements |
| `CLMBIND` | Yes | Let bindings |
| `CLMERR` | Yes | Runtime error (prints message, exits) |
| `CLMLIT` (other numeric) | Yes | Int8-64, UInt8-64, Float32, Byte |
| `CLMARRAY` | Partial | Fixed-size array literals only |
| `CLMPRIMCALL` | Partial | Arithmetic/comparison intrinsics only |
| `CLMEMPTY` | Yes | Erased type info (compiles to nothing) |
| `CLMU` | Yes | Erased (compiles to nothing) |
| `CLMIAP` | **No** | Needs monomorphization (Phase C) |
| `CLMTYPED` | **No** | Needs type-directed dispatch |
| `CLMPAP` | **No** | Needs closure representation (Phase F) |
| `CLMHANDLE` | **No** | Needs effect compilation (Phase G) |
| `CLMMCALL`/`CLMSCALL`/`CLMNEW` | **No** | Needs class compilation |
| `CLMREF`/`CLMMUTARRAY` | **No** | Needs mutable state runtime |

### 1.3 Success Criteria

1. `fibonacci(35)` compiles and produces correct result as native executable
2. `factorial(20)` compiles and produces correct result
3. Simple pattern matching (Maybe, List, Bool) compiles correctly
4. Basic constructor allocation + field access works
5. String literals compile (as C string pointers for now)
6. `main` function entry point convention established
7. All tests pass via `stack test` (existing interpreter tests unchanged)
8. At least 3 new integration tests for native compilation

### 1.4 Non-Goals for A.1

- No optimization passes (no monomorphization, no unboxing, no inlining)
- No garbage collection (Boehm GC as stopgap, or leak-and-exit for short programs)
- No polymorphic dispatch (CLMIAP not supported — programs must be monomorphic)
- No closures with captured variables (lambdas must be top-level or non-capturing)
- No IO (printf for output only, via runtime helper)
- No tail call optimization
- No SIMD

---

## 2. Architecture Overview

### 2.1 Pipeline Extension

```
                    EXISTING                           NEW (Phase A.1)
┌─────────┐   ┌──────────┐   ┌─────────┐   ┌──────────┐   ┌───────────┐   ┌──────────┐
│ Source   │ → │ Parser   │ → │ Passes  │ → │  CLM     │ → │  CLMToLIR │ → │ LIRToLLVM│
│ (.tl)    │   │ (Parse.hs│   │ (0-4.5) │   │ (CLM.hs) │   │  (.hs)    │   │ (.hs)    │
└─────────┘   └──────────┘   └─────────┘   └──────────┘   └──────────┘   └──────────┘
                                                                              │
                                                                              ▼
                                                                        ┌───────────┐
                                                                        │ .ll file  │
                                                                        │ (LLVM IR) │
                                                                        └─────┬─────┘
                                                                              │ llc
                                                                              ▼
                                                                        ┌───────────┐
                                                                        │ .o file   │
                                                                        └─────┬─────┘
                                                                              │ clang
                                                                              ▼ (link with runtime.c)
                                                                        ┌───────────┐
                                                                        │ executable│
                                                                        └───────────┘
```

### 2.2 New Haskell Modules

All LLVM backend code lives under `src/Backends/LLVM/`. Future backends (.NET, JS, etc.) will have their own sibling directories.

| Module | Role | Depends On |
|--------|------|------------|
| `src/Backends/LLVM/LIR.hs` | LIR data types | Surface.hs (for ConsTag, Literal, Name) |
| `src/Backends/LLVM/CLMToLIR.hs` | CLM → LIR lowering | CLM.hs, LIR.hs, State.hs |
| `src/Backends/LLVM/LIRToLLVM.hs` | LIR → LLVM IR text emission | LIR.hs |
| `src/Backends/LLVM/NativeCompile.hs` | Orchestration: pipeline, file I/O, exec llc/clang++ | All above |

```
src/
├── Backends/
│   └── LLVM/
│       ├── LIR.hs           -- Low-level IR types
│       ├── CLMToLIR.hs      -- CLM → LIR lowering
│       ├── LIRToLLVM.hs     -- LIR → LLVM IR text emission
│       └── NativeCompile.hs -- Build orchestration
│   (future:)
│   ├── DotNet/              -- .NET/C# backend
│   ├── JS/                  -- JavaScript backend
│   └── CPP/                 -- Direct C++ emission (if ever needed alongside LLVM)
```

### 2.3 New Non-Haskell Files: C++ Runtime (RTS)

The runtime is written in **C++** rather than C. This is a deliberate choice driven by tulam's class system design:

**Why C++ for the RTS:**

1. **tulam classes map 1:1 to target classes** (per `doc/ClassDesign.md`). On the native target, that means C++ classes. A C++ RTS gives us vtable-compatible object layouts, so tulam classes can directly subclass extern C++ classes (and vice versa).
2. **C interop is free** — `extern "C"` blocks expose all allocation/RC/intrinsic functions with C calling conventions. LLVM IR calls these with zero overhead. We get both C and C++ interop.
3. **Exception infrastructure** — tulam's `error`/`panic` can map to C++ exceptions, enabling proper stack unwinding and RAII-based cleanup (important for Perceus RC drop operations in later phases).
4. **Standard library access** — `std::string`, `std::vector`, `std::unordered_map` available in the RTS for internal bookkeeping without reinventing them.
5. **Future extern class support** — when tulam code does `import SomeLib target native`, the extern class metadata resolution can directly `#include` C++ headers and use `typeid`/RTTI.

**What stays `extern "C"`** (called from LLVM IR):

All functions called from generated LLVM IR use `extern "C"` linkage — simple, stable ABI, no name mangling. The C++ features are used *internally* within the RTS, not at the LLVM IR boundary.

| File | Role |
|------|------|
| `runtime/LLVM/tlm_runtime.hpp` | Runtime header (types, `extern "C"` declarations) |
| `runtime/LLVM/tlm_runtime.cpp` | Runtime implementation (allocation, RC, printing, error) |
| `runtime/LLVM/tlm_object.hpp` | Object layout, header struct, class infrastructure |
| `runtime/CMakeLists.txt` | Build config (or just a simple Makefile for now) |

---

## 3. LIR: Low-Level Intermediate Representation

### 3.1 Design Principles

LIR is a **flat, first-order, explicitly-typed** IR between CLM and LLVM:

- **Flat**: No nested expressions — all sub-expressions are named via `let` bindings (A-normal form)
- **First-order**: No higher-order values in the core — closures are explicit heap objects
- **Explicit types**: Every binding has a concrete LIR type (needed for LLVM IR emission)
- **Explicit allocation**: Constructor creation = explicit `alloc` + field stores
- **Explicit control flow**: Pattern matching = explicit branches (if/switch)

### 3.2 LIR Types

```haskell
module Backends.LLVM.LIR where

import Surface (Name, ConsTag, Literal, SourceInfo)

-- ============================================================================
-- Types
-- ============================================================================

-- LIR types — concrete, no polymorphism
data LType
  = LTInt64                    -- i64 (tulam Int, default integer)
  | LTInt32                    -- i32
  | LTInt16                    -- i16
  | LTInt8                     -- i8
  | LTWord64                   -- i64 unsigned (UInt64, UInt)
  | LTWord32                   -- i32 unsigned
  | LTWord16                   -- i16 unsigned
  | LTWord8                    -- i8 unsigned (Byte, UInt8)
  | LTFloat64                  -- double
  | LTFloat32                  -- float
  | LTBool                     -- i1
  | LTChar                     -- i32 (Unicode code point)
  | LTPtr                      -- ptr (opaque pointer — heap objects, strings)
  | LTVoid                     -- void (for statements with no return value)
  | LTFunPtr [LType] LType    -- function pointer type
  deriving (Show, Eq)

-- ============================================================================
-- Operands
-- ============================================================================

-- An operand is either a named variable or an immediate constant
data LOperand
  = LVar Name LType            -- Named variable with type
  | LLitInt Integer LType      -- Integer constant (with specific int type)
  | LLitFloat Double LType     -- Float constant
  | LLitBool Bool              -- Boolean constant (i1)
  | LLitNull                   -- Null pointer
  | LLitString String          -- String constant (emitted as global)
  deriving (Show, Eq)

-- ============================================================================
-- Instructions (A-normal form — one operation per instruction)
-- ============================================================================

data LInstr
  -- Arithmetic
  = LAdd LOperand LOperand          -- integer add
  | LSub LOperand LOperand          -- integer sub
  | LMul LOperand LOperand          -- integer mul
  | LDiv LOperand LOperand          -- signed integer div
  | LRem LOperand LOperand          -- signed integer rem
  | LUDiv LOperand LOperand         -- unsigned div
  | LURem LOperand LOperand         -- unsigned rem
  | LNeg LOperand                   -- integer negate
  | LFAdd LOperand LOperand         -- float add
  | LFSub LOperand LOperand         -- float sub
  | LFMul LOperand LOperand         -- float mul
  | LFDiv LOperand LOperand         -- float div
  | LFNeg LOperand                  -- float negate

  -- Comparison
  | LICmpEq LOperand LOperand       -- integer ==
  | LICmpNe LOperand LOperand       -- integer !=
  | LICmpLt LOperand LOperand       -- signed <
  | LICmpLe LOperand LOperand       -- signed <=
  | LICmpGt LOperand LOperand       -- signed >
  | LICmpGe LOperand LOperand       -- signed >=
  | LICmpULt LOperand LOperand      -- unsigned <
  | LICmpULe LOperand LOperand      -- unsigned <=
  | LICmpUGt LOperand LOperand      -- unsigned >
  | LICmpUGe LOperand LOperand      -- unsigned >=
  | LFCmpEq LOperand LOperand       -- float ==
  | LFCmpLt LOperand LOperand       -- float <
  | LFCmpLe LOperand LOperand       -- float <=
  | LFCmpGt LOperand LOperand       -- float >
  | LFCmpGe LOperand LOperand       -- float >=

  -- Bitwise
  | LAnd LOperand LOperand          -- bitwise and
  | LOr LOperand LOperand           -- bitwise or
  | LXor LOperand LOperand          -- bitwise xor
  | LShl LOperand LOperand          -- shift left
  | LAShr LOperand LOperand         -- arithmetic shift right
  | LLShr LOperand LOperand         -- logical shift right

  -- Memory / Heap
  | LAlloc Int Int                   -- alloc(tag, numFields) → ptr to heap object
  | LStore LOperand LOperand Int     -- store value at object field index
  | LLoad LOperand Int LType         -- load field from object at index, with expected type
  | LGetTag LOperand                 -- extract tag (i16) from heap object header

  -- Function calls
  | LCall Name [LOperand] LType     -- call named function with args, return type
  | LCallPtr LOperand [LOperand] LType  -- call function pointer

  -- Conversions
  | LSext LOperand LType             -- sign-extend
  | LZext LOperand LType             -- zero-extend
  | LTrunc LOperand LType            -- truncate
  | LSIToFP LOperand LType           -- signed int to float
  | LFPToSI LOperand LType           -- float to signed int
  | LFPExt LOperand LType            -- float extend (f32 → f64)
  | LFPTrunc LOperand LType          -- float truncate (f64 → f32)
  | LBitcast LOperand LType          -- bitcast (ptr to ptr, int to int of same size)
  | LIntToPtr LOperand               -- int to ptr
  | LPtrToInt LOperand LType         -- ptr to int

  -- Misc
  | LPhi [(LOperand, Name)]         -- phi node (value, from-block label)
  | LCopy LOperand                   -- just copy a value (for let-binding trivial operands)
  deriving (Show, Eq)

-- ============================================================================
-- Terminators (end-of-block control flow)
-- ============================================================================

data LTerminator
  = LRet LOperand                    -- return value
  | LRetVoid                         -- return void
  | LBr Name                         -- unconditional branch to block
  | LCondBr LOperand Name Name       -- conditional branch (cond, trueBlock, falseBlock)
  | LSwitch LOperand Name [(Integer, Name)]  -- switch(val, default, [(tag, block)])
  | LUnreachable                     -- unreachable (after error calls)
  deriving (Show, Eq)

-- ============================================================================
-- Blocks, Functions, Modules
-- ============================================================================

-- A basic block: label + instructions + terminator
data LBlock = LBlock
  { lblockName  :: Name
  , lblockInstrs :: [(Name, LInstr)]    -- [(result_name, instruction)]
  , lblockTerm  :: LTerminator
  } deriving (Show, Eq)

-- A function definition
data LFunction = LFunction
  { lfuncName    :: Name
  , lfuncParams  :: [(Name, LType)]
  , lfuncRetType :: LType
  , lfuncBlocks  :: [LBlock]            -- First block = entry
  , lfuncExtern  :: Bool                -- True = declaration only (extern)
  } deriving (Show, Eq)

-- A global string constant
data LGlobal
  = LGlobalString Name String          -- @name = private unnamed_addr constant [N x i8] c"..."
  | LGlobalInt Name Integer LType      -- @name = global iN value
  deriving (Show, Eq)

-- A complete LIR module (one per tulam module / compilation unit)
data LModule = LModule
  { lmodName    :: Name
  , lmodGlobals :: [LGlobal]
  , lmodFuncs   :: [LFunction]
  , lmodExterns :: [LFunction]          -- External function declarations
  } deriving (Show, Eq)
```

### 3.3 Design Decisions

**Why A-normal form?** Every sub-expression gets a name. This makes LLVM IR emission trivial — each LIR instruction maps to exactly one LLVM IR instruction. No expression tree flattening needed at the LLVM level.

**Why explicit heap allocation?** `LAlloc tag numFields` maps directly to a runtime call that allocates `header + numFields * 8` bytes. Field stores are separate instructions. This makes the allocation strategy visible and replaceable (Boehm GC now → Perceus RC later).

**Why LTPtr for all heap objects?** In Phase A.1, all constructed values (constructors, closures) are heap-allocated and represented as opaque pointers. Type-specific optimizations (unboxing, stack allocation) come in later phases. The `LType` on loads tells LLVM what type to expect when reading a field.

**Why no LTStruct?** We don't need named struct types in Phase A.1. Heap objects use a uniform layout (header + fields array). Structs would add complexity without enabling any optimization we need yet.

---

## 4. CLM to LIR Lowering

### 4.1 Lowering Context

```haskell
module Backends.LLVM.CLMToLIR where

-- State maintained during lowering
data LowerState = LowerState
  { lsNextVar    :: !Int               -- Fresh variable counter
  , lsNextBlock  :: !Int               -- Fresh block counter
  , lsBlocks     :: [LBlock]           -- Accumulated blocks (current function)
  , lsCurrentInstrs :: [(Name, LInstr)] -- Instructions in current block
  , lsCurrentBlock :: Name             -- Current block label
  , lsGlobals    :: [LGlobal]          -- Accumulated globals (string constants)
  , lsStringMap  :: HashMap String Name -- Dedup string constants
  , lsEnv        :: HashMap Name LOperand  -- Variable bindings (name → operand)
  , lsFuncMap    :: HashMap Name (Name, [LType], LType)  -- CLM func name → (llvm name, param types, ret type)
  }
```

### 4.2 Lowering Rules

Each CLM node lowers to a sequence of LIR instructions that produce a result operand:

**`CLMLIT`** — Immediate constants:

```
CLMLIT (LInt n)     → LLitInt n LTInt64
CLMLIT (LFloat64 d) → LLitFloat d LTFloat64
CLMLIT (LString s)  → emit global string, return LVar @str_N LTPtr
CLMLIT (LChar c)    → LLitInt (fromEnum c) LTChar
CLMLIT (LBool True) → LLitBool True        -- note: booleans are compiled
CLMLIT (LBool False)→ LLitBool False        --       as i1, not constructors
```

**`CLMID`** — Variable lookup:

```
CLMID name → look up name in lsEnv → return the stored LOperand
             if not found in local env → assume it's a global function name
```

**`CLMAPP func args`** — Direct function application:

```
1. Lower func → get function name (must be CLMID for Phase A.1)
2. Lower each arg → get operands
3. Look up function signature in lsFuncMap
4. Emit: result = LCall funcName [arg_operands] retType
5. Return: LVar result retType
```

**`CLMCON (ConsTag name tag) fields`** — Constructor allocation:

```
1. Lower each field → get operands
2. Emit: obj = LAlloc tag (length fields)
3. For each field[i]:
   Emit: LStore field_operand obj i
4. Return: LVar obj LTPtr
```

**`CLMCASE checks body`** — Pattern matching:

A `CLMCASE` has a list of pattern checks and a body. In CLMLamCases, multiple CLMCASE nodes form alternatives. Lowering depends on context:

```
-- Single CLMCASE (in a CLMLamCases context):
-- Generated as a basic block that checks all guards, falls through on success

-- For a constructor check:
CLMCheckTag (ConsTag _ tag) scrutinee:
  1. Lower scrutinee → get operand
  2. Emit: tagVal = LGetTag operand
  3. Emit: match = LICmpEq tagVal (LLitInt tag LTInt64)
  4. Return: match as condition for branch

-- For a literal check:
CLMCheckLit (LInt n) scrutinee:
  1. Lower scrutinee → get operand
  2. Emit: match = LICmpEq operand (LLitInt n LTInt64)
  3. Return: match as condition for branch
```

**`CLMFieldAccess (name, index) obj`** — Field extraction:

```
1. Lower obj → get operand (must be LTPtr to heap object)
2. Emit: val = LLoad operand index LTPtr
   (Phase A.1: all fields are boxed, so type is LTPtr;
    for known-Int fields, emit additional unboxing load)
3. Return: LVar val LTPtr
```

**`CLMPROG [stmts]`** — Sequential statements:

```
1. Lower each stmt in order
2. Return the result of the last stmt
```

**`CLMBIND name expr`** — Let binding:

```
1. Lower expr → get operand
2. Store name → operand in lsEnv
3. Return CLMEMPTY / void
```

**`CLMERR msg srcInfo`** — Runtime error:

```
1. Emit global string for msg
2. Emit: LCall "tlm_error" [LVar @msg_str LTPtr] LTVoid
3. Emit: LUnreachable
```

**`CLMEMPTY` / `CLMU`** — Erased:

```
Return LLitNull (these are type-level constructs, erased at runtime)
```

### 4.3 Function Lowering

Each `CLMLam` in `clmLambdas` becomes an `LFunction`:

```haskell
lowerFunction :: Name -> CLMLam -> LowerM LFunction

-- CLMLam [params] body:
--   1. Create entry block
--   2. Bind params in lsEnv
--   3. Lower body to get result operand
--   4. Emit LRet result

-- CLMLamCases [params] [cases]:
--   1. Create entry block
--   2. Bind params in lsEnv
--   3. For each CLMCASE in cases:
--      a. Create a new block for this case
--      b. Lower pattern checks → branch conditions
--      c. Lower body → result
--      d. Branch to next case on check failure
--   4. Final fallthrough → LCall "tlm_error" "pattern match failure"
```

### 4.4 Pattern Match Compilation (Simplified)

Phase A.1 uses a **linear chain** of case checks (not optimized decision trees — that's Phase B):

```
entry:
  %tag = getTag(%scrutinee)
  %check0 = icmp eq %tag, 0          ; first constructor
  br %check0, case0_body, case1_check

case1_check:
  %check1 = icmp eq %tag, 1          ; second constructor
  br %check1, case1_body, case2_check

case2_check:
  ...                                  ; continue chain

caseN_fail:
  call @tlm_error("pattern match failure")
  unreachable

case0_body:
  ; extract fields, evaluate body
  %field0 = load ptr, ptr getelementptr(%scrutinee, 0, field_offset_0)
  ...
  ret %result

case1_body:
  ...
```

This is O(n) in the number of constructors but correct. Phase B will optimize to O(log n) decision trees or O(1) switch tables.

### 4.5 Handling Intrinsics (CLMPRIMCALL)

In Phase A.1, intrinsic calls are detected during lowering by function name. When `CLMAPP (CLMID funcName) args` is lowered and `funcName` is a known intrinsic, instead of emitting a `LCall`, we emit the corresponding LIR instruction:

```haskell
-- Mapping from intrinsic names to LIR instructions
intrinsicToLIR :: Name -> Maybe ([LOperand] -> LInstr)
intrinsicToLIR "add_Int"     = Just (\[a,b] -> LAdd a b)
intrinsicToLIR "sub_Int"     = Just (\[a,b] -> LSub a b)
intrinsicToLIR "mul_Int"     = Just (\[a,b] -> LMul a b)
intrinsicToLIR "div_Int"     = Just (\[a,b] -> LDiv a b)
intrinsicToLIR "eq_Int"      = Just (\[a,b] -> LICmpEq a b)
intrinsicToLIR "lt_Int"      = Just (\[a,b] -> LICmpLt a b)
intrinsicToLIR "negate_Int"  = Just (\[a] -> LNeg a)
intrinsicToLIR "add_Float64" = Just (\[a,b] -> LFAdd a b)
-- ... etc for all arithmetic/comparison intrinsics
intrinsicToLIR _ = Nothing
```

**Important**: This requires that the CLM has been monomorphized for these specific types. Since Phase A.1 doesn't support CLMIAP, all calls to `+`, `-`, etc. must have been resolved to type-specific names during earlier passes. This means Phase A.1 test programs must use explicitly-typed functions, or we add a mini pre-pass that resolves monomorphic CLMIAP calls.

**Alternative (simpler for A.1)**: Compile intrinsics as calls to C++ runtime functions:

```cpp
// In runtime/LLVM/tlm_runtime.cpp (with extern "C" linkage):
extern "C" int64_t tlm_add_Int(int64_t a, int64_t b) { return a + b; }
extern "C" int64_t tlm_sub_Int(int64_t a, int64_t b) { return a - b; }
// ... LLVM will inline these with -O2
```

This is simpler because the CLM already has intrinsic dispatch resolved — `CLMPRIMCALL` bodies just need to map to the right runtime function. LLVM's inliner will eliminate the call overhead.

---

## 5. LIR to LLVM IR Emission

### 5.1 Strategy: Text-Based Emission

Phase A.1 emits LLVM IR as **text** (`.ll` files), not via the LLVM C API or Haskell bindings. Reasons:

1. **No extra dependencies** — no `llvm-hs` package (which has version-pinning issues)
2. **Human-readable output** — easy to inspect and debug
3. **Stable format** — LLVM IR text syntax is more stable than the C API across versions
4. **Sufficient for correctness** — LLVM's parser handles all the complexity

Future phases may switch to the LLVM C API for build speed, but text is the right choice for bootstrapping.

### 5.2 Emission Functions

```haskell
module Backends.LLVM.LIRToLLVM where

-- Top-level: emit a complete .ll file
emitModule :: LModule -> Text

-- Emit structure:
-- 1. Target triple and data layout
-- 2. Runtime function declarations (extern)
-- 3. Global string constants
-- 4. Function definitions

emitFunction :: LFunction -> Text
-- define <retType> @<name>(<params>) {
-- <blocks>
-- }

emitBlock :: LBlock -> Text
-- <label>:
--   <instr1>
--   <instr2>
--   ...
--   <terminator>

emitInstr :: Name -> LInstr -> Text
-- %<name> = <instruction>

emitTerminator :: LTerminator -> Text
-- ret/br/switch/unreachable

emitType :: LType -> Text
-- i64, double, ptr, void, etc.

emitOperand :: LOperand -> Text
-- %name, 42, 3.14, true, null, @global_str
```

### 5.3 LLVM IR Template

The emitted `.ll` file looks like:

```llvm
; Generated by tulam compiler (Phase A.1)
target triple = "arm64-apple-darwin"    ; or "x86_64-unknown-linux-gnu"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"

; --- Runtime function declarations ---
declare ptr @tlm_alloc(i32, i32)        ; alloc(tag, numFields) → ptr
declare void @tlm_error(ptr)            ; error(message) → noreturn
declare void @tlm_print_int(i64)        ; print integer
declare void @tlm_print_string(ptr)     ; print string
declare i16 @tlm_get_tag(ptr)           ; get constructor tag from header
declare ptr @tlm_get_field(ptr, i32)    ; get field[i] as ptr
declare void @tlm_set_field(ptr, i32, ptr) ; set field[i]

; For Boehm GC:
declare ptr @GC_malloc(i64)

; --- String constants ---
@str_0 = private unnamed_addr constant [6 x i8] c"hello\00"
@str_1 = private unnamed_addr constant [22 x i8] c"pattern match failure\00"

; --- User functions ---
define i64 @tulam_fibonacci(i64 %n) {
entry:
  %cmp0 = icmp sle i64 %n, 1
  br i1 %cmp0, label %base, label %recurse

base:
  ret i64 %n

recurse:
  %n1 = sub i64 %n, 1
  %n2 = sub i64 %n, 2
  %fib1 = call i64 @tulam_fibonacci(i64 %n1)
  %fib2 = call i64 @tulam_fibonacci(i64 %n2)
  %result = add i64 %fib1, %fib2
  ret i64 %result
}

; --- Entry point ---
define i32 @main() {
entry:
  %result = call i64 @tulam_fibonacci(i64 35)
  call void @tlm_print_int(i64 %result)
  ret i32 0
}
```

### 5.4 Target Triple Detection

```haskell
-- Detect host platform for target triple
detectTargetTriple :: IO String
detectTargetTriple = do
  -- Run: llvm-config --host-target
  -- Or fall back to: uname -m + uname -s based detection
  -- macOS ARM: "arm64-apple-darwin"
  -- macOS x86: "x86_64-apple-darwin"
  -- Linux x86: "x86_64-unknown-linux-gnu"
  -- Linux ARM: "aarch64-unknown-linux-gnu"
```

---

## 6. Runtime Library (C++)

### 6.1 Why C++ Instead of C

tulam's class system maps 1:1 to target platform classes (see `doc/ClassDesign.md`). On the native/LLVM target, that target is C++. Writing the RTS in C++ provides:

1. **Vtable-compatible object layout** — tulam classes can directly extend extern C++ classes with matching vtable structure. `CLMMCALL` dispatches via the same vtable mechanism as C++ virtual method calls.
2. **C interop for free** — all functions callable from LLVM IR are declared `extern "C"` (no name mangling, C ABI). The C++ features are used *internally* within the RTS.
3. **Exception infrastructure** — `error`/`panic` map to C++ exceptions, enabling proper stack unwinding. Critical for Perceus RC drop operations in later phases.
4. **Standard library in the RTS** — `std::string`, `std::unordered_map`, `std::vector` available for internal bookkeeping without reinventing.
5. **RTTI for class hierarchy** — `typeid`/`dynamic_cast` available for extern class metadata resolution and safe downcasts.
6. **Namespace isolation** — RTS internals live in `namespace tlm`, preventing symbol collisions.

### 6.2 Object Header and Types

```cpp
// runtime/LLVM/tlm_object.hpp
#pragma once
#include <cstdint>
#include <cstddef>

namespace tlm {

struct Header {
    uint16_t tag;
    uint8_t  arity;
    uint8_t  flags;       // reserved for future use
    uint32_t rc;          // bit 31 = 0 always in Phase A.1

    Header(uint16_t tag, uint8_t arity)
        : tag(tag), arity(arity), flags(0), rc(1) {}
};

static_assert(sizeof(Header) == 8, "Header must be 8 bytes");

struct Object {
    Header header;
    uint64_t fields[];    // flexible array member

    uint16_t getTag() const { return header.tag; }
    uint8_t  getArity() const { return header.arity; }
    uint64_t getField(int index) const { return fields[index]; }
    void     setField(int index, uint64_t value) { fields[index] = value; }

    Object*  getFieldPtr(int index) const {
        return reinterpret_cast<Object*>(static_cast<uintptr_t>(fields[index]));
    }
    void setFieldPtr(int index, Object* ptr) {
        fields[index] = static_cast<uint64_t>(reinterpret_cast<uintptr_t>(ptr));
    }
};

constexpr size_t objectSize(int numFields) {
    return sizeof(Header) + numFields * sizeof(uint64_t);
}

} // namespace tlm
```

### 6.3 Runtime API (extern "C")

```cpp
// runtime/LLVM/tlm_runtime.hpp
#pragma once
#include "tlm_object.hpp"

// All functions callable from LLVM IR use extern "C" linkage.
// No name mangling, standard C calling convention, zero overhead.

extern "C" {

tlm::Object* tlm_alloc(int tag, int num_fields);
uint64_t tlm_get_field_raw(tlm::Object* obj, int index);
void     tlm_set_field_raw(tlm::Object* obj, int index, uint64_t value);
uint16_t tlm_get_tag(tlm::Object* obj);

void tlm_print_int(int64_t val);
void tlm_print_float(double val);
void tlm_print_string(const char* s);
void tlm_print_newline();
void tlm_print_bool(int val);

[[noreturn]] void tlm_error(const char* msg);

void tlm_rc_inc(tlm::Object* obj);
void tlm_rc_dec(tlm::Object* obj);

} // extern "C"
```

### 6.4 Runtime Implementation

```cpp
// runtime/LLVM/tlm_runtime.cpp
#include "tlm_runtime.hpp"
#include <cstdio>
#include <cstdlib>
#include <cstring>

#ifdef USE_BOEHM_GC
  #include <gc.h>
  static void* tlm_malloc(size_t size) { return GC_malloc(size); }
#else
  static void* tlm_malloc(size_t size) { return std::malloc(size); }
#endif

extern "C" tlm::Object* tlm_alloc(int tag, int num_fields) {
    size_t size = tlm::objectSize(num_fields);
    auto* obj = static_cast<tlm::Object*>(tlm_malloc(size));
    if (!obj) { std::fprintf(stderr, "tulam: out of memory\n"); std::exit(1); }
    new (&obj->header) tlm::Header(static_cast<uint16_t>(tag),
                                    static_cast<uint8_t>(num_fields));
    std::memset(obj->fields, 0, num_fields * sizeof(uint64_t));
    return obj;
}

extern "C" uint64_t tlm_get_field_raw(tlm::Object* obj, int index) {
    return obj->getField(index);
}

extern "C" void tlm_set_field_raw(tlm::Object* obj, int index, uint64_t value) {
    obj->setField(index, value);
}

extern "C" uint16_t tlm_get_tag(tlm::Object* obj) { return obj->getTag(); }

extern "C" void tlm_print_int(int64_t val)    { std::printf("%lld", (long long)val); }
extern "C" void tlm_print_float(double val)    { std::printf("%g", val); }
extern "C" void tlm_print_string(const char* s){ std::printf("%s", s); }
extern "C" void tlm_print_newline()            { std::printf("\n"); }
extern "C" void tlm_print_bool(int val)        { std::printf("%s", val ? "True" : "False"); }

extern "C" [[noreturn]] void tlm_error(const char* msg) {
    std::fprintf(stderr, "tulam runtime error: %s\n", msg);
    std::exit(1);
    // Future: throw tlm::RuntimeError(msg) for proper stack unwinding
}

extern "C" void tlm_rc_inc(tlm::Object* obj) { if (obj) obj->header.rc++; }
extern "C" void tlm_rc_dec(tlm::Object* obj) {
    if (obj) {
        obj->header.rc--;
        // Phase A.1: don't free — Boehm GC or leak. Phase E: Perceus drop+reuse.
    }
}
```

### 6.5 Header Layout (Future-Proofed)

```
Byte offset:  0    1    2    3    4    5    6    7    8   ...
            ┌────┬────┬────┬────┬────┬────┬────┬────┬────────────
            │ tag (u16)│ari │flag│    rc_or_idx (u32)    │ fields...
            │          │(u8)│(u8)│ bit31=0: local RC     │
            └────┴────┴────┴────┴────┴────┴────┴────┴────────────
                                      │
                                      └─ Phase 2: bit31=1 → side table index
```

This is the **exact same layout** that will be used in all future phases. Only the interpretation of the `rc_or_idx` field changes.

### 6.6 Future: Class-Aware Objects

When tulam class compilation lands (Phase G+), the RTS adds `tlm::ClassObject` extending `tlm::Object`:

```cpp
namespace tlm {
// Future — not in Phase A.1:
struct VTable {
    const char* className;
    VTable* parent;                    // single inheritance chain
    void* methods[];                   // function pointers (indexed by method slot)
};

struct ClassObject : Object {
    VTable* vtable;                    // points to class vtable (shared, static)
    // fields[] from Object store instance fields (parent-first, own-last)
};
} // namespace tlm
```

Because the RTS is C++, this is a natural extension — `ClassObject` inherits `Object`'s header and fields. A tulam `class Foo extends CppBar` can literally inherit from a C++ class compiled with clang, sharing the same vtable layout. LLVM IR calls `extern "C"` dispatch functions that internally do the vtable lookup.

---

## 7. Memory Representation

### 7.1 Value Representation (Phase A.1)

In Phase A.1, **everything is boxed** (heap-allocated). This is intentionally simple:

| tulam Type | LIR Type | Runtime Representation |
|------------|----------|----------------------|
| Int | LTInt64 | Passed as i64 in registers/stack (unboxed for function args) |
| Float64 | LTFloat64 | Passed as double in registers/stack (unboxed for function args) |
| Bool | LTBool | Passed as i1 |
| Char | LTChar | Passed as i32 |
| String | LTPtr | Pointer to C string (immutable) |
| Constructor (e.g. Just(x)) | LTPtr | Pointer to `tlm_object` on heap |
| Function | LTPtr | Pointer to `tlm_object` (closure) or function pointer |

**Key simplification**: Primitive types (Int, Float64, Bool, Char) are passed **unboxed** in function parameters and return values, but **boxed** (as `uint64_t` in the fields array) when stored in constructors. This gives us efficient register passing for arithmetic while keeping constructor layout uniform.

**Boxing/unboxing** at constructor boundaries:

```
// Storing Int into constructor field:
%val = ... ; i64
%val_as_u64 = bitcast i64 %val to i64    ; (no-op, same repr)
call void @tlm_set_field_raw(%obj, 0, %val_as_u64)

// Loading Int from constructor field:
%raw = call i64 @tlm_get_field_raw(%obj, 0)
%val = bitcast i64 %raw to i64            ; (no-op)
```

For pointers (constructors, strings):
```
// Storing ptr into constructor field:
%val_as_u64 = ptrtoint ptr %val to i64
call void @tlm_set_field_raw(%obj, 0, %val_as_u64)

// Loading ptr from constructor field:
%raw = call i64 @tlm_get_field_raw(%obj, 0)
%val = inttoptr i64 %raw to ptr
```

### 7.2 Constructor Examples

```
-- tulam: Just(42)
-- tag = 1 (Just is second constructor of Maybe), arity = 1
%obj = call ptr @tlm_alloc(i32 1, i32 1)           ; alloc Just with 1 field
call void @tlm_set_field_raw(ptr %obj, i32 0, i64 42) ; field[0] = 42
; result: %obj : ptr

-- tulam: Cons(1, Cons(2, Nil))
%nil = call ptr @tlm_alloc(i32 0, i32 0)            ; Nil: tag=0, 0 fields
%inner = call ptr @tlm_alloc(i32 1, i32 2)          ; Cons: tag=1, 2 fields
call void @tlm_set_field_raw(ptr %inner, i32 0, i64 2)  ; head = 2
%nil_u64 = ptrtoint ptr %nil to i64
call void @tlm_set_field_raw(ptr %inner, i32 1, i64 %nil_u64) ; tail = Nil
%outer = call ptr @tlm_alloc(i32 1, i32 2)          ; Cons: tag=1, 2 fields
call void @tlm_set_field_raw(ptr %outer, i32 0, i64 1)  ; head = 1
%inner_u64 = ptrtoint ptr %inner to i64
call void @tlm_set_field_raw(ptr %outer, i32 1, i64 %inner_u64) ; tail = inner
```

---

## 8. Build & Link Pipeline

### 8.1 Compilation Steps

```
1. tulam compiler (Haskell):
   Source → Parse → Pipeline → CLM → LIR → LLVM IR text (.ll file)

2. LLVM assembler (llc):
   .ll file → .o file (native object)
   Command: llc -O2 -filetype=obj output.ll -o output.o

3. C++ compiler (clang++):
   Compile runtime: clang++ -std=c++17 -c -O2 runtime/LLVM/tlm_runtime.cpp -o tlm_runtime.o
   Link: clang++ output.o tlm_runtime.o -o output [-lgc for Boehm]

Alternative for step 2+3 combined:
   clang++ -std=c++17 -O2 output.ll runtime/LLVM/tlm_runtime.cpp -o output [-lgc]
```

**Why clang++, not clang?** The runtime is C++, so we need C++ linkage for the runtime object file. The generated LLVM IR is pure C-ABI — all `extern "C"` calls — so there's no ABI mismatch. `clang++` links `libstdc++`/`libc++` automatically, which the runtime may use internally.

### 8.2 Orchestration Module

```haskell
module Backends.LLVM.NativeCompile where

-- Entry point for native compilation
compileToNative :: InterpreterState -> CompileOptions -> IO CompileResult
compileToNative state opts = do
  -- 1. Extract clmLambdas from environment
  let clmEnv = clmLambdas (environment state)

  -- 2. Find the 'main' function
  case HashMap.lookup "main" clmEnv of
    Nothing -> return $ CompileError "No 'main' function defined"
    Just mainLam -> do
      -- 3. Lower CLM → LIR
      let lirModule = lowerModule clmEnv mainLam

      -- 4. Emit LLVM IR
      let llvmIR = emitModule lirModule

      -- 5. Write .ll file
      let llFile = coOutputPath opts ++ ".ll"
      Text.writeFile llFile llvmIR

      -- 6. Compile and link (C++ runtime)
      let runtimePath = "runtime/LLVM/tlm_runtime.cpp"
      exitCode <- rawSystem "clang++"
        [ "-std=c++17", "-O2"
        , llFile, runtimePath
        , "-o", coOutputPath opts ]
      case exitCode of
        ExitSuccess   -> return $ CompileSuccess (coOutputPath opts)
        ExitFailure n -> return $ CompileError $ "clang++ failed with exit code " ++ show n
```

### 8.3 REPL Integration

New REPL command:

```
:compile <file> [output]    — Compile file to native executable
```

```haskell
-- In Main.hs, add command handler:
processCommand ":compile" args = do
  let (file, output) = parseCompileArgs args
  state <- get
  result <- liftIO $ compileToNative state output
  case result of
    Left err   -> liftIO $ putStrLn $ "Compilation error: " ++ err
    Right path -> liftIO $ putStrLn $ "Compiled to: " ++ path
```

---

## 9. Test Strategy

### 9.1 Test Programs

Create `tests/native/` directory with simple programs that exercise Phase A.1:

```
tests/native/
├── N01_Fibonacci.tl      — fibonacci(35), pure recursion
├── N02_Factorial.tl       — factorial(20), recursion + multiplication
├── N03_PatternMatch.tl    — Maybe/Bool pattern matching
├── N04_List.tl            — List construction + length/sum
├── N05_Arithmetic.tl      — All arithmetic ops on Int
├── N06_Comparison.tl      — Comparison ops, if/then/else
├── N07_NestedMatch.tl     — Nested pattern matching
├── N08_StringLit.tl       — String literal printing
├── N09_MultiFunc.tl       — Multiple functions calling each other
├── N10_FieldAccess.tl     — Constructor field access
```

### 9.2 Test Harness

Each test file has a `main` function that prints expected output. Tests are verified by:

```haskell
-- In test/Spec.hs (or a new test/NativeSpec.hs):
describe "Native compilation (Phase A.1)" $ do
  forM_ nativeTests $ \(testFile, expectedOutput) ->
    it ("compiles and runs " ++ testFile) $ do
      -- 1. Load and compile through pipeline
      state <- loadAndCompile testFile
      -- 2. Compile to native
      result <- compileToNative state "/tmp/tulam_test"
      result `shouldBe` Right "/tmp/tulam_test"
      -- 3. Run and capture output
      output <- readProcess "/tmp/tulam_test" [] ""
      -- 4. Check output
      output `shouldBe` expectedOutput
```

### 9.3 Dual-Mode Testing

Every native test program should also work in the interpreter. This ensures the native backend produces the same results:

```haskell
it "native matches interpreter for fibonacci" $ do
  -- Run in interpreter
  interpResult <- runInterpreter "tests/native/N01_Fibonacci.tl" "main()"
  -- Run as native
  nativeResult <- compileAndRun "tests/native/N01_Fibonacci.tl"
  -- Compare
  nativeResult `shouldBe` interpResult
```

---

## 10. File Plan

### 10.1 New Files to Create

```
src/Backends/
└── LLVM/
    ├── LIR.hs              — LIR data types (§3.2)
    ├── CLMToLIR.hs         — CLM → LIR lowering (§4)
    ├── LIRToLLVM.hs        — LIR → LLVM IR text emission (§5)
    └── NativeCompile.hs    — Orchestration: pipeline + file I/O + exec (§8)

runtime/
├── tlm_object.hpp      — Object/Header types, namespace tlm (§6.2)
├── tlm_runtime.hpp     — extern "C" API declarations (§6.3)
├── tlm_runtime.cpp     — Runtime implementation (§6.4)
└── Makefile            — Simple build for runtime .o (or just let clang++ handle it)

tests/native/
├── N01_Fibonacci.tl    — through N10_FieldAccess.tl (§9.1)

test/
├── NativeSpec.hs       — Native compilation tests (§9.2)
```

### 10.2 Files to Modify

```
package.yaml            — Add Backends.LLVM.* modules to exposed-modules
app/Main.hs             — Add :compile REPL command
test/Spec.hs            — Import NativeSpec if separate, or add native tests
```

### 10.3 No Modifications to Existing Pipeline

**Critical**: Phase A.1 does NOT modify any existing file in the compilation pipeline (Parser.hs, Pipeline.hs, CLM.hs, TypeCheck.hs, Interpreter.hs, etc.). It is purely additive — a new output path from existing CLM.

---

## 11. Implementation Order

### Stage 1 (Foundation — ~2-3 sessions)

```
Step 1: Create src/Backends/LLVM/LIR.hs
  - All data types from §3.2
  - Pretty-printer for debugging (Show instances are sufficient initially)
  - No logic, just types

Step 2: Create runtime/LLVM/tlm_object.hpp + tlm_runtime.hpp + tlm_runtime.cpp
  - Object/Header types in tlm_object.hpp
  - extern "C" API in tlm_runtime.hpp
  - Implementation in tlm_runtime.cpp
  - Compile standalone to verify: clang++ -c runtime/LLVM/tlm_runtime.cpp

Step 3: Create src/Backends/LLVM/LIRToLLVM.hs
  - Text emission for all LIR types
  - Start with a hardcoded test: emit fibonacci manually in LIR, verify .ll compiles
  - Test: write LIR for fibonacci by hand → emit → llc → clang++ → run → correct output

Step 4: Create src/Backends/LLVM/CLMToLIR.hs (core lowering)
  - Start with: CLMLIT, CLMID, CLMAPP (direct calls only), CLMERR
  - Then add: CLMCON, CLMFieldAccess
  - Then add: CLMCASE (linear chain pattern matching)
  - Then add: CLMPROG, CLMBIND
  - Then add: CLMLAM (non-capturing only)
  - Test each addition by writing a tulam program, running through pipeline, inspecting LIR

Step 5: Create src/Backends/LLVM/NativeCompile.hs
  - Wire up: load file → run passes → extract CLM → lower → emit → compile → link
  - Add :compile command to REPL

Step 6: Create test programs and test harness
  - N01_Fibonacci.tl through N10_FieldAccess.tl
  - Dual-mode tests (interpreter vs native)
```

### Stage 2 (Intrinsics — ~1-2 sessions)

```
Step 7: Intrinsic lowering in CLMToLIR.hs
  - Map known intrinsic names to LIR arithmetic/comparison instructions
  - Or: map to C runtime function calls (simpler, LLVM inlines anyway)
  - Test: arithmetic-heavy programs compile and produce correct results

Step 8: Float64 support
  - Float literals, float arithmetic instructions
  - Test: N05 expanded with float tests
```

### Stage 3 (Polish — ~1 session)

```
Step 9: Error messages and diagnostics
  - Source location in error messages
  - Unsupported CLM node errors (clear message: "CLMIAP not supported in Phase A.1")

Step 10: Update package.yaml, documentation
  - Add new modules
  - Update ImplementationPlan.md
  - Update CLAUDE.md with backend info
```

---

## 12. What's In / What's Out

### In (Phase A.1)

| Feature | Status |
|---------|--------|
| Int64 arithmetic (+, -, *, div, rem, negate) | Yes |
| Float64 arithmetic (+, -, *, /, negate) | Yes |
| Int/Float comparison (<, <=, >, >=, ==, !=) | Yes |
| Bool (as i1), if/then/else (as branches) | Yes |
| Constructor allocation (CLMCON) | Yes |
| Pattern matching (CLMCASE, linear chain) | Yes |
| Field access (CLMFieldAccess) | Yes |
| Direct function calls (CLMAPP) | Yes |
| Non-capturing lambdas (top-level CLMLam) | Yes |
| String literals (as C string pointers) | Yes |
| Let bindings (CLMBIND) | Yes |
| Sequential statements (CLMPROG) | Yes |
| Error/unreachable | Yes |
| Multiple functions per module | Yes |
| main entry point | Yes |
| Boehm GC (or leak-and-exit) | Yes |

### Out (Deferred)

| Feature | Deferred To | Reason |
|---------|-------------|--------|
| CLMIAP (polymorphic dispatch) | Phase C (Monomorphization) | Needs type specialization |
| CLMPAP (partial application) | Phase F (Defunctionalization) | Needs closure representation |
| Closures with captures | Phase F | Needs environment capture lowering |
| CLMTYPED (type-directed dispatch) | Phase C | Depends on monomorphization |
| CLMHANDLE (effect handlers) | Phase G | Complex control flow |
| CLMMCALL/CLMSCALL/CLMNEW (classes) | Phase G | Needs vtable/dispatch compilation |
| Tail call optimization | Phase G | `musttail` in LLVM |
| CLMREF/CLMMUTARRAY (mutable state) | Phase G | Needs mutable state runtime |
| CLMARRAY (dynamic arrays) | Phase B+ | Needs array runtime |
| Perceus RC | Phase E | Requires liveness + reuse analysis |
| Unboxing / worker-wrapper | Phase D | Requires demand analysis |
| Decision tree pattern matching | Phase B | Optimization, not correctness |
| SIMD | Phase H | Needs vector type mapping |
| IO (file, console) | Phase B+ | Needs IO runtime |

---

## 13. Detailed Module Specifications

### 13.1 src/Backends/LLVM/LIR.hs — Complete Specification

See §3.2 for full data type definitions. Additional utilities:

```haskell
-- Type of an operand
operandType :: LOperand -> LType
operandType (LVar _ ty) = ty
operandType (LLitInt _ ty) = ty
operandType (LLitFloat _ ty) = ty
operandType (LLitBool _) = LTBool
operandType LLitNull = LTPtr
operandType (LLitString _) = LTPtr

-- Type of an instruction's result
instrResultType :: LInstr -> LType
instrResultType (LAdd a _) = operandType a
instrResultType (LICmpEq _ _) = LTBool
instrResultType (LAlloc _ _) = LTPtr
instrResultType (LLoad _ _ ty) = ty
instrResultType (LGetTag _) = LTInt16
instrResultType (LCall _ _ ty) = ty
-- ... etc for all instructions

-- Check if a type is a floating-point type
isFloatType :: LType -> Bool
isFloatType LTFloat64 = True
isFloatType LTFloat32 = True
isFloatType _ = False

-- Check if a type is an integer type
isIntType :: LType -> Bool
isIntType LTInt64 = True
isIntType LTInt32 = True
isIntType LTInt16 = True
isIntType LTInt8 = True
isIntType LTWord64 = True
isIntType LTWord32 = True
isIntType LTWord16 = True
isIntType LTWord8 = True
isIntType LTBool = True
isIntType LTChar = True
isIntType _ = False

-- Size of a type in bytes
typeSize :: LType -> Int
typeSize LTInt64 = 8
typeSize LTInt32 = 4
typeSize LTInt16 = 2
typeSize LTInt8 = 1
typeSize LTWord64 = 8
typeSize LTWord32 = 4
typeSize LTWord16 = 2
typeSize LTWord8 = 1
typeSize LTFloat64 = 8
typeSize LTFloat32 = 4
typeSize LTBool = 1
typeSize LTChar = 4
typeSize LTPtr = 8
typeSize LTVoid = 0
typeSize (LTFunPtr _ _) = 8
```

### 13.2 src/Backends/LLVM/CLMToLIR.hs — Key Functions

```haskell
module CLMToLIR
  ( lowerModule
  , LowerError(..)
  ) where

import qualified Data.HashMap.Strict as HashMap
import CLM
import LIR
import State (Environment(..), InterpreterState(..))
import Surface (Name, ConsTag(..), Literal(..), SourceInfo(..))

-- Errors during lowering
data LowerError
  = UnsupportedCLMNode String SourceInfo
  | UnknownVariable Name
  | UnknownFunction Name
  | ArityMismatch Name Int Int   -- name, expected, got
  deriving (Show)

-- Top-level: lower all functions in the environment to an LIR module
lowerModule :: Environment -> LModule

-- Lower a single CLM lambda to an LIR function
lowerFunction :: Name -> CLMLam -> LowerM LFunction

-- Lower a CLM expression to an LIR operand (may emit multiple instructions)
lowerExpr :: CLMExpr -> LowerM LOperand

-- Lower pattern matching (CLMLamCases)
lowerCases :: [CLMExpr] -> LOperand -> LowerM LOperand

-- Fresh name generation
freshVar :: String -> LowerM Name    -- e.g., freshVar "tmp" → "%tmp.42"
freshBlock :: String -> LowerM Name  -- e.g., freshBlock "case" → "case.7"

-- Emit an instruction into the current block
emit :: Name -> LInstr -> LowerM ()

-- Terminate current block and start a new one
terminateBlock :: LTerminator -> Name -> LowerM ()
```

### 13.3 src/Backends/LLVM/LIRToLLVM.hs — Key Functions

```haskell
module LIRToLLVM
  ( emitModule
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import LIR

-- Emit a complete LLVM IR module as text
emitModule :: LModule -> Text

-- Internal emission functions (all pure, building Text)
emitPreamble :: String -> Text         -- target triple, datalayout
emitExternDecls :: [LFunction] -> Text -- declare statements for runtime + externs
emitGlobals :: [LGlobal] -> Text       -- @str_N = private constant ...
emitFunction :: LFunction -> Text      -- define <ret> @<name>(<params>) { <blocks> }
emitBlock :: LBlock -> Text            -- <label>: <instrs> <term>
emitInstr :: Name -> LInstr -> Text    -- %<name> = <op> <args>
emitTerminator :: LTerminator -> Text  -- ret/br/switch/unreachable
emitType :: LType -> Text              -- i64/double/ptr/void/...
emitOperand :: LOperand -> Text        -- %name/42/3.14/null/@str
```

### 13.4 src/Backends/LLVM/NativeCompile.hs — Key Functions

```haskell
module Backends.LLVM.NativeCompile
  ( compileToNative
  , CompileOptions(..)
  , CompileResult(..)
  ) where

import State (InterpreterState(..))
import CLMToLIR (lowerModule)
import LIRToLLVM (emitModule)

data CompileOptions = CompileOptions
  { coOutputPath    :: FilePath
  , coOptLevel      :: Int          -- 0-3, passed to clang
  , coEmitLLVMIR    :: Bool         -- If True, also save .ll file
  , coVerbose       :: Bool         -- Print compilation steps
  }

data CompileResult
  = CompileSuccess FilePath          -- Path to executable
  | CompileError String              -- Error message

-- Main entry point
compileToNative :: InterpreterState -> CompileOptions -> IO CompileResult

-- Find the LLVM/clang toolchain
findToolchain :: IO (Either String (FilePath, FilePath))  -- (llc, clang)

-- Detect target triple
detectTargetTriple :: IO String
```

### 13.5 Pre-Lowering Mini-Pass: CLMIAP Resolution for Known Types

Since Phase A.1 doesn't support CLMIAP, but the existing pipeline produces CLMIAP for algebra method calls, we need a mini-pass that resolves **monomorphic** CLMIAP calls to direct CLMAPP calls:

```haskell
-- In CLMToLIR.hs or a helper module:

-- Resolve CLMIAP calls where all argument types are known
-- This is a simplified version of what Phase C (Monomorphization) will do
resolveMonomorphicIAP :: Environment -> CLMExpr -> CLMExpr
resolveMonomorphicIAP env expr = case expr of
  CLMIAP (CLMID funcName) args ->
    -- Try to determine concrete types from literal/constructor args
    let argTypes = map inferCLMType args
    in case allJust argTypes of
      Just types ->
        let key = mkInstanceKey funcName types
        in case HashMap.lookup key (clmInstances env) of
          Just lam -> CLMAPP (CLMLAM lam) args  -- Resolved!
          Nothing  ->
            -- Try intrinsic
            case lookupIntrinsicForCompile funcName types of
              Just intrinsicName -> CLMAPP (CLMID intrinsicName) args
              Nothing -> expr  -- Can't resolve → will error at lowering
      Nothing -> expr  -- Can't determine types → will error at lowering
  _ -> descendCLM (resolveMonomorphicIAP env) expr

-- Infer CLM expression type for dispatch resolution
inferCLMType :: CLMExpr -> Maybe Name
inferCLMType (CLMLIT (LInt _))     = Just "Int"
inferCLMType (CLMLIT (LFloat64 _)) = Just "Float64"
inferCLMType (CLMLIT (LString _))  = Just "String"
inferCLMType (CLMLIT (LChar _))    = Just "Char"
inferCLMType (CLMCON (ConsTag name _) _) = Just name  -- constructor's type
inferCLMType _ = Nothing
```

This is intentionally simple and incomplete. It handles the common case of arithmetic on literals and constructor operations. Anything it can't resolve will produce a clear error message at lowering time.

---

## Appendix A: Example End-to-End Trace

### Input: `tests/native/N01_Fibonacci.tl`

```tulam
module Test.Fibonacci;

function fibonacci(n: Int) : Int =
    if n <= 1 then n
    else fibonacci(n - 1) + fibonacci(n - 2);

function main() : Int = fibonacci(35);
```

### After CLM conversion (Pass 4):

```
clmLambdas["fibonacci"] = CLMLam [("n", CLMEMPTY)]
  (CLMCASE [CLMCheckLit ...] ...)  -- simplified

clmLambdas["main"] = CLMLam []
  (CLMAPP (CLMID "fibonacci") [CLMLIT (LInt 35)])
```

### After LIR lowering:

```haskell
LModule "Test.Fibonacci"
  []  -- no globals
  [ LFunction "tulam_fibonacci" [("n", LTInt64)] LTInt64
      [ LBlock "entry"
          [ ("cmp", LICmpLe (LVar "n" LTInt64) (LLitInt 1 LTInt64)) ]
          (LCondBr (LVar "cmp" LTBool) "base" "recurse")
      , LBlock "base"
          []
          (LRet (LVar "n" LTInt64))
      , LBlock "recurse"
          [ ("n1", LSub (LVar "n" LTInt64) (LLitInt 1 LTInt64))
          , ("n2", LSub (LVar "n" LTInt64) (LLitInt 2 LTInt64))
          , ("fib1", LCall "tulam_fibonacci" [LVar "n1" LTInt64] LTInt64)
          , ("fib2", LCall "tulam_fibonacci" [LVar "n2" LTInt64] LTInt64)
          , ("result", LAdd (LVar "fib1" LTInt64) (LVar "fib2" LTInt64))
          ]
          (LRet (LVar "result" LTInt64))
      ]
      False
  , LFunction "main" [] LTInt64     -- tulam main
      [ LBlock "entry"
          [ ("r", LCall "tulam_fibonacci" [LLitInt 35 LTInt64] LTInt64) ]
          (LRet (LVar "r" LTInt64))
      ]
      False
  ]
  [ LFunction "tlm_alloc" [...] LTPtr True         -- extern declarations
  , LFunction "tlm_print_int" [...] LTVoid True
  , ...
  ]
```

### After LLVM IR emission:

```llvm
target triple = "arm64-apple-darwin"

declare ptr @tlm_alloc(i32, i32)
declare void @tlm_print_int(i64)
declare void @tlm_error(ptr)

define i64 @tulam_fibonacci(i64 %n) {
entry:
  %cmp = icmp sle i64 %n, 1
  br i1 %cmp, label %base, label %recurse

base:
  ret i64 %n

recurse:
  %n1 = sub i64 %n, 1
  %n2 = sub i64 %n, 2
  %fib1 = call i64 @tulam_fibonacci(i64 %n1)
  %fib2 = call i64 @tulam_fibonacci(i64 %n2)
  %result = add i64 %fib1, %fib2
  ret i64 %result
}

define i32 @main() {
entry:
  %r = call i64 @tulam_fibonacci(i64 35)
  call void @tlm_print_int(i64 %r)
  ret i32 0
}
```

### Compile and run:

```bash
$ clang++ -std=c++17 -O2 output.ll runtime/LLVM/tlm_runtime.cpp -o fibonacci
$ ./fibonacci
9227465
```

---

## Appendix B: Intrinsic Name Mapping

For Phase A.1, the lowering needs to know which CLM function names correspond to machine operations. Since the existing interpreter resolves intrinsics by `(funcName, typeName)` key in `Intrinsics.hs`, the native backend needs a similar mapping:

| CLM Function Call Pattern | LIR Instruction | LLVM IR |
|--------------------------|-----------------|---------|
| `combine(Int, x, y)` or `+` with Int args | `LAdd x y` | `add i64 %x, %y` |
| `combine(Float64, x, y)` or `+` with Float64 | `LFAdd x y` | `fadd double %x, %y` |
| `negate(Int, x)` | `LNeg x` | `sub i64 0, %x` |
| `mul(Int, x, y)` or `*` with Int | `LMul x y` | `mul i64 %x, %y` |
| `div(Int, x, y)` | `LDiv x y` | `sdiv i64 %x, %y` |
| `eq(Int, x, y)` or `==` with Int | `LICmpEq x y` | `icmp eq i64 %x, %y` |
| `lt(Int, x, y)` or `<` with Int | `LICmpLt x y` | `icmp slt i64 %x, %y` |
| `le(Int, x, y)` or `<=` with Int | `LICmpLe x y` | `icmp sle i64 %x, %y` |

The full mapping is generated from `Intrinsics.hs` entries. For Phase A.1, we support arithmetic/comparison on Int and Float64 only. All other intrinsics fall back to C runtime calls or produce "unsupported" errors.

---

*End of Phase A.1 Design Document*
