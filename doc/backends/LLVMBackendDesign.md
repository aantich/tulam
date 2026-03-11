# tulam LLVM Backend Design

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Design Philosophy](#2-design-philosophy)
3. [Comparison with Existing Approaches](#3-comparison-with-existing-approaches)
4. [Evaluation Strategy: Strict with Clairvoyant Laziness](#4-evaluation-strategy-strict-with-clairvoyant-laziness)
5. [Compilation Pipeline: CLM to LLVM](#5-compilation-pipeline-clm-to-llvm)
6. [Memory Representation](#6-memory-representation)
7. [Closure Representation & Defunctionalization](#7-closure-representation--defunctionalization)
8. [Memory Management: Perceus Reference Counting](#8-memory-management-perceus-reference-counting)
9. [Monomorphization](#9-monomorphization)
10. [Unboxing & Worker/Wrapper](#10-unboxing--workerwrapper)
11. [Pattern Matching Compilation](#11-pattern-matching-compilation)
12. [Algebra/Structure Dispatch Compilation](#12-algebrastructure-dispatch-compilation)
13. [Class System Compilation](#13-class-system-compilation)
14. [Effect Handler Compilation](#14-effect-handler-compilation)
15. [SIMD & Numeric Fast Paths](#15-simd--numeric-fast-paths)
16. [CLM IR Reference for Codegen](#16-clm-ir-reference-for-codegen)
17. [LLVM IR Mapping](#17-llvm-ir-mapping)
18. [Implementation Phases](#18-implementation-phases)
19. [Research References](#19-research-references)

---

## 1. Executive Summary

This document describes the design for tulam's LLVM native code backend, targeting high-performance numeric/math computation with full unboxing, zero-overhead algebraic abstractions, and smart memory management.

### Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| **Default evaluation** | Strict (CBV) | Zero thunk overhead for math; predictable performance |
| **Memory management** | Perceus RC with reuse | No GC pauses; constructor recycling; simple runtime |
| **Polymorphism compilation** | Monomorphization + witness tables | Direct calls for known types; fallback for generics |
| **Closure strategy** | Defunctionalization + flat closures | No indirect calls; cache-friendly; LLVM-optimizable |
| **Value representation** | Three-tier (unboxed / tagged-pointer / heap) | Zero allocation for primitives; compact polymorphic repr |
| **Laziness** | Explicit opt-in with demand-analysis suggestions | No hidden thunk overhead; lazy only where beneficial |
| **Calling convention** | Standard C (cdecl/aapcs) | Avoids GHC's LLVM pain points; interop-friendly |

### Performance Targets

| Workload | Target vs C | Target vs GHC |
|----------|-------------|---------------|
| Pure numeric (Int/Float64 arithmetic) | 1.0-1.2x | 2-5x faster |
| Data structure traversal (List/Tree map) | 2-3x | 1-2x faster |
| Higher-order functions (map/filter/fold) | 2-4x | 1-3x faster |
| Pattern-matching-heavy code | 1-2x | Comparable |
| Mixed numeric + ADT code | 1.5-3x | 1.5-3x faster |

---

## 2. Design Philosophy

### tulam's Unique Advantages for Native Compilation

tulam has three properties that make it fundamentally different from GHC/Haskell for codegen purposes:

**1. Strict by Default**
The CLM evaluator (`evalCLM` in Interpreter.hs) evaluates all function arguments before application. There are no thunks, no lazy indirections, no blackholing. This means:
- Every function call is a simple call (no `eval`/`enter` dance like GHC's STG)
- Stack allocation is straightforward (no thunk-to-value updates)
- LLVM's optimization passes work at full effectiveness (no hidden side effects)

**2. Explicit Constructor Tags**
Every constructor carries an integer discriminant: `ConsTag Name !Int`. Pattern matching is a simple integer comparison. This gives us:
- Direct mapping to LLVM `switch` instructions
- No info-table indirection (unlike GHC)
- Known-size discriminants for branch prediction

**3. Type Dispatch Resolved at Compile Time**
Type-dependent functions are erased to `CLMEMPTY` during CLM conversion (Pass 4). `CLMIAP` calls can be monomorphized to `CLMAPP` direct calls when types are known. This means:
- No runtime type-class dictionary lookup for monomorphized code
- Witness tables only at genuinely polymorphic boundaries
- Full inlining of algebra methods into hot loops

### Design Principles

1. **Zero-cost abstractions for math**: `algebra Additive(a) = { function (+)(x:a, y:a) : a }` with `instance Additive(Int) = intrinsic` must compile to a single `add` instruction.

2. **Pay only for what you use**: Lazy evaluation, closures, and polymorphism have cost only when explicitly needed. Strict numeric code compiles to the same IR as C.

3. **Bottom-up optimization**: Start with the tightest possible representation (unboxed registers), widen only when forced (polymorphism, escaping values, unknown types).

4. **Incremental compilation with LTO**: Per-module compilation for fast iteration; LLVM Link-Time Optimization for release builds to recover whole-program optimization.

---

## 3. Comparison with Existing Approaches

### 3.1 Evaluation Strategy Comparison

| Language | Default Eval | Strictness Info | Thunk Cost | Sharing |
|----------|-------------|-----------------|------------|---------|
| **GHC/Haskell** | Lazy (CBN + memoization) | Demand analysis (inferred) | Every value potentially a thunk | Automatic via thunk update |
| **OCaml** | Strict (CBV) | N/A (always strict) | Zero | Manual (`lazy`/`Lazy.force`) |
| **Lean 4** | Strict (CBV) | N/A (always strict) | Zero | Via RC (natural sharing) |
| **Idris 2** | Strict (CBV) + erasure | QTT quantities (0/1/omega) | Zero for erased | Explicit |
| **MLton (SML)** | Strict (CBV) | N/A (always strict) | Zero | Manual (refs) |
| **Koka** | Strict (CBV) | N/A | Zero | Perceus RC reuse |
| **tulam (proposed)** | **Strict (CBV) + selective lazy** | **Demand analysis + explicit `lazy`** | **Zero by default; thunk only for `lazy`** | **Perceus RC + memoized thunks** |

### 3.2 Memory Management Comparison

| Language | GC Strategy | Pause Times | Unboxing | Allocation Rate |
|----------|------------|-------------|----------|-----------------|
| **GHC** | Generational copying | Unpredictable (ms-100ms) | Worker/wrapper | Very high (nursery) |
| **OCaml** | Generational semi-space | ~1ms minor, longer major | Uniform boxing | Moderate |
| **Lean 4** | Reference counting + reuse | Zero (deterministic) | Tagged pointers | Low (reuse) |
| **Koka** | Perceus RC + reuse | Zero (deterministic) | Some | Very low (reuse) |
| **MLton** | Copying GC | Stop-the-world | Full (after mono) | Low (after mono) |
| **Rust** | Ownership (no GC) | Zero | Full | Minimal |
| **tulam (proposed)** | **Perceus RC + reuse** | **Zero (deterministic)** | **Full (after mono)** | **Minimal (reuse + unboxing)** |

### 3.3 Polymorphism Compilation Comparison

| Language | Strategy | Overhead per Call | Code Size | Separate Compilation |
|----------|----------|-------------------|-----------|---------------------|
| **GHC** | Dictionary passing | 1 indirect call + dict alloc | O(1) per poly fn | Yes |
| **OCaml** | Uniform representation | 0 (boxing cost instead) | O(1) | Yes |
| **Lean 4** | Tagged pointers + boxing | Tag check per use | O(1) | Yes |
| **MLton** | Full monomorphization | 0 (direct call) | O(types x fns) | No (whole-program) |
| **Rust** | Monomorphization | 0 (direct call) | O(types x fns) | Per-crate mono |
| **Swift** | Witness tables + specialization | 0 (specialized) or 1 indirect (generic) | Moderate | Yes |
| **tulam (proposed)** | **Mono + witness table fallback** | **0 (known types) or 1 indirect** | **Moderate (dedup)** | **Yes (module-level)** |

### 3.4 Closure Representation Comparison

| Language | Strategy | Memory per Closure | Indirect Calls | Cache Behavior |
|----------|----------|-------------------|----------------|----------------|
| **GHC** | Heap-allocated with info table | Header + free vars | Always (through info table) | Poor (pointer chasing) |
| **OCaml** | Heap-allocated flat | Header + free vars | Always (through code ptr) | Moderate |
| **MLton** | Defunctionalized + flat | Tag + free vars | Direct (switch dispatch) | Good (data locality) |
| **Lean 4** | Flat with RC header | RC header + free vars | Through function ptr | Moderate |
| **Rust** | Stack or heap (compiler decides) | Free vars only (no header) | Direct (monomorphized) | Excellent |
| **tulam (proposed)** | **Defunctionalized + flat** | **Tag + free vars (no header if known)** | **Direct (defunc) or ptr (generic)** | **Excellent** |

### 3.5 Pattern Matching Compilation Comparison

| Language | Strategy | Branch Cost | Dead Branch Elimination |
|----------|----------|-------------|------------------------|
| **GHC** | Decision trees via STG `case` | Tag inspect + jump | Via demand analysis |
| **OCaml** | Optimized decision trees (Maranget) | Tag inspect + jump table | Static analysis |
| **Rust** | Exhaustive match → MIR switch | Direct switch | Compile-time (exhaustive) |
| **tulam (proposed)** | **CLMCASE → LLVM switch + GRIN points-to** | **Single switch instruction** | **Points-to + known-constructor pass** |

### 3.6 Overall Compiler Pipeline Comparison

| Stage | GHC | OCaml | MLton | Lean 4 | tulam (proposed) |
|-------|-----|-------|-------|--------|-----------------|
| **Source → Core** | Desugar + typecheck → Core | Parse + typecheck → Lambda | Parse + typecheck → AST | Elaboration → IR | Parse → Surface → CLM |
| **Core → Core** | Simplifier (20+ passes) | Flambda/Closure | SSA opts | LCNF opts | 5 CLM opts + 7 new passes |
| **Core → Low-level** | STG → Cmm → NCG/LLVM | Closure convert → C-- | Defunc → SSA → RSSA | Compiler IR → C | Defunc + mono → LLVM IR |
| **Low-level → Native** | NCG or LLVM backend | Emit x86/ARM | Emit x86 | cc (gcc/clang) | LLVM optimizer + codegen |
| **Key optimization** | Demand analysis | Flambda inlining | Whole-program mono | RC reuse | Mono + Perceus + unboxing |

---

## 4. Evaluation Strategy: Strict with Clairvoyant Laziness

### 4.1 Theoretical Foundation

Based on Hackett & Hutton (ICFP 2019): *"Call-by-need is clairvoyant call-by-value"* — lazy evaluation produces the same results as strict evaluation with a perfect oracle that skips unused computations. This means **the better our strictness/demand analysis, the closer strict code gets to lazy code's best case**.

tulam inverts the usual approach:
- **GHC**: Everything lazy by default → analyze to find strict positions → optimize to strict
- **tulam**: Everything strict by default → analyze to find where laziness helps → add laziness selectively

### 4.2 Three-Tier Evaluation

| Tier | Detection | Runtime Cost | When Used |
|------|-----------|-------------|-----------|
| **Strict (default)** | Everything not marked lazy | Zero overhead | All arguments, all bindings |
| **Absent** | Demand analysis proves unused | Negative (saves work) | Unused fields, dead code |
| **Lazy (memoized)** | Explicit `lazy` keyword or compiler suggestion | Thunk alloc + force check | Coinductive types, conditional expensive computation |

### 4.3 Demand Analysis (Pass 6)

Adapted from GHC's demand analysis but simplified for strict semantics. For each function parameter and let-binding, compute:

```
Demand = Absent           -- never evaluated on any path
       | Used(Usage)      -- evaluated at least once

Usage  = Once             -- evaluated exactly once (enables linear optimizations)
       | Many             -- evaluated potentially multiple times

ProductDemand = Demand per field (for tuples/constructors)
```

**Example:**

```
function f(p: {x:Int, y:Int, z:Int}) : Int =
    if p.x > 0 then p.x + p.y
    else 0;
```

Demand signature: `f :: <S(S1, S, A)>` — strict in `p`, strict-once in `x`, strict in `y`, absent in `z`.

**Optimization actions:**
- `z` field: Erase (never allocate/pass)
- `x` field: Unbox + evaluate immediately
- `y` field: Unbox, evaluate only in `then` branch (code motion)

### 4.4 Selective Lazy Bindings

For cases where laziness genuinely helps (e.g., infinite streams, expensive-but-rarely-needed computations):

```
// tulam syntax (future):
lazy value expensiveTable : Array(Int) = generateLargeTable(1000000);

// Compiles to:
// Thunk layout:
//   [header: tag=THUNK | evaluated:bool | rc:u16]
//   [payload: union { closure_ptr (unevaluated), value (evaluated) }]
//
// Force: if (thunk.evaluated) return thunk.value;
//        else { thunk.value = eval(thunk.closure); thunk.evaluated = true; return thunk.value; }
```

The `lazy` keyword would be validated by the demand analyzer: if a `lazy` binding is always strict (used on every path), the compiler emits a warning suggesting it should be strict.

### 4.5 Call-by-Need with Value Reuse

For `lazy` bindings, **memoization is automatic** — the thunk is updated in place after first evaluation (Lean 4 style, since RC means we can mutate when refcount is 1):

```
// First access:  evaluate closure → store result → return result
// Second access: return cached result (zero cost)
//
// If the thunk has refcount 1 (unshared):
//   In-place update (no allocation)
// If refcount > 1 (shared):
//   Allocate new evaluated node, decrement old
```

For strict code, **LLVM's own CSE (Common Subexpression Elimination)** handles value reuse within a function — no language-level mechanism needed.

---

## 5. Compilation Pipeline: CLM to LLVM

### 5.1 Full Pipeline

```
Source (.tl)
  → Lexer/Parser          (existing)
  → Surface AST           (existing)
  → Pass 0-0.5: Desugar   (existing)
  → Pass 1-1.5: Env Build (existing)
  → Pass 2-2.3: Case Opt  (existing)
  → Pass 3-3.1: TypeCheck (existing)
  → Pass 4: CLM Convert   (existing)
  → Pass 4.5: CLM Opt     (existing: eta, inline, fold, known-ctor, DCE)
  │
  │  ══════════════════════ NEW PASSES ══════════════════════
  │
  → Pass 6: Demand Analysis
  │   ├── Compute per-parameter demands (Strict/Absent/Lazy)
  │   ├── Compute product demands (per-field for constructors)
  │   └── Annotate CLM expressions with demand info
  │
  → Pass 7: Monomorphization
  │   ├── Collect all CLMIAP call sites with known types
  │   ├── Generate specialized function variants
  │   ├── Replace CLMIAP → CLMAPP to specialized variant
  │   ├── Deduplicate identical specializations
  │   └── Keep polymorphic originals only if needed
  │
  → Pass 8: Defunctionalization
  │   ├── Assign unique tags to each lambda/closure
  │   ├── Convert captured free vars to constructor fields
  │   ├── Generate dispatch functions (apply_tag → body)
  │   └── Keep true closures only for unresolvable HOFs
  │
  → Pass 9: Worker/Wrapper + Unboxing
  │   ├── Use demand info to split wrapper (boxed) / worker (unboxed)
  │   ├── Flatten single-constructor types (newtype elimination)
  │   ├── Unbox primitive fields in multi-field constructors
  │   └── Inline wrappers at known call sites
  │
  → Pass 10: Perceus RC Insertion
  │   ├── Compute liveness for each binding
  │   ├── Insert dup(x) where x gains a new reference
  │   ├── Insert drop(x) at last use
  │   ├── Reuse analysis: reset/reuse for consumed-then-rebuilt constructors
  │   └── Borrowing: skip dup/drop for observed-only parameters
  │
  → Pass 11: Decision Tree Compilation
  │   ├── Convert CLMCASE chains to decision trees
  │   ├── Points-to analysis for dead branch elimination
  │   ├── Compute jump tables for dense tag ranges
  │   └── Flatten nested cases to multi-level switches
  │
  → Pass 12: LLVM IR Emission
      ├── Function definitions → LLVM function decls
      ├── Constructor allocation → LLVM struct init
      ├── Pattern match → LLVM switch/br
      ├── Intrinsics → LLVM native instructions
      ├── RC operations → LLVM call to RC runtime
      └── Module assembly → .ll / .bc output
```

### 5.2 New Intermediate Representation: LIR (Low-level IR)

Between CLM and LLVM IR, we introduce **LIR** — a flat, first-order representation where:
- All closures are defunctionalized or closure-converted
- All polymorphism is monomorphized or uses witness tables
- All pattern matching is compiled to decision trees
- All RC operations are explicit
- All values have known representations (unboxed/tagged/heap)

```haskell
-- Proposed LIR types (src/LIR.hs)
data LIRExpr
    = LIRLit LIRLiteral                    -- immediate value
    | LIRVar Name LIRType                  -- typed variable reference
    | LIRCall Name [LIRExpr] LIRType       -- direct function call
    | LIRIndirectCall LIRExpr [LIRExpr] LIRType  -- call through function pointer
    | LIRAlloc LIRType [LIRExpr]           -- heap allocation
    | LIRLoad LIRExpr Int LIRType          -- field access (base, offset, type)
    | LIRStore LIRExpr Int LIRExpr         -- field store (base, offset, value)
    | LIRSwitch LIRExpr [(Int, LIRBlock)] LIRBlock  -- switch on tag
    | LIRBranch LIRExpr LIRBlock LIRBlock  -- conditional branch
    | LIRSeq [LIRExpr]                     -- sequential evaluation
    | LIRLet Name LIRExpr LIRExpr          -- let binding
    | LIRDup LIRExpr                       -- RC increment
    | LIRDrop LIRExpr                      -- RC decrement
    | LIRReuse LIRExpr Int [LIRExpr]       -- reuse allocation (old, new_tag, new_fields)
    | LIRError String                      -- runtime error
    deriving (Show, Eq)

data LIRType
    = LIRInt64 | LIRFloat64 | LIRFloat32
    | LIRInt8 | LIRInt16 | LIRInt32
    | LIRWord8 | LIRWord16 | LIRWord32 | LIRWord64
    | LIRChar | LIRBool
    | LIRPtr LIRType                       -- heap pointer to type
    | LIRTagged                            -- tagged pointer (polymorphic)
    | LIRStruct [LIRType]                  -- known-layout struct
    | LIRFuncPtr [LIRType] LIRType         -- function pointer type
    | LIRVoid
    | LIRVec Int LIRType                   -- SIMD vector (count, element type)
    deriving (Show, Eq)

data LIRLiteral
    = LIRLitInt Int64
    | LIRLitFloat Double
    | LIRLitFloat32 Float
    | LIRLitChar Char
    | LIRLitBool Bool
    | LIRLitString String
    deriving (Show, Eq)

type LIRBlock = [LIRExpr]

data LIRFunc = LIRFunc
    { lirFuncName   :: Name
    , lirFuncParams :: [(Name, LIRType)]
    , lirFuncReturn :: LIRType
    , lirFuncBody   :: LIRBlock
    , lirFuncAttrs  :: [LIRFuncAttr]
    } deriving (Show, Eq)

data LIRFuncAttr
    = LIRInline         -- always inline
    | LIRNoInline       -- never inline
    | LIRPure           -- no side effects
    | LIRBorrowed Int   -- parameter at index is borrowed (no RC)
    deriving (Show, Eq)
```

### 5.3 Module Structure

```haskell
-- New source files for the LLVM backend:
src/
  DemandAnalysis.hs    -- Pass 6: demand/absence analysis
  Monomorphize.hs      -- Pass 7: type specialization
  Defunctionalize.hs   -- Pass 8: closure elimination
  WorkerWrapper.hs     -- Pass 9: unboxing transformation
  PerceusRC.hs         -- Pass 10: reference counting insertion
  DecisionTree.hs      -- Pass 11: pattern match compilation
  LIR.hs               -- Low-level IR types
  CLMToLIR.hs          -- CLM → LIR lowering (orchestrates passes 6-11)
  LIRToLLVM.hs         -- LIR → LLVM IR emission
  LLVMRuntime.hs       -- Runtime library generation (RC, alloc, error handling)
  CodegenDriver.hs     -- Top-level codegen orchestration
```

---

## 6. Memory Representation

### 6.1 Three-Tier Value Representation

Values exist in one of three tiers, chosen at compile time based on type information after monomorphization:

#### Tier 1: Unboxed (Registers / Stack)

For primitive types and small known-shape values. **Zero heap allocation.**

| Type | LLVM Type | Size | Notes |
|------|-----------|------|-------|
| `Int` | `i64` | 8B | Machine integer |
| `Float64` | `double` | 8B | IEEE 754 double |
| `Float32` | `float` | 4B | IEEE 754 single |
| `Int8` | `i8` | 1B | Signed byte |
| `Int16` | `i16` | 2B | |
| `Int32` | `i32` | 4B | |
| `Int64` | `i64` | 8B | Same as Int |
| `UInt8` / `Byte` | `i8` | 1B | Unsigned byte |
| `UInt16` | `i16` | 2B | |
| `UInt32` | `i32` | 4B | |
| `UInt64` | `i64` | 8B | |
| `Char` | `i32` | 4B | Unicode code point |
| `Bool` | `i1` | 1b | Single bit |
| `Unit` | `void` | 0B | Erased entirely |

**Compound unboxing** (after monomorphization + worker/wrapper):

```
-- tulam: {x:Int, y:Float64}
-- Unboxed: two separate values (i64, double) in registers
-- No allocation, no indirection

-- tulam: Maybe(Int) after mono
-- Unboxed: {tag: i1, val: i64} — 9 bytes on stack, no heap
-- 'Nothing' = {0, undef}, 'Just(n)' = {1, n}
```

#### Tier 2: Tagged Pointer

For polymorphic boundaries where values must have a uniform representation. Lean 4-style pointer tagging:

```
63                              2  1  0
┌──────────────────────────────┬──┬──┐
│         payload (61 bits)    │tt│ 1│  ← immediate (tag bit = 1)
└──────────────────────────────┴──┴──┘

63                              2  1  0
┌──────────────────────────────┬──┬──┐
│     heap pointer (aligned)   │  │ 0│  ← heap object (tag bit = 0)
└──────────────────────────────┴──┴──┘

Tag bits (tt) for immediates:
  00 = small integer (61-bit signed, range ±2^60)
  01 = character (Unicode code point, 21 bits used)
  10 = boolean/small enum (0=False, 1=True, or small enum tag)
  11 = reserved (future: Float32, small constructors)
```

This means:
- `Int` values in range [-2^60, 2^60) → **zero heap allocation** even in polymorphic code
- `Char` → always immediate (21-bit code point fits easily)
- `Bool` → always immediate
- Small enum types (up to ~2^59 constructors) → immediate

#### Tier 3: Heap Object

For large constructors, arrays, strings, and closures:

```
┌──────────────────────────────────────────────────────┐
│ Header (8 bytes)                                     │
│  ┌─────────────────┬────────┬────────┬──────────────┐│
│  │ constructor_tag  │ arity  │ flags  │ refcount     ││
│  │ u32              │ u8     │ u8     │ u16          ││
│  └─────────────────┴────────┴────────┴──────────────┘│
├──────────────────────────────────────────────────────┤
│ Field 0 : 8 bytes (tagged pointer OR unboxed value)  │
│ Field 1 : 8 bytes                                    │
│ ...                                                  │
│ Field (arity-1) : 8 bytes                            │
└──────────────────────────────────────────────────────┘
```

**Header flags byte:**

| Bit | Meaning |
|-----|---------|
| 0 | `is_forwarding` — used during RC cycle breaking |
| 1 | `is_lazy_thunk` — lazy value, may need forcing |
| 2 | `is_evaluated` — thunk has been forced (value cached) |
| 3 | `is_array` — payload is a contiguous array, not fields |
| 4-7 | Reserved |

**After monomorphization**, the field types are known, enabling unboxed field storage:

```
-- Polymorphic (Tier 2 fields): Pair(a, b)
┌────────┬──────────────────┬──────────────────┐
│ header │ field0: tagged   │ field1: tagged   │
│ 8B     │ 8B               │ 8B               │
└────────┴──────────────────┴──────────────────┘
Total: 24 bytes, fields are tagged pointers

-- Monomorphized (Tier 1 fields): Pair(Int, Float64) = Pair_Int_Float64
┌────────┬──────────────────┬──────────────────┐
│ header │ field0: i64      │ field1: double   │
│ 8B     │ 8B (raw int)     │ 8B (raw double)  │
└────────┴──────────────────┴──────────────────┘
Total: 24 bytes, fields are raw machine values (no tag overhead)

-- After worker/wrapper (fully unboxed):
-- No heap allocation at all — just two registers: (i64, double)
```

### 6.2 Array Representation

```
┌──────────────────────────────────────────────┐
│ Header (8 bytes)                             │
│  constructor_tag = ARRAY_TAG                 │
│  arity = 0 (payload is contiguous)           │
│  flags = 0x08 (is_array)                     │
│  refcount                                    │
├──────────────────────────────────────────────┤
│ Length : i64 (8 bytes)                       │
├──────────────────────────────────────────────┤
│ Capacity : i64 (8 bytes, for push/growth)    │
├──────────────────────────────────────────────┤
│ Element 0 : element_size bytes               │
│ Element 1 : element_size bytes               │
│ ...                                          │
│ Element (length-1) : element_size bytes      │
└──────────────────────────────────────────────┘
```

After monomorphization, element_size is known:
- `Array(Int)` → 8 bytes per element, no tagging
- `Array(Bool)` → 1 byte per element (packed)
- `Array(a)` (polymorphic) → 8 bytes per element (tagged pointers)

### 6.3 String Representation

The built-in `String` type compiles to a UTF-8 byte array:

```
┌──────────┬──────────┬──────────┬────────────────────┐
│ header   │ length   │ capacity │ UTF-8 bytes...     │
│ 8B       │ 8B       │ 8B       │ length bytes       │
└──────────┴──────────┴──────────┴────────────────────┘
```

Small string optimization (SSO): strings ≤ 22 bytes stored inline in the tagged pointer (no heap allocation). This covers most identifiers, keywords, and short messages.

---

## 7. Closure Representation & Defunctionalization

### 7.1 Defunctionalization Strategy

Based on Reynolds (1972) and MLton's lightweight defunctionalization (Cejtin et al., 2000).

**Phase 1: Identify all lambda creation sites**

Each `CLMLAM` in the program is assigned a unique closure tag. Free variables become constructor fields:

```
// tulam source:
function addN(n:Int) : Int -> Int = \x -> x + n;
function mulN(n:Int) : Int -> Int = \x -> x * n;

// After defunctionalization:
// Closure constructors (reuse CLMCON tag infrastructure):
//   Closure_addN = ConsTag "Closure_addN" 1000, fields: [n:Int]
//   Closure_mulN = ConsTag "Closure_mulN" 1001, fields: [n:Int]

// Dispatch function:
function apply_IntToInt(closure, x:Int) : Int = match closure
    | Closure_addN(n) -> x + n
    | Closure_mulN(n) -> x * n;
```

**Phase 2: Replace lambda creation with constructor creation**

```
-- Before:
addN(5)  -- creates CLMLAM closure

-- After:
Closure_addN(5)  -- creates CLMCON, no lambda
```

**Phase 3: Replace indirect calls with dispatch**

```
-- Before:
f(3)  -- indirect call through closure

-- After:
apply_IntToInt(f, 3)  -- direct call to dispatch function
```

### 7.2 When Defunctionalization is NOT Applied

Some closures cannot be defunctionalized:
- **Closures crossing module boundaries** in polymorphic positions
- **Closures stored in polymorphic data structures** (e.g., `List(a -> b)`)
- **Closures received from extern/FFI**

These retain the flat closure representation:

```
┌──────────────────────────────┐
│ Header (8 bytes)             │
│  tag = CLOSURE_TAG           │
│  flags.is_closure = 1        │
│  refcount                    │
├──────────────────────────────┤
│ Function pointer : ptr       │
│ Arity : u16                  │
│ Num captured : u16           │
├──────────────────────────────┤
│ Captured var 0 : tagged      │
│ Captured var 1 : tagged      │
│ ...                          │
└──────────────────────────────┘
```

### 7.3 Partial Application

Partial application creates a new closure containing the already-applied arguments:

```
// tulam: map(f)  -- partial application, missing array argument
// f is Closure_addN(5)

// After defunc:
// Creates: PAP_map { func: Closure_addN(5) }
// When called with remaining arg (arr):
//   map(PAP_map.func, arr) — direct call
```

For monomorphized call sites, partial application is often eliminated entirely by inlining.

---

## 8. Memory Management: Perceus Reference Counting

### 8.1 Why Perceus over Tracing GC

Based on Reinking, Xie, de Moura, Leijen — *"Perceus: Garbage-Free Reference Counting with Reuse"* (ICFP 2021).

| Property | Tracing GC | Perceus RC |
|----------|-----------|------------|
| **Pause times** | Unpredictable (ms to 100ms) | Zero (deterministic deallocation) |
| **Memory overhead** | ~2x live data (copying GC) | Near-optimal (immediate reclaim) |
| **Cache behavior** | Poor (copying scatters data) | Good (allocate/free in sequence) |
| **Cycle handling** | Automatic (trace finds cycles) | Requires cycle-breaking (rare for FP) |
| **Unboxing compatibility** | Complex (GC must know layouts) | Simple (RC only on heap objects) |
| **Implementation complexity** | High (write barriers, card tables, generations) | Moderate (dup/drop insertion) |

**Why cycles are rare in tulam:**
- tulam is strict → no thunk chains → no lazy-induced cycles
- Algebraic data types are acyclic by construction (constructors are values)
- Mutable references (`Ref`, `MutArray`) are the only cycle source → handled by weak references or explicit cycle breaking

### 8.2 Perceus Operations

```
dup(x)    — increment refcount of x (compile-time inserted)
drop(x)   — decrement refcount; if 0, recursively drop fields and free
reset(x)  — if refcount == 1, mark x for in-place reuse; else drop(x)
reuse(x, tag, fields...) — if x was reset (reusable), write new tag+fields in place;
                           else allocate fresh
```

### 8.3 RC Insertion Algorithm

For each function body, compute liveness and insert RC operations:

```
1. For each variable binding:
   a. If variable is used N times in the body:
      - If N > 1: insert dup(x) at binding site, producing N references
      - If N == 1: no dup needed (ownership transfers)
      - If N == 0: insert drop(x) immediately (absent — or erase entirely)

2. For function parameters:
   a. If parameter is BORROWED (only read, never stored):
      - No dup at entry, no drop at exit
      - Caller retains ownership
   b. If parameter is OWNED:
      - Caller transfers ownership
      - Callee responsible for drop at last use

3. At each branch point (match/if):
   a. Variables used in only one branch: drop in the other branches
   b. Variables used in all branches: no extra ops needed
```

### 8.4 Reuse Analysis

The key optimization for functional data structure code. When a constructor is consumed (pattern-matched) and a same-shaped constructor is immediately built:

```
// tulam:
function map(f, xs:List(a)) : List(a) = match
    | f, Nil -> Nil
    | f, Cons(h, t) -> Cons(f(h), map(f, t));

// After Perceus RC insertion:
function map(f, xs) =
    dup(f);                          // f used in both f(h) and recursive map(f, t)
    match xs
    | Nil ->
        drop(f);                     // f unused on Nil path
        Nil                          // allocate Nil (or reuse xs — same shape!)
    | Cons(h, t) ->
        let w = reset(xs);           // xs consumed here; if unshared, mark for reuse
        let h' = f(h);
        let t' = map(f, t);
        drop(f);                     // last use of f
        reuse(w, Cons, h', t');      // reuse xs's memory if it was unshared
```

**Reuse eligibility**: two constructors are same-shaped if they have the same number of fields and all fields are the same size (8 bytes for tagged pointers, or matching unboxed sizes after monomorphization).

### 8.5 Borrowing Optimization

Many function parameters are only *observed* (read), not *stored* (captured in a closure or data structure). For these, skip RC entirely:

```
// tulam:
function length(xs:List(a)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> 1 + length(t);

// xs is only pattern-matched, never stored → BORROWED
// h is only ignored → BORROWED (or ABSENT)
// t is passed to recursive call → BORROWED (ownership stays with caller)

// After Perceus with borrowing:
function length(xs) =      // xs: borrowed (no dup/drop)
    match xs
    | Nil -> 0
    | Cons(h, t) -> 1 + length(t)    // t: borrowed pass-through
    // No RC operations at all! Zero overhead.
```

### 8.6 Performance Impact

| Operation | Cost with Tracing GC | Cost with Perceus |
|-----------|---------------------|-------------------|
| Allocate small object | Fast (bump allocator) | Fast (free-list or reuse) |
| Free small object | Deferred (next GC) | Immediate (on drop) |
| `map f xs` (1M elements) | 1M allocs + 1M GC reclaims | 0 allocs (in-place reuse) |
| `length xs` | 0 allocs (but GC still scans) | 0 allocs, 0 RC ops (borrowed) |
| Numeric loop (no allocs) | Zero cost | Zero cost |

---

## 9. Monomorphization

### 9.1 Strategy

Monomorphization specializes polymorphic functions to concrete types, eliminating all dispatch overhead. This is the **single biggest performance win** for tulam.

**tulam-specific advantage**: `CLMIAP` (implicit-param application) explicitly marks dispatch points. Every `CLMIAP` is a monomorphization candidate.

### 9.2 Algorithm

```
Input: clmLambdas, clmInstances, call graph
Output: specialized clmLambdas with CLMIAP → CLMAPP rewrites

Phase 1: Collect specialization targets
    For each CLMIAP in all function bodies:
        Determine concrete type(s) from:
          a. Literal arguments (CLMLIT → type is known)
          b. Constructor arguments (CLMCON → type from ConsTag)
          c. Caller context (return type annotation, CLMTYPED hint)
        If all type params known → mark for specialization

Phase 2: Generate specialized variants
    For each (function, type_args) pair:
        Clone the function body
        Substitute type parameters with concrete types
        Rename: "funcName" → "funcName$Type1$Type2"
        If the function was an instance method:
            Inline the instance body (no dispatch needed)

Phase 3: Rewrite call sites
    Replace CLMIAP(funcName, args) → CLMAPP(funcName$Types, args)

Phase 4: Deduplication
    If two specializations produce identical code (common for
    pointer-sized types), merge them under a shared name.

Phase 5: Dead code elimination
    Remove unspecialized originals if no polymorphic callers remain.
    Keep originals that are:
      a. Exported from the module
      b. Used in existential/polymorphic contexts
      c. Stored in data structures as function values
```

### 9.3 Example

```
// tulam: algebra Eq(a) with instance Eq(Int) = intrinsic;
// function allEq(xs:List(a)) : Bool = match
//     | Nil -> True
//     | Cons(h, Cons(h2, t)) -> (h == h2) && allEq(Cons(h2, t))
//     | Cons(h, Nil) -> True;

// Call site: allEq(Cons(1, Cons(2, Cons(3, Nil))))

// After monomorphization:
function allEq$Int(xs:List(Int)) : Bool = match
    | Nil -> True
    | Cons(h, Cons(h2, t)) -> int_eq(h, h2) && allEq$Int(Cons(h2, t))
    //                        ^^^^^^^^^^^^^^ direct intrinsic call!
    | Cons(h, Nil) -> True;

// The CLMIAP for (==) is now a CLMAPP to int_eq — zero dispatch overhead.
```

### 9.4 Witness Tables (Polymorphic Fallback)

For genuinely polymorphic code (library exports, existentials), use Swift-style witness tables:

```
// Witness table struct:
struct Eq_witness {
    bool (*eq)(tagged_ptr, tagged_ptr);
    bool (*neq)(tagged_ptr, tagged_ptr);
};

// Concrete witness:
static Eq_witness Eq_Int = { .eq = int_eq_boxed, .neq = int_neq_boxed };

// Polymorphic function receives witness table as hidden first parameter:
bool allEq_poly(Eq_witness* w, tagged_ptr xs) {
    // ... w->eq(h, h2) ...
}

// At monomorphized call sites, the witness table is eliminated:
allEq(Cons(1, Cons(2, Nil)))
  → allEq$Int(Cons(1, Cons(2, Nil)))  // no witness table, direct call
```

### 9.5 Monomorphization vs Code Size

| Strategy | Call Overhead | Code Size | Compile Time |
|----------|-------------|-----------|-------------|
| Full monomorphization (MLton) | Zero | O(types x functions) | Slow (whole-program) |
| Dictionary passing (GHC) | 1 indirect call per method | O(1) per function | Fast |
| Witness tables (Swift) | 0-1 indirect calls | Moderate | Moderate |
| **tulam (mono + witness fallback)** | **Zero (known) / 1 (generic)** | **Moderate (dedup)** | **Moderate** |

Deduplication keeps code size manageable: specializations that produce identical machine code (common for pointer-sized types like `List(a)` instantiated at `List(Int)` vs `List(String)`) share a single implementation.

---

## 10. Unboxing & Worker/Wrapper

### 10.1 Worker/Wrapper Transformation

Based on Gill & Hutton (JFP 2009). Split functions into:
- **Wrapper**: public interface, handles boxing/unboxing at polymorphic boundaries
- **Worker**: internal, operates entirely on unboxed values

```
// tulam:
function dot(a:{x:Float64, y:Float64}, b:{x:Float64, y:Float64}) : Float64 =
    a.x * b.x + a.y * b.y;

// After worker/wrapper:

// Worker (what actually executes — pure registers, zero allocation):
define double @dot_worker(double %ax, double %ay, double %bx, double %by) {
    %t1 = fmul double %ax, %bx
    %t2 = fmul double %ay, %by
    %r  = fadd double %t1, %t2
    ret double %r
}

// Wrapper (only at polymorphic boundaries — inlined and eliminated when possible):
define %tagged @dot(%tagged %a, %tagged %b) {
    %ax = call double @unbox_float(%a, 0)
    %ay = call double @unbox_float(%a, 1)
    %bx = call double @unbox_float(%b, 0)
    %by = call double @unbox_float(%b, 1)
    %r  = call double @dot_worker(%ax, %ay, %bx, %by)
    ret %tagged (call %tagged @box_float(%r))
}
```

At monomorphized call sites:
```
// tulam: dot({1.0, 2.0}, {3.0, 4.0})
// Compiles to (after inlining wrapper + constructor cancellation):
call double @dot_worker(1.0, 2.0, 3.0, 4.0)
// Zero heap allocation. Same code as C.
```

### 10.2 Unboxing Rules

| Type Pattern | Unboxing | Representation |
|-------------|----------|----------------|
| Primitive type (`Int`, `Float64`, etc.) | Always unbox | Machine register |
| Single-constructor, single-field (`type Wrapper = val:Int`) | Newtype elimination | Same as inner type |
| Single-constructor, multi-field (`type Point = x:Float64 * y:Float64`) | Flatten to N registers | N machine values |
| Multi-constructor, all-primitive fields (`Maybe(Int)`) | Tag + fields in registers | `{i1, i64}` or `{i32, i64}` |
| Multi-constructor, mixed fields (`Either(Int, String)`) | Tag on stack, fields boxed | `{i32, tagged}` |
| Recursive type (`List(a)`) | Cannot fully unbox | Heap-allocated |
| Array | Header + contiguous elements | Heap-allocated |

### 10.3 Absent Field Elimination

From demand analysis: fields that are never accessed are erased entirely.

```
// tulam:
function getX(p:{x:Int, y:Int, z:Int}) : Int = p.x;

// Demand: p -> <S(S, A, A)> — only x is used
// After worker/wrapper:
define i64 @getX_worker(i64 %x) {
    ret i64 %x
}
// The tuple construction + y/z fields are completely eliminated
```

---

## 11. Pattern Matching Compilation

### 11.1 From CLMCASE to Decision Trees

tulam's CLM pattern matching:
```haskell
CLMLamCases [vars] [
    CLMCASE [CLMCheckTag (ConsTag "Nil" 0) scrutinee] body1,
    CLMCASE [CLMCheckTag (ConsTag "Cons" 1) scrutinee] body2
]
```

Compiles to an optimized decision tree:

```
// Step 1: Extract tag from scrutinee
%tag = load i32, ptr %scrutinee  // read constructor_tag from header

// Step 2: Switch on tag
switch i32 %tag, label %unreachable [
    i32 0, label %case_Nil     ; Nil
    i32 1, label %case_Cons    ; Cons
]

case_Nil:
    ... body1 ...
    br label %merge

case_Cons:
    %h = getelementptr ptr, ptr %scrutinee, i64 1  ; field 0
    %t = getelementptr ptr, ptr %scrutinee, i64 2  ; field 1
    ... body2 using %h, %t ...
    br label %merge
```

### 11.2 Nested Pattern Compilation

For nested patterns, compile to a multi-level decision tree (Maranget, JFP 2008):

```
// tulam:
match xs
    | Cons(0, t) -> ...       // literal check nested in constructor
    | Cons(h, Nil) -> ...     // constructor check nested in constructor
    | Cons(h, t) -> ...       // catch-all
    | Nil -> ...

// Decision tree:
switch xs.tag:
    case 0 (Nil): → body4
    case 1 (Cons):
        %h = xs.field[0]
        %t = xs.field[1]
        if %h == 0:           // literal check
            → body1
        else:
            switch %t.tag:
                case 0 (Nil): → body2
                default:      → body3
```

### 11.3 Points-to Analysis for Branch Elimination

Using GRIN-style heap points-to analysis (Boquist 1999):

```
// If analysis proves: xs always points to Cons(_, _) at this call site
// Then: Nil branch is dead → emit only Cons case (no switch needed)

// Before:
switch xs.tag [Nil → ..., Cons → ...]

// After points-to:
// xs is always Cons → direct field access, no branch
%h = load ptr, ptr (xs + 8)
%t = load ptr, ptr (xs + 16)
... Cons body ...
```

### 11.4 Jump Table Optimization

For types with many constructors (e.g., large enums), use LLVM's switch instruction which compiles to a jump table:

```
// tulam: type Color = Red + Green + Blue + Yellow + Cyan + Magenta + White + Black;
// 8 constructors → dense range [0, 7] → jump table

switch i32 %tag, label %unreachable [
    i32 0, label %red
    i32 1, label %green
    i32 2, label %blue
    i32 3, label %yellow
    i32 4, label %cyan
    i32 5, label %magenta
    i32 6, label %white
    i32 7, label %black
]
// LLVM compiles this to a single indexed branch — O(1) dispatch
```

---

## 12. Algebra/Structure Dispatch Compilation

### 12.1 Three Compilation Strategies

Algebra method calls (`CLMIAP`) compile differently based on type knowledge:

| Type Knowledge | Strategy | Runtime Cost |
|---------------|----------|-------------|
| **Fully known** (after monomorphization) | Direct call to instance method | Zero (same as `CLMAPP`) |
| **Partially known** (one of N possible types) | Switch on type tag | O(1) switch |
| **Unknown** (polymorphic boundary) | Witness table indirect call | 1 indirect call |

### 12.2 Fully Monomorphized (Most Common Case)

```
// tulam: x + y  where x, y : Int
// CLM: CLMIAP (CLMID "+") [CLMID "x", CLMID "y"]

// After monomorphization:
// CLMAPP (CLMID "+$Int") [CLMID "x", CLMID "y"]

// LLVM IR:
%result = add i64 %x, %y    // Single machine instruction!
```

### 12.3 Switch Dispatch (Known Set of Types)

```
// tulam: show(x)  where x : Int | Float64 (from branch analysis)

// LLVM IR:
%tag = call i32 @infer_type_tag(%x)
switch i32 %tag, label %error [
    i32 0, label %show_int
    i32 1, label %show_float
]
show_int:
    %r1 = call %string @show$Int(%x)
    br label %merge
show_float:
    %r2 = call %string @show$Float64(%x)
    br label %merge
```

### 12.4 Witness Table Dispatch (Polymorphic)

```
// tulam: function printAll [a:Type] (xs:List(a)) : Unit  -- requires Show(a)
// Compiled with hidden witness table parameter:

// Witness table type:
%Show_witness = type { ptr }  ; { show: (tagged) -> string }

// Function signature:
define void @printAll(%Show_witness* %w, %tagged %xs) {
    ...
    %shown = call %string %w.show(%elem)  ; indirect call through witness
    ...
}

// At known call site: printAll(intList)
// → printAll(&Show_Int_witness, intList)   ; concrete witness, could inline
```

---

## 13. Class System Compilation

### 13.1 Object Layout

tulam classes compile to tagged constructors with method dispatch via vtable:

```
// tulam:
// class Animal(name:String) = {
//     function speak(self) : String = "...";
// };
// class Dog(breed:String) extends Animal = {
//     override function speak(self) : String = "Woof!";
// };

// Object layout (parent-first):
// Dog instance: CLMCON (ConsTag "Dog" tag) [name_string, breed_string]
//
// LLVM struct:
%Dog = type {
    i32,       ; constructor tag
    %String*,  ; field 0: name (inherited from Animal)
    %String*   ; field 1: breed (own field)
}
```

### 13.2 Method Dispatch

Method dispatch uses `ClassMeta.cmMethods` — a pre-merged HashMap of all methods including inherited ones:

```
// CLMMCALL obj "speak" []

// After compilation:
// 1. Extract class tag from object header
// 2. Index into class dispatch table
// 3. Call method

%tag = extractvalue %obj, 0
%method = getelementptr %dispatch_table, %tag, %speak_idx
call %String (%method)(%obj)
```

For `sealed` classes, the set of subclasses is known → dispatch compiles to a switch:

```
// sealed class Shape = { area(self):Float64 }
// class Circle extends Shape ...
// class Rect extends Shape ...

switch %tag [
    Circle_tag → call @Circle_area(%obj)
    Rect_tag   → call @Rect_area(%obj)
]
// Closed set → no default case needed (exhaustive)
```

### 13.3 `abstract` and `final` Optimizations

- `abstract` classes: constructor calls are compile-time errors (no CLMNEW)
- `final` methods: always direct call (no dispatch needed)
- `static` methods: plain function call (no `self` parameter)

---

## 14. Effect Handler Compilation

### 14.1 Current CLM Representation

```haskell
CLMHANDLE body effectName [(letName, letExpr)] [(opName, implementation)]
```

### 14.2 Compilation Strategy

Based on Xie et al. (2020) — *"Effect Handlers, Evidently"*:

**Option A: CPS Transformation** (simpler, moderate performance)
- Transform effect operations into continuation-passing style
- Handler becomes a function that receives the continuation
- Good for: simple effects (State, Reader, Exception)

**Option B: Segmented Stacks** (more complex, better performance)
- Each handler gets a stack segment
- Effect operation saves current continuation on the segment
- Resume restores the continuation
- Good for: coroutine-like effects, async/await

**Recommended for Phase A**: CPS transformation (simpler to implement, sufficient for initial backend). Migrate to segmented stacks if benchmarks show it matters.

```
// tulam:
// handle { putStrLn("hello"); readLine() }
//   with ConsoleHandler { putStrLn(s) = ...; readLine() = ... }

// After CPS:
ConsoleHandler_putStrLn("hello", \() ->
    ConsoleHandler_readLine(\line ->
        line))

// LLVM: standard function calls with continuation closures
```

---

## 15. SIMD & Numeric Fast Paths

### 15.1 SIMD Type Mapping

tulam's `Vec2`/`Vec4`/`Vec8`/`Vec16` types map directly to LLVM vector types:

| tulam Type | LLVM Type | Hardware |
|-----------|-----------|----------|
| `Vec2(Float64)` | `<2 x double>` | SSE2 (128-bit) |
| `Vec4(Float32)` | `<4 x float>` | SSE (128-bit) |
| `Vec4(Float64)` | `<4 x double>` | AVX2 (256-bit) |
| `Vec8(Float32)` | `<8 x float>` | AVX2 (256-bit) |
| `Vec16(Float32)` | `<16 x float>` | AVX-512 (512-bit) |

### 15.2 Lane Algebra Compilation

```
// tulam: instance Lane(Vec4) = intrinsic;
// splat(1.0) : Vec4(Float64)

// LLVM IR:
%v = insertelement <4 x double> undef, double 1.0, i32 0
%v2 = shufflevector <4 x double> %v, <4 x double> undef,
                    <4 x i32> zeroinitializer
// → <1.0, 1.0, 1.0, 1.0>
```

### 15.3 Auto-Vectorization Hints

For loops over `Array(Float64)`, emit LLVM metadata to enable auto-vectorization:

```llvm
; Loop over array with vectorization hint
loop:
    %i = phi i64 [0, %entry], [%i.next, %loop]
    %elem = load double, ptr %arr_ptr
    %result = fmul double %elem, %scalar
    store double %result, ptr %out_ptr
    %i.next = add i64 %i, 1
    %cond = icmp slt i64 %i.next, %len
    br i1 %cond, label %loop, label %exit, !llvm.loop !1

!1 = !{!1, !2}
!2 = !{!"llvm.loop.vectorize.enable", i1 true}
```

### 15.4 Math Intrinsic Mapping

| tulam (via algebras) | LLVM Intrinsic |
|---------------------|---------------|
| `sqrt(x:Float64)` | `@llvm.sqrt.f64` |
| `sin(x:Float64)` | `@llvm.sin.f64` |
| `cos(x:Float64)` | `@llvm.cos.f64` |
| `exp(x:Float64)` | `@llvm.exp.f64` |
| `log(x:Float64)` | `@llvm.log.f64` |
| `pow(x:Float64, y:Float64)` | `@llvm.pow.f64` |
| `abs(x:Float64)` | `@llvm.fabs.f64` |
| `abs(x:Int)` | Select on sign bit |
| `x + y` (Int) | `add i64` |
| `x * y` (Float64) | `fmul double` |
| `x .&. y` (Int) | `and i64` |
| `shiftL(x, n)` (Int) | `shl i64` |

After monomorphization, these are emitted as **single LLVM instructions** — zero dispatch overhead.

---

## 16. CLM IR Reference for Codegen

### 16.1 Complete CLM Node Types

```haskell
data CLMExpr
    = CLMEMPTY                                    -- erased (unit/type-level)
    | CLMERR String SourceInfo                    -- runtime error with location
    | CLMID Name                                  -- variable reference
    | CLMLAM CLMLam                               -- lambda abstraction
    | CLMBIND Name CLMExpr                        -- local binding
    | CLMAPP CLMExpr [CLMExpr]                    -- direct application (no dispatch)
    | CLMPAP CLMExpr [CLMExpr]                    -- partial application
    | CLMCON ConsTag [CLMExpr]                    -- constructor (tag + fields)
    | CLMIAP CLMExpr [CLMExpr]                    -- implicit-param dispatch
    | CLMFieldAccess (Name, Int) CLMExpr          -- field access (name, index)
    | CLMCASE [CLMPatternCheck] CLMExpr           -- pattern guard → body
    | CLMPROG [CLMExpr]                           -- sequential composition
    | CLMTYPED CLMExpr CLMExpr                    -- type-annotated expression
    | CLMPRIMCALL                                 -- intrinsic body marker
    | CLMLIT Literal                              -- literal value
    | CLMU Level                                  -- universe level
    | CLMARRAY [CLMExpr]                          -- immutable array
    | CLMREF Int                                  -- mutable reference handle
    | CLMMUTARRAY Int                             -- mutable array handle
    | CLMMCALL CLMExpr Name [CLMExpr]             -- method call (obj.method(args))
    | CLMSCALL CLMExpr Name [CLMExpr]             -- super method call
    | CLMNEW Name [CLMExpr]                       -- class construction
    | CLMHANDLE CLMExpr Name [(Name,CLMExpr)] [(Name,CLMExpr)]
                                                  -- effect handler
```

### 16.2 CLM → LIR Mapping

| CLM Node | LIR Lowering | Notes |
|----------|-------------|-------|
| `CLMEMPTY` | Erase (void/undef) | Type-level computation result |
| `CLMERR msg si` | `LIRError msg` | Runtime error with source location |
| `CLMID name` | `LIRVar name type` | Variable lookup (type from analysis) |
| `CLMLAM (CLMLam vars body)` | Closure allocation or defunc constructor | Depends on escape analysis |
| `CLMBIND name val` | `LIRLet name val body` | Local binding |
| `CLMAPP func args` | `LIRCall funcName args retType` | Direct call (function known) |
| `CLMPAP func args` | Closure with partial args | Or inline if arity known |
| `CLMCON tag fields` | `LIRAlloc structType fields` or unboxed | Tier 1/2/3 based on type |
| `CLMIAP func args` | **After mono**: `LIRCall specialized args` | Key dispatch resolution point |
| `CLMIAP func args` | **Polymorphic**: `LIRIndirectCall witness.method args` | Witness table fallback |
| `CLMFieldAccess (_, idx) expr` | `LIRLoad expr idx fieldType` | Direct offset load |
| `CLMCASE checks body` | `LIRSwitch` or `LIRBranch` | Decision tree |
| `CLMPROG exprs` | `LIRSeq exprs` | Sequential evaluation |
| `CLMTYPED inner hint` | Same as inner (hint used during mono) | Type hint consumed by Pass 7 |
| `CLMPRIMCALL` | N/A (replaced during CLM conversion) | Never reaches codegen |
| `CLMLIT (LInt n)` | `LIRLit (LIRLitInt n)` | Immediate value |
| `CLMLIT (LFloat f)` | `LIRLit (LIRLitFloat f)` | Immediate value |
| `CLMLIT (LString s)` | Global string constant | Statically allocated |
| `CLMU level` | Erase | Type-level only |
| `CLMARRAY elems` | Array allocation + initialization | Heap object |
| `CLMREF _` | N/A (interpreter-only) | Reimplement for native |
| `CLMMUTARRAY _` | N/A (interpreter-only) | Reimplement for native |
| `CLMMCALL obj meth args` | Vtable dispatch or switch | Based on sealed/final |
| `CLMSCALL obj meth args` | Direct call to parent method | Always known |
| `CLMNEW cls args` | Allocation + field init | Tier 2/3 |
| `CLMHANDLE body eff lets ops` | CPS or segmented stack | Phase-dependent |

### 16.3 Literal Types

```haskell
data Literal
    = LInt !Int          -- → i64
    | LFloat !Double     -- → double
    | LChar !Char        -- → i32 (Unicode code point)
    | LString !String    -- → global constant string
    | LInt8 !Int8        -- → i8
    | LInt16 !Int16      -- → i16
    | LInt32 !Int32      -- → i32
    | LInt64 !Int64      -- → i64
    | LWord8 !Word8      -- → i8
    | LWord16 !Word16    -- → i16
    | LWord32 !Word32    -- → i32
    | LWord64 !Word64    -- → i64
    | LFloat32 !Float    -- → float
    | LList [Expr]       -- → array/list construction
    | LVec [Expr]        -- → LLVM vector
    | LNTuple [...]      -- → struct/tuple
```

---

## 17. LLVM IR Mapping

### 17.1 Runtime Types

```llvm
; Tagged pointer (polymorphic values)
%tagged = type i64    ; bit 0: 0=pointer, 1=immediate

; Heap object header
%obj_header = type {
    i32,    ; constructor_tag
    i8,     ; arity
    i8,     ; flags
    i16     ; refcount
}

; Generic heap object (fields are tagged pointers)
%heap_obj = type {
    %obj_header,
    [0 x %tagged]    ; variable-length field array
}

; Array object
%array_obj = type {
    %obj_header,
    i64,             ; length
    i64,             ; capacity
    [0 x %tagged]    ; elements (or unboxed after mono)
}

; Closure (for non-defunctionalized functions)
%closure = type {
    ptr,     ; function pointer
    i16,     ; arity
    i16,     ; num_captured
    [0 x %tagged]  ; captured variables
}
```

### 17.2 Runtime Functions

```llvm
; Memory management (Perceus)
declare ptr @tlm_alloc(i64 %size)           ; allocate heap object
declare void @tlm_dup(ptr %obj)             ; increment RC
declare void @tlm_drop(ptr %obj)            ; decrement RC, free if 0
declare ptr @tlm_reset(ptr %obj)            ; mark for reuse if RC==1
declare ptr @tlm_reuse(ptr %old, i32 %tag, i64 %size)  ; reuse or alloc

; Tagged pointer operations
declare %tagged @tlm_box_int(i64 %val)      ; int → tagged (may be immediate)
declare i64 @tlm_unbox_int(%tagged %val)    ; tagged → int
declare %tagged @tlm_box_float(double %val) ; float → tagged (always heap)
declare double @tlm_unbox_float(%tagged %val)
declare i1 @tlm_is_immediate(%tagged %val)  ; check tag bit
declare i32 @tlm_get_tag(ptr %obj)          ; extract constructor tag

; String operations
declare ptr @tlm_string_concat(ptr %a, ptr %b)
declare i64 @tlm_string_length(ptr %s)
; ... etc (compiled from StringOps/StringExt intrinsics)

; Array operations
declare ptr @tlm_array_alloc(i64 %len)
declare %tagged @tlm_array_index(ptr %arr, i64 %idx)
declare ptr @tlm_array_slice(ptr %arr, i64 %start, i64 %end)
; ... etc

; Error handling
declare void @tlm_error(ptr %msg) noreturn  ; runtime error
declare void @tlm_unreachable() noreturn    ; impossible branch
```

### 17.3 Example: Complete Function Compilation

```
// tulam source:
function fibonacci(n:Int) : Int = match
    | 0 -> 0
    | 1 -> 1
    | n -> fibonacci(n - 1) + fibonacci(n - 2);
```

```llvm
; After monomorphization + unboxing (no boxing at all):
define i64 @fibonacci(i64 %n) {
entry:
    switch i64 %n, label %default [
        i64 0, label %case_0
        i64 1, label %case_1
    ]

case_0:
    ret i64 0

case_1:
    ret i64 1

default:
    %n1 = sub i64 %n, 1
    %n2 = sub i64 %n, 2
    %fib1 = call i64 @fibonacci(i64 %n1)
    %fib2 = call i64 @fibonacci(i64 %n2)
    %result = add i64 %fib1, %fib2
    ret i64 %result
}
; This is identical to what a C compiler would produce!
```

### 17.4 Example: Data Structure with Perceus

```
// tulam:
function map(f: a -> b, xs:List(a)) : List(b) = match
    | f, Nil -> Nil
    | f, Cons(h, t) -> Cons(f(h), map(f, t));

// Call: map(\x -> x + 1, myList)  -- monomorphized to Int
```

```llvm
; After mono + defunc + Perceus:
define ptr @map$Int$AddOne(ptr %xs) {
entry:
    %tag = call i32 @tlm_get_tag(ptr %xs)
    switch i32 %tag, label %unreachable [
        i32 0, label %case_nil    ; Nil
        i32 1, label %case_cons   ; Cons
    ]

case_nil:
    ; xs is Nil — return Nil (can reuse xs if unshared)
    %w0 = call ptr @tlm_reset(ptr %xs)
    %nil = call ptr @tlm_reuse(ptr %w0, i32 0, i64 8)  ; Nil = just header
    ret ptr %nil

case_cons:
    ; Extract fields
    %h_ptr = getelementptr %heap_obj, ptr %xs, i32 0, i32 1, i64 0
    %h = load i64, ptr %h_ptr          ; h : Int (unboxed)
    %t_ptr = getelementptr %heap_obj, ptr %xs, i32 0, i32 1, i64 1
    %t = load ptr, ptr %t_ptr          ; t : List(Int)

    ; Reset xs for reuse (consumed here)
    call void @tlm_dup(ptr %t)         ; t needs to survive into recursive call
    %w = call ptr @tlm_reset(ptr %xs)  ; mark xs for reuse

    ; Apply function (inlined: x + 1)
    %h_new = add i64 %h, 1

    ; Recursive call
    %t_new = call ptr @map$Int$AddOne(ptr %t)

    ; Reuse xs's memory for new Cons cell
    %cons = call ptr @tlm_reuse(ptr %w, i32 1, i64 24)  ; Cons = header + 2 fields
    %f0 = getelementptr %heap_obj, ptr %cons, i32 0, i32 1, i64 0
    store i64 %h_new, ptr %f0
    %f1 = getelementptr %heap_obj, ptr %cons, i32 0, i32 1, i64 1
    store ptr %t_new, ptr %f1
    ret ptr %cons

unreachable:
    call void @tlm_unreachable()
    unreachable
}
; When myList is unshared: ZERO allocations — all Cons cells reused in-place!
```

---

## 18. Implementation Phases

### Phase A: Basic LLVM IR Emission (Foundation)

**Goal**: Compile a subset of CLM to working LLVM IR with a simple runtime.

| Task | Description | Complexity |
|------|-------------|-----------|
| A.1 | Create `src/LIR.hs` with LIR types | Low |
| A.2 | Create `src/LIRToLLVM.hs` — emit LLVM IR text from LIR | Medium |
| A.3 | Create `src/LLVMRuntime.hs` — minimal runtime (alloc, error) | Medium |
| A.4 | Create `src/CLMToLIR.hs` — direct CLM→LIR lowering (boxed, no optimizations) | High |
| A.5 | Compile & link with LLVM toolchain (`llc` + `clang`) | Medium |
| A.6 | Test: fibonacci, factorial, basic pattern matching | Low |

**Supported in Phase A**: `CLMLIT`, `CLMAPP`, `CLMCON`, `CLMCASE`, `CLMLAM` (simple), `CLMFieldAccess`, `CLMPROG`, `CLMERR`.

**Not supported in Phase A**: `CLMIAP` (use interpreter fallback), `CLMHANDLE`, `CLMTYPED`, classes.

**Memory management in Phase A**: Boehm GC (conservative, drop-in) as stopgap.

### Phase B: Decision Trees + Intrinsics

**Goal**: Efficient pattern matching and native math operations.

| Task | Description | Complexity |
|------|-------------|-----------|
| B.1 | Create `src/DecisionTree.hs` — compile CLMCASE to optimized switches | Medium |
| B.2 | Map arithmetic intrinsics to LLVM instructions | Medium |
| B.3 | Map comparison intrinsics to LLVM icmp/fcmp | Low |
| B.4 | Map bitwise intrinsics to LLVM and/or/xor/shl/shr | Low |
| B.5 | Map floating-point intrinsics to LLVM math intrinsics | Low |
| B.6 | Benchmark: numeric code should be within 2x of C | — |

### Phase C: Monomorphization

**Goal**: Eliminate `CLMIAP` dispatch for known types.

| Task | Description | Complexity |
|------|-------------|-----------|
| C.1 | Create `src/Monomorphize.hs` — collect CLMIAP call sites | High |
| C.2 | Generate specialized function clones | High |
| C.3 | Replace CLMIAP → CLMAPP at known sites | Medium |
| C.4 | Deduplication of identical specializations | Medium |
| C.5 | Generate witness tables for remaining polymorphic code | High |
| C.6 | Benchmark: algebra method calls should be zero-overhead | — |

### Phase D: Worker/Wrapper + Unboxing

**Goal**: Eliminate boxing for numeric code.

| Task | Description | Complexity |
|------|-------------|-----------|
| D.1 | Create `src/DemandAnalysis.hs` — per-parameter demand computation | High |
| D.2 | Create `src/WorkerWrapper.hs` — split boxed/unboxed function variants | High |
| D.3 | Newtype elimination (single-constructor, single-field) | Medium |
| D.4 | Product demand: flatten multi-field constructors to registers | Medium |
| D.5 | Absent field elimination | Medium |
| D.6 | Benchmark: `dot({x,y}, {x,y})` should be identical to C | — |

### Phase E: Perceus Reference Counting

**Goal**: Replace Boehm GC with precise, reuse-enabled RC.

| Task | Description | Complexity |
|------|-------------|-----------|
| E.1 | Create `src/PerceusRC.hs` — liveness analysis + dup/drop insertion | High |
| E.2 | Implement reuse analysis (same-shape detection) | High |
| E.3 | Implement borrowing optimization | Medium |
| E.4 | Implement drop specialization (recursive field dropping) | Medium |
| E.5 | Create runtime RC functions (`tlm_dup`, `tlm_drop`, `tlm_reset`, `tlm_reuse`) | Medium |
| E.6 | Remove Boehm GC dependency | Low |
| E.7 | Benchmark: `map f xs` with sole consumer should do zero allocs | — |

### Phase F: Defunctionalization

**Goal**: Eliminate indirect function calls for known closures.

| Task | Description | Complexity |
|------|-------------|-----------|
| F.1 | Create `src/Defunctionalize.hs` — lambda-to-constructor conversion | High |
| F.2 | Generate dispatch functions | Medium |
| F.3 | Partial application compilation | Medium |
| F.4 | Flat closure fallback for unresolvable HOFs | Medium |
| F.5 | Benchmark: HOF-heavy code should use direct calls | — |

### Phase G: Advanced Optimizations

**Goal**: Demand-driven laziness, points-to analysis, LTO.

| Task | Description | Complexity |
|------|-------------|-----------|
| G.1 | Selective laziness (thunk allocation for `lazy` bindings) | Medium |
| G.2 | GRIN-style points-to analysis for dead branch elimination | High |
| G.3 | LLVM LTO integration for release builds | Medium |
| G.4 | Escape analysis for stack-allocated closures | High |
| G.5 | Tail call optimization (LLVM `musttail`) | Medium |

### Phase H: SIMD + Platform

**Goal**: Activate SIMD types and platform-specific optimizations.

| Task | Description | Complexity |
|------|-------------|-----------|
| H.1 | Map Vec2/4/8/16 to LLVM vector types | Medium |
| H.2 | Compile Lane algebra methods to LLVM vector ops | Medium |
| H.3 | Auto-vectorization hints for array loops | Medium |
| H.4 | Platform-specific calling convention tuning | Medium |
| H.5 | AArch64 / x86-64 specific optimizations | Medium |

---

## 19. Research References

### Core References

1. **Perceus RC**: Reinking, Xie, de Moura, Leijen — *"Perceus: Garbage-Free Reference Counting with Reuse"* (ICFP 2021). Foundation for memory management.

2. **Demand Analysis**: Sergey, Peyton Jones, et al. — *"Demand Analysis with Strictness and Absence"* (JFP 2017). Adapted for strict language (absence + product demands).

3. **Worker/Wrapper**: Gill & Hutton — *"The Worker/Wrapper Transformation"* (JFP 2009). Foundation for unboxing.

4. **Defunctionalization**: Reynolds (1972); Danvy & Nielsen — *"Defunctionalization at Work"* (BRICS 2001). Closure elimination.

5. **Decision Trees**: Maranget — *"Compiling Pattern Matching to Good Decision Trees"* (JFP 2008). Optimal pattern match compilation.

6. **GRIN**: Boquist — *"Code Optimisation Techniques for Lazy Functional Languages"* (PhD thesis, 1999); Hruska et al. revival (2018-2023). Points-to analysis.

### Evaluation Strategy

7. **Clairvoyant CBV**: Hackett & Hutton — *"Call-by-Need is Clairvoyant Call-by-Value"* (ICFP 2019). Theoretical foundation for strict-with-selective-lazy.

8. **Optimistic Evaluation**: Ennals & Peyton Jones — *"Optimistic Evaluation: An Adaptive Evaluation Strategy for Non-Strict Programs"* (ICFP 2003). Speculative strict evaluation.

### Compilation Techniques

9. **Lean 4 RC**: Ullrich & de Moura — *"Counting Immutable Beans: Reference Counting Optimized for Purely Functional Programming"* (ITP 2020). RC for functional languages.

10. **Lean 4 Reuse**: Lorenzen, Leijen, et al. — *"Reference Counting with Frame-Limited Reuse"* (MSR 2023). Advanced reuse analysis.

11. **Levity Polymorphism**: Eisenberg & Peyton Jones — *"Levity Polymorphism"* (PLDI 2017). Runtime representation tracking.

12. **QTT**: Brady — *"Idris 2: Quantitative Type Theory in Practice"* (ECOOP 2021). Erasure via quantities.

13. **Calling Conventions**: Downen et al. — *"Kinds Are Calling Conventions"* (ICFP 2020). Type-directed calling convention selection.

### Packed Representations & Advanced

14. **Gibbon**: Vollmer et al. — *"Compiling Tree Transforms to Operate on Packed Representations"* (ICFP 2017). Cache-friendly data layout.

15. **Effect Compilation**: Xie, Brachthäuser, et al. — *"Effect Handlers, Evidently"* (2020). Efficient effect handler compilation.

16. **Staged Compilation**: Kovacs — *"Staged Compilation with Two-Level Type Theory"* (2022). Type-directed optimization via universes.

17. **MLton**: Cejtin, Jagannathan, Weeks — *"Flow-Directed Closure Conversion for Typed Languages"* (ESOP 2000). Whole-program defunctionalization.

18. **Flambda 2**: OCaml team (ongoing 2021-2025). Escape analysis and upward/downward closure classification.

---

## Appendix A: Comparison Summary Table

| Feature | GHC | OCaml | MLton | Lean 4 | Koka | Rust | **tulam** |
|---------|-----|-------|-------|--------|------|------|-----------|
| Evaluation | Lazy | Strict | Strict | Strict | Strict | Strict | **Strict + selective lazy** |
| GC | Gen. copying | Gen. semi-space | Copying | RC + reuse | Perceus RC | None (ownership) | **Perceus RC + reuse** |
| Polymorphism | Dictionaries | Uniform repr | Monomorphize | Tagged ptrs | Tagged ptrs | Monomorphize | **Mono + witness tables** |
| Closures | Heap + info table | Heap flat | Defunctionalized | Flat + RC | Flat + RC | Stack/heap (borrow) | **Defunc + flat** |
| Unboxing | Worker/wrapper | Limited | Full | Tagged ptrs | Limited | Full (ownership) | **Full (mono + W/W)** |
| Pattern match | STG case | Decision trees | Decision trees | Switch | Switch | MIR switch | **Decision trees + points-to** |
| Separate comp | Yes | Yes | No (whole-prog) | Yes | Yes | Per-crate | **Yes (module + LTO)** |
| Effect system | IO monad | None (algebraic effects planned) | None | do notation | Algebraic effects | None | **Algebraic effects (CPS)** |
| SIMD | Limited | None | None | None | None | Excellent | **Vec2/4/8/16 → LLVM vectors** |
| Target | NCG / LLVM | x86/ARM | x86 | C | C | LLVM | **LLVM** |
| Math performance | Good (after strictness) | Good | Excellent | Good | Good | Excellent | **Target: Excellent** |
