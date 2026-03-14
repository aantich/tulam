# tulam Implementation Plan

Incremental roadmap tracking implementation progress and next steps.

---

## Completed Phases (Summary)

All phases below are fully implemented and tested. See git history for details.

| Phase | What | Tests |
|-------|------|-------|
| 1 | Structures (typeclasses) — instance dispatch, CLMIAP eval | ✅ |
| 2 | Language basics — if/else, let/in, records, spread, named construction | ✅ |
| 3 | Structure inheritance (`extends`), value declarations, `where` clauses | ✅ |
| 4 | Primitive types, intrinsics, prelude.tl, numeric dispatch | ✅ |
| 4+ | Lambdas, ADTs (Maybe/Either/List), morphism composition | ✅ |
| 5 | Repr system (`repr ... as ... where { ... }`, `expr as Type`) | ✅ |
| 6 | Convertible morphism, automated composition | ✅ |
| 7 | HKT (Functor, Applicative, Monad on Type1) | ✅ |
| 8 | Arrow types, function types in all positions | ✅ |
| 9 | Effect system (row-polymorphic effects, handlers, action blocks) | ✅ |
| 9.5 | Intrinsic completeness (Char, Bounded, Enum, Hashable, StringExt, Array ops) | ✅ |
| 9.6 | Real programs (Array HOFs, universal Show, monadic traversal) | ✅ |
| 9.7 | Mutable references and arrays (Ref, MutArray) | ✅ |
| 10.1-2 | Reflection + derive (structuralEq/Compare/Show/Hash) | ✅ |
| 10.3 | Type-directed dispatch (CLMTYPED return type hints) | ✅ |
| 11 | Type checker (bidirectional, row polymorphism, constraint resolution) | ✅ |
| Module | Module system (import/export/private/opaque, dependency resolution) | ✅ |
| Prim | Primitive type expansion (Int8-64, UInt8-64, Float32, Byte, SIMD stubs) | ✅ |
| 13.3 | Type-test patterns + downcast | ✅ |
| 13.6 | TC subtyping | ✅ |
| 14 | Anonymous lambdas (`fn(x) = expr`), per-function `requires` constraints | ✅ |
| 15 | Operator fixity (Pratt parser, `infixl`/`infixr`/`infix` declarations) | ✅ |
| — | Strict mode hardening (zero TC errors in strict mode) | ✅ |
| — | Effect composition (default handlers, override app, effect sequencing) | ✅ |
| — | Universe-polymorphic core + GADT support | ✅ |
| — | Safety passes (positivity, termination, coverage checking) | ✅ |
| — | CLM optimization (eta-reduce, inline-small, constant-fold, known-constructor, DCE) | ✅ |
| — | OOP classes (single inheritance, abstract/sealed, dynamic dispatch, implements) | ✅ |
| — | Str type (UTF-8 encoded immutable strings) | ✅ |

**Test suite**: 1086+ hspec tests, all passing.

**Standard library**: 13 consolidated modules in `lib/`, loaded via `loadModuleTree "lib/Base.tl"`.

**Compilation pipeline**: Pass 0 → 0.25 → 0.5 → 1 → 1.5 → 2 → 2.1/2.2/2.3 → 3 → 3.1 → 4 → 4.5 → 5+ (codegen).

---

## Backend Status

### Bytecode VM (Phase A — Complete)

Register-based bytecode VM with 59+ opcodes, flat closures, tail call optimization, mutable refs/arrays, effect handlers. All 9 AWFY benchmarks pass.

- **Value representation**: Haskell tagged union `Val` (VInt, VFloat, VBool, VObj, ...)
- **Register file**: `IOVector Val`, frame-relative addressing
- **Dispatch**: Tight tail-recursive `execLoop`, 32-bit fixed-width instructions
- **Performance**: ~1564x vs C++ (Haskell GC overhead dominates)

### LLVM Native Backend (Phase A.1-A.2 — Complete)

CLM → LIR → LLVM IR → native binary via clang++. All 9 AWFY benchmarks pass.

**Implemented optimizations**:
- Inline bump allocation (fast/slow path arena, `@tlm_arena_ptr`/`@tlm_arena_end`)
- Free-list allocator (8 size classes, `tlm_free`)
- Function attributes: three-tier inlining (alwaysinline ≤15 leaf / ≤25 no-alloc, inlinehint ≤40, nounwind), readonly for leaf functions only
- Tail call optimization (musttail)
- Switch-based pattern matching
- Bool unboxing (i1, no heap alloc)
- **Null-pointer nullary constructors** (Phase N3): Nil/Nothing/etc. → null pointer, with null guards at all pattern match sites
- **Inline Ref operations**: readRef/writeRef/modifyRef → direct load/store, non-escaping refs → alloca
- **Inline MutArray operations**: mutRead/mutWrite → GEP + load/store (no function call)
- Compilation: `-O3 -flto -march=native`

**Not yet implemented**:
- Capturing closures / lambda lifting with captures
- Perceus RC (stubs only — no actual freeing)
- Class/method dispatch, effect handlers, CLMIAP (blocked on shared monomorphization pass)

**N4 (float field unboxing) — Not needed**: Fields stored as i64 with bitcast, but LLVM -O3 eliminates all bitcasts after inlining. Zero remaining bitcasts in optimized IR.

**N5 (last-use deallocation) — Disabled**: Alias tracking unsound (frees function params via scrutinee aliases). Infrastructure preserved for future correct implementation with provenance analysis.

### Benchmark Results (2026-03-14, after N3 + readonly fix)

| Benchmark | native | cpp | nat/cpp |
|-----------|--------|-----|---------|
| Permute | 2us | 16us | **0.1x** |
| Bounce | 7us | 6us | **1.1x** |
| NBody | 12.9ms | 9.0ms | **1.4x** |
| Sieve | 18us | 11us | **1.6x** |
| Queens | 23us | 12us | **1.9x** |
| List | 18us | 9us | **2.0x** |
| Towers | 46us | 20us | **2.3x** |
| Storage | 172us | 240us | **0.7x** |
| Mandelbrot | 12.2ms | 18.2ms | **0.6x** |
| **Geometric mean** | | | **1.3x** |

---

## Remaining Gaps

1. **Type checker permissive by default**: errors are warnings; `strictTypes` flag makes them fatal
2. **SIMD Vec operations are stubs** (need native backend integration)
3. **TC pattern variable warnings**: spurious "Unbound variable" for pattern match binders (~95 warnings in stdlib)
4. **Monomorphization gap**: Pattern-match variable type inference now covers constructor destructuring; remaining edge cases: nested patterns, inline match with parameterized types (TypeEnv only stores type name, not args)
5. **Native backend gaps**: no capturing closures, no class dispatch, no effect handlers
6. **Bytecode VM performance**: Haskell GC overhead (~1564x vs C++)

---

## Phase N1: Ref-to-Alloca Optimization (Native Backend)

**Goal**: Convert non-escaping `Ref(Int)` and `Ref(Float64)` to LLVM `alloca` + `load`/`store` instead of heap allocation + function calls.

**Why**: Bounce is 6.2x because every `newRef`/`readRef`/`writeRef` involves heap alloc + function call. In C++/Haskell these are stack locals. Queens and Towers also use Ref extensively.

**Root cause**:
- `newRef(0)` → `call @__newref(i64 0)` → malloc 8 bytes, store, return pointer
- `readRef(r)` → `call @__readref(ptr %r)` → load from pointer
- `writeRef(r, v)` → `call @__writeref(ptr %r, i64 %v)` → store to pointer
- Stack alloca eliminates all 3 function calls per ref operation.

**Detection** (in CLMToLIR.hs):
1. Scan CLM body for `CLMAPP "__newref" [init]` calls
2. Track which variables hold ref values
3. Check escape: passed to non-ref function, stored in constructor, or returned
4. Non-escaping refs → `alloca i64` at function entry

**Changes**:

`src/Backends/LLVM/LIR.hs`:
- Add `LAlloca LType` instruction

`src/Backends/LLVM/CLMToLIR.hs`:
- Add `analyzeRefEscape :: CLMExpr -> HashMap Name Bool`
- `__newref` for non-escaping → `LAlloca LTInt64` + `LStore initVal`
- `__readref` → `LLoad allocaPtr 0 LTInt64`
- `__writeref` → `LStore val allocaPtr 0`
- `__modifyref` → load + call closure + store

`src/Backends/LLVM/LIRToLLVM.hs`:
- Emit `LAlloca ty` as `%name = alloca <ty>`

**Expected impact**: Bounce 6.2x → ~2-3x, Queens 6.1x → ~3-4x, Towers 6.5x → ~4x

**Test**: All 9 AWFY benchmarks produce identical output. Generated IR contains `alloca` for Bounce/Queens/Towers.

---

## Phase N2: Aggressive Inlining (Native Backend)

**Goal**: Enable LLVM to inline more functions by widening `alwaysinline` criteria and adding `inlinehint`.

**Why**: Sieve (8.1x), Queens (6.1x), Towers (6.5x), Bounce (6.2x) — function call overhead for small helpers. Current threshold (≤20 instrs, no calls) misses functions that call other small functions.

**Changes**:

`src/Backends/LLVM/CLMToLIR.hs` — `computeFuncAttrs`:
- **Tier 1 `alwaysinline`**: ≤10 instructions, no calls (leaf functions)
- **Tier 2 `alwaysinline`**: ≤30 instructions, may have calls (let LLVM cascade)
- **Tier 3 `inlinehint`**: ≤50 instructions (suggest to optimizer)
- **Default**: no hint (LLVM's own heuristics)

`src/Backends/LLVM/LIRToLLVM.hs`:
- Add `inlinehint` to attribute emission

**Expected impact**: Sieve 8.1x → ~4x, Queens → ~3x (with N1), Towers → ~3x (with N1)

**Test**: All AWFY benchmarks identical output. Inspect .ll for attributes.

---

## Phase N3: Null-Pointer Nullary Constructors (Native Backend) — COMPLETE

**Goal**: Represent `Nil`, `DiskNone`, `Nothing` as null pointers. Zero allocation, zero memory access for matching.

**Status**: Complete and active (2026-03-14). All infrastructure was already implemented — verified and enabled.

**Implementation**:
- `detectNullaryAsNull` + `collectConTags`: global detection across all functions (CLMToLIR.hs:1229-1256)
- Construction: nullary constructors emit `LLitNull` (CLMToLIR.hs:455-466)
- Null guards at all three pattern match sites:
  - `lowerOneCheck` (single tag): `LIsNull`/phi-node safe extraction (CLMToLIR.hs:588-615)
  - `lowerTagSwitchRet`: null check → branch past tag switch (CLMToLIR.hs:748-770)
  - `lowerTagSwitchExpr`: same for expression switches (CLMToLIR.hs:890-908)
- Bool excluded (stays unboxed `i1`)

**Verification**: List benchmark IR shows `icmp eq ptr %xs, null` (3 null checks). No zero-field allocations. All 9 AWFY benchmarks pass.

---

## Phase N4: Float Field Unboxing (Native Backend) — NOT NEEDED

**Goal was**: Store `Float64` fields as `double` directly instead of `i64` bitcast.

**Finding** (2026-03-14): LLVM -O3 with `-flto` eliminates ALL i64↔double bitcasts after inlining. Compiled `advanceInner` (NBody hot loop) with `clang++ -O3 -S -emit-llvm` shows zero remaining bitcasts. NBody runs at 1.4x vs C++ — bitcasts are not a bottleneck.

**No changes needed.** LLVM's optimizer handles this perfectly.

---

## Phase N5: Last-Use Deallocation (Native Backend) — DISABLED

**Goal**: Emit `tlm_free` when heap objects are destructured for the last time.

**Status**: Disabled (2026-03-14). Fundamental soundness bug: CLM desugars `match xs | ...` to `(λ __m. ...) xs`, creating an alias. `canFreeAfterMatch` checks `__m` (not in funcParams) but `__m` IS the function parameter `xs`. Freeing `__m` = freeing `xs` = use-after-free.

**Infrastructure preserved** in CLMToLIR.hs (`countVarUses`, `canFreeAfterMatch`, `lsBodyUseCounts`, `lsFuncParams`). LFree emission removed from `lowerTagSwitchRet` and `lowerTagSwitchExpr`.

**Future**: Requires alias/provenance tracking. Expected impact 5-10% — not worth correctness risk without proper analysis.

---

## Phase B1: NaN-Boxing (Bytecode VM)

**Goal**: Replace Haskell tagged union `Val` with 64-bit NaN-boxed `Word64`. Eliminate GC pressure.

**Why**: Bytecode VM is 1564x vs C++. Haskell heap allocation for every `Val` is the dominant cost. NaN-boxing encodes Int/Float/Bool/Char/Unit in a Word64 with zero allocation.

**Encoding**:
```
Float64:  any valid IEEE 754 double
Tagged:   [1111_1111_1111_TTTT] [48-bit payload]
  0000 = Heap pointer (48-bit address)
  0001 = Int (signed 48-bit)
  0010 = Bool (0/1)
  0011 = Char (Unicode codepoint)
  0100 = Unit
  0101 = Empty/Nil
```

**Changes**:

`src/Backends/Bytecode/Value.hs`:
- `newtype Val = Val Word64` with smart constructors/extractors
- Heap objects remain as Haskell `HeapObj` (via StablePtr or IORef)

`src/Backends/Bytecode/VM.hs`:
- Replace pattern matches with extractor functions
- Register file: `IOVector Word64`

`src/Backends/Bytecode/Compile.hs`:
- Constant pool: `Word64`

**Expected impact**: 5-10x speedup on numeric benchmarks, 2-3x overall.

**Test**: All 19 BytecodeSpec tests pass. All 9 AWFY benchmarks via `:bc run main`.

---

## Phase B2: SWITCH Instruction (Bytecode VM)

**Goal**: Replace linear JMPF chains with table-jump SWITCH for pattern matching.

**Why**: O(N) tag checks → O(1) jump table dispatch.

**Changes**:

`src/Backends/Bytecode/Compile.hs`:
- Detect all-tag-check case chains → emit SWITCH + jump table

`src/Backends/Bytecode/VM.hs`:
- SWITCH dispatch: extract tag, index jump table, jump

**Expected impact**: 10-20% on pattern-match-heavy benchmarks (Towers, Queens, List).

---

## Implementation Order

```
Phase N1 (Ref-to-Alloca)       ✅ COMPLETE
Phase N2 (Aggressive Inlining) ✅ COMPLETE
Phase N3 (Null Nullary)        ✅ COMPLETE (2026-03-14)
Phase N4 (Float Unboxing)      ✅ NOT NEEDED (LLVM handles it)
Phase N5 (Last-Use Free)       ❌ DISABLED (alias bug)
  ↓
Phase B1 (NaN-Boxing)          ← bytecode VM major overhaul
  ↓
Phase B2 (SWITCH)              ← bytecode pattern match opt
```

## Current Results (2026-03-14)

| Benchmark | native | cpp | nat/cpp | Status |
|-----------|--------|-----|---------|--------|
| Permute | 2us | 16us | **0.1x** | 8x faster than C++ |
| Bounce | 7us | 6us | **1.1x** | at parity |
| Storage | 172us | 240us | **0.7x** | faster than C++ |
| Mandelbrot | 12.2ms | 18.2ms | **0.6x** | faster than C++ |
| NBody | 12.9ms | 9.0ms | **1.4x** | |
| Sieve | 18us | 11us | **1.6x** | |
| Queens | 23us | 12us | **1.9x** | |
| List | 18us | 9us | **2.0x** | |
| Towers | 46us | 20us | **2.3x** | |
| **Geometric mean** | | | **1.3x** | |

Native N-phase optimizations complete. Remaining perf gaps (Queens, Towers, List) are likely allocation overhead — will improve with Perceus RC (reuse analysis).

---

## Next Stages — Native Backend

### Phase N6: Capturing Closures / Lambda Lifting

**Goal**: Support lambdas that capture free variables from their enclosing scope.

**Why**: Currently only top-level known functions compile. Any higher-order code (map, fold, callbacks) requires closures. This is the biggest native backend gap.

**Approach**:
1. **Closure representation**: `{func_ptr, capture1, capture2, ...}` as heap object. Tag = special closure tag. Field 0 = function pointer, fields 1+ = captured values.
2. **Lambda lifting**: For each lambda with free vars, emit a lifted top-level function with extra params for captures. At call site, allocate closure object.
3. **Known-call optimization**: When call target is statically known, bypass closure and call lifted function directly with captures as extra args.
4. **Defunctionalization** (future): For small closure sets, replace with tagged union + apply function.

**Files**: CLMToLIR.hs (closure construction, apply dispatch), LIR.hs (may need LClosureAlloc), LIRToLLVM.hs (closure calling convention).

### Phase N7: Perceus Reference Counting

**Goal**: Automatic memory management via precise reference counting with drop/reuse semantics.

**Why**: Current arena allocator never frees. Programs that allocate heavily (Storage, List) will eventually exhaust memory for long-running programs.

**Approach** (per Koka/Lean design):
1. **RC field in header**: 4 bytes in existing header (flags field). Bump on share, decrement on last use.
2. **Drop insertion**: After last use of each variable, emit `rc_dec`. When RC hits 0, free recursively.
3. **Reuse analysis**: When destructuring and immediately constructing same-size object, reuse the memory (in-place update).
4. **Borrow optimization**: For function params that are only read (not stored/returned), pass as borrowed (no RC bump).

**Files**: New `src/Backends/LLVM/Perceus.hs` for analysis pass. CLMToLIR.hs for RC instruction emission. Runtime for `tlm_rc_inc`/`tlm_rc_dec`.

### Phase N8: Class/Method Dispatch

**Goal**: Compile OOP classes (CLMMCALL, CLMSCALL, CLMNEW) to native code.

**Approach**: Tag-based dispatch (no vtable). Object is `CLMCON` with class tag. Method lookup via jump table indexed by tag.

### Phase N9: Effect Handlers

**Goal**: Compile algebraic effect handlers to native code.

**Approach**: Lightweight — for IO/State effects, inline the handler (zero overhead). For dynamic handlers, use setjmp/longjmp or continuation-passing.

---

## Next Stages — Bytecode VM

### Phase B1: NaN-Boxing

**Goal**: Replace Haskell tagged union `Val` with 64-bit NaN-boxed `Word64`. Eliminate GC pressure.

**Why**: Bytecode VM is ~1564x vs C++. Haskell heap allocation for every `Val` is the dominant cost.

**Encoding**:
```
Float64:  any valid IEEE 754 double
Tagged:   [1111_1111_1111_TTTT] [48-bit payload]
  0000 = Heap pointer (48-bit address)
  0001 = Int (signed 48-bit)
  0010 = Bool (0/1)
  0011 = Char (Unicode codepoint)
  0100 = Unit
  0101 = Empty/Nil
```

**Changes**: Value.hs (newtype Val = Val Word64), VM.hs (pattern match → extractors), Compile.hs (constant pool Word64).

**Expected impact**: 5-10x speedup on numeric benchmarks, 2-3x overall.

### Phase B2: SWITCH Instruction

**Goal**: Replace linear JMPF chains with table-jump SWITCH for pattern matching.

**Expected impact**: 10-20% on pattern-match-heavy benchmarks.

### Phase B3: Direct-Threaded Dispatch

**Goal**: Replace Haskell case-based opcode dispatch with computed goto (via GHC FFI or unboxed arrays).

**Expected impact**: 2-3x from eliminating dispatch overhead.

---

## Implementation Order

```
NATIVE BACKEND:
  N1-N3 ✅ COMPLETE    (ref-to-alloca, inlining, null nullary)
  N4    ✅ NOT NEEDED   (LLVM handles float unboxing)
  N5    ❌ DISABLED     (alias bug — needs provenance analysis)
  N6    ← Capturing closures (biggest gap)
  N7    ← Perceus RC
  N8    ← Class dispatch
  N9    ← Effect handlers

BYTECODE VM:
  B1    ← NaN-Boxing (biggest impact)
  B2    ← SWITCH instruction
  B3    ← Direct-threaded dispatch
```

---

## Future Work (Not Yet Planned)

- **SIMD codegen**: Vec2/4/8/16 operations mapped to hardware SIMD
- **Bytecode caching**: .tlb serialization format
- **Parallel compilation**: Multi-threaded module compilation
