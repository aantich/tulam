# Laziness Treatment & Multi-Threading Options for the tulam LLVM Backend

## Table of Contents

1. [Introduction](#1-introduction)
2. [Laziness: Design Space](#2-laziness-design-space)
3. [The Lazy(a) Type](#3-the-lazya-type)
4. [Demand Analysis for Laziness Suggestions](#4-demand-analysis-for-laziness-suggestions)
5. [Corecursion Detection](#5-corecursion-detection)
6. [LLVM Compilation of Lazy Values](#6-llvm-compilation-of-lazy-values)
7. [Laziness × Perceus RC Interaction](#7-laziness--perceus-rc-interaction)
8. [Threading: Design Space](#8-threading-design-space)
9. [Reference Counting Under Concurrency](#9-reference-counting-under-concurrency)
10. [Software Transactional Memory (STM)](#10-software-transactional-memory-stm)
11. [Threading Models](#11-threading-models)
12. [Object Header Layout Options](#12-object-header-layout-options)
13. [Thread-Safe Lazy Forcing](#13-thread-safe-lazy-forcing)
14. [Laziness × Threading Interaction Matrix](#14-laziness--threading-interaction-matrix)
15. [Comparison Tables](#15-comparison-tables)
16. [Open Questions](#16-open-questions)
17. [References](#17-references)

---

## 1. Introduction

This document explores **two orthogonal but interacting design dimensions** for tulam's LLVM backend:

1. **Laziness treatment** — how to support deferred computation in a fundamentally strict language
2. **Multi-threading** — how to make Perceus RC and the runtime safe and performant under concurrency

tulam is **strict by default** (CBV), which gives us excellent performance for numeric and data-processing workloads. However, certain patterns — infinite streams, demand-driven pipelines, coinductive data — genuinely benefit from lazy evaluation. The question is: how do we add laziness without sacrificing the performance advantages of strictness?

Similarly, tulam's chosen memory management (Perceus RC) is inherently single-threaded in its basic form. Supporting multi-threading requires careful decisions about atomicity, ownership, and synchronization.

**This document presents options, not decisions.** Each section describes the design space, tradeoffs, and interactions. Final choices will be made after prototyping.

### Relationship to Other Design Documents

- **LLVMBackendDesign.md** — the parent design; this document elaborates on §4 (Evaluation Strategy) and adds threading
- **ConcurrencyDesign.md** — the high-level effect-based concurrency model; this document covers the *runtime implementation* options for that model
- **PrimitiveDesign.md** — primitive types, intrinsics, SIMD; Lazy(a) would join these as a new primitive

---

## 2. Laziness: Design Space

### 2.1 The Spectrum of Approaches

| Approach | Description | Examples | Pros | Cons |
|----------|-------------|----------|------|------|
| **Fully strict** | No laziness at all | MLton, Rust | Simplest compilation, best numeric perf | Cannot express infinite data, some algorithms become O(n²) |
| **Fully lazy** | Everything is a thunk by default | Haskell/GHC | Natural infinite data, elegant APIs | Space leaks, unpredictable performance, thunk overhead everywhere |
| **Explicit lazy type** | Lazy(a) is an opt-in wrapper | Scala `lazy val`, OCaml `Lazy.t`, Kotlin `lazy` | Control over where laziness lives, no hidden overhead | Programmer must annotate; may miss beneficial laziness |
| **Demand-analyzed strictness** | Compiler infers strictness for a lazy-default language | GHC strictness analyzer | Best of both worlds in theory | Complex analysis, still misses cases, requires bang patterns as escape hatch |
| **Strict + demand-analyzed laziness** | Strict default, compiler *suggests* laziness | **Our approach** | Clean model, zero overhead by default, IDE assistance | Novel territory; demand analysis for laziness suggestions less studied than for strictness |

### 2.2 Why Not Fully Lazy?

GHC's experience shows the cost of lazy-by-default:

1. **Thunk allocation overhead**: Every unevaluated expression allocates a closure on the heap. In numeric-heavy code, this can be 10-50× slower than unboxed strict evaluation.
2. **Space leaks**: Thunks accumulate unevaluated chains. A simple `foldl (+) 0 [1..1000000]` builds a million-deep thunk chain before evaluating.
3. **Unpredictable GC pressure**: Thunk chains create unpredictable memory spikes. Profiling is harder because allocation sites don't correspond to execution sites.
4. **LLVM optimization interference**: Thunks are opaque closures that LLVM cannot see through. Strict code lets LLVM inline, vectorize, and pipeline freely.
5. **Perceus RC incompatibility**: Thunks that capture large environments keep those environments alive. RC works best with prompt deallocation, which strictness provides.

### 2.3 Why Not Fully Strict?

Some patterns genuinely require or strongly benefit from laziness:

1. **Infinite data structures**: `nats = 0 : map (+1) nats` — the canonical example. Without laziness, this diverges.
2. **Demand-driven pipelines**: `take 10 (filter isPrime (map square [1..]))` — evaluating the full intermediate lists wastes work.
3. **Coinductive definitions**: Streams, infinite trees, process algebras — these are inherently corecursive.
4. **Short-circuiting**: `x && expensiveCheck()` — the second operand should only evaluate if the first is True.
5. **Memoized expensive computations**: `let config = parseConfig(file) in ...` where the result should be computed at most once.
6. **Tie-the-knot patterns**: Circular data structures built via mutual recursion (e.g., doubly-linked lists, graphs).

### 2.4 Theoretical Justification: Clairvoyant Call-by-Value

Hackett & Hutton (ICFP 2019) proved that **clairvoyant call-by-value** — where a strict evaluator non-deterministically chooses to evaluate or defer each argument — produces the same results as call-by-need when the program terminates. This means:

> A strict language with selective laziness is *semantically equivalent* to a lazy language with selective strictness, for terminating programs.

This validates tulam's approach: strict by default, with programmer-chosen or compiler-suggested laziness where needed.

---

## 3. The Lazy(a) Type

### 3.1 Core Design

```tulam
/// A deferred computation that produces a value of type `a`.
/// The computation is executed at most once; subsequent forces return the cached result.
primitive Lazy(a: Type);

/// Create a lazy value from a computation.
/// The body is NOT evaluated until `force` is called.
function delay(body: Unit -> a) : Lazy(a) = intrinsic;

/// Force a lazy value, evaluating it if not yet computed.
/// Subsequent calls return the cached result.
/// Thread-safe: concurrent forces are serialized (only one evaluates, others wait).
function force(lazy: Lazy(a)) : a = intrinsic;
```

### 3.2 Syntax Sugar: The `~` Prefix

**Option A: Field-level laziness**

```tulam
type Stream(a: Type) = Cons * head: a * ~tail: Stream(a);
// Desugars to:
type Stream(a: Type) = Cons * head: a * tail: Lazy(Stream(a));
```

**Option B: Expression-level laziness**

```tulam
value fibs = ~zipWith((+), fibs, tail(fibs));
// Desugars to:
value fibs = delay(fn () => zipWith((+), fibs, tail(fibs)));
```

**Option C: Both (recommended)**

The `~` prefix works in both contexts:
- Before a field name in a type definition: wraps the field type in `Lazy(...)`
- Before an expression: wraps in `delay(fn () => ...)`

### 3.3 Auto-Force Semantics

**Option A: Explicit force everywhere**

```tulam
match stream
| Cons(h, t) -> h + sum(force(t))  // Must call force() explicitly
```

**Option B: Pattern-match auto-force**

```tulam
match stream
| Cons(h, t) -> h + sum(t)  // t is auto-forced when bound in pattern
```

**Option C: Transparent coercion (Haskell-style)**

```tulam
match stream
| Cons(h, t) -> h + sum(t)  // t : Lazy(Stream(Int)) auto-coerces to Stream(Int) anywhere
```

**Tradeoff analysis:**

| Option | Explicitness | Ergonomics | Performance visibility | Implementation complexity |
|--------|-------------|------------|----------------------|--------------------------|
| A: Explicit force | Full | Verbose | Full — every force is visible | Minimal |
| B: Pattern auto-force | Moderate | Good | Good — forces happen at pattern sites | Moderate (desugar in pattern compiler) |
| C: Transparent | None | Excellent | Poor — forces are invisible | High (implicit coercions throughout TC) |

### 3.4 Laziness and Type Checking

`Lazy(a)` is a distinct type from `a`. The type checker does NOT automatically coerce between them (unless Option C is chosen). This means:

```tulam
function sum(xs: List(Int)) : Int = ...;

value lazy_list : Lazy(List(Int)) = delay(fn () => [1, 2, 3]);

// Type error: expected List(Int), got Lazy(List(Int))
sum(lazy_list)

// Correct:
sum(force(lazy_list))
```

This explicitness is deliberate — it makes laziness visible in types, preventing accidental space leaks.

### 3.5 Algebra Instances for Lazy

```tulam
/// Lazy values support equality if their contents do (forces both).
instance Eq(Lazy(a)) [Eq(a)] = {
    function eq(x: Lazy(a), y: Lazy(a)) : Bool = force(x) == force(y)
};

/// Functor instance: transforms the result without forcing.
instance Functor(Lazy) = {
    function fmap(f: a -> b, la: Lazy(a)) : Lazy(b) =
        delay(fn () => f(force(la)))
};

/// Applicative instance: lazy function application.
instance Applicative(Lazy) = {
    function pure(x: a) : Lazy(a) = delay(fn () => x);
    function ap(lf: Lazy(a -> b), la: Lazy(a)) : Lazy(b) =
        delay(fn () => force(lf)(force(la)))
};
```

---

## 4. Demand Analysis for Laziness Suggestions

### 4.1 Overview

In a strict language, demand analysis runs *backwards* from how it works in GHC. Instead of finding which lazy values can be safely made strict, we find which strict positions would benefit from being lazy.

The analyzer identifies three patterns:

1. **Unnecessary computation**: A strict argument is computed but the result is only used in some branches
2. **Infinite divergence**: A recursive call would diverge under strict evaluation but would terminate with lazy evaluation
3. **Excessive intermediate allocation**: Strict evaluation materializes large intermediate structures that are consumed incrementally

### 4.2 Analysis Passes

**Pass 1: Usage analysis** — For each function parameter and let-binding, track:
- Is it used in all branches or only some? (absence analysis)
- Is it used at most once? (linearity analysis)
- Is it used only as a selector (e.g., `fst(pair)`)? (product demand)

**Pass 2: Termination interaction** — For each recursive call:
- Does strict evaluation of argument X require evaluating the recursive result?
- Would deferring argument X break the infinite chain?

**Pass 3: Intermediate structure analysis** — For function compositions:
- Does `f(g(xs))` fully materialize `g(xs)` before `f` consumes it?
- Would a lazy intermediate allow streaming/fusion?

### 4.3 Suggestion Tiers

| Tier | Confidence | Action | Example |
|------|-----------|--------|---------|
| **Strong** | >95% | Compiler warning | `nats = 0 : map (+1) nats` — infinite without laziness |
| **Medium** | 70-95% | IDE suggestion | `take 10 (filter p (map f xs))` — lazy intermediates avoid wasted work |
| **Weak** | 50-70% | IDE hint (dimmed) | `let x = expensive() in if cond then x else default` — conditional use |

### 4.4 Compiler vs IDE Placement

**Option A: All in the compiler**
- Laziness suggestions as compiler warnings (`-Wsuggest-lazy`)
- Pro: Available everywhere, no tooling dependency
- Con: Adds noise to build output; hard to act on in a terminal

**Option B: Compiler + IDE integration (recommended)**
- Strong suggestions: compiler warnings
- Medium/weak suggestions: LSP diagnostics (info-level), with quick-fix actions to add `~` or `delay`
- Pro: Right information at the right level; quick-fix makes acting on suggestions easy
- Con: Full benefit requires IDE support

**Option C: IDE only**
- All analysis runs in the language server, not the compiler
- Pro: Clean compiler output
- Con: Analysis unavailable in CI/batch builds; duplication of compiler internals

---

## 5. Corecursion Detection

### 5.1 The Problem

Under strict evaluation, certain recursive definitions diverge that would converge under lazy evaluation:

```tulam
// DIVERGES under strict evaluation:
value nats : List(Int) = Cons(0, map((+1), nats));
// Strict eval tries to fully evaluate `map((+1), nats)` which requires `nats` which requires...

// CONVERGES under strict evaluation:
value nats : Lazy(List(Int)) = delay(fn () => Cons(0, map((+1), force(nats))));
// `delay` defers the computation; each `force` produces one more element
```

The compiler should detect the first pattern and suggest the second.

### 5.2 Detection Patterns

**Pattern 1: Direct recursive constructor application**

```
value x = Con(..., x, ...)        // x appears as argument to its own constructor
```

Detection: Trivial syntactic check in Pass 2 (case optimization) or a dedicated pre-pass.

**Pattern 2: Mutual recursion through constructors**

```
value evens = Cons(0, odds);
value odds  = map((+1), evens);
```

Detection: Build the dependency graph of top-level values. If a cycle exists and at least one edge passes through a constructor application, flag it.

**Pattern 3: Higher-order producer functions**

```
value xs = unfold(f, seed);      // unfold produces a potentially infinite list
value ys = iterate(g, init);     // iterate produces an infinite list
```

Detection: Maintain a set of known "producer" functions (unfold, iterate, repeat, cycle, replicate when given infinite input). Flag when these appear in strict value bindings.

**Pattern 4: Efficiency-only laziness (no divergence)**

```
function f(xs) = take(10, map(g, xs));
// xs is finite but potentially large — lazy intermediate avoids materializing map(g, xs)
```

Detection: Requires the demand analysis from §4.3. No divergence risk, but laziness improves efficiency.

### 5.3 Detection Algorithm

```
INPUT: Set of top-level value bindings and function definitions (post-desugaring)
OUTPUT: Set of (location, pattern, confidence, suggestion) tuples

1. Build the Value Dependency Graph (VDG):
   - Nodes: top-level values and function definitions
   - Edges: v1 -> v2 if v2 appears free in the body of v1
   - Edge labels: "constructor-arg", "function-arg", "function-call", "other"

2. Find all strongly-connected components (SCCs) in the VDG.

3. For each SCC with >1 node, or a self-loop:
   a. If ANY edge in the cycle is labeled "constructor-arg":
      → STRONG suggestion: "This recursive value requires laziness to avoid divergence"
      → Suggest: wrap the recursive field/value in Lazy(...)
   b. If edges are all "function-call" but the function is a known producer:
      → STRONG suggestion: "Producer function creates infinite structure under strict eval"
      → Suggest: use Lazy(a) or streaming alternative
   c. If edges are all "function-call" with unknown termination:
      → MEDIUM suggestion: "Potentially infinite recursion; consider laziness"
      → Run termination checker (Pass 2.2) for confirmation

4. For non-cyclic patterns:
   a. Demand analysis (§4) identifies conditionally-used computations
   b. Intermediate-structure analysis identifies fusion opportunities

5. Filter suggestions by confidence threshold (configurable per tier)
```

### 5.4 Runtime Fallback: Fuel-Based Detection

For patterns that static analysis misses, a runtime mechanism can catch divergence:

```
// Debug/development builds only:
function forceWithFuel(lazy: Lazy(a), maxSteps: Int) : Either(String, a)
```

In debug mode, the runtime counts reduction steps during `force`. If the count exceeds a threshold, it returns an error with a stack trace showing the cycle, rather than hanging.

**This is a development tool only** — production builds use standard `force` with no overhead.

### 5.5 IDE Integration

The language server can provide:

1. **Inline warnings**: Red squiggly under `value nats = Cons(0, map((+1), nats))` with message "Strict-infinite: this value will diverge under strict evaluation"
2. **Quick-fix**: "Wrap in Lazy(a)" — automatically adds `delay`/`force` annotations
3. **Hover information**: "This expression is corecursive (produces infinite data). Consider using `~` for lazy evaluation."
4. **Gutter icons**: A small icon (e.g., ∞) next to corecursive definitions

---

## 6. LLVM Compilation of Lazy Values

### 6.1 Runtime Representation

A `Lazy(a)` value at runtime is a tagged pointer to a heap object:

```
┌─────────────────────────────────────────────┐
│ Lazy Object (unevaluated)                   │
├─────────┬───────────┬───────────────────────┤
│ tag: 0  │ refcount  │ closure_ptr           │
│ (1 byte)│ (4 bytes) │ (8 bytes = fn ptr)    │
│         │           │ env_ptr (8 bytes)     │
└─────────┴───────────┴───────────────────────┘

┌─────────────────────────────────────────────┐
│ Lazy Object (evaluated / memoized)          │
├─────────┬───────────┬───────────────────────┤
│ tag: 1  │ refcount  │ value (8 bytes)       │
│ (1 byte)│ (4 bytes) │ (padded to 16 bytes)  │
└─────────┴───────────┴───────────────────────┘
```

The tag byte discriminates between unevaluated (0) and evaluated (1). After forcing, the closure pointer is replaced with the computed value and the tag is set to 1 (**in-place update**).

### 6.2 LLVM IR for `delay`

```llvm
; delay(body) — allocate a lazy thunk
define %Lazy* @tulam_delay(%Closure* %body) {
  %thunk = call %Lazy* @tulam_alloc_lazy()
  store i8 0, i8* getelementptr(%Lazy, %Lazy* %thunk, i32 0, i32 0)  ; tag = unevaluated
  store i32 1, i32* getelementptr(%Lazy, %Lazy* %thunk, i32 0, i32 1) ; refcount = 1
  store %Closure* %body, %Closure** getelementptr(%Lazy, %Lazy* %thunk, i32 0, i32 2) ; closure
  ret %Lazy* %thunk
}
```

### 6.3 LLVM IR for `force`

```llvm
; force(lazy) — evaluate if needed, return cached result
define %Value @tulam_force(%Lazy* %lazy) {
entry:
  %tag = load i8, i8* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 0)
  %is_evaluated = icmp eq i8 %tag, 1
  br i1 %is_evaluated, label %cached, label %evaluate

evaluate:
  %closure = load %Closure*, %Closure** getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  %result = call %Value @tulam_apply_closure(%Closure* %closure, %Value zeroinitializer)
  ; In-place memoization:
  store %Value %result, %Value* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  store i8 1, i8* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 0)  ; tag = evaluated
  ; Drop the closure (it's no longer needed):
  call void @tulam_rc_dec(%Closure* %closure)
  ret %Value %result

cached:
  %val = load %Value, %Value* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  ret %Value %val
}
```

Key points:
- **Branch-based force**: A single branch on the tag byte. Predicted-taken for the cached path after first force.
- **In-place memoization**: The closure pointer is overwritten with the result value. No separate "value" field needed.
- **Closure drop**: After evaluation, the closure's refcount is decremented, potentially freeing it and its captured environment. This is critical for preventing space leaks.

### 6.4 Optimization: Unboxed Lazy for Small Types

For types that fit in a machine word (Int, Float64, Bool, pointers), the lazy value can store the result inline without heap allocation for the value itself. Only the thunk object is heap-allocated; the result overwrites the closure pointer in-place.

For larger types (tuples, records), the result is a pointer to a heap object, and that pointer is stored in the lazy object.

---

## 7. Laziness × Perceus RC Interaction

### 7.1 The Challenge

Perceus RC achieves its performance by:
1. **Prompt deallocation**: Objects are freed as soon as their refcount hits zero
2. **Reuse analysis**: Freed objects' memory is recycled for new allocations of the same size
3. **Borrowing**: Parameters known to be alive for the duration of a call skip RC operations

Laziness introduces complications:

| Perceus Feature | Impact of Laziness |
|----------------|-------------------|
| Prompt deallocation | Thunks keep captured variables alive longer than strict code would |
| Reuse analysis | Thunks have variable size (closure + env); reuse is less predictable |
| Borrowing | Cannot borrow through a lazy value (force may or may not happen) |
| Drop specialization | Dropping a Lazy(a) must check: unevaluated → drop closure; evaluated → drop value |

### 7.2 Mitigation Strategies

**Strategy 1: Closure trimming**

When creating a `delay(fn () => expr)`, capture only the free variables of `expr`, not the entire enclosing scope. This is standard closure conversion, but for lazy values it's especially important because the closure may live much longer than a normal function argument.

**Strategy 2: Eager drop of forced closures**

After `force` evaluates a thunk, immediately drop the closure (decrement RC on all captured variables). This converts the space leak from "O(thunk chain length)" to "O(1 thunk at a time)". Shown in §6.3 above.

**Strategy 3: Lazy-aware borrowing**

The borrowing analysis can be extended:
- If a `Lazy(a)` is forced exactly once in a borrowing context, the force itself can borrow the closure's captures
- If a `Lazy(a)` is passed to a function that forces it, the parameter can still be borrowed if the force is guaranteed

**Strategy 4: Size-class allocation for thunks**

Since thunks have a fixed header size and variable closure size, use size-class allocation pools (like jemalloc). Common closure sizes (0-capture, 1-capture, 2-capture) get dedicated pools for fast allocation and reuse.

### 7.3 Space Leak Prevention

The main risk with laziness + RC is thunk accumulation:

```tulam
// Classic space leak:
function sum(xs: List(Int)) : Int =
    foldl((+), 0, xs);
// Under laziness: foldl builds a thunk chain (((0 + 1) + 2) + 3) + ...
```

Since tulam is strict by default, this pattern only arises when the programmer explicitly uses `Lazy`. The type system makes it visible:

```tulam
// This cannot space-leak (strict foldl):
foldl((+), 0, [1,2,3,4,5])  // : Int, evaluated immediately

// This CAN space-leak (lazy accumulator):
foldl((+), delay(fn () => 0), lazy_list)  // : Lazy(Int), thunk chain
```

The type checker can warn when `Lazy` appears in accumulator position of a fold.

---

## 8. Threading: Design Space

### 8.1 The Central Challenge

Perceus reference counting uses **non-atomic** reference count operations by default. This is a major performance advantage — atomic operations (e.g., `lock xadd` on x86) are 10-60× slower than non-atomic increments, and they pollute the CPU cache.

To support multi-threading, we need to make RC safe without sacrificing the single-threaded fast path.

### 8.2 The Spectrum of Approaches

| Approach | Description | Examples |
|----------|-------------|----------|
| **No sharing** | Each thread has its own heap; communication via message passing only | Erlang, early Rust |
| **Atomic RC everywhere** | All RC operations use atomic instructions | Swift (initially), Python (proposed) |
| **Biased RC** | Owner thread uses non-atomic RC; other threads use separate atomic counter | Lean 4, proposed for tulam |
| **Thread-local arenas** | Each thread allocates in its own arena; shared data uses separate management | Various research systems |
| **Tracing GC** | Abandon RC for shared heap; use concurrent tracing GC | Go, Java, OCaml 5 |

---

## 9. Reference Counting Under Concurrency

### 9.1 Option A: Atomic RC Everywhere

**Description**: Replace all `inc`/`dec` operations with atomic variants (`fetch_add`, `fetch_sub`).

**Implementation**:
```llvm
; Atomic increment
define void @tulam_rc_inc(%Object* %obj) {
  %rc_ptr = getelementptr %Object, %Object* %obj, i32 0, i32 1
  %old = atomicrmw add i32* %rc_ptr, i32 1 acq_rel
  ret void
}

; Atomic decrement (with conditional free)
define void @tulam_rc_dec(%Object* %obj) {
  %rc_ptr = getelementptr %Object, %Object* %obj, i32 0, i32 1
  %old = atomicrmw sub i32* %rc_ptr, i32 1 acq_rel
  %is_zero = icmp eq i32 %old, 1
  br i1 %is_zero, label %free, label %done
free:
  call void @tulam_drop(%Object* %obj)
  ret void
done:
  ret void
}
```

**Tradeoffs**:

| Dimension | Assessment |
|-----------|-----------|
| Correctness | Trivially correct |
| Single-threaded perf | 3-10× RC overhead vs non-atomic (measured on real workloads) |
| Multi-threaded perf | Good for shared-heavy workloads |
| Implementation effort | Minimal — replace `add`/`sub` with atomic variants |
| Reuse analysis | Still works but reuse becomes thread-local (can't reuse across threads) |
| Borrowing | Still works unchanged |

**Verdict**: Simple but pays a constant tax even in single-threaded code. Unacceptable for the numeric fast-path goal.

### 9.2 Option B: Biased Reference Counting (Recommended to Explore)

**Description**: Each object has an **owner thread**. The owner uses fast non-atomic RC. Other threads use a separate **shared RC** counter with atomic operations. When the owner thread's RC hits zero, it checks the shared RC before freeing.

**Based on**: Lean 4's approach (de Moura et al.), adapted from biased locking (Dice et al. 2001).

**Layout**:
```
┌──────────────────────────────────────────────────┐
│ Object Header (Biased RC)                        │
├─────────┬───────────┬───────────┬────────────────┤
│ tag     │ local_rc  │ shared_rc │ owner_thread   │
│ (1 byte)│ (4 bytes) │ (4 bytes) │ (2 bytes)      │
│         │ non-atomic│ atomic    │ thread ID      │
└─────────┴───────────┴───────────┴────────────────┘
```

**Operations**:

```
rc_inc(obj):
  if current_thread == obj.owner_thread:
    obj.local_rc += 1                    // Non-atomic! Fast path.
  else:
    atomic_add(obj.shared_rc, 1)         // Atomic. Slow path.

rc_dec(obj):
  if current_thread == obj.owner_thread:
    obj.local_rc -= 1                    // Non-atomic!
    if obj.local_rc == 0:
      if atomic_load(obj.shared_rc) == 0:
        free(obj)                        // Both counters zero → safe to free
      else:
        schedule_deferred_free(obj)      // Shared refs exist; defer
  else:
    old = atomic_sub(obj.shared_rc, 1)
    if old == 1 && obj.local_rc == 0:    // We were the last shared ref
      free(obj)                          // Owner already dropped; safe to free
```

**Tradeoffs**:

| Dimension | Assessment |
|-----------|-----------|
| Correctness | Correct with careful ordering (acquire/release on shared_rc reads) |
| Single-threaded perf | Identical to non-concurrent RC (fast path is non-atomic) |
| Multi-threaded perf | Good — only cross-thread sharing pays atomic cost |
| Implementation effort | Moderate — need thread ID tracking, deferred free queue |
| Reuse analysis | Works for owner thread; cross-thread reuse requires care |
| Memory overhead | +6 bytes per object (shared_rc + owner_thread) |
| Borrowing | Works unchanged for owner; cross-thread borrows need atomic flag |

**Key insight**: In most functional programs, 90%+ of objects are created and consumed by the same thread. The biased approach makes this common case as fast as single-threaded RC.

### 9.3 Option C: Thread-Local Arenas + Message Passing

**Description**: Each thread has a private heap (arena). Objects in a thread's arena use non-atomic RC. Cross-thread communication happens via **message passing** — the sender serializes/deep-copies data into the receiver's arena.

**Inspired by**: Erlang's process model, Pony's reference capabilities.

**Implementation**:
```
Thread-local arena:
  ┌─────────────────────────────┐
  │ Thread 1 Arena              │
  │ [obj1] [obj2] [obj3]       │  ← Non-atomic RC within arena
  └─────────────────────────────┘
         │ send(channel, obj2)
         ▼ deep-copy into Thread 2's arena
  ┌─────────────────────────────┐
  │ Thread 2 Arena              │
  │ [obj2'] [obj4] [obj5]      │  ← obj2' is a copy, not shared
  └─────────────────────────────┘
```

**Tradeoffs**:

| Dimension | Assessment |
|-----------|-----------|
| Correctness | Trivially correct — no shared mutable state |
| Single-threaded perf | Identical to non-concurrent RC |
| Multi-threaded perf | Excellent for embarrassingly parallel; poor for shared-state workloads |
| Implementation effort | High — need arena allocator, deep-copy serialization, channel runtime |
| Memory overhead | None per-object; duplication cost for shared data |
| Copying cost | O(n) for each cross-thread message; prohibitive for large shared structures |
| STM compatibility | **Incompatible** — STM requires shared mutable state |

**Verdict**: Clean model for actor-style concurrency but too restrictive for general-purpose multi-threading. Cannot support STM.

### 9.4 Comparison Summary

| Feature | Atomic RC | Biased RC | Thread-Local Arenas |
|---------|-----------|-----------|-------------------|
| Single-thread overhead | 3-10× RC cost | 0% | 0% |
| Shared-data cost | Low (atomic ops) | Low (atomic ops for shared) | High (deep copy) |
| Memory per object | +0 bytes | +6 bytes | +0 bytes |
| STM compatible | Yes | Yes | No |
| Implementation complexity | Low | Medium | High |
| Theoretical max throughput | Good | Excellent | Good (if little sharing) |
| Reuse analysis | Thread-local | Owner-local | Thread-local |

---

## 10. Software Transactional Memory (STM)

### 10.1 Why STM?

Locks are the traditional synchronization primitive, but they suffer from:
- **Deadlocks**: Lock ordering discipline is hard to maintain
- **Priority inversion**: Low-priority thread holding lock blocks high-priority thread
- **Non-composability**: Two correct lock-based modules composed together may deadlock

STM provides **composable, deadlock-free** shared-memory concurrency:

```tulam
// Transfer between accounts — composable, deadlock-free:
function transfer(from: TVar(Int), to: TVar(Int), amount: Int) : Eff { stm: STM } Unit =
    atomically {
        balance <- readTVar(from);
        if balance < amount then retry   // Block until balance is sufficient
        else {
            writeTVar(from, balance - amount);
            writeTVar(to, readTVar(to) + amount)
        }
    };

// Compose two transfers — still deadlock-free:
function doubleTransfer(...) =
    atomically {
        transfer(a, b, 100);
        transfer(b, c, 50)
    };
```

### 10.2 STM Implementation Options

**Option A: Log-based optimistic STM (GHC-style)**

Each transaction maintains a **read set** and **write set**:
1. `readTVar`: Record (TVar, value-at-read-time) in read set; return value
2. `writeTVar`: Record (TVar, new-value) in write set; buffer the write
3. `commit`: Acquire lock on all TVars in write set → validate read set (no values changed since read) → apply writes → release locks
4. `retry`: Abort transaction; block until any TVar in read set changes; re-execute
5. `orElse`: If first transaction retries, try second; if second retries too, block

**Transaction log structure**:
```
TransactionLog = {
    readSet:  HashMap(TVar, (Value, Version)),    // TVar → (value-at-read, version-at-read)
    writeSet: HashMap(TVar, Value),                // TVar → buffered-new-value
}
```

**Tradeoffs**:
- Pro: Well-understood (20+ years of GHC experience), composable, deadlock-free
- Pro: Read-only transactions are lock-free (just validate, no writes)
- Con: Overhead of maintaining transaction logs (~100-200ns per TVar access)
- Con: Long transactions may repeatedly abort due to conflicts (livelock risk)
- Con: I/O inside transactions is problematic (side effects may execute multiple times)

**Option B: Lock-based pessimistic STM (TL2-style)**

Each TVar has a version number and a lock. Reads validate version; writes acquire lock eagerly.

**Tradeoffs**:
- Pro: Less wasted work on conflict (detect early)
- Con: Can deadlock (need lock ordering), less composable
- Con: Read contention on version numbers

**Option C: Hardware Transactional Memory (HTM) + Software Fallback**

Use Intel TSX or ARM TME for small transactions; fall back to software STM for large ones.

**Tradeoffs**:
- Pro: Near-zero overhead for small uncontended transactions
- Con: HTM has capacity limits (transactions abort if they touch too many cache lines)
- Con: Intel deprecated TSX on some CPUs; availability is inconsistent
- Con: Software fallback still needed (complexity of two implementations)

**Recommendation to explore**: Option A (log-based optimistic) as primary, with Option C as a future optimization for hot paths.

### 10.3 STM × Perceus RC Interaction

STM transactions create a challenge for RC:

| Issue | Description | Mitigation |
|-------|-------------|------------|
| **Buffered writes** | Write set holds references to new values; old values' RC shouldn't drop until commit | Write set increments RC on new values at `writeTVar`; decrements old values at `commit` |
| **Aborted transactions** | On abort, undo all RC changes from the transaction | Write set tracks old values; on abort, decrement new values' RC and restore old |
| **Retry blocking** | Transaction blocked on `retry` may hold references in read/write sets | Clear write set on retry (decrement buffered values); read set only holds `(TVar, version)`, not values |
| **Multiple reads of same TVar** | Reading a TVar returns buffered write if present, original otherwise | Write set checked first; no extra RC operation needed |

### 10.4 STM × Effect System Integration

tulam's effect system provides a natural home for STM:

```tulam
effect STM = {
    function readTVar(ref: TVar(a)) : a;
    function writeTVar(ref: TVar(a), val: a) : Unit;
    function retry() : a;                       // Block and retry
    function orElse(a: Unit -> b, b: Unit -> b) : b  // Alternative
};

/// Run an STM transaction atomically.
/// The body must only use STM effects (no IO, no mutation).
function atomically(body: Eff { stm: STM } a) : Eff { io: IO } a = intrinsic;
```

The effect type `Eff { stm: STM } a` ensures at compile time that:
- No IO operations inside transactions (side effects would execute multiple times)
- No uncontrolled mutation (only TVar reads/writes)
- Transactions compose naturally (nested `atomically` flattens)

### 10.5 TVar Implementation

```tulam
/// A transactional variable. Can only be read/written inside `atomically` blocks.
primitive TVar(a: Type);

/// Create a new TVar with an initial value.
function newTVar(val: a) : Eff { io: IO } TVar(a) = intrinsic;

/// Create a new TVar in STM context (for use inside transactions).
function newTVarSTM(val: a) : Eff { stm: STM } TVar(a) = intrinsic;
```

**Runtime representation**:
```
TVar = {
    value:   Ptr(Value),    // Current committed value (heap-allocated)
    version: AtomicWord64,  // Version counter (incremented on each commit)
    lock:    AtomicWord8,   // 0 = unlocked, 1 = locked (for commit phase)
    waiters: Ptr(WaitQueue) // Threads blocked on `retry` watching this TVar
}
```

---

## 11. Threading Models

### 11.1 Overview of Options

tulam's `ConcurrencyDesign.md` already defines the effect-based structured concurrency model. The question here is: what **runtime implementation** backs the `Async` effect?

| Model | Description | Best For |
|-------|-------------|----------|
| **OS threads + locks** | 1:1 threading, mutex/condvar synchronization | CPU-bound, few threads |
| **OS threads + STM** | 1:1 threading, STM for coordination | Shared-state-heavy workloads |
| **Green threads (M:N)** | Many lightweight tasks on few OS threads | IO-bound, many tasks |
| **Actor model** | Message-passing between isolated actors | Distributed, fault-tolerant |
| **Fork-join** | Work-stealing pool for parallel divide-and-conquer | CPU-bound data parallelism |
| **Structured concurrency** | Scoped task hierarchies with automatic cleanup | General purpose |

### 11.2 Option A: OS Threads (1:1)

Each `fork` creates an OS thread. Simple, well-understood.

```
fork(body) → pthread_create(body_wrapper)
await(task) → pthread_join(task.thread)
cancel(task) → set task.cancelled flag; task checks at yield points
```

**Tradeoffs**:
- Pro: Simplest implementation, full OS scheduler support, true parallelism
- Pro: Easy debugging (each thread shows in `ps`, `gdb`, etc.)
- Con: Thread creation is expensive (~50μs on Linux, ~1ms on macOS)
- Con: Stack memory overhead (~1-8MB per thread; limits to ~1000 threads)
- Con: Context switching cost (~1-10μs per switch)
- Con: Not suitable for "millions of lightweight tasks" style

**Best for**: CPU-bound workloads with moderate parallelism (< 100 threads).

### 11.3 Option B: Green Threads (M:N)

Many lightweight tasks multiplexed onto a smaller number of OS threads (the "thread pool").

```
fork(body) → create Task descriptor → enqueue on work-stealing deque
await(task) → if task complete: return result; else: suspend current task, schedule next
yield() → enqueue current task → schedule next
```

**Implementation requirements**:
1. **Work-stealing scheduler**: Each OS thread has a deque of ready tasks. Idle threads steal from busy threads' deques.
2. **Segmented/growable stacks**: Each green thread needs a stack but starting at ~1-4KB (not 1MB). Options:
   - Segmented stacks (Go-style): allocate small, grow on demand via stack overflow handler
   - Stackless (continuation-based): tasks are heap-allocated continuations, no stack at all
   - Cactus stacks: shared stack segments for parent-child task chains
3. **Cooperative yielding**: Tasks yield at function calls (insert yield checks) or at effect handler boundaries.

**Tradeoffs**:
- Pro: Millions of concurrent tasks with low memory overhead
- Pro: Fast task creation (~100ns vs ~50μs for OS threads)
- Pro: Efficient IO multiplexing (epoll/kqueue integration)
- Con: Significant runtime complexity (scheduler, stack management)
- Con: Cooperative yielding means long-running CPU tasks can starve others (need preemption)
- Con: FFI calls block the OS thread (need pinned threads for FFI)
- Con: Debugging is harder (green threads don't show in `gdb` by default)

**Best for**: IO-heavy workloads, server applications, many-task concurrent systems.

### 11.4 Option C: Fork-Join (Work-Stealing)

A specialized model for parallel divide-and-conquer:

```tulam
function parallelMergeSort(xs: Array(Int)) : Array(Int) =
    if length(xs) <= threshold then sort(xs)
    else {
        (left, right) = split(xs);
        par {
            sortedLeft  = parallelMergeSort(left);
            sortedRight = parallelMergeSort(right)
        };
        merge(sortedLeft, sortedRight)
    };
```

The `par { ... }` block forks child tasks that are joined at the block exit. Uses work-stealing deques.

**Tradeoffs**:
- Pro: Excellent for CPU-bound data parallelism
- Pro: Well-understood algorithm (Cilk, Java ForkJoinPool, Rayon)
- Pro: Good cache locality (work-stealing is LIFO for local, FIFO for stealing)
- Con: Not suitable for IO-bound or long-running tasks
- Con: Less general than green threads

**Best for**: Numeric computations, array processing, recursive algorithms.

### 11.5 Option D: Actor Model

Each actor has a private mailbox and processes messages sequentially:

```tulam
actor Counter = {
    state count : Int = 0;

    receive Increment -> { count = count + 1 };
    receive GetCount(replyTo: ActorRef(Int)) -> { send(replyTo, count) }
};
```

**Tradeoffs**:
- Pro: No shared mutable state → no data races by construction
- Pro: Natural model for distributed systems (location transparency)
- Pro: Fault isolation (one actor crashing doesn't affect others)
- Con: Overhead of message serialization for local communication
- Con: Deadlock possible via circular message dependencies
- Con: Debugging distributed state is hard
- Con: Thread-local arena RC (Option C above) is the natural fit, but limits STM

**Best for**: Distributed systems, fault-tolerant services, agent-based architectures.

### 11.6 Option E: Hybrid Model (Recommended to Explore)

Combine green threads + fork-join + optional STM:

```
Runtime Architecture:
┌─────────────────────────────────────────────┐
│ Application Code (tulam)                    │
│ ┌─────────────┐ ┌────────┐ ┌─────────────┐ │
│ │ async/await  │ │ par {} │ │ atomically  │ │
│ │ (Async eff)  │ │(Par eff│ │ (STM effect)│ │
│ └──────┬───────┘ └───┬────┘ └──────┬──────┘ │
│        │             │             │         │
├────────┴─────────────┴─────────────┴─────────┤
│ Green Thread Scheduler (M:N)                 │
│ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐        │
│ │ OS-1 │ │ OS-2 │ │ OS-3 │ │ OS-4 │ ...    │
│ │[deque]│ │[deque]│ │[deque]│ │[deque]│      │
│ └──────┘ └──────┘ └──────┘ └──────┘        │
├──────────────────────────────────────────────┤
│ OS / Hardware                                │
└──────────────────────────────────────────────┘
```

- **Green threads** provide the general concurrency substrate (many tasks, IO multiplexing)
- **Fork-join** provides parallel data processing (par blocks, pmap, parallel folds)
- **STM** provides shared-state coordination (transactions, retry, orElse)
- **Channels** provide message passing (MPMC queues)
- **Structured concurrency** provides lifetime management (all tasks bounded by scope)

This mirrors what Go, Kotlin, and Java 21 have converged toward, but with:
- Effect-type tracking (you know what a function does from its type)
- STM instead of locks (composable, deadlock-free)
- Perceus RC instead of tracing GC (deterministic resource management)

### 11.7 Threading Model Comparison

| Feature | OS Threads | Green Threads | Fork-Join | Actors | Hybrid |
|---------|-----------|---------------|-----------|--------|--------|
| Max concurrent tasks | ~1K | ~1M+ | ~1K active | ~1M+ | ~1M+ |
| Task creation cost | ~50μs | ~100ns | ~100ns | ~500ns | ~100ns |
| True parallelism | Yes | Yes (M:N) | Yes | Yes | Yes |
| IO efficiency | Poor (blocks thread) | Excellent | Poor | Good | Excellent |
| CPU parallelism | Good | Good | Excellent | Good | Excellent |
| Shared state | Locks/STM | Locks/STM | Limited | None | STM |
| Implementation effort | Low | High | Medium | Medium | High |
| Debugging ease | Best | Moderate | Good | Hard | Moderate |

---

## 12. Object Header Layout Options

### 12.1 Current Single-Threaded Design (from LLVMBackendDesign.md)

```
┌─────────┬───────────┬─────────────────┐
│ tag     │ arity     │ rc              │
│ (2 bytes)│ (1 byte) │ (4 bytes)       │
│         │           │ non-atomic      │
├─────────┴───────────┴─────────────────┤
│ fields[0], fields[1], ...             │
└───────────────────────────────────────┘
Total header: 7 bytes (padded to 8)
```

### 12.2 Option A: Biased RC Header

```
┌─────────┬───────────┬──────────┬───────────┬──────────────┐
│ tag     │ arity     │ local_rc │ shared_rc │ owner_tid    │
│ (2 bytes)│ (1 byte) │ (4 bytes)│ (4 bytes) │ (2 bytes)    │
│         │           │ non-atom │ atomic    │ thread ID    │
├─────────┴───────────┴──────────┴───────────┴──────────────┤
│ fields[0], fields[1], ...                                 │
└───────────────────────────────────────────────────────────┘
Total header: 13 bytes (padded to 16)
```

+8 bytes per object vs single-threaded. For a program with 1M live objects, this is ~8MB extra.

### 12.3 Option B: Compact Biased RC Header

```
┌─────────┬───────────┬──────────┬───────────┐
│ tag+arity│ local_rc │ shared_rc │ flags    │
│ (2 bytes) │ (3 bytes)│ (2 bytes) │ (1 byte) │
│ 12b + 4b │ max 16M  │ max 64K   │ owner/gc │
├───────────┴──────────┴───────────┴──────────┤
│ fields[0], fields[1], ...                   │
└─────────────────────────────────────────────┘
Total header: 8 bytes (same as single-threaded!)
```

By using smaller counters:
- `local_rc` (3 bytes = 24 bits): max 16M local references — sufficient for virtually all objects
- `shared_rc` (2 bytes = 16 bits): max 64K cross-thread references — overflow triggers migration to a side table
- `flags` (1 byte): 5 bits for owner thread ID (max 32 threads for fast path), 1 bit for "has side-table entry", 2 bits reserved

**Tradeoff**: Same memory footprint as single-threaded! But limited to 32 threads in the fast path, and overflow handling adds complexity.

### 12.4 Option C: Indirection-Based (Side Table)

```
┌─────────┬───────────┬───────────┐
│ tag     │ arity     │ rc_or_idx │
│ (2 bytes)│ (1 byte) │ (4 bytes) │
│         │           │ bit 31=flag│
├─────────┴───────────┴───────────┤
│ fields[0], fields[1], ...       │
└─────────────────────────────────┘
Total header: 7 bytes (padded to 8) — same as single-threaded

Side table (one per thread pool):
  Index → { local_rc, shared_rc, owner_tid, stm_version }
```

If bit 31 of `rc_or_idx` is 0: it's a simple local RC (single-threaded fast path, max 2^31).
If bit 31 is 1: bits 0-30 are an index into the side table (shared object).

**Tradeoffs**:
- Pro: Zero overhead for objects that never leave their thread (the common case)
- Pro: Identical header layout for single-threaded compilation
- Con: Extra indirection for shared objects (cache miss)
- Con: Side table management (allocation, GC of stale entries)

### 12.5 Header Layout Comparison

| Feature | Current (ST) | Biased RC | Compact Biased | Side Table |
|---------|-------------|-----------|----------------|------------|
| Header size | 8 bytes | 16 bytes | 8 bytes | 8 bytes |
| ST overhead | 0% | +100% | 0% | 0% |
| Thread limit (fast) | 1 | Unlimited | 32 | Unlimited |
| Shared object cost | N/A | +0ns | +0ns | +cache miss |
| STM metadata | N/A | Separate | Separate | In side table |
| Implementation | Simple | Moderate | Complex | Complex |

---

## 13. Thread-Safe Lazy Forcing

### 13.1 The Problem

When multiple threads share a `Lazy(a)` value, concurrent `force` calls must be coordinated:
- Only one thread should evaluate the thunk
- Other threads should wait for the result
- The evaluated result must be visible to all threads (memory ordering)

### 13.2 Option A: CAS-Based Force (Lock-Free)

```llvm
; Thread-safe force using compare-and-swap
define %Value @tulam_force_mt(%Lazy* %lazy) {
entry:
  %tag = load atomic i8, i8* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 0) acquire
  switch i8 %tag, label %evaluate [
    i8 1, label %cached       ; Already evaluated
    i8 2, label %wait         ; Another thread is evaluating
  ]

evaluate:
  ; Try to claim the thunk: CAS tag from 0 (unevaluated) to 2 (evaluating)
  %claimed = cmpxchg i8* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 0),
                      i8 0, i8 2 acq_rel acquire
  %won = extractvalue { i8, i1 } %claimed, 1
  br i1 %won, label %do_eval, label %recheck

do_eval:
  %closure = load %Closure*, %Closure** getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  %result = call %Value @tulam_apply_closure(%Closure* %closure, %Value zeroinitializer)
  ; Store result, then set tag to 1 (evaluated) with release semantics
  store %Value %result, %Value* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  store atomic i8 1, i8* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 0) release
  call void @tulam_rc_dec(%Closure* %closure)
  ; Wake any waiters
  call void @tulam_lazy_wake_waiters(%Lazy* %lazy)
  ret %Value %result

wait:
  ; Another thread is evaluating — spin briefly, then park
  call void @tulam_lazy_wait(%Lazy* %lazy)
  br label %entry   ; Re-check after waking

cached:
  %val = load %Value, %Value* getelementptr(%Lazy, %Lazy* %lazy, i32 0, i32 2)
  ret %Value %val

recheck:
  ; CAS failed — someone else claimed it. Re-read tag.
  br label %entry
}
```

**States**: 0 = unevaluated, 1 = evaluated, 2 = being-evaluated.

**Tradeoffs**:
- Pro: Lock-free for the common case (already evaluated)
- Pro: No mutex overhead
- Con: Need a wait/wake mechanism for the "being-evaluated" state (futex or parking lot)
- Con: Spin-wait for very short evaluations; park for long ones (need adaptive strategy)

### 13.3 Option B: Mutex-Based Force

```
force(lazy):
  if lazy.tag == 1: return lazy.value      // Fast path: no lock needed (atomic read)
  lock(lazy.mutex)
  if lazy.tag == 1:                        // Double-check under lock
    unlock(lazy.mutex)
    return lazy.value
  result = evaluate(lazy.closure)
  lazy.value = result
  lazy.tag = 1
  unlock(lazy.mutex)
  wake_all(lazy.condvar)
  return result
```

**Tradeoffs**:
- Pro: Simple, correct, well-understood
- Con: Mutex per lazy value (memory overhead) or global/striped mutex (contention)
- Con: Locking overhead even when no contention

### 13.4 Option C: Thread-Local Lazy Values (No Synchronization)

If a `Lazy(a)` is never shared across threads (the common case), no synchronization is needed. The type system can distinguish:

```tulam
/// Thread-local lazy value — no synchronization overhead.
primitive Lazy(a: Type);

/// Shared lazy value — thread-safe force with CAS.
primitive SharedLazy(a: Type);
```

Or use a marker in the object header to dynamically switch between single-threaded and multi-threaded force.

**Tradeoffs**:
- Pro: Zero overhead for thread-local lazy values (the common case)
- Pro: Programmer controls where synchronization cost is paid
- Con: Two types for the same concept (API complexity)
- Con: Static analysis may not be able to prove thread-locality in all cases

### 13.5 Thread-Safe Force Comparison

| Feature | CAS-Based | Mutex-Based | Thread-Local |
|---------|-----------|-------------|--------------|
| Already-evaluated cost | Atomic load (~1ns) | Atomic load (~1ns) | Non-atomic load (~0.3ns) |
| First-force cost | CAS + eval + store | Lock + eval + unlock | Eval + store |
| Concurrent force | CAS + spin/park | Lock + wait | N/A (no sharing) |
| Memory per lazy value | +1 byte (state) | +~40 bytes (mutex) | +0 bytes |
| Correctness complexity | Medium (CAS ordering) | Low | Low |
| Best for | Shared lazy values | Simple implementation | Performance-critical paths |

---

## 14. Laziness × Threading Interaction Matrix

This section maps how laziness and threading options interact:

| Lazy Option | RC Option | Force Strategy | Notes |
|-------------|-----------|---------------|-------|
| Explicit Lazy(a) | Atomic RC | CAS force | Simple but RC overhead everywhere |
| Explicit Lazy(a) | Biased RC | CAS force (shared) / simple (local) | Best combo: fast local, safe shared |
| Explicit Lazy(a) | Thread-local arenas | Simple force (no sharing) | Cannot share lazy values across threads |
| ~field sugar | Biased RC | CAS for shared fields | Natural: struct fields may be shared |
| Auto-force (Option C) | Any | Same as above | Transparent coercion hides force cost |

**Recommended exploration path**: `Explicit Lazy(a)` + `~field sugar` + `Biased RC` + `CAS force for shared / simple for local`.

---

## 15. Comparison Tables

### 15.1 Laziness Approach Comparison Across Languages

| Language | Default | Explicit Lazy | Demand Analysis | Corecursion | Space Leak Prevention |
|----------|---------|--------------|-----------------|-------------|----------------------|
| Haskell/GHC | Lazy | `!` (strict) | Strictness analysis | Natural | `BangPatterns`, `Strict` pragma |
| OCaml | Strict | `Lazy.t` | No | Manual `Lazy.t` | N/A (strict default) |
| Scala | Strict | `lazy val` | No | Manual | N/A |
| Kotlin | Strict | `by lazy` | No | Manual | N/A |
| Lean 4 | Strict | `Thunk α` | Partial | Manual | N/A |
| Idris 2 | Strict | `Lazy a` | No | `Inf` for codata | N/A |
| Koka | Strict | No built-in | No | Manual | N/A |
| **tulam** | **Strict** | **Lazy(a), ~** | **Yes (suggestions)** | **Detection + suggestion** | **Eager closure drop** |

### 15.2 Threading/RC Approach Comparison

| Language | GC/RC | Threading | STM | Lazy+Threading |
|----------|-------|-----------|-----|----------------|
| Haskell/GHC | Tracing GC | Green (RTS) | Yes (built-in) | Blackholing + CAS |
| OCaml 5 | Tracing GC | Domains (M:N) | No (planned) | N/A (strict) |
| Lean 4 | RC (biased) | Task (M:N) | No | N/A (strict) |
| Swift | ARC (atomic) | GCD/async | No | N/A (strict) |
| Rust | Ownership | OS threads | No (crates) | N/A (strict) |
| Koka | Perceus RC | Limited | No | N/A |
| Go | Tracing GC | Goroutines (M:N) | No | N/A (strict) |
| **tulam** | **Perceus RC** | **Hybrid (M:N)** | **Yes (planned)** | **CAS / biased** |

### 15.3 Memory Overhead Comparison

| System | Header Size | RC Cost (local) | RC Cost (shared) | Lazy Overhead |
|--------|-------------|-----------------|------------------|---------------|
| GHC | 16 bytes (info + fwd) | N/A (tracing GC) | N/A | 0 (everything is thunk-ready) |
| OCaml 5 | 8 bytes (header) | N/A (tracing GC) | N/A | 24 bytes (Lazy.t closure) |
| Lean 4 | 8 bytes (RC + tag) | 1 non-atomic op | 1 atomic op | ~16 bytes (Thunk) |
| Swift | 16 bytes (isa + RC) | 1 atomic op | 1 atomic op | N/A |
| Koka | 8 bytes (tag + RC) | 1 non-atomic op | N/A (single-threaded) | N/A |
| **tulam (projected)** | **8-16 bytes** | **1 non-atomic op** | **1 atomic op** | **~16-24 bytes** |

---

## 16. Open Questions

### Laziness

1. **Auto-force semantics**: Should pattern matching auto-force `Lazy` fields? (Option B is pragmatic but hides cost)
2. **Syntax sugar scope**: Should `~` work only on fields, or also on expressions and let-bindings?
3. **Demand analysis priority**: Should laziness suggestions be in Phase A (MVP) or deferred to Phase C+?
4. **Corecursion in the type system**: Should we add a `codata` keyword for coinductive types (like Idris `Inf`)?
5. **Stream fusion**: Can we fuse lazy list operations (map/filter/fold) to avoid intermediate thunks entirely?

### Threading

6. **RC strategy**: Biased RC adds per-object overhead. Is the compact header (Option B, §12.3) feasible?
7. **Green thread stacks**: Segmented stacks (Go-style) or stackless continuations (Kotlin-style)?
8. **STM priority**: Should STM be in the initial threading MVP or a later addition?
9. **FFI and threading**: How do blocking FFI calls interact with the green thread scheduler?
10. **Thread-local vs shared default**: Should values be thread-local by default with explicit sharing, or shared by default?

### Combined

11. **Lazy + STM**: Can `TVar(Lazy(a))` work? (forcing inside a transaction has interesting semantics)
12. **Lazy + structured concurrency**: Should `force` be an effect? (`Eff { lazy: LazyEval } a`)
13. **Profiling**: How to profile lazy value lifetimes and thread contention in a unified tool?
14. **Incremental adoption**: What's the minimum viable threading support that doesn't require lazy?

---

## 17. References

### Laziness

- Hackett, J. & Hutton, G. (2019). "Call-by-Need Is Clairvoyant Call-by-Value." *ICFP 2019*. — Theoretical justification for strict-with-selective-lazy.
- Marlow, S. & Peyton Jones, S. (2012). "The GHC Inliner." *JFP*. — Demand analysis and strictness in GHC.
- Okasaki, C. (1998). *Purely Functional Data Structures*. Cambridge University Press. — Amortized lazy data structures.
- Danielsson, N.A. et al. (2006). "Fast and Loose Reasoning is Morally Correct." *POPL 2006*. — Semantic foundations for laziness reasoning.
- Sergey, I. et al. (2014). "Modular, Higher-Order Cardinality Analysis in Theory and Practice." *POPL 2014*. — GHC's usage analysis for let-to-case and lazy-to-strict.
- Abel, A. (2010). "MiniAgda: Integrating Sized and Dependent Types." *PAR 2010*. — Sized types for termination/productivity checking.

### Reference Counting & Memory

- Reinking, A. et al. (2021). "Perceus: Garbage Free Reference Counting with Reuse." *ICFP 2021*. — Core RC design.
- de Moura, L. & Ullrich, S. (2021). "The Lean 4 Theorem Prover and Programming Language." *CADE 2021*. — Biased RC for Lean 4.
- Bacon, D. et al. (2001). "A Unified Theory of Garbage Collection." *OOPSLA 2001*. — RC vs tracing duality.
- Dice, D. et al. (2001). "Thin Locks: Featherweight Synchronization for Java." *PLDI 2001*. — Biased locking (inspiration for biased RC).
- Levanoni, Y. & Petrank, E. (2006). "An On-the-Fly Reference-Counting Garbage Collector for Java." *TOPLAS*. — Concurrent RC techniques.

### STM & Concurrency

- Harris, T. et al. (2005). "Composable Memory Transactions." *PPoPP 2005*. — The original STM paper (GHC's design).
- Dice, D. et al. (2006). "Transactional Locking II (TL2)." *DISC 2006*. — Pessimistic STM alternative.
- Herlihy, M. & Shavit, N. (2008). *The Art of Multiprocessor Programming*. — Comprehensive concurrent data structures reference.
- Drepper, U. (2011). "Futexes Are Tricky." — Linux futex for wait/wake in CAS-based lazy forcing.
- Blumofe, R. & Leiserson, C. (1999). "Scheduling Multithreaded Computations by Work Stealing." *JACM*. — Work-stealing for fork-join.
- Swalens, J. et al. (2016). "Transactional Tasks: Parallelism in Software Transactions." *ECOOP 2016*. — STM + parallelism interaction.
- Marlow, S. et al. (2009). "Runtime Support for Multicore Haskell." *ICFP 2009*. — GHC's RTS design, lazy blackholing.

---

## 18. Phased Implementation Strategy: Single-Threaded First

### 18.1 Key Insight: Start Single-Threaded, Upgrade Cleanly

The core compilation pipeline — monomorphization, defunctionalization, worker/wrapper, demand analysis, pattern match compilation, Perceus RC — is **entirely orthogonal to threading**. These passes produce identical LIR regardless of whether the runtime is single-threaded or multi-threaded.

This enables a clean phased approach where the single-threaded backend is **feature-complete** before any threading concern is introduced.

### 18.2 The Three Phases

```
┌─────────────────────────────────────────────────────────────────────┐
│ Phase 1: Single-Threaded LLVM Backend (Foundation + Optimizations) │
│                                                                     │
│  All CLM→LIR→LLVM compilation, all optimizations,                  │
│  Perceus RC (non-atomic), Lazy(a) with simple force,               │
│  full numeric perf, full ADT support                               │
│                                                                     │
│  Header: 8 bytes [tag(2) + arity(1) + rc_or_idx(4) + pad(1)]      │
│  RC: non-atomic inc/dec (bit 31 of rc always 0)                    │
│  Lazy: 3-state tag [0=unevaluated, 1=evaluated, 2=evaluating]     │
│         (state 2 used only for recursive-force detection)          │
├─────────────────────────────────────────────────────────────────────┤
│ Phase 2: OS-Thread Multi-Threading (Minimal Concurrency)           │
│                                                                     │
│  Biased RC via side-table promotion (bit 31 = 1 → side table),    │
│  CAS-based lazy force for shared values,                           │
│  Fork-join for par {} blocks (work-stealing deque),                │
│  OS threads (1:1) for Async effect,                                │
│  No green threads, no STM                                          │
│                                                                     │
│  Header: SAME 8 bytes (bit 31 flag distinguishes local vs shared)  │
│  RC: non-atomic for owner (unchanged), atomic for shared counter   │
│  Lazy: same 3 states, but state transitions use CAS for shared     │
├─────────────────────────────────────────────────────────────────────┤
│ Phase 3: Full Concurrency Runtime                                  │
│                                                                     │
│  Green thread scheduler (M:N), STM (log-based optimistic),        │
│  Channel runtime, structured concurrency enforcement,              │
│  Stack checks in codegen (for green thread stacks),                │
│  Profiling + debugging integration                                 │
│                                                                     │
│  Header: may grow to 16 bytes for green thread metadata            │
│  New codegen: function prologues emit stack-check instructions     │
│  STM: TVar runtime, transaction log, retry/commit                  │
└─────────────────────────────────────────────────────────────────────┘
```

### 18.3 What to Account for Now (Zero-Cost Future-Proofing)

Two design choices cost nothing in Phase 1 but prevent painful retrofits later:

**1. Object Header: Reserve Bit 31**

```
rc_or_idx field (4 bytes / 32 bits):
  ┌──────────────────────────────────┐
  │ bit 31 │ bits 30..0             │
  │ = 0    │ = local refcount       │  ← Phase 1: always 0, pure local RC
  │ = 1    │ = side table index     │  ← Phase 2+: shared object, look up side table
  └──────────────────────────────────┘
```

In Phase 1, bit 31 is always 0. All RC operations are non-atomic word increments on bits 30..0 (max 2^31 = 2 billion refs — more than sufficient). In Phase 2, setting bit 31 = 1 promotes the object to shared status with a side-table entry containing `{local_rc, shared_rc, owner_tid}`.

**Cost in Phase 1**: Zero. Literally identical code — we just don't use the full 32-bit range.

**2. Lazy(a): Use 3-State Tag from Day One**

```
Lazy tag byte:
  0 = unevaluated (closure stored)
  1 = evaluated (value stored)
  2 = being-evaluated (in-progress)
```

In Phase 1 (single-threaded), state 2 is entered before evaluating and checked for recursive forcing:

```c
// Single-threaded force:
force(lazy):
  if lazy.tag == 1: return lazy.value      // Cached
  if lazy.tag == 2: error("recursive force detected!")  // Infinite loop!
  lazy.tag = 2                             // Mark as in-progress
  result = evaluate(lazy.closure)
  lazy.value = result
  lazy.tag = 1                             // Mark as evaluated
  drop(lazy.closure)                       // Free closure (prevent space leak)
  return result
```

In Phase 2, the same states get CAS semantics for thread safety:

```c
// Multi-threaded force:
force(lazy):
  tag = atomic_load(lazy.tag)
  if tag == 1: return lazy.value
  if tag == 2: wait_and_retry()
  if CAS(lazy.tag, 0, 2):                 // Win the race to evaluate
    result = evaluate(lazy.closure)
    lazy.value = result
    atomic_store(lazy.tag, 1)
    wake_waiters(lazy)
    return result
  else: retry()                            // Lost the race
```

**Cost in Phase 1**: One extra byte per Lazy object (already needed for the tag). The recursive-force check is a bonus diagnostic that helps catch bugs.

### 18.4 What's Genuinely Hard to Retrofit

| Feature | Difficulty to Add Later | Mitigation |
|---------|------------------------|------------|
| Side-table RC (biased RC) | Easy — bit 31 flip, add side table | Reserve bit 31 now |
| CAS-based lazy force | Easy — swap force function | Use 3-state tag now |
| OS threads (1:1) | Easy — pthread_create, atomic shared_rc | No codegen changes needed |
| Fork-join work stealing | Easy — runtime library addition | No codegen changes needed |
| Green thread stacks | **Hard** — function prologues need stack-check instructions | Defer to Phase 3; start with OS threads |
| STM | Medium — runtime addition, but needs careful RC interaction | Defer to Phase 3 |

**The only genuinely hard-to-retrofit feature is green thread stacks**, because it requires every function's codegen to emit a stack-overflow check prologue. But OS threads (Phase 2) need zero codegen changes — they use the standard C stack.

### 18.5 Phase Transition Contract

**Phase 1 → Phase 2 contract** (no recompilation of user code needed):
- Object header layout is identical (8 bytes, bit 31 meaning changes)
- Lazy(a) representation is identical (3-state tag, meaning of state 2 changes)
- All LIR-level code is unchanged — only runtime library functions are swapped
- Runtime library is linked dynamically (or re-linked statically with new implementations)

**Phase 2 → Phase 3 contract** (recompilation required):
- Function prologues change (stack-check addition for green threads)
- Header may grow for per-object scheduler metadata
- STM requires new intrinsics and effect handler compilation changes

---

*This document is a design exploration. No final decisions have been made. Each option will be evaluated through prototyping and benchmarking before committing to an implementation.*
