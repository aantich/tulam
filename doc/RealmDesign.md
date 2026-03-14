# Realm Design: Categorical Heterogeneous Execution for tulam

## 1. Design Philosophy

tulam is built on two primitives: **tuples** and **lambdas**. The Realm system extends this foundation to a world where computation happens not just locally, but across GPUs, remote servers, browsers, cloud functions, .NET runtimes, and beyond — all through a single, categorically-grounded abstraction.

The core insight: **an execution environment is a category** (objects = representable types, morphisms = executable functions), **marshalling is a functor** between categories, and **coordination is composition of arrows** across categories. This maps directly onto tulam's existing algebra/morphism/categorical framework.

### Core Tenets

1. **Write once, run anywhere — literally.** The same function composes into a GPU pipeline, a distributed map-reduce, or a local thread pool. The realm abstraction handles marshalling, dispatch, and failure automatically.
2. **Categorical composition is the API.** Pipelines are built with `>>>` (sequential), `fanout` (parallel), and `split` (independent). No framework classes, no boilerplate — just arrow composition.
3. **Realms are discovered at runtime, typed at compile time.** The compiler knows "this runs on a GPU realm" and generates correct marshalling code. Which specific GPU, on which machine — that's runtime configuration.
4. **Zero-cost when local.** Thread-to-thread marshalling within shared memory is identity. The abstraction adds overhead only when crossing real boundaries (network, device memory, process isolation).
5. **Failure is algebraic.** Each realm declares its failure modes as a type. Recovery strategies compose with `>>>` just like the happy path. The compiler checks exhaustiveness.

### Why Not Other Approaches?

| Approach | Why Not |
|----------|---------|
| Erlang/OTP | Location-transparent but untyped messages, no compile-time safety for cross-node data |
| Ray (Python) | Runtime-only, no compile-time marshalling verification, GIL limitations |
| CUDA/OpenCL | GPU-specific, doesn't generalize to network/browser/cloud |
| gRPC/Thrift | Boilerplate-heavy, code generation, no composition |
| Spark | Data-parallel only, no heterogeneous compute (GPU + cloud + local in one pipeline) |
| Actor model alone | Forces message-passing even when shared state is simpler (same criticism as for concurrency) |
| SYCL/oneAPI | Good heterogeneous model but C++ ceremony, no categorical composition |

tulam's approach: a **single categorical abstraction** (RealmArrow) that subsumes threads, GPU dispatch, distributed computing, and cross-runtime interop. The arrow structure gives us composability, the type system gives us safety, and the effect system gives us realm capabilities.

### Relationship to Existing Design Documents

- **ConcurrencyDesign.md**: Realms subsume the async/structured concurrency model. A `Thread` realm IS the concurrency primitive. The `Async` effect becomes a derived capability of thread-capable realms.
- **CategoricalDesign.md**: RealmArrow extends the Category/Arrow hierarchy with cross-realm semantics. All existing arrow combinators (`>>>`, `first`, `second`, `fanout`, `split`) work on RealmArrows.
- **InteropDesign.md**: Each interop target (.NET, JS, native) becomes a Realm. The existing `import ... target` mechanism maps to realm capability declarations. Extern types are realm-local types.
- **PrimitiveDesign.md**: GPU SIMD types are realm-local primitives of the GPU realm. The repr system provides one mechanism for marshalling; the Realm Marshal morphism generalizes it.

---

## 2. Core Abstractions

### 2.1 Realm — The Execution Environment

A Realm is a category of computations: it has types it can represent, functions it can execute, and a way to run them.

```tulam
/// A computational realm — an abstract execution environment.
/// Objects in the realm category are types representable in that realm.
/// Morphisms are functions executable in that realm.
///
/// Every realm declares:
///   - What capabilities it provides (which effects it can handle)
///   - What failure modes it has
///   - How to address a specific instance of this realm
algebra Realm(r: Type1) = {
    /// The failure modes of this realm (sum type)
    type Failure(r);

    /// How to locate a specific instance (IP address, device ID, thread ID, etc.)
    type Address(r);

    /// Execute a pure computation within this realm
    function eval(f: a -> b, x: Remote(r, a)) : Remote(r, b);

    /// Lift a local value into this realm
    function lift(x: a) : Remote(r, a) requires Portable(a);

    /// Extract a value from this realm back to local
    function extract(x: Remote(r, a)) : a requires Portable(a);

    /// Laws: eval preserves identity and composition
    law evalId(x: Remote(r, a)) =
        eval(id, x) === x;
    law evalCompose(f: b -> c, g: a -> b, x: Remote(r, a)) =
        eval(compose(f, g), x) === eval(f, eval(g, x));
};
```

### 2.2 Remote — A Value in Another Realm

```tulam
/// A value of type `a` that resides in realm `r`.
/// Cannot be inspected locally — must be `extract`ed or processed via `eval`.
/// This is the key abstraction: Remote values are opaque handles.
///
/// At runtime:
///   - Local/Thread: a plain pointer (zero overhead)
///   - GPU: a device memory handle
///   - Remote: a reference ID on the remote node
///   - Browser: a JS object reference
///   - Cloud: a serialized payload or storage key
primitive Remote(r: Type1, a: Type);
```

### 2.3 Portable — Types That Cross Realm Boundaries

```tulam
/// A type that can be marshalled across realm boundaries.
/// Auto-derived for types composed of primitives + other Portable types.
/// User provides explicit instances only for opaque/extern types.
algebra Portable(a: Type) = {
    function serialize(x: a) : Bytes;
    function deserialize(b: Bytes) : Maybe(a);

    law roundtrip(x: a) =
        deserialize(serialize(x)) === Just(x);
};
```

**Auto-derivation rules:**

| Type form | Derived Portable instance |
|-----------|--------------------------|
| Primitive types (Int, Float64, String, Char, Byte, Bool) | Built-in binary encoding |
| Sum types (`A + B + C`) | Tag byte + recursive serialization of fields |
| Product types / records (`x:A * y:B`) | Sequential serialization of fields |
| `Maybe(a)`, `Either(a,b)`, `List(a)` | Recursive, using inner Portable instances |
| Type parameters | Constraint propagation: `Portable(List(a)) requires Portable(a)` |
| Function types (`a -> b`) | **NOT Portable** — functions cannot cross realm boundaries (see §2.4) |
| `Remote(r, a)` | **NOT Portable** — realm-local references are not transferable |
| Opaque/extern types | Must provide explicit instance |

### 2.4 Portable Functions — Closures That Can Travel

Plain functions (`a -> b`) are not Portable because they contain code pointers and captured closures that are realm-specific. However, many distributed patterns require sending *computation* to data. We introduce:

```tulam
/// A function that can be sent across realm boundaries.
/// Must be a top-level named function or a closure over only Portable values.
/// The compiler verifies this constraint statically.
type PortableFn(a: Type, b: Type) requires Portable(a), Portable(b);

/// Lift a top-level function into a PortableFn
/// (compiler checks: no non-Portable free variables)
function portable(f: a -> b) : PortableFn(a, b) = intrinsic;

/// Apply a PortableFn
function applyPortable(f: PortableFn(a, b), x: a) : b = intrinsic;
```

The compiler statically verifies that `portable(f)` only captures Portable values. This is checked during type checking (Pass 3) by inspecting the closure's free variables.

At runtime:
- **Same-machine realms (Thread, Local)**: PortableFn is just a function pointer — zero overhead
- **Cross-machine realms (Remote, Cloud)**: PortableFn is serialized as (function name + serialized captures)
- **GPU realm**: PortableFn must be a restricted subset (no recursion, no allocation) — checked at compile time

### 2.5 Realm Capabilities — Effects as What a Realm Can Do

Each realm declares which effects it can handle. This reuses tulam's existing effect system directly.

```tulam
/// Capability declaration: this realm can handle these effects
/// The compiler checks at pipeline construction time that the realm
/// can handle what you're asking it to do.

realm Local : handles { Compute, Memory, SharedMem, IO, Console, FileIO };
realm Thread : handles { Compute, Memory, SharedMem };
realm GPU : handles { Compute, SIMD, DeviceMemory };
realm Browser : handles { Compute, Console, Http, DOM };
realm DotNet : handles { Compute, Memory, IO, DotNetInterop };
realm Remote(addr: Address) : handles { Compute, Memory, IO };
realm Cloud(provider: CloudProvider) : handles { Compute, IO, Storage };
```

Capability checking is compile-time: if you try to `runOn [GPU] (readFile("data.txt"))`, the compiler rejects it because GPU doesn't handle `FileIO`.

---

## 3. RealmArrow — The Fundamental Composition Unit

### 3.1 Definition

A RealmArrow is a computation that takes input in one realm and produces output in (possibly another) realm. It is the core unit of the entire framework.

```tulam
/// A computation from type `a` in realm `r1` to type `b` in realm `r2`.
/// When r1 = r2, this is a local computation within one realm.
/// When r1 /= r2, this crosses a realm boundary (marshalling happens automatically).
///
/// RealmArrow is THE fundamental abstraction:
///   - A local function is RealmArrow(Local, Local, a, b)
///   - A GPU kernel is RealmArrow(Local, GPU, Input, Output)
///   - An RPC call is RealmArrow(Local, Remote(addr), Request, Response)
///   - A thread fork is RealmArrow(Local, Thread, a, b)
type RealmArrow(r1: Type1, r2: Type1, a: Type, b: Type);
```

### 3.2 Category Instance — Sequential Composition

```tulam
/// RealmArrows form a category: they compose sequentially with >>>.
/// When composing f: r1->r2 with g: r2->r3, marshalling between r2's
/// representation is handled automatically if needed.
instance Category(RealmArrow) = {
    /// Identity: stay in the same realm, pass through unchanged
    function id() : RealmArrow(r, r, a, a);

    /// Compose: chain two realm-crossing computations.
    /// The intermediate realm r2 must be compatible — types must be
    /// marshalable between r1's r2-representation and r2's self-representation.
    function compose(f: RealmArrow(r2, r3, b, c),
                     g: RealmArrow(r1, r2, a, b))
        : RealmArrow(r1, r3, a, c);

    law leftId(f: RealmArrow(r1, r2, a, b)) =
        compose(id(), f) === f;
    law rightId(f: RealmArrow(r1, r2, a, b)) =
        compose(f, id()) === f;
    law assoc(f: RealmArrow(r3, r4, c, d),
              g: RealmArrow(r2, r3, b, c),
              h: RealmArrow(r1, r2, a, b)) =
        compose(f, compose(g, h)) === compose(compose(f, g), h);
};
```

### 3.3 Arrow Instance — Parallel Composition

```tulam
/// RealmArrows form an Arrow: they support parallel composition,
/// fan-out, and lifting of pure functions.
instance Arrow(RealmArrow) = {
    /// Lift a pure function into a realm arrow (stays in same realm)
    function arr(f: a -> b) : RealmArrow(r, r, a, b);

    /// Process first component, pass second through unchanged
    function first(f: RealmArrow(r1, r2, a, b))
        : RealmArrow(r1, r2, {a, c}, {b, c});

    /// Process second component (derived from first)
    function second(f: RealmArrow(r1, r2, a, b))
        : RealmArrow(r1, r2, {c, a}, {c, b});

    /// Run two arrows in parallel on independent inputs
    /// Both arrows execute concurrently when realms support it
    function split(f: RealmArrow(r1, r2, a, b),
                   g: RealmArrow(r3, r4, c, d))
        : RealmArrow({r1, r3}, {r2, r4}, {a, c}, {b, d});

    /// Send same input to two different computations (possibly different realms)
    function fanout(f: RealmArrow(r, r2, a, b),
                    g: RealmArrow(r, r3, a, c))
        : RealmArrow(r, {r2, r3}, a, {b, c});

    law arrId() = arr(id) === id;
    law arrCompose(f: a -> b, g: b -> c) =
        arr(compose(g, f)) === compose(arr(g), arr(f));
};
```

### 3.4 Realm-Specific Arrow Constructors

```tulam
/// Execute a function on a specific realm.
/// The compiler inserts marshalling at the boundary automatically.
function runOn [r: Type1] (f: a -> b) : RealmArrow(Local, r, a, b)
    requires Realm(r), Portable(a), Portable(b);

/// Marshal a value from one realm to another (explicit boundary crossing).
/// Usually inserted automatically by >>> composition, but can be explicit.
function marshal [from: Type1, to: Type1] ()
    : RealmArrow(from, to, a, a)
    requires Realm(from), Realm(to), Portable(a);

/// Execute on a specific realm instance (runtime-discovered)
function runAt [r: Type1] (addr: Address(r), f: a -> b)
    : RealmArrow(Local, r, a, b)
    requires Realm(r), Portable(a), Portable(b);
```

### 3.5 The `>>>` Operator

The `>>>` operator is the primary user-facing API. It is `compose` with arguments flipped (left-to-right reading order), consistent with Haskell's Arrow convention and already defined in tulam's `CategoricalDesign.md`.

```tulam
/// Left-to-right arrow composition (flip of compose)
function (>>>)(f: RealmArrow(r1, r2, a, b),
               g: RealmArrow(r2, r3, b, c))
    : RealmArrow(r1, r3, a, c) =
    compose(g, f);

infixl 1 (>>>);
```

---

## 4. Threading as Realms

A thread is a realm with zero-cost marshalling (shared memory). This unification means every threading pattern is expressible as arrow composition — no separate concurrency API needed.

### 4.1 Thread Realm Declaration

```tulam
/// A thread realm — same machine, shared address space.
/// Marshalling between Thread realms is identity (zero-cost pointer sharing).
realm Thread : handles { Compute, Memory, SharedMem };

/// The realm of the current thread — sequential execution
realm Local : handles { Compute, Memory, SharedMem, IO, Console, FileIO };

/// Thread pool — a composite realm managing N worker threads
realm ThreadPool(n: Int) : handles { Compute, Memory, SharedMem };
```

### 4.2 Zero-Cost Marshalling

When both source and target realms have the `SharedMem` capability, the compiler knows they share an address space. Marshalling is identity:

```tulam
/// Auto-generated by the compiler when both realms share SharedMem
instance Marshal(a, Thread, Thread) = {
    function marshalTo(x) = x;    /// identity — just pass the pointer
};

instance Marshal(a, Local, Thread) = {
    function marshalTo(x) = x;    /// same address space
};

instance Marshal(a, Thread, Local) = {
    function marshalTo(x) = x;    /// same address space
};
```

This means all thread-based arrow compositions have **zero marshalling overhead**.

### 4.3 Threading Patterns as Arrow Compositions

Every common threading pattern maps directly to arrow combinators:

| Pattern | Arrow Expression | Description |
|---------|-----------------|-------------|
| Sequential execution | `step1 >>> step2 >>> step3` | Same realm, composed arrows |
| Fork-join | `fanout(runOn [Thread] f, runOn [Thread] g)` | Two threads, join results |
| Parallel map | `arr(partition) >>> fanout(map(runOn [Thread] f)) >>> arr(flatten)` | Partition data, process in parallel, merge |
| Pipeline parallelism | `runOn [Thread] stage1 >>> runOn [Thread] stage2 >>> runOn [Thread] stage3` | Each stage on its own thread, data flows through |
| Thread pool dispatch | `runOn [ThreadPool(8)] f` | Pool manages thread assignment |
| Producer-consumer | `runOn [Thread] produce >>> channel >>> runOn [Thread] consume` | Buffered channel between pipeline stages |
| Map-reduce | `arr(partition) >>> fanout(map(runOn [Thread] mapper)) >>> arr(flatten) >>> arr(reducer)` | Classic map-reduce within one machine |

### 4.4 Examples

```tulam
/// Parallel quicksort: partition, sort halves on separate threads, merge
function parallelSort(xs: List(Int)) : List(Int) =
    arr(partition)
    >>> fanout(runOn [Thread] (sort), runOn [Thread] (sort))
    >>> arr(fn({left, right}) = merge(left, right));

/// Pipeline: read → parse → transform → write, each stage on its own thread
filePipeline : RealmArrow(Local, Local, FilePath, Unit) =
    runOn [Thread] (readLines)
    >>> runOn [Thread] (map(parseLine))
    >>> runOn [Thread] (map(transform))
    >>> runOn [Thread] (writeResults);

/// Image processing with GPU fallback to CPU
processImage : RealmArrow(Local, Local, Image, Image) =
    runOn [GPU] (applyFilters)
    |> onFail (Fallback [Local]);   /// if GPU unavailable, run locally
```

### 4.5 Relationship to ConcurrencyDesign.md

The existing `Async` effect from ConcurrencyDesign.md maps directly to realm operations:

| Async effect | Realm equivalent |
|--------------|------------------|
| `fork(body)` | `runOn [Thread] body` (returns a RealmArrow, not a Task) |
| `await(task)` | Arrow composition `>>>` (result flows through the pipeline) |
| `cancel(task)` | Failure recovery: `\|> onFail (Abort)` |
| `yield()` | Internal to thread realm scheduler (not exposed in arrow API) |
| `par(f, g)` | `fanout(runOn [Thread] f, runOn [Thread] g)` |
| Structured concurrency | Arrow scoping: all child arrows in a `fanout` must complete before `>>>` continues |

The arrow model provides structured concurrency automatically: `fanout` creates child computations that must all complete before the arrow produces its output. There is no way to "leak" a thread — every spawned computation is part of an arrow that eventually joins.

---

## 5. GPU as a First-Class Realm

### 5.1 GPU Realm Declaration

```tulam
/// GPU compute realm — massively parallel, restricted computation model
realm GPU : handles { Compute, SIMD, DeviceMemory };

/// Specific GPU device (runtime-discovered)
realm GPUDevice(id: Int) : handles { Compute, SIMD, DeviceMemory };

/// GPU failure modes
type Failure(GPU)
    = OutOfMemory * requested: Int * available: Int
    + DeviceLost * reason: String
    + KernelCompilationError * message: String
    + UnsupportedOperation * op: String;
```

### 5.2 GPU-Specific Marshalling

GPU memory layout differs from CPU. The Marshal morphism handles this:

```tulam
/// GPU requires packed, aligned data. The compiler generates appropriate layouts.
instance Marshal(Array(Float64), Local, GPU) = {
    /// CPU array → GPU device buffer (DMA transfer)
    function marshalTo(arr) = intrinsic;  /// cudaMemcpyHostToDevice or equivalent
};

instance Marshal(Array(Float64), GPU, Local) = {
    /// GPU device buffer → CPU array
    function marshalTo(arr) = intrinsic;  /// cudaMemcpyDeviceToHost or equivalent
};

/// Matrices marshal with optional transposition for GPU-optimal layout
instance Marshal(Matrix, Local, GPU) = {
    function marshalTo(m) = transposeAndPack(m);  /// row-major → column-major
};
```

### 5.3 GPU Computation Restrictions

Not all functions can run on a GPU. The compiler enforces restrictions when `runOn [GPU]` is used:

| Allowed on GPU | Not allowed on GPU |
|----------------|--------------------|
| Arithmetic on primitives (Int, Float32, Float64) | Heap allocation (no `new`, no list construction) |
| SIMD operations | Recursion (no stack on GPU) |
| Array indexing and slicing | String operations |
| Conditional expressions (`if/match`) | IO effects (Console, FileIO) |
| Fixed-iteration loops | Dynamic dispatch (no algebra/morphism calls) |
| Struct field access | Closures over non-primitive types |

These restrictions are checked at compile time. The type checker verifies that the function body inside `runOn [GPU]` uses only GPU-compatible operations. This check is equivalent to requiring that the function's effect set is a subset of GPU's declared capabilities.

### 5.4 GPU Pipeline Examples

```tulam
/// Matrix multiplication: marshal to GPU, compute, marshal back
matmul(a: Matrix, b: Matrix) : Matrix =
    arr(fn(_) = {a, b})
    >>> marshal [GPU]
    >>> runOn [GPU] (fn({a, b}) = gpuMatmul(a, b))
    >>> marshal [Local];

/// Image processing pipeline: chain GPU kernels without intermediate marshalling
processImage : RealmArrow(Local, Local, Image, Image) =
    marshal [GPU]                          /// upload once
    >>> runOn [GPU] (blur)                 /// GPU → GPU (no marshal!)
    >>> runOn [GPU] (edgeDetect)           /// GPU → GPU (no marshal!)
    >>> runOn [GPU] (colorCorrect)         /// GPU → GPU (no marshal!)
    >>> marshal [Local];                   /// download once

/// The compiler optimizes: consecutive runOn [GPU] steps are fused.
/// Data stays on GPU between steps — no intermediate round-trips.

/// Training step with automatic GPU fallback
trainStep : RealmArrow(Local, Local, Batch, Metrics) =
    marshal [GPU]
    >>> runOn [GPU] (forward >>> backward >>> updateWeights)
    >>> marshal [Local]
    >>> arr(extractMetrics)
    |> onFail [GPU] (Fallback [Local]);    /// CPU fallback for testing/CI
```

### 5.5 SIMD Integration

tulam's existing SIMD types (from `lib/SIMD.tl`) are the native vocabulary of the GPU realm:

```tulam
/// SIMD types are Portable between Local and GPU
instance Portable(Vec4f) = intrinsic;   /// 128-bit packed float vector
instance Portable(Vec8f) = intrinsic;   /// 256-bit packed float vector

/// On GPU realm, SIMD operations map to native GPU SIMD instructions
/// On Local realm, they map to CPU SIMD (SSE/AVX/NEON)
/// Same code, different backend — the realm handles it
function dotProduct(a: Vec4f, b: Vec4f) : Float32 =
    simdSum(simdMul(a, b));  /// works on both Local and GPU realms
```

---

## 6. Marshalling — The Functorial Bridge

### 6.1 Marshal Morphism

Marshalling between realms is a morphism — it relates the representation of a type in one realm to its representation in another. Different realms may represent the same logical type very differently.

```tulam
/// Realm-specific marshalling.
/// The compiler auto-derives this for most types, but explicit instances
/// allow realm-specific optimizations (GPU data layout, cloud encoding, etc.)
morphism Marshal(a: Type, from: Type1, to: Type1) = {
    function marshalTo(x: Remote(from, a)) : Remote(to, a);

    /// Optional inverse — not all marshalling is invertible
    /// (e.g., lossy GPU float16 conversion)
};
```

### 6.2 Auto-Derivation

The compiler automatically derives `Marshal` instances using the following rules:

| Source → Target | Strategy | Cost |
|----------------|----------|------|
| SharedMem → SharedMem (Thread↔Thread, Local↔Thread) | Identity (pointer sharing) | Zero |
| Local → Remote | Portable serialization → network send | Serialization + network latency |
| Remote → Local | Network receive → Portable deserialization | Network latency + deserialization |
| Local → GPU | Pack to device-compatible layout → DMA transfer | Packing + DMA |
| GPU → Local | DMA transfer → unpack from device layout | DMA + unpacking |
| Local → Browser | JSON/binary encoding → postMessage | Encoding + IPC |
| Local → DotNet | CLR type mapping (from InteropDesign.md) | Minimal (same-process) |
| Local → Cloud | Portable serialization → cloud storage/message | Serialization + cloud latency |
| GPU → GPU (same device) | Identity | Zero |
| GPU → GPU (different device) | Device-to-device transfer (peer DMA if available) | DMA |
| Remote → Remote (different nodes) | Coordinated by source realm (no double-hop to Local) | Network |

### 6.3 Marshal as a Natural Transformation

Mathematically, marshalling is a natural transformation between two functors:
- The "Local representation" functor maps each type to how it looks in local memory
- The "GPU representation" functor maps each type to how it looks in device memory
- `Marshal(_, Local, GPU)` is a natural transformation between these functors

This means marshalling commutes with `fmap`:

```
marshal >>> runOn [GPU] (fmap(f))  ===  runOn [Local] (fmap(f)) >>> marshal
```

The compiler can use this law to optimize: if `f` is cheap locally and expensive on GPU, do it before marshalling. If `f` is cheap on GPU, do it after.

### 6.4 Explicit Marshal Instances

When auto-derivation produces suboptimal results, the user provides explicit instances:

```tulam
/// Sparse matrices: marshal as compressed sparse row (CSR) for GPU
instance Marshal(SparseMatrix, Local, GPU) = {
    function marshalTo(m) = {
        values   = marshal(m.values);     /// Float64 array → GPU buffer
        colIdx   = marshal(m.colIndices); /// Int array → GPU buffer
        rowPtr   = marshal(m.rowPointers); /// Int array → GPU buffer
        {values, colIdx, rowPtr}          /// GPU-side CSR representation
    };
};

/// JSON encoding for browser realm
instance Marshal(UserProfile, Local, Browser) = {
    function marshalTo(profile) = toJSON(profile);  /// uses derived Show/JSON
};
```

---

## 7. Failure Model — Algebraic Failure with Recovery Arrows

### 7.1 Design Principle

Failure in the Realm system is not an afterthought — it is a first-class part of the arrow type. Every realm declares its failure modes as a sum type. Recovery strategies are themselves arrows, composing with the happy path.

### 7.2 Realm Failure Types

Each realm declares what can go wrong:

```tulam
/// Local realm: can't really fail (modulo process-level crashes)
type Failure(Local) = Unreachable;

/// Thread realm: limited failure modes
type Failure(Thread)
    = ThreadKilled * reason: String
    + StackOverflow
    + Deadlock;

/// GPU realm: hardware-specific failures
type Failure(GPU)
    = OutOfMemory     * requested: Int * available: Int
    + DeviceLost      * reason: String
    + KernelError     * message: String
    + UnsupportedOp   * op: String;

/// Remote realm: network failures
type Failure(Remote)
    = ConnectionRefused * addr: Address(Remote)
    + Timeout           * elapsed: Duration * limit: Duration
    + NetworkPartition
    + RemoteCrash       * addr: Address(Remote) * reason: String
    + SerializationError * message: String;

/// Browser realm: tab/runtime failures
type Failure(Browser)
    = TabClosed
    + SecurityError    * message: String
    + QuotaExceeded;

/// Cloud realm: cloud-specific failures
type Failure(Cloud)
    = ColdStart         * delay: Duration
    + FunctionTimeout   * limit: Duration
    + QuotaExceeded
    + ProviderError     * code: Int * message: String
    + RegionUnavailable * region: String;
```

### 7.3 Recovery Strategies

Recovery is a sum type describing what to do when a realm fails:

```tulam
/// What to do when a realm step fails.
/// Recovery strategies are values — they can be computed, composed, and stored.
type Recovery(r1: Type1, r2: Type1, a: Type, b: Type)
    = Retry       * maxAttempts: Int * delay: Duration
    + RetryWith   * maxAttempts: Int * backoff: Int -> Duration  /// exponential backoff etc.
    + Fallback    * realm: r3 * arrow: RealmArrow(r1, r3, a, b) /// try a different realm
    + Degrade     * value: b                                     /// return a degraded/default result
    + Escalate                                                   /// propagate failure upward
    + Abort       * reason: String;                              /// hard stop, propagate error
```

### 7.4 Attaching Recovery to Arrows

The `|> onFail` combinator attaches a recovery strategy to an arrow:

```tulam
/// Attach a failure recovery strategy to a realm arrow.
/// This wraps the arrow: if the original arrow fails with Failure(r2),
/// the recovery strategy is applied.
function onFail [r2: Type1]
    (strategy: Recovery(r1, r2, a, b))
    (arrow: RealmArrow(r1, r2, a, b))
    : RealmArrow(r1, r2, a, b);

/// Attach a handler function that pattern-matches on failure
function onFailWith [r2: Type1]
    (handler: Failure(r2) -> Recovery(r1, r2, a, b))
    (arrow: RealmArrow(r1, r2, a, b))
    : RealmArrow(r1, r2, a, b);
```

### 7.5 Recovery Composition

Recovery composes with `>>>` — each step in a pipeline can have its own recovery strategy:

```tulam
/// Each step has independent recovery
pipeline : RealmArrow(Local, Local, Input, Output) =
    (runOn [GPU] (preprocess)   |> onFail (Fallback [Local] (arr(preprocessCPU))))
    >>> (runOn [GPU] (compute)  |> onFail (Retry(3, seconds(1))))
    >>> (runOn [Cloud] (store)  |> onFail (RetryWith(5, exponentialBackoff)));

/// Recovery can redirect to a completely different realm
robustCompute : RealmArrow(Local, Local, Data, Result) =
    runOn [GPU] (gpuCompute)
    |> onFailWith (fn(failure) = match failure
        | OutOfMemory(req, avail) -> Fallback [Local] (arr(cpuCompute))
        | DeviceLost(reason)      -> Fallback [Local] (arr(cpuCompute))
        | KernelError(msg)        -> Abort(msg));
```

### 7.6 Exhaustiveness Checking

The compiler can check that all failure modes of a realm are handled:

```tulam
/// WARNING: non-exhaustive failure handling for GPU realm
/// Unhandled: KernelError, UnsupportedOp
incomplete : RealmArrow(Local, Local, Data, Result) =
    runOn [GPU] (compute)
    |> onFailWith (fn(failure) = match failure
        | OutOfMemory(_, _) -> Fallback [Local] (arr(cpuCompute))
        | DeviceLost(_)     -> Retry(3, seconds(1)));
    /// ^ compiler warning: KernelError and UnsupportedOp not handled
```

With `checkCoverage` enabled, the compiler warns about unhandled failure cases, just like it warns about incomplete pattern matches.

---

## 8. Transactional Arrows — STM Across Realms

### 8.1 Layer 1: Local STM (Within a Single Realm)

Traditional STM, expressed as an algebra, works within any realm that has `SharedMem` capability.

```tulam
/// Software Transactional Memory — composable atomic transactions.
/// Requires SharedMem capability (threads on same machine).
algebra STM(r: Type1) requires Realm(r), SharedMem(r) = {
    /// A transactional variable in realm r
    type TVar(r, a);

    /// Create a new transactional variable
    function newTVar(init: a) : TVar(r, a);

    /// Read a TVar (within a transaction)
    function readTVar(v: TVar(r, a)) : a;

    /// Write a TVar (within a transaction)
    function writeTVar(v: TVar(r, a), val: a) : Unit;

    /// Retry: block until a read variable changes, then re-run the transaction
    function retry() : a;

    /// Choice: try left transaction, if it retries, try right
    function orElse(left: Tx(r, a, b), right: Tx(r, a, b)) : Tx(r, a, b);

    law atomicity =
        "Transactions either commit entirely or have no effect";
    law isolation =
        "Intermediate states within a transaction are not visible to other transactions";
    law composability(tx1: Tx(r, a, b), tx2: Tx(r, b, c)) =
        "compose(tx2, tx1) is a single atomic transaction";
};
```

### 8.2 Tx — Transaction as an Arrow

A transaction within a single realm is an arrow. This is the key insight: **composing transactions with `>>>` produces a single atomic transaction**.

```tulam
/// A transaction within realm r, from type a to type b.
/// This IS an arrow — compose with >>> for atomic composition.
type Tx(r: Type1, a: Type, b: Type)
    extends RealmArrow(r, r, a, b);

/// Transactions form a Category — sequential composition is atomic
instance Category(Tx(r)) requires STM(r) = {
    function id() : Tx(r, a, a);
    function compose(f: Tx(r, b, c), g: Tx(r, a, b)) : Tx(r, a, c);
    /// GUARANTEE: compose(f, g) executes atomically as ONE transaction
};

/// Transactions form an Arrow — parallel composition is atomic
instance Arrow(Tx(r)) requires STM(r) = {
    function arr(f: a -> b) : Tx(r, a, b);
    function first(f: Tx(r, a, b)) : Tx(r, {a, c}, {b, c});
    /// GUARANTEE: split(f, g) executes atomically as ONE transaction
};

/// Run a transaction atomically
function atomically(tx: Tx(r, Unit, a)) : RealmArrow(r, r, Unit, a);
```

### 8.3 STM Examples

```tulam
/// Transfer money between accounts — atomically
transfer(from: TVar(r, Int), to: TVar(r, Int), amount: Int)
    : Tx(r, Unit, Unit) =
    arr(fn(_) = readTVar(from))
    >>> arr(fn(balance) =
        if balance < amount
        then retry()             /// block until funds available!
        else {
            writeTVar(from, balance - amount);
            writeTVar(to, readTVar(to) + amount)
        });

/// Compose two transfers into one atomic operation
/// BOTH transfers happen or NEITHER does
doubleTransfer(a: TVar(r, Int), b: TVar(r, Int), c: TVar(r, Int))
    : Tx(r, Unit, Unit) =
    transfer(a, b, 100) >>> transfer(b, c, 50);
    /// This is ONE atomic transaction — not two separate ones!

/// Choice: try transferring from primary, fallback to secondary
robustTransfer(primary: TVar(r, Int), secondary: TVar(r, Int),
               to: TVar(r, Int), amount: Int)
    : Tx(r, Unit, Unit) =
    orElse(
        transfer(primary, to, amount),     /// try primary first
        transfer(secondary, to, amount)    /// if primary retries, try secondary
    );
```

### 8.4 Layer 2: Cross-Realm Transactions (TxArrow)

When a transaction spans realms that don't share memory, we need a distributed commit protocol. The arrow structure naturally provides the 2-phase commit skeleton.

```tulam
/// A transactional arrow across realm boundaries.
/// Each realm segment is a "prepare" phase; the whole arrow commits or rolls back.
///
/// Semantics: TxArrow(r1, r2, a, b) means:
///   1. Prepare phase in r1 (tentative changes, locks held)
///   2. Marshal data from r1 to r2
///   3. Prepare phase in r2 (tentative changes, locks held)
///   4. If ALL phases prepared successfully: commit all
///   5. If ANY phase fails: rollback all
type TxArrow(r1: Type1, r2: Type1, a: Type, b: Type)
    extends RealmArrow(r1, r2, a, b);

/// TxArrows form a Category — composition IS the 2PC protocol
instance Category(TxArrow) = {
    function compose(f: TxArrow(r2, r3, b, c),
                     g: TxArrow(r1, r2, a, b))
        : TxArrow(r1, r3, a, c);
    /// The compiler generates:
    ///   Phase 1: prepare g (in realm r1)
    ///   Phase 2: marshal to r2
    ///   Phase 3: prepare f (in realm r2)
    ///   Phase 4: if all prepared → commit g, marshal to r3, commit f
    ///            if any failed  → rollback f, rollback g
};
```

### 8.5 Layer 3: Sagas (Eventually Consistent Cross-Realm)

When strong consistency is too expensive (CAP theorem: you sacrifice availability), Sagas provide eventual consistency with compensation.

```tulam
/// A saga step: forward action + compensating action (undo).
/// If any step in a composed saga fails, all preceding steps are compensated
/// in reverse order.
type Saga(r1: Type1, r2: Type1, a: Type, b: Type) = {
    forward:    RealmArrow(r1, r2, a, b),
    compensate: RealmArrow(r2, r1, b, Unit)
};

/// Sagas form a Category — failure triggers reverse compensation
instance Category(Saga) = {
    function id() : Saga(r, r, a, a) = {
        forward = id(),
        compensate = arr(fn(_) = Unit)
    };

    function compose(f: Saga(r2, r3, b, c),
                     g: Saga(r1, r2, a, b))
        : Saga(r1, r3, a, c) = {
        forward = g.forward >>> f.forward,
        compensate = fn(c) = {
            f.compensate(c);               /// undo f first (most recent)
            g.compensate(g.forward.result); /// then undo g
        }
    };
};

/// Construct a saga step
function saga(forward: RealmArrow(r1, r2, a, b),
              compensate: RealmArrow(r2, r1, b, Unit))
    : Saga(r1, r2, a, b) = { forward, compensate };
```

### 8.6 Saga Example: E-Commerce Order

```tulam
/// Each step has a forward action and a compensating (undo) action.
/// If shipping fails, inventory is released, then payment is refunded — automatically.
placeOrder : Saga(Local, Local, Order, Confirmation) =
    saga(
        forward    = runOn [PaymentService] (chargeCard),
        compensate = runOn [PaymentService] (refundCard)
    )
    >>> saga(
        forward    = runOn [InventoryService] (reserveStock),
        compensate = runOn [InventoryService] (releaseStock)
    )
    >>> saga(
        forward    = runOn [ShippingService] (createShipment),
        compensate = runOn [ShippingService] (cancelShipment)
    );

/// Execute the saga — handles compensation automatically on failure
function executeSaga(s: Saga(r1, r2, a, b), input: a) : Either(SagaError, b);
```

### 8.7 Layer 4: CRDTs (Conflict-Free Replicated State)

For state shared across realms without any coordination protocol:

```tulam
/// A conflict-free replicated data type.
/// Merge is commutative, associative, and idempotent — no conflicts possible.
algebra CRDT(a: Type) = {
    function merge(x: a, y: a) : a;

    law commutative(x: a, y: a)      = merge(x, y) === merge(y, x);
    law associative(x: a, y: a, z: a) = merge(x, merge(y, z)) === merge(merge(x, y), z);
    law idempotent(x: a)              = merge(x, x) === x;
};

/// A replicated variable: each realm has a local copy.
/// Reads are local (fast). Writes are local + async propagation.
/// Convergence is guaranteed by the CRDT laws.
type RVar(a: Type) requires CRDT(a);

/// Create a replicated variable across multiple realm instances
function replicate(init: a, realms: List(r)) : RVar(a)
    requires CRDT(a), Realm(r), Portable(a);

/// Read local copy (instant, no coordination)
function readLocal(v: RVar(a)) : a;

/// Write to local copy (propagates asynchronously to other replicas)
function writeLocal(v: RVar(a), val: a) : Unit;
```

**Common CRDT instances:**

```tulam
/// G-Counter: grow-only counter (increment only)
instance CRDT(GCounter) = {
    function merge(a, b) = pointwiseMax(a, b);
};

/// LWW-Register: last-writer-wins (timestamp-based)
instance CRDT(LWWRegister(a)) = {
    function merge(a, b) = if a.timestamp >= b.timestamp then a else b;
};

/// OR-Set: observed-remove set (add and remove without conflicts)
instance CRDT(ORSet(a)) = {
    function merge(a, b) = {
        adds    = union(a.adds, b.adds),
        removes = union(a.removes, b.removes)
    };
};
```

### 8.8 Choosing the Right Consistency Model

| Model | Consistency | Availability | Latency | Use When |
|-------|-------------|-------------|---------|----------|
| **Tx** (local STM) | Strong (serializable) | N/A (single realm) | Microseconds | Shared mutable state within one machine |
| **TxArrow** (distributed 2PC) | Strong (serializable) | Reduced during prepare phase | Milliseconds–seconds | Financial transactions, inventory, anything requiring ACID |
| **Saga** (compensating) | Eventual | High | Milliseconds per step | Multi-service workflows, e-commerce, bookings |
| **CRDT** (conflict-free) | Eventual (convergent) | Maximum | Microseconds (local read) | Counters, caches, collaborative editing, presence |

---

## 9. Realm Discovery and Configuration

### 9.1 Compile-Time vs Runtime

Realm *types* are known at compile time — the compiler generates correct marshalling code, checks capability constraints, and verifies failure handling. Realm *instances* (specific GPUs, specific servers) are discovered at runtime.

```tulam
/// Compile time: the compiler knows this targets a GPU realm
pipeline = runOn [GPU] (compute);          /// type-checked at compile time

/// Runtime: which specific GPU
function main() = action {
    gpus <- discoverRealms [GPU] ();       /// runtime discovery
    gpu  = head(gpus);                     /// pick first available
    result <- runAt [GPU] (gpu.address, compute, input);
    putStrLn(show(result))
};
```

### 9.2 Realm Discovery

```tulam
/// Discover available realm instances at runtime.
/// Returns a list of realm descriptors with addresses and capabilities.
effect RealmDiscovery = {
    function discoverRealms [r: Type1] () : List(RealmInfo(r))
        requires Realm(r);
    function realmStatus [r: Type1] (addr: Address(r)) : RealmStatus;
    function realmCapabilities [r: Type1] (addr: Address(r)) : Set(String);
};

type RealmInfo(r: Type1) = {
    address:      Address(r),
    status:       RealmStatus,
    capabilities: Set(String),
    load:         Float64          /// current load (0.0 to 1.0)
};

type RealmStatus
    = Available
    + Busy        * currentLoad: Float64
    + Unavailable * reason: String
    + Unknown;
```

### 9.3 Realm Configuration

```tulam
/// Configuration for realm instances
/// Passed when connecting to a realm
type RealmConfig(r: Type1) = {
    address:        Address(r),
    timeout:        Duration,
    retryPolicy:    RetryPolicy,
    marshalFormat:  MarshalFormat     /// Binary, JSON, Protobuf, etc.
};

/// Default configs for common realms
defaultLocalConfig  : RealmConfig(Local)  = { timeout = never, ... };
defaultGPUConfig    : RealmConfig(GPU)    = { timeout = seconds(30), ... };
defaultRemoteConfig : RealmConfig(Remote) = { timeout = seconds(10), ... };
```

### 9.4 Placement Strategies

For automatic realm selection:

```tulam
/// A placement strategy decides where to execute a computation.
/// The scheduler uses this when multiple realms are available.
algebra Placement(s: Type) = {
    function selectRealm(candidates: List(RealmInfo(r)),
                         computation: RealmArrow(Local, r, a, b))
        : Address(r);
};

/// Built-in strategies
type RoundRobin;
type LeastLoaded;
type LocalityAware;   /// prefer realms close to data
type CostAware;       /// minimize cloud compute cost

instance Placement(RoundRobin) = {
    function selectRealm(candidates, _) = cycleNext(candidates).address;
};

instance Placement(LeastLoaded) = {
    function selectRealm(candidates, _) =
        minimumBy(fn(r) = r.load, candidates).address;
};
```

---

## 10. Supervision Trees — Built on Algebraic Failure

Supervision trees are a library pattern built on the algebraic failure primitives (§7), not a language-level concept.

### 10.1 Supervisor Algebra

```tulam
/// A supervisor watches a group of realm arrows and applies a restart strategy.
/// Built entirely from the recovery primitives in §7.
algebra Supervisor(s: Type) = {
    function strategy(s) : RestartStrategy;
    function maxRestarts(s) : Int;             /// max restarts within window
    function restartWindow(s) : Duration;      /// time window for counting restarts
};

type RestartStrategy
    = OneForOne      /// restart only the failed computation
    + OneForAll      /// restart all computations in the group
    + RestForOne     /// restart the failed + all started after it
    + Escalate;      /// propagate failure to parent supervisor
```

### 10.2 Supervised Pipelines

```tulam
/// Wrap a realm arrow in supervision.
/// On failure, the supervisor's strategy is applied.
function supervised [s: Type]
    (supervisor: s, arrow: RealmArrow(r1, r2, a, b))
    : RealmArrow(r1, r2, a, b)
    requires Supervisor(s) =
    arrow |> onFailWith (fn(failure) =
        match strategy(supervisor)
        | OneForOne -> Retry(maxRestarts(supervisor), restartDelay(supervisor))
        | Escalate  -> Escalate);

/// Supervisor tree: hierarchical supervision
function supervisorTree(
    root: RestartStrategy,
    children: List(RealmArrow(r1, r2, a, b)))
    : RealmArrow(r1, r2, List(a), List(b));
```

### 10.3 Example: Supervised Worker Pool

```tulam
/// A pool of workers across remote realms, supervised for fault tolerance
workerPool : RealmArrow(Local, Local, List(Task), List(Result)) =
    supervisorTree(
        OneForOne,     /// restart individual failed workers
        [
            supervised(worker, runOn [Remote("node1")] (processTask)),
            supervised(worker, runOn [Remote("node2")] (processTask)),
            supervised(worker, runOn [Remote("node3")] (processTask))
        ]
    )
    where worker = DefaultSupervisor { maxRestarts = 5, window = minutes(1) };
```

---

## 11. Built-In Realms

### 11.1 Summary Table

| Realm | Capabilities | Failure Type | Marshalling Cost | Address Type | Use Case |
|-------|-------------|-------------|-----------------|-------------|----------|
| **Local** | Compute, Memory, SharedMem, IO, Console, FileIO | Unreachable | Zero | Unit | Default execution environment |
| **Thread** | Compute, Memory, SharedMem | ThreadKilled, StackOverflow, Deadlock | Zero (shared memory) | ThreadId | Parallel computation on same machine |
| **ThreadPool(n)** | Compute, Memory, SharedMem | (delegates to Thread) | Zero | PoolId | Managed parallel execution |
| **GPU** | Compute, SIMD, DeviceMemory | OutOfMemory, DeviceLost, KernelError, UnsupportedOp | DMA transfer | DeviceId | Massively parallel numeric computation |
| **Browser** | Compute, Console, Http, DOM | TabClosed, SecurityError, QuotaExceeded | JSON/binary encoding | TabId | Client-side web computation |
| **DotNet** | Compute, Memory, IO, DotNetInterop | CLRException | CLR type mapping | ProcessId | .NET interop |
| **Remote(addr)** | Compute, Memory, IO | ConnectionRefused, Timeout, NetworkPartition, RemoteCrash, SerializationError | Serialization + network | IpAddr + Port | Distributed computation |
| **Cloud(provider)** | Compute, IO, Storage | ColdStart, FunctionTimeout, QuotaExceeded, ProviderError, RegionUnavailable | Serialization + cloud | FunctionArn / URL | Serverless execution |

### 11.2 Realm Relationship Diagram

```
                              Realm(r)
                            /    |    \
                           /     |     \
                 SharedMem(r)    |     NetworkAccessible(r)
                /        \      |      /          \
             Local     Thread   |   Remote      Cloud
               |         |     |      |           |
               |    ThreadPool |      |     Cloud(AWS)
               |               |      |     Cloud(GCP)
               |              GPU   Remote("10.0.0.1")
               |                    Remote("10.0.0.2")
            DotNet
            Browser
```

### 11.3 Composite Realms

Some realms are built from other realms:

```tulam
/// Thread pool: manages N Thread realms internally
realm ThreadPool(n: Int) : handles { Compute, Memory, SharedMem } = {
    workers: Array(Thread) = Array.generate(n, fn(i) = Thread(i));

    function eval(f, x) = {
        worker = selectLeastBusy(workers);
        runOn [worker] (f)(x)
    };
};

/// GPU cluster: multiple GPUs accessible as one realm
realm GPUCluster(devices: List(GPUDevice)) : handles { Compute, SIMD, DeviceMemory } = {
    function eval(f, x) = {
        device = selectByMemory(devices, sizeOf(x));
        runOn [device] (f)(x)
    };
};

/// Hybrid realm: tries GPU first, falls back to ThreadPool
realm HybridCompute(gpu: GPU, pool: ThreadPool)
    : handles { Compute, Memory, SharedMem, SIMD } = {
    function eval(f, x) =
        runOn [gpu] (f)(x) |> onFail (Fallback [pool] (arr(f)));
};
```

---

## 12. Complete Examples

### 12.1 Image Processing Pipeline (Local + GPU)

```tulam
/// Load image locally, process on GPU, save locally.
/// If GPU is unavailable, fall back to CPU processing.
imageProcessor : RealmArrow(Local, Local, FilePath, FilePath) =
    arr(loadImage)                                    /// Local → Local
    >>> marshal [GPU]                                 /// Local → GPU (upload texture)
    >>> runOn [GPU] (gaussianBlur(5.0))               /// GPU → GPU
    >>> runOn [GPU] (sobelEdgeDetect)                 /// GPU → GPU (no intermediate download!)
    >>> runOn [GPU] (adjustContrast(1.2))             /// GPU → GPU
    >>> marshal [Local]                               /// GPU → Local (download result)
    >>> arr(fn(img) = { saveImage(img, "out.png"); "out.png" })
    |> onFail [GPU] (Fallback [Local] (
        arr(loadImage) >>> arr(cpuBlur) >>> arr(cpuEdgeDetect) >>> arr(cpuContrast)
        >>> arr(fn(img) = { saveImage(img, "out.png"); "out.png" })
    ));
```

### 12.2 Distributed Map-Reduce

```tulam
/// Distribute work across remote nodes, reduce locally.
mapReduce(mapper: a -> b, reducer: List(b) -> c)
    : RealmArrow(Local, Local, List(a), c) =
    arr(partition(3))                                 /// split into 3 chunks
    >>> fanout(
        runOn [Remote("node1")] (map(mapper)),        /// chunk 1 → node1
        runOn [Remote("node2")] (map(mapper)),        /// chunk 2 → node2
        runOn [Remote("node3")] (map(mapper))         /// chunk 3 → node3
    )
    >>> arr(fn({r1, r2, r3}) = flatten([r1, r2, r3])) /// merge results locally
    >>> arr(reducer)
    |> onFail [Remote] (Retry(3, seconds(2)));        /// retry on network failure
```

### 12.3 ML Training Step (Local + GPU + Cloud)

```tulam
/// Load batch locally, train on GPU, upload metrics to cloud dashboard.
trainAndReport : RealmArrow(Local, Local, Batch, Unit) =
    /// GPU computation
    marshal [GPU]
    >>> runOn [GPU] (forward >>> backward >>> updateWeights)
    >>> marshal [Local]
    /// Extract metrics and upload
    >>> fanout(
        arr(extractLoss),                              /// Local: extract loss value
        arr(extractGradientNorm)                       /// Local: extract gradient stats
    )
    >>> arr(fn({loss, gradNorm}) = MetricsReport(loss, gradNorm))
    >>> runOn [Cloud("dashboard")] (uploadMetrics)     /// Cloud: store metrics
    >>> arr(fn(_) = Unit)
    |> onFail [GPU] (Fallback [Local] (arr(cpuTrainStep)))
    |> onFail [Cloud] (Degrade(Unit));                 /// if cloud fails, skip metrics
```

### 12.4 Full-Stack Web App (Browser + Server + Database)

```tulam
/// User action flows from browser → server → database and back.
handleUserAction : RealmArrow(Browser, Browser, UserAction, UIUpdate) =
    marshal [Local]                                    /// Browser → Server
    >>> arr(validateAction)                            /// Server: validate
    >>> runOn [Remote("db")] (persistAction)           /// Server → Database
    >>> arr(computeUIUpdate)                           /// Server: compute response
    >>> marshal [Browser]                              /// Server → Browser
    |> onFail [Remote("db")] (
        onFailWith(fn(f) = match f
            | Timeout(_, _)      -> Retry(2, seconds(1))
            | ConnectionRefused  -> Degrade(UIUpdate.error("Database unavailable"))
            | _                  -> Escalate));
```

### 12.5 Atomic Cross-Service Transaction

```tulam
/// Book a trip: flight + hotel + car. All or nothing.
bookTrip : Saga(Local, Local, TripRequest, TripConfirmation) =
    saga(
        forward    = runOn [Remote("flights")] (bookFlight),
        compensate = runOn [Remote("flights")] (cancelFlight)
    )
    >>> saga(
        forward    = runOn [Remote("hotels")] (bookHotel),
        compensate = runOn [Remote("hotels")] (cancelHotel)
    )
    >>> saga(
        forward    = runOn [Remote("cars")] (bookCar),
        compensate = runOn [Remote("cars")] (cancelCar)
    )
    >>> saga(
        forward    = arr(fn({flight, hotel, car}) =
            TripConfirmation(flight, hotel, car)),
        compensate = arr(fn(_) = Unit)  /// nothing to undo for pure computation
    );
```

### 12.6 STM Bank Transfer (Local Threads)

```tulam
/// Two concurrent transfers sharing accounts — STM ensures correctness.
bankDemo : RealmArrow(Local, Local, Unit, Unit) =
    arr(fn(_) = {
        alice   = atomically(newTVar(1000));
        bob     = atomically(newTVar(500));
        charlie = atomically(newTVar(200));
        {alice, bob, charlie}
    })
    >>> fanout(
        /// Thread 1: Alice → Bob $200
        runOn [Thread] (fn({alice, bob, _}) =
            atomically(transfer(alice, bob, 200))),
        /// Thread 2: Bob → Charlie $100
        runOn [Thread] (fn({_, bob, charlie}) =
            atomically(transfer(bob, charlie, 100)))
    )
    >>> arr(fn(_) = Unit);

/// Both transfers are atomic individually.
/// STM ensures no lost updates even though they touch `bob` concurrently.
```

### 12.7 CRDT-Based Collaborative Counter

```tulam
/// A distributed counter across three nodes — no coordination needed.
collaborativeCounter : RealmArrow(Local, Local, Unit, Int) =
    arr(fn(_) = replicate(GCounter.zero, [
        Remote("node1"), Remote("node2"), Remote("node3")
    ]))
    >>> fanout(
        runOn [Remote("node1")] (fn(counter) = {
            repeat(1000, fn(_) = increment(counter));
            readLocal(counter)
        }),
        runOn [Remote("node2")] (fn(counter) = {
            repeat(1000, fn(_) = increment(counter));
            readLocal(counter)
        }),
        runOn [Remote("node3")] (fn(counter) = {
            repeat(1000, fn(_) = increment(counter));
            readLocal(counter)
        })
    )
    >>> arr(fn({a, b, c}) = merge(a, merge(b, c)).value);
    /// Result converges to 3000 after all replicas sync
```

---

## 13. Arrow Hierarchy — The Full Picture

### 13.1 Type Hierarchy

```
Category(arr)                          /// composable morphisms with identity
 └── Arrow(arr)                        /// + lifting, products, parallelism
      └── RealmArrow(r1, r2, a, b)     /// cross-realm computation
           ├── Tx(r, a, b)             /// single-realm STM transaction
           │    └── extends RealmArrow(r, r, a, b)
           │    └── >>> gives atomic composition
           ├── TxArrow(r1, r2, a, b)   /// cross-realm 2PC transaction
           │    └── extends RealmArrow(r1, r2, a, b)
           │    └── >>> generates 2PC protocol
           ├── Saga(r1, r2, a, b)      /// cross-realm compensating transaction
           │    └── has forward: RealmArrow + compensate: RealmArrow
           │    └── >>> chains with automatic compensation on failure
           └── (user-defined arrow types that extend RealmArrow)
```

### 13.2 Consistency Model Hierarchy

```
Strong consistency                              Eventual consistency
◄──────────────────────────────────────────────────────────────────►

Tx(r)          TxArrow(r1,r2)       Saga(r1,r2)           CRDT
(local STM)    (distributed 2PC)    (compensation)    (conflict-free)
│              │                    │                 │
│ Serializable │ Serializable       │ Saga-level      │ Convergent
│ Single realm │ Multi-realm        │ Multi-realm     │ Multi-realm
│ μs latency   │ ms-s latency       │ ms per step     │ μs local read
│ Blocks        │ Blocks during      │ Available       │ Always available
│ on conflict  │ prepare phase      │ (compensates)   │ (no coordination)
```

### 13.3 Common Patterns Table

| Pattern | Arrow Expression | Realms Involved |
|---------|-----------------|-----------------|
| Local function call | `arr(f)` | Local → Local |
| Sequential pipeline | `f >>> g >>> h` | Same realm chain |
| Fork-join parallelism | `fanout(runOn [Thread] f, runOn [Thread] g)` | Local → Thread × Thread → Local |
| GPU offload | `marshal [GPU] >>> runOn [GPU] f >>> marshal [Local]` | Local → GPU → Local |
| GPU kernel chain | `marshal [GPU] >>> runOn [GPU] (f >>> g >>> h) >>> marshal [Local]` | Local → GPU → Local (no intermediate transfer) |
| RPC call | `runOn [Remote(addr)] f` | Local → Remote → Local |
| Distributed map-reduce | `arr(partition) >>> fanout(map(runOn [Remote] f)) >>> arr(flatten) >>> arr(reduce)` | Local → Remote × N → Local |
| Pipeline parallelism | `runOn [Thread] f >>> runOn [Thread] g >>> runOn [Thread] h` | Thread → Thread → Thread |
| Thread pool dispatch | `runOn [ThreadPool(8)] f` | Local → ThreadPool → Local |
| Browser round-trip | `marshal [Browser] >>> runOn [Browser] f >>> marshal [Local]` | Local → Browser → Local |
| Cloud function | `runOn [Cloud("lambda")] f` | Local → Cloud → Local |
| Atomic transaction | `atomically(tx1 >>> tx2 >>> tx3)` | Single realm (STM) |
| Distributed transaction | `txArrow1 >>> txArrow2 >>> txArrow3` | Multi-realm (2PC) |
| Saga | `saga(f1, undo1) >>> saga(f2, undo2)` | Multi-realm (compensating) |
| CRDT counter | `replicate(init, realms)` → `readLocal` / `writeLocal` | Multi-realm (convergent) |
| GPU with CPU fallback | `runOn [GPU] f \|> onFail (Fallback [Local] (arr(cpuF)))` | GPU preferred, Local fallback |
| Retry with backoff | `runOn [Remote] f \|> onFail (RetryWith(5, exponentialBackoff))` | Remote with retry |
| Supervised worker | `supervised(supervisor, runOn [Remote] worker)` | Remote with supervision |

---

## 14. Integration with Existing tulam Systems

### 14.1 Effect System

Realm capabilities ARE effects. The `handles` clause on a realm declaration specifies which effects the realm can handle:

```tulam
/// This:
realm GPU : handles { Compute, SIMD, DeviceMemory };

/// Means: when you runOn [GPU] (f), the compiler checks that
/// f's effect requirements are a subset of { Compute, SIMD, DeviceMemory }.
/// If f requires Console, it won't compile.
```

Existing effects (Console, FileIO, Exception, State) work unchanged. New realm-specific effects (DeviceMemory, DOM, Storage) are declared for specific realms.

### 14.2 Interop System

Each interop target from InteropDesign.md maps to a realm:

```tulam
/// .NET interop:
/// import System.Windows.Forms target dotnet;
/// becomes: import within DotNet realm
realm DotNet : handles { Compute, Memory, IO, DotNetInterop };

/// Extern types are DotNet-realm-local types:
/// extern class Button = System.Windows.Forms.Button target dotnet;
/// Button is Remote(DotNet, Button) when accessed from Local realm

/// JS interop:
realm Browser : handles { Compute, Console, Http, DOM };

/// C/C++ native interop:
realm Native : handles { Compute, Memory, IO, NativeInterop };
```

The existing `target` blocks in tulam source map directly to realm-specific effect handlers:

```tulam
/// Existing:
target llvm {
    instance Additive(Int) = { function (+)(x, y) = __add_Int(x, y); };
};

/// Realm equivalent:
instance Additive(Int) where Realm(Native) = {
    function (+)(x, y) = __add_Int(x, y);
};
```

### 14.3 Module System

Realm-specific modules:

```tulam
/// Modules can be realm-qualified
module Realm.GPU;          /// GPU-specific utilities
module Realm.Browser;      /// Browser-specific DOM bindings
module Realm.Cloud.AWS;    /// AWS-specific cloud realm

/// Import realm modules like any other
import Realm.GPU;
import Realm.Browser (DOM, fetch);
```

### 14.4 Type Checker Integration

The type checker gains new responsibilities:

1. **Capability checking**: When `runOn [r] (f)` is used, verify f's effects ⊆ r's capabilities
2. **Portability checking**: When crossing realm boundaries, verify all types are Portable
3. **PortableFn checking**: When `portable(f)` is used, verify f captures only Portable values
4. **GPU restriction checking**: When `runOn [GPU] (f)` is used, verify f uses only GPU-compatible operations
5. **Failure exhaustiveness**: When `onFailWith` is used, warn on unhandled failure cases
6. **Transaction integrity**: When composing Tx arrows, verify atomicity guarantees

### 14.5 Compilation Pipeline Integration

```
Source (.tl)
  → Parser (recognizes realm, handles, runOn, marshal, onFail keywords)
  → Surface AST (RealmArrow nodes carry source/target realm info)
  → Pass 1: Environment building (registers realms, capabilities, addresses)
  → Pass 2: Case optimization (unchanged)
  → Pass 3: Type checking (capability check, portability check, GPU restrictions)
  → Pass 3.5: Realm resolution (resolve composite realms, insert auto-marshalling)
  → Pass 4: CLM conversion (RealmArrow → CLMREALM nodes with marshalling)
  → Pass 4.5: Marshalling insertion (auto-derive Marshal instances where needed)
  → Pass 5+: Code generation (realm-specific: GPU → SPIR-V/PTX, Remote → RPC, etc.)
```

---

## 15. Syntax Summary

### 15.1 New Keywords

| Keyword | Context | Meaning |
|---------|---------|---------|
| `realm` | Top-level declaration | Declare a new realm type |
| `handles` | Realm declaration | Specify which effects a realm can handle |
| `runOn` | Expression | Execute computation on a specific realm type |
| `runAt` | Expression | Execute on a specific realm instance (runtime address) |
| `marshal` | Expression | Explicitly marshal data between realms |
| `onFail` | Arrow modifier | Attach failure recovery strategy |
| `onFailWith` | Arrow modifier | Attach failure handler function |
| `portable` | Expression | Lift a function to a PortableFn |
| `atomically` | Expression | Execute a Tx arrow atomically |
| `saga` | Expression | Construct a saga step (forward + compensate) |
| `replicate` | Expression | Create a CRDT-replicated variable |

### 15.2 New Types

| Type | Kind | Meaning |
|------|------|---------|
| `Remote(r, a)` | Type | A value of type `a` residing in realm `r` |
| `RealmArrow(r1, r2, a, b)` | Type | A computation from `a` in `r1` to `b` in `r2` |
| `Tx(r, a, b)` | Type | A transaction arrow within realm `r` |
| `TxArrow(r1, r2, a, b)` | Type | A distributed transaction arrow |
| `Saga(r1, r2, a, b)` | Type | A saga step with forward + compensate |
| `PortableFn(a, b)` | Type | A serializable function |
| `RVar(a)` | Type | A CRDT-replicated variable |
| `Recovery(r1, r2, a, b)` | Type | A failure recovery strategy |
| `RealmInfo(r)` | Type | Runtime information about a realm instance |
| `RealmStatus` | Type | Current status of a realm instance |

### 15.3 New Algebras and Morphisms

| Name | Kind | Meaning |
|------|------|---------|
| `Realm(r)` | Algebra on Type1 | The core realm abstraction |
| `Portable(a)` | Algebra on Type | Types that can cross realm boundaries |
| `STM(r)` | Algebra on Type1 | Software transactional memory within a realm |
| `CRDT(a)` | Algebra on Type | Conflict-free replicated data types |
| `Supervisor(s)` | Algebra on Type | Supervision strategy |
| `Placement(s)` | Algebra on Type | Realm selection strategy |
| `Marshal(a, from, to)` | Morphism | Realm-specific marshalling |

### 15.4 Operator Fixity

```tulam
infixl 1 (>>>);    /// arrow composition (left-to-right)
/// |> onFail is a postfix modifier, parsed specially (like existing |> pipe)
```

---

## 16. Open Questions

### 16.1 STM `retry` Across Realms

**Question**: Should `retry` in a `TxArrow` block the entire distributed transaction, or just the local realm segment?

**Options**:
- **Local only (lean)**: `retry` blocks the local transaction; the distributed coordinator sees it as "preparing, please wait." If the local retry exceeds the transaction timeout, the whole TxArrow aborts.
- **Distributed**: `retry` propagates to the coordinator, which decides whether to wait or abort based on a timeout policy.

**Recommendation**: Local only. Distributed retry is a saga-level concern — if you need "wait and retry across services," use a Saga with a polling step, not a TxArrow with distributed retry.

### 16.2 CRDT Scope

**Question**: Should CRDTs be part of this core design document, or a separate library design?

**Options**:
- **Core**: CRDTs are a fundamental consistency model alongside Tx/TxArrow/Saga — they belong in the hierarchy.
- **Library**: CRDTs are an advanced topic; the core design should focus on arrows and realms.

**Recommendation**: Include in core design (as done here) because the consistency model hierarchy is incomplete without them. The actual CRDT type library (GCounter, ORSet, etc.) lives in a separate library module.

### 16.3 Composite Realms

**Question**: Should composite realms (ThreadPool, GPUCluster, HybridCompute) be first-class language constructs or library patterns?

**Options**:
- **First-class**: `realm ThreadPool(n) = group(Thread, n);` as a language-level composite.
- **Library**: Composite realms are just functions that return realm instances, built using existing realm primitives.

**Recommendation**: Library pattern for now. The `realm` keyword declares capability sets; combining realms is a runtime concern that doesn't need special syntax.

### 16.4 Arrow Notation Syntax

**Question**: Should tulam have dedicated arrow notation (like Haskell's `proc` notation) for complex arrow pipelines, or is `>>>` / `fanout` / `split` sufficient?

**Options**:
- **No special syntax**: `>>>`, `fanout`, `split`, `first`, `second` are sufficient and clear.
- **Proc notation**: `proc x -> do { y <- f -< x; z <- g -< y; returnA -< z }` — more readable for complex routing.
- **Pipeline syntax sugar**: Something lighter than proc notation but more readable than raw combinators.

**Recommendation**: Start with raw combinators (`>>>`, `fanout`, `split`). Add syntactic sugar only if real-world usage shows readability problems. The tulam `action` block already provides monadic sequencing; an equivalent `pipeline` block for arrows could be added later if needed.

### 16.5 `fanout` Arity

**Question**: `fanout` as defined in Arrow takes exactly two arrows. Multi-way fanout (3, 4, N arms) requires nested `fanout` or a variadic version.

**Options**:
- **Binary only**: `fanout(f, g)`. Three-way is `fanout(f, fanout(g, h))` with flattening.
- **Variadic**: `fanout(f, g, h, ...)` using tulam's existing variadic support (n-tuples).
- **List-based**: `fanoutAll(List(RealmArrow(r, r', a, b))) : RealmArrow(r, r', a, List(b))` for homogeneous fanout.

**Recommendation**: Binary `fanout` for the Arrow instance (categorical purity), plus a `fanoutAll` convenience function for the common case of mapping the same computation across multiple realms.

### 16.6 Realm-Qualified Types

**Question**: Should types be realm-qualified in the surface language?

**Options**:
- **Implicit**: The compiler tracks which realm a value is in. Users see `Matrix`, not `Remote(GPU, Matrix)`.
- **Explicit**: Users annotate realm-specific code with `Remote(GPU, Matrix)` when needed.
- **Inferred with annotation**: The compiler infers realms, but users can annotate for clarity. Annotations shown in error messages.

**Recommendation**: Inferred with optional annotation. Most code shouldn't think about realms. The type checker infers realm placement and reports errors with realm information. Users annotate only when disambiguation is needed.

### 16.7 Realm Polymorphism

**Question**: Should functions be polymorphic over realms?

```tulam
/// Does this make sense?
function mapOnAnyRealm [r: Type1] (f: a -> b, xs: Remote(r, List(a)))
    : Remote(r, List(b))
    requires Realm(r);
```

**Options**:
- **Yes, fully polymorphic**: Any function can abstract over realms. Maximum generality.
- **Constrained polymorphism**: Polymorphism only with capability constraints (`requires Realm(r), SharedMem(r)`).
- **No polymorphism**: Functions target specific realms. Generality via arrow composition instead.

**Recommendation**: Constrained polymorphism. Realm polymorphism is useful for generic arrow combinators, but should always carry capability constraints so the compiler can verify safety.

---

## 17. Implementation Roadmap

### Phase R.1: Foundation — RealmArrow + Local + Thread
- [ ] `realm` keyword in parser and AST
- [ ] `RealmArrow` type in Surface AST and type system
- [ ] `Category(RealmArrow)` and `Arrow(RealmArrow)` instances
- [ ] `Local` and `Thread` realms (shared memory, zero-cost marshal)
- [ ] `>>>`, `fanout`, `split`, `arr` working for Local/Thread
- [ ] Basic examples: fork-join, parallel map, pipeline parallelism
- **Dependencies**: CategoricalDesign.md Phase 7 (Category/Arrow in stdlib)

### Phase R.2: Marshalling + Portable
- [ ] `Portable` algebra with auto-derivation
- [ ] `Marshal` morphism with auto-derivation for shared-memory realms
- [ ] `PortableFn` type with static closure checking
- [ ] Serialization infrastructure (Bytes encoding/decoding)
- **Dependencies**: Phase R.1

### Phase R.3: GPU Realm
- [ ] `GPU` realm with capability checking
- [ ] GPU-specific `Marshal` instances (DMA transfer, layout conversion)
- [ ] GPU restriction checking in type checker
- [ ] SPIR-V or PTX code generation for GPU kernel functions
- [ ] GPU ↔ CPU fallback via `onFail`
- **Dependencies**: Phase R.2, LLVM backend (Phase A from LLVMBackendDesign.md)

### Phase R.4: Failure + Recovery
- [ ] `Failure(r)` associated type on Realm algebra
- [ ] `Recovery` sum type
- [ ] `onFail` and `onFailWith` combinators
- [ ] Failure exhaustiveness checking in type checker
- [ ] Retry with backoff
- **Dependencies**: Phase R.1

### Phase R.5: Remote Realm + Networking
- [ ] `Remote(addr)` realm
- [ ] Network `Marshal` instances (serialization + transport)
- [ ] Realm discovery effect (`discoverRealms`)
- [ ] RPC code generation
- **Dependencies**: Phase R.2, Phase R.4, NetworkingDesign.md

### Phase R.6: STM (Local Transactions)
- [ ] `STM(r)` algebra for SharedMem realms
- [ ] `TVar` implementation
- [ ] `Tx(r)` arrow type with atomic composition
- [ ] `retry` and `orElse` combinators
- [ ] `atomically` function
- **Dependencies**: Phase R.1 (Thread realm)

### Phase R.7: Distributed Transactions
- [ ] `TxArrow(r1, r2)` with 2PC protocol generation
- [ ] `Saga` type with compensating composition
- [ ] Saga execution engine
- **Dependencies**: Phase R.5, Phase R.6

### Phase R.8: CRDTs
- [ ] `CRDT` algebra
- [ ] `RVar` replicated variable
- [ ] Built-in CRDT types (GCounter, LWWRegister, ORSet)
- [ ] Async replication across realms
- **Dependencies**: Phase R.5

### Phase R.9: Browser + Cloud + DotNet Realms
- [ ] `Browser` realm with JS codegen bridge
- [ ] `Cloud` realm with serverless function deployment
- [ ] `DotNet` realm integrating with InteropDesign.md
- [ ] Realm-specific Marshal instances for each
- **Dependencies**: Phase R.5, respective backend codegen

### Phase R.10: Advanced
- [ ] Supervision trees (library, built on Phase R.4)
- [ ] Placement strategies (library)
- [ ] Composite realms (ThreadPool, GPUCluster)
- [ ] Arrow notation syntax (if needed based on real-world usage)
- [ ] Realm polymorphism with capability constraints
- [ ] Performance optimization: marshal fusion, realm-local caching
- **Dependencies**: All previous phases

---

## 18. Relation to "Everything is Tuples + Lambdas"

The Realm system doesn't violate tulam's foundation — it extends it:

- A **Realm** is an algebra (a tuple of operations on a type-level entity)
- A **RealmArrow** is a value (a tuple of: source realm, target realm, computation, marshalling)
- **Marshal** is a morphism (a tuple of: a function from one representation to another)
- **Recovery** is a sum type (a tagged tuple)
- **Saga** is a product type (a tuple of: forward arrow, compensate arrow)
- **Tx** is an arrow that happens to guarantee atomicity (algebraic property)
- **CRDT** is an algebra with specific laws (commutativity, associativity, idempotence)

Internally, all of these compile down to tuples and lambdas, with the Realm system being a type discipline that the compiler erases after verification — exactly like the existing categorical vocabulary.

The categorical structure tells us *what operations are valid* (arrows compose, sagas compensate, CRDTs merge), while the runtime representation is just data flowing through functions — tuples and lambdas all the way down.
