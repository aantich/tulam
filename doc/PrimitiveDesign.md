# Primitive Types, Representations, and Acceleration Design

## Motivation

tulam is built on two primitives: tuples and lambdas. But real programs need machine-level numbers, arrays, and high-performance computation. This document designs a system where:

1. Primitive types feel like ordinary types — no `#` suffix, no special syntax
2. User-defined types (e.g. Nat) can interoperate with primitives (e.g. Int) seamlessly
3. Native performance is the default for numeric code
4. GPU/SIMD acceleration is available transparently or with explicit control
5. The algebraic structure system (algebras, laws) drives optimization

---

## 1. Primitive Type Declarations

### 1.1 The `primitive` Keyword

Primitive types are **declared**, not defined. The runtime provides them.

```tulam
-- In prelude.tl (loaded before base.tl)

-- Machine integers (signed, platform-native word size: 64-bit on modern systems)
primitive Int;

-- Fixed-width integers
primitive Int8;
primitive Int16;
primitive Int32;
primitive Int64;

-- Unsigned integers
primitive UInt;
primitive UInt8;
primitive UInt16;
primitive UInt32;
primitive UInt64;

-- Floating point
primitive Float32;
primitive Float64;

-- Text and bytes
primitive Char;        -- Unicode scalar value
primitive Byte;        -- raw byte (unsigned 8-bit)
primitive String;      -- UTF-8 string (opaque, compiler-internal literal carrier)
```

**Note on Strings:** `primitive String` is retained as the compiler-internal string literal type. The user-facing string type is `Str` (a pure tulam record: `type Str = bytes:Array(Byte) * byteLen:Int;`). When `:s newstrings on`, string literals desugar to `Str` constructor calls. The `StringLike` algebra provides universal string operations, and `FromString` enables string literal overloading. See `lib/String/` and `doc/InteropPattern.md`.

After declaration, a primitive type is used identically to any user-defined type. `Int` is just a type name — no magic syntax anywhere it appears.

### 1.2 Parameterized Primitive Types

Some primitive types take type parameters:

```tulam
-- Contiguous arrays (the fundamental bulk container)
primitive Array(a:Type);

-- SIMD vector types (fixed-width, lane-parallel)
primitive Vec2(a:Type);
primitive Vec4(a:Type);
primitive Vec8(a:Type);
primitive Vec16(a:Type);
```

These work exactly like parameterized user types (`Maybe(a)`, `List(a)`) but with compiler-provided representation.

### 1.3 What `primitive` Means to the Compiler

A `primitive` declaration tells the compiler:

1. **No user-visible constructors** — you cannot pattern match on Int's internal structure
2. **Target-specific representation** — the compiler knows how to represent this on each backend
3. **Literals may produce values of this type** — `42` can be an `Int`, `3.14` can be a `Float64`
4. **Operations must be declared explicitly** — no built-in operations exist until algebra instances provide them

A primitive type has NO operations by default. All operations come through algebra instances, which may be `intrinsic` (compiler-provided) or user-defined.

### 1.4 Target Representation Map

| Type | JS | .NET | x86 native | GPU |
|------|-----|------|------------|-----|
| `Int` | `number` (53-bit) / `BigInt` | `System.Int64` | `i64` register | N/A |
| `Int32` | `number` | `System.Int32` | `i32` register | `int` |
| `UInt8` | `number` | `System.Byte` | `u8` register | `uint8_t` |
| `Float32` | `number` | `System.Single` | `f32` (xmm) | `float` |
| `Float64` | `number` | `System.Double` | `f64` (xmm) | `double` |
| `String` | `string` | `System.String` | `char*` + len | N/A |
| `Array(Float32)` | `Float32Array` | `Span<float>` | `float*` + len | device buffer |
| `Vec4(Float32)` | polyfill / WASM SIMD | `Vector128<float>` | `__m128` (SSE) | N/A (use Array) |
| `Vec8(Float32)` | polyfill / WASM SIMD | `Vector256<float>` | `__m256` (AVX) | N/A (use Array) |

---

## 2. The `intrinsic` Keyword

### 2.1 Purpose

`intrinsic` replaces the `primop#` convention. It means "the compiler/runtime provides this implementation using target-native instructions."

### 2.2 Three Levels of Intrinsic

**Instance level** — the entire algebra instance is compiler-provided:

```tulam
instance Num(Int) = intrinsic;
instance Eq(Int) = intrinsic;
instance Ord(Int) = intrinsic;
instance Num(Float64) = intrinsic;
instance Floating(Float64) = intrinsic;
```

The compiler knows how to generate native instructions for every operation in the algebra for this type.

**Function level** — a single function is compiler-provided:

```tulam
function intToString(x:Int) : String = intrinsic;
function sqrt(x:Float64) : Float64 = intrinsic;
function arrayLength(xs:Array(a)) : Int = intrinsic;
```

**Mixed** — an instance with some intrinsic and some user-defined operations:

```tulam
instance Show(Int) = {
    function show(x:Int) : String = intToString(x)
};

instance Num(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
    function (-)(x:Nat, y:Nat) : Nat = minus(x, y);
    function (*)(x:Nat, y:Nat) : Nat = mult(x, y);
    function fromInt(n:Int) : Nat = intToNat(n)
};
```

### 2.3 Compiler Behavior for `intrinsic`

When the compiler encounters `instance Num(Int) = intrinsic`:

1. It registers all operations from the `Num` algebra for type `Int`
2. For each operation, it emits target-native instructions:
   - `(+)` on Int → `add` instruction (x86), `+` operator (JS), `System.Int64.op_Addition` (.NET)
   - `(*)` on Int → `imul` instruction (x86), etc.
3. No CLM intermediate representation is generated — these go directly to codegen

When the compiler encounters `function foo(x:Int) : Int = intrinsic`:

1. It registers `foo` as a function with the given signature
2. It looks up `foo` in a built-in table of known intrinsic functions
3. If not found, it's a compile error (typo protection)

### 2.4 Intrinsic Registry

The compiler maintains a registry mapping `(function_name, type)` pairs to target-specific code generation. This registry is populated at compiler initialization, not at parse time. Adding a new intrinsic requires modifying the compiler, not the standard library.

Example entries:

```
("(+)",  "Int")     → x86: ADD, JS: +, .NET: op_Addition
("(+)",  "Float64") → x86: ADDSD, JS: +, .NET: op_Addition
("sqrt", "Float64") → x86: SQRTSD, JS: Math.sqrt, .NET: System.Math.Sqrt
("(==)", "Int")     → x86: CMP+SETE, JS: ===, .NET: op_Equality
```

---

## 3. The Numeric Algebra Hierarchy

### 3.1 Core Numeric Algebras

```tulam
algebra Num(a:Type) extends Eq(a), Ord(a) = {
    function (+)(x:a, y:a) : a;
    function (-)(x:a, y:a) : a;
    function (*)(x:a, y:a) : a;
    function negate(x:a) : a;
    function abs(x:a) : a;
    function fromInt(n:Int) : a;

    -- Default implementations
    function negate(x:a) : a = fromInt(0) - x;
    function abs(x:a) : a = if x >= fromInt(0) then x else negate(x);

    -- Laws (optimization licenses)
    law addAssociative(x:a, y:a, z:a) = (x + (y + z)) === ((x + y) + z);
    law addCommutative(x:a, y:a) = (x + y) === (y + x);
    law mulAssociative(x:a, y:a, z:a) = (x * (y * z)) === ((x * y) * z);
    law mulDistributive(x:a, y:a, z:a) = (x * (y + z)) === (x * y + x * z);
    law addIdentity(x:a) = (x + fromInt(0)) === x;
    law mulIdentity(x:a) = (x * fromInt(1)) === x
};

algebra Integral(a:Type) extends Num(a) = {
    function div(x:a, y:a) : a;
    function mod(x:a, y:a) : a;
    function toInt(x:a) : Int;

    law divMod(x:a, y:a) = (div(x, y) * y + mod(x, y)) === x
};

algebra Fractional(a:Type) extends Num(a) = {
    function (/)(x:a, y:a) : a;
    function recip(x:a) : a;
    function fromFloat(x:Float64) : a;

    function recip(x:a) : a = fromFloat(1.0) / x
};

algebra Floating(a:Type) extends Fractional(a) = {
    function sqrt(x:a) : a;
    function exp(x:a) : a;
    function log(x:a) : a;
    function sin(x:a) : a;
    function cos(x:a) : a;
    function tan(x:a) : a;
    function pow(x:a, y:a) : a;
    function pi() : a;

    function tan(x:a) : a = sin(x) / cos(x)
};
```

### 3.2 Bitwise Operations

```tulam
algebra Bits(a:Type) = {
    function (.&.)(x:a, y:a) : a;     -- AND
    function (.|.)(x:a, y:a) : a;     -- OR
    function xor(x:a, y:a) : a;       -- XOR
    function complement(x:a) : a;     -- NOT
    function shiftL(x:a, n:Int) : a;  -- left shift
    function shiftR(x:a, n:Int) : a;  -- right shift
    function bitSize(x:a) : Int       -- number of bits
};

instance Bits(Int) = intrinsic;
instance Bits(Int32) = intrinsic;
instance Bits(UInt8) = intrinsic;
-- etc.
```

### 3.3 Intrinsic Instances for All Primitives

```tulam
-- Signed integers
instance Num(Int) = intrinsic;
instance Integral(Int) = intrinsic;
instance Bits(Int) = intrinsic;

instance Num(Int32) = intrinsic;
instance Integral(Int32) = intrinsic;
instance Bits(Int32) = intrinsic;

-- (similar for Int8, Int16, Int64, UInt, UInt8, UInt16, UInt32, UInt64)

-- Floating point
instance Num(Float32) = intrinsic;
instance Fractional(Float32) = intrinsic;
instance Floating(Float32) = intrinsic;

instance Num(Float64) = intrinsic;
instance Fractional(Float64) = intrinsic;
instance Floating(Float64) = intrinsic;

-- Equality and ordering for all primitives
instance Eq(Int) = intrinsic;
instance Ord(Int) = intrinsic;
-- (similar for all primitive types)
```

### 3.4 Num also extends Monoid (additive)

Since `Num` has `(+)` and an identity `fromInt(0)`, every `Num` type is automatically a `Monoid` under addition. The compiler can derive this:

```tulam
-- Automatically derived by the compiler when Num(a) exists:
instance Semigroup(a) requires Num(a) = {
    function combine(x:a, y:a) : a = x + y
};
instance Monoid(a) requires Num(a) = {
    function combine(x:a, y:a) : a = x + y;
    value empty = fromInt(0)
};
```

This means `fold(combine, empty, xs)` on a list of numbers automatically uses `(+)` and `0`.

---

## 4. Polymorphic Literals

### 4.1 Rules

| Literal form | Type | Default |
|-------------|------|---------|
| `42` | `Num(a) => a` | `Int` |
| `3.14` | `Fractional(a) => a` | `Float64` |
| `'x'` | `Char` | `Char` |
| `"hello"` | `String` | `String` |

### 4.2 Resolution

Integer and float literals are **polymorphic**. Their concrete type is determined by context:

```tulam
-- Context determines the type:
42 + 1                    -- Int (default, no other constraint)
42.0 + 1.0                -- Float64 (default for fractional)
let x : Float32 = 42.0   -- Float32 (annotation forces it)
sqrt(2.0)                 -- Float64 (sqrt requires Floating, default is Float64)
let n : Nat = 3           -- Nat (if Num(Nat) instance exists, uses fromInt)
```

### 4.3 Defaulting Rules

When the type of a numeric literal is ambiguous (no context determines it):

1. Integer literals default to `Int`
2. Float literals default to `Float64`
3. If a `Fractional` constraint is present but no specific type, default to `Float64`
4. If a `Num` constraint is present but no specific type, default to `Int`

### 4.4 Compile-Time Resolution

The compiler resolves literal types **at compile time**. There is no runtime `fromInt` dispatch. When the compiler determines that `42` is an `Int`, it emits the machine literal directly. When it determines `42` is a `Nat`, it emits the `fromInt(42)` call which, if Nat has `repr Int`, compiles to just storing `42`.

### 4.5 The `fromInt` / `fromFloat` Protocol

For a type `T` to accept integer literals, it needs `Num(T)` which includes `fromInt`. For float literals, it needs `Fractional(T)` which includes `fromFloat`. This is how user types participate in literal syntax:

```tulam
-- User-defined complex numbers can use literals:
type Complex = re:Float64 * im:Float64;

instance Num(Complex) = {
    function fromInt(n:Int) : Complex = Complex(fromInt(n), 0.0);
    function (+)(x:Complex, y:Complex) : Complex =
        Complex(x.re + y.re, x.im + y.im);
    -- ...
};

-- Now this works:
let z : Complex = 42;    -- Complex(42.0, 0.0)
z + 1                    -- Complex(43.0, 0.0)
```

---

## 5. Representation System (`repr`)

### 5.1 The `repr` Declaration

`repr` is a **separate declaration** (not part of the type definition) that declares an isomorphism or embedding between a user type and a representation type:

```tulam
repr <UserType> as <ReprType> [default] where {
    function toRepr(x:<UserType>) : <ReprType>;
    function fromRepr(x:<ReprType>) : <UserType>;
    [invariant(x:<ReprType>) = <Bool expression>]
};
```

### 5.2 Examples

**Full isomorphism (Nat ↔ Int):**

```tulam
type Nat = Z + Succ * n:Nat;

repr Nat as Int default where {
    function toRepr(n:Nat) : Int =
        match | Z -> 0
              | Succ(m) -> 1 + toRepr(m);
    function fromRepr(i:Int) : Nat =
        match | 0 -> Z
              | n -> Succ(fromRepr(n - 1));
    invariant(i:Int) = i >= 0
};
```

**Packed representation (Color ↔ UInt32):**

```tulam
type Color = RGBA * r:Byte * g:Byte * b:Byte * a:Byte;

repr Color as UInt32 where {
    function toRepr(c:Color) : UInt32 =
        (widen(c.r) .<<. 24) .|. (widen(c.g) .<<. 16)
            .|. (widen(c.b) .<<. 8) .|. widen(c.a);
    function fromRepr(i:UInt32) : Color =
        RGBA(truncate(i .>>. 24), truncate(i .>>. 16),
             truncate(i .>>. 8), truncate(i))
};
```

**Multiple representations for the same type:**

```tulam
-- CPU-friendly: full Int
repr Nat as Int default where { ... };

-- Network-friendly: compact UInt32
repr Nat as UInt32 where {
    function toRepr(n:Nat) : UInt32 = truncate(natToInt(n));
    function fromRepr(i:UInt32) : Nat = intToNat(widen(i));
    invariant(i:UInt32) = i <= 2147483647
};

-- GPU-friendly: Int32
repr Nat as Int32 where {
    function toRepr(n:Nat) : Int32 = truncate(natToInt(n));
    function fromRepr(i:Int32) : Nat = intToNat(widen(i));
    invariant(i:Int32) = i >= 0
};
```

### 5.3 The `default` Modifier

When multiple `repr` declarations exist for a type, `default` marks the preferred one. The compiler uses the default representation unless:

1. A specific representation is requested via `as` (see 5.5)
2. The context requires a specific representation (e.g., GPU code requires Int32)
3. A downstream repr chain leads to a more efficient choice

If no `default` is specified and only one repr exists, it is implicitly default.

### 5.4 What `repr` Gives the Compiler

1. **Representation substitution**: The compiler MAY store values of the user type using the repr type internally. Pattern matching on `Z` becomes `== 0`; `Succ(n)` becomes `n - 1`.

2. **Operation forwarding**: If the user type and repr type share algebra instances, operations can be forwarded to the repr's native implementation. E.g., `Nat + Nat` can compile to `Int + Int` if `repr Nat as Int` exists and `Num(Int)` is intrinsic.

3. **Zero-cost at boundaries**: When the compiler is already using the repr type internally, `toRepr`/`fromRepr` compile to no-ops.

4. **Invariant checking**: In debug/safe mode, `invariant` is checked at `fromRepr` boundaries. In release mode, it's elided.

### 5.5 Parameterized Repr

The `repr` declaration supports parameterized types as the user type:

```tulam
repr Array(Byte) as PackedBytes where {
    function toRepr(a:Array(Byte)) : PackedBytes = ...;
    function fromRepr(p:PackedBytes) : Array(Byte) = ...
};
```

This enables representation mappings for generic types. The compiler uses qualified keys internally (e.g., `"Array\0Byte"`) following the same `\0`-separator pattern as instance lambda keys.

### 5.6 The `as` Expression — Explicit Repr Casts

```tulam
let packed : UInt32 = myColor as UInt32;   -- explicit repr conversion
let unpacked : Color = packed as Color;    -- back to logical type
```

`as` invokes the `toRepr`/`fromRepr` from a declared `repr` relationship. It is:

- **Type-safe**: only works when a `repr` declaration exists
- **Zero-cost** when the compiler already uses that representation
- **Checked** when an invariant exists (in safe mode)

`as` is NOT a general type cast. It cannot convert between unrelated types. It is strictly for repr relationships.

### 5.6 Repr Chains

The compiler can compose repr declarations:

```tulam
repr Nat as Int default where { ... };
repr Int as Int32 where { ... };

-- Compiler can derive: Nat → Int → Int32
-- Useful when targeting a platform where Int32 is preferred (e.g., GPU, WASM)
```

The compiler composes `toRepr`/`fromRepr` functions and intersects invariants. This composition is automatic and requires no user intervention.

### 5.7 Repr and Algebra Forwarding

When `repr A as B` exists and `instance SomeAlgebra(B) = intrinsic`, the compiler can automatically generate an efficient `instance SomeAlgebra(A)` that delegates through the repr:

```tulam
-- Given:
repr Nat as Int default where { ... };
instance Num(Int) = intrinsic;

-- The compiler could auto-derive (if the user doesn't provide one):
instance Num(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = fromRepr(toRepr(x) + toRepr(y));
    function (*)(x:Nat, y:Nat) : Nat = fromRepr(toRepr(x) * toRepr(y));
    function fromInt(n:Int) : Nat = fromRepr(n);
    -- ... etc
};

-- And since toRepr/fromRepr are no-ops when using Int repr internally,
-- this compiles to native Int arithmetic.
```

This auto-derivation is opt-in. The user can write `deriving Num via repr;` or the compiler can suggest it. The user can always write their own instance instead.

### 5.8 Repr and Pattern Matching

When the compiler uses a repr, pattern matching must be translated:

```tulam
-- Source code:
function isZero(n:Nat) : Bool =
    match | Z -> True
          | Succ(m) -> False;

-- With repr Nat as Int, compiles to:
-- isZero(n) = if n == 0 then True else False
-- (no allocation, no Peano deconstruction)
```

The compiler knows that `Z` maps to `0` via `toRepr` and `Succ(m)` maps to `m + 1`, so it can translate patterns into arithmetic comparisons.

For non-trivial patterns, the compiler may need to `fromRepr` to match structurally, but can often optimize this away.

---

## 6. Array and Bulk Data

### 6.1 Array is Primitive

```tulam
primitive Array(a:Type);
```

Array is honestly primitive — it IS machine-level contiguous memory. Pretending otherwise would add abstraction cost for the most performance-critical data structure.

### 6.2 Array Operations

```tulam
-- Core operations (all intrinsic)
function length(xs:Array(a)) : Int = intrinsic;
function index(xs:Array(a), i:Int) : a = intrinsic;
function slice(xs:Array(a), start:Int, end:Int) : Array(a) = intrinsic;

-- Construction
function arrayOf(xs:List(a)) : Array(a) = intrinsic;
function generate(n:Int, f:Int -> a) : Array(a) = intrinsic;
function replicate(n:Int, x:a) : Array(a) = intrinsic;

-- Intrinsic algebra instances
instance Functor(Array) = intrinsic;     -- fmap = parallel map
instance Foldable(Array) = intrinsic;    -- fold = parallel reduce
instance Traversable(Array) = intrinsic;
instance Eq(Array(a)) requires Eq(a) = intrinsic;
```

### 6.3 The `Bulk` Algebra — Open Acceleration

GPU/SIMD acceleration is NOT hard-coded to Array. It's governed by an open algebra that any type can implement:

```tulam
algebra Bulk(c:Type1) extends Functor(c), Foldable(c) = {
    function length(xs:c(a)) : Int;
    function index(xs:c(a), i:Int) : a;
    function generate(n:Int, f:Int -> a) : c(a);
    function fromArray(xs:Array(a)) : c(a);
    function toArray(xs:c(a)) : Array(a);

    -- Laws that license parallelism
    law indexMap(f:a -> b, xs:c(a), i:Int) =
        index(fmap(f, xs), i) === f(index(xs, i));
    law generateLength(n:Int, f:Int -> a) =
        length(generate(n, f)) === n;
    law roundtrip(xs:c(a)) =
        fromArray(toArray(xs)) === xs
};

instance Bulk(Array) = intrinsic;
```

**Why Bulk matters**: Any user type that implements `Bulk` and ultimately delegates to Array operations can participate in GPU acceleration:

```tulam
type Matrix(a:Type) = MatrixImpl * rows:Int * cols:Int * data:Array(a);

instance Bulk(Matrix) = {
    function length(m:Matrix(a)) : Int = m.rows * m.cols;
    function index(m:Matrix(a), i:Int) : a = index(m.data, i);
    function fmap(f, m) = MatrixImpl(m.rows, m.cols, fmap(f, m.data));
    function generate(n, f) = MatrixImpl(n, 1, generate(n, f));
    function toArray(m) = m.data;
    function fromArray(xs) = MatrixImpl(length(xs), 1, xs)
};
```

The compiler sees that `fmap` on Matrix delegates to `fmap` on Array, so it can GPU-accelerate the inner operation.

### 6.4 Specialized Intrinsic Operations

Some operations are too specialized for algebras but important for performance:

```tulam
-- Matrix multiply: delegates to BLAS/cuBLAS
function matMul(a:Matrix(Float32), b:Matrix(Float32)) : Matrix(Float32) = intrinsic;
function matMul(a:Matrix(Float64), b:Matrix(Float64)) : Matrix(Float64) = intrinsic;

-- Sorting (in-place, returns new array)
function sort(xs:Array(a)) : Array(a) requires Ord(a) = intrinsic;

-- Scan (prefix sum) — important for GPU algorithms
function scan(f:a -> a -> a, init:a, xs:Array(a)) : Array(a) = intrinsic;
```

---

## 7. SIMD Types

### 7.1 Exposed as Types

SIMD types are primitive types with direct user access:

```tulam
primitive Vec2(a:Type);      -- 2 lanes
primitive Vec4(a:Type);      -- 4 lanes  (SSE / NEON)
primitive Vec8(a:Type);      -- 8 lanes  (AVX / AVX2)
primitive Vec16(a:Type);     -- 16 lanes (AVX-512)
```

### 7.2 Lane-wise Arithmetic via Num

All arithmetic operations work lane-wise through intrinsic Num instances:

```tulam
instance Num(Vec4(Float32)) = intrinsic;    -- SSE float ops
instance Num(Vec8(Float32)) = intrinsic;    -- AVX float ops
instance Num(Vec4(Float64)) = intrinsic;    -- SSE double ops
instance Num(Vec4(Int32)) = intrinsic;      -- SSE integer ops
instance Num(Vec8(Float64)) = intrinsic;    -- AVX double ops
-- etc.
```

This means `xs + ys` where `xs, ys : Vec4(Float32)` compiles directly to an `ADDPS` instruction.

### 7.3 The SIMD Algebra

SIMD-specific operations beyond lane-wise arithmetic:

```tulam
algebra Lane(v:Type1) = {
    -- Broadcast
    function splat(x:a) : v(a);

    -- Lane access
    function extract(xs:v(a), i:Int) : a;
    function insert(xs:v(a), i:Int, x:a) : v(a);

    -- Permutation
    function shuffle(xs:v(a), ys:v(a), mask:v(Int32)) : v(a);

    -- Horizontal operations (cross-lane reductions)
    function hsum(xs:v(a)) : a requires Num(a);
    function hmin(xs:v(a)) : a requires Ord(a);
    function hmax(xs:v(a)) : a requires Ord(a);

    -- Comparison (returns lane mask)
    function cmpEq(xs:v(a), ys:v(a)) : v(Int32) requires Eq(a);
    function cmpLt(xs:v(a), ys:v(a)) : v(Int32) requires Ord(a);

    -- Masked operations
    function blend(mask:v(Int32), xs:v(a), ys:v(a)) : v(a);

    -- Width
    function lanes(xs:v(a)) : Int
};

instance Lane(Vec4) = intrinsic;
instance Lane(Vec8) = intrinsic;
instance Lane(Vec16) = intrinsic;
```

### 7.4 Loading/Storing Between Array and Vec

```tulam
-- Load SIMD vector from array at offset
function loadVec4(xs:Array(a), offset:Int) : Vec4(a) = intrinsic;
function loadVec8(xs:Array(a), offset:Int) : Vec8(a) = intrinsic;

-- Store SIMD vector to array at offset
function storeVec4(xs:Array(a), offset:Int, v:Vec4(a)) : Array(a) = intrinsic;
function storeVec8(xs:Array(a), offset:Int, v:Vec8(a)) : Array(a) = intrinsic;
```

### 7.5 Three Levels of SIMD Usage

**Level 1 — Automatic**: User writes scalar code, compiler auto-vectorizes.

```tulam
function scale(xs:Array(Float32), factor:Float32) : Array(Float32) =
    fmap(\x -> x * factor, xs);
-- Compiler emits: loop with Vec8(Float32) ops, scalar tail handling
```

**Level 2 — Explicit SIMD types**: User writes SIMD code with Vec types.

```tulam
function dotProduct4(xs:Vec4(Float32), ys:Vec4(Float32)) : Float32 =
    hsum(xs * ys);
-- Compiles to: MULPS + HADDPS (or DPPS on SSE4.1)
```

**Level 3 — Manual vectorization**: User manages the entire SIMD loop.

```tulam
function manualDot(xs:Array(Float32), ys:Array(Float32)) : Float32 =
    let n = length(xs) in
    let chunks = n / 8 in
    let acc = splat(0.0) : Vec8(Float32) in
    let result = foldRange(0, chunks, acc, \i, acc ->
        let xv = loadVec8(xs, i * 8) in
        let yv = loadVec8(ys, i * 8) in
        acc + xv * yv
    ) in
    hsum(result) + scalarTail(xs, ys, chunks * 8, n);
```

### 7.6 Future: Unified `Vec(a, n)` with Type-Level Naturals

When tulam supports type-level computation:

```tulam
-- Unify all Vec types with a width parameter
primitive Vec(a:Type, n:Nat);

-- Old names become type aliases
type Vec2(a:Type) = Vec(a, 2);
type Vec4(a:Type) = Vec(a, 4);
type Vec8(a:Type) = Vec(a, 8);
type Vec16(a:Type) = Vec(a, 16);
```

This enables width-polymorphic SIMD code:

```tulam
function simdAdd(xs:Vec(Float32, n), ys:Vec(Float32, n)) : Vec(Float32, n) =
    xs + ys;
-- Compiler picks the right instruction width based on n
```

---

## 8. GPU Compute (Overview)

> Full GPU memory model and execution design is a separate document.
> This section covers how the type system enables GPU acceleration.

### 8.1 What the Compiler Knows

The compiler can GPU-accelerate an operation when ALL of these hold:

1. **Element type is primitive** (Float32, Float64, Int32, etc.) — can live in GPU memory
2. **Container has `Bulk` instance** — compiler knows how to map over it
3. **Operations are from intrinsic instances** — have GPU kernel implementations
4. **Algebraic laws license reordering** — associativity enables tree reduction, commutativity means thread order doesn't matter

### 8.2 Law-Driven Optimization

| Law | GPU Optimization |
|-----|-----------------|
| Associativity of `(+)` | Tree reduction instead of sequential fold |
| Commutativity of `(+)` | Thread execution order irrelevant |
| Distributivity | Fuse multiply-add → FMA instruction |
| Functor composition: `fmap f . fmap g === fmap (f . g)` | One kernel pass instead of two |
| Monad associativity | Pipeline GPU kernel chains |

### 8.3 Explicit Control via Annotations

```tulam
-- Force GPU execution (compile error if impossible)
@device(gpu)
function matMul(a:Matrix(Float32), b:Matrix(Float32)) : Matrix(Float32) = ...;

-- Hint parallelism strategy
@parallel(strategy: tiled, tile_size: 32)
function conv2d(input:Array2D(Float32), kernel:Array2D(Float32)) : Array2D(Float32) = ...;

-- Prevent GPU offload (stay on CPU)
@device(cpu)
function smallCompute(xs:Array(Float32)) : Float32 = fold((+), 0.0, xs);
```

Annotations are hints, not types. The code works identically without them. `@device(gpu)` is a hard constraint (compile error if the compiler can't GPU-accelerate it); `@parallel` is a soft hint.

### 8.4 Transparent Data Movement

The compiler manages CPU↔GPU data transfers automatically based on data flow analysis. A value is transferred to GPU memory when:

1. It's used in a GPU-accelerated operation
2. It hasn't been modified since the last transfer (caching)
3. The transfer cost is amortized by the computation (heuristic)

The user can force transfers with explicit functions:

```tulam
function toDevice(xs:Array(a)) : Array(a) = intrinsic;   -- CPU → GPU
function toHost(xs:Array(a)) : Array(a) = intrinsic;     -- GPU → CPU
```

But this is rarely needed — the compiler handles it.

---

## 9. Morphisms for Type Conversions

### 9.1 `Convertible` Morphism

Conversions between numeric types use the existing morphism system:

```tulam
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

-- Widening conversions (always safe, intrinsic)
instance Convertible(Int32, Int64) = intrinsic;
instance Convertible(Float32, Float64) = intrinsic;
instance Convertible(Int, Float64) = intrinsic;
instance Convertible(Byte, Int) = intrinsic;

-- Narrowing conversions (may lose data, intrinsic with runtime check in safe mode)
instance Convertible(Int64, Int32) = intrinsic;  -- may truncate
instance Convertible(Float64, Float32) = intrinsic;  -- may lose precision

-- User type ↔ primitive (via repr)
instance Convertible(Nat, Int) = {
    function convert(n:Nat) : Int = toRepr(n)
};
instance Convertible(Int, Nat) = {
    function convert(i:Int) : Nat = fromRepr(i)
};
```

### 9.2 Implicit Conversions

The compiler can insert `convert` calls automatically in certain safe cases:

1. **Widening**: `Int32 → Int64`, `Float32 → Float64` — always safe, inserted automatically
2. **Literal adaptation**: `42` in a `Float64` context → `fromInt(42)` → `42.0` — no `convert` needed, handled by `fromInt`
3. **Narrowing**: `Int64 → Int32` — NEVER implicit, requires explicit `convert` or `as`

The rule: **implicit conversion only when no information is lost**. This is enforced by a `SafeConvertible` refinement:

```tulam
morphism SafeConvertible(a:Type, b:Type) extends Convertible(a, b) = {
    law preserveValue(x:a) = convert(convert(x) as a) === x
    -- round-trip must preserve the value (this makes it an embedding)
};
```

Only `SafeConvertible` instances are eligible for implicit insertion.

---

## 10. Worked Example: Full Program

```tulam
-- Everything below uses the primitives and algebras from prelude.tl

-- Peano naturals with efficient representation
type Nat = Z + Succ * n:Nat;
repr Nat as Int default where {
    function toRepr(n:Nat) : Int =
        match | Z -> 0
              | Succ(m) -> 1 + toRepr(m);
    function fromRepr(i:Int) : Nat =
        match | 0 -> Z
              | n -> Succ(fromRepr(n - 1));
    invariant(i:Int) = i >= 0
};

instance Num(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
    function (*)(x:Nat, y:Nat) : Nat = mult(x, y);
    function (-)(x:Nat, y:Nat) : Nat = minus(x, y);
    function fromInt(n:Int) : Nat = fromRepr(n);
    function negate(x:Nat) : Nat = Z;    -- Nat can't be negative
    function abs(x:Nat) : Nat = x
};

-- Factorial: polymorphic, works on any Integral type
function factorial(n:a) : a requires Integral(a) =
    if n <= fromInt(1) then fromInt(1) else n * factorial(n - fromInt(1));

-- Usage:
-- factorial(10)          → 3628800 : Int
-- factorial(10 : Int32)  → 3628800 : Int32
-- factorial(10 : Nat)    → Succ(Succ(...)) but internally computed as Int

-- Array operations: automatically parallelized
function normalize(xs:Array(Float32)) : Array(Float32) =
    let total = fold((+), 0.0, xs) in
    fmap(\x -> x / total, xs);

-- Works on any Num type with Bulk container
function sumOfSquares(xs:c(a)) : a requires Num(a), Bulk(c) =
    fold((+), fromInt(0), fmap(\x -> x * x, xs));

-- sumOfSquares on Array(Float32) → GPU-accelerated
-- sumOfSquares on Array(Int)     → CPU SIMD or GPU
-- sumOfSquares on List(Nat)      → interpreted (no Bulk for List)

-- Explicit SIMD for hot inner loop
function fastDot(xs:Array(Float32), ys:Array(Float32)) : Float32 =
    let n = length(xs) in
    let vec_n = n / 8 in
    let acc = splat(0.0) : Vec8(Float32) in
    let total = foldRange(0, vec_n, acc, \i, a ->
        a + loadVec8(xs, i * 8) * loadVec8(ys, i * 8)
    ) in
    hsum(total);
```

---

## 11. Implementation Roadmap

### Phase 2.5: Primitive Foundation ✅ COMPLETE

This phase adds the infrastructure for primitive types and operations.

**Step 2.5.1: Parse `primitive` declarations**

Files: Lexer.hs, Parser.hs, Surface.hs

- Add `primitive` to reserved words in Lexer.hs
- Add `Primitive Lambda` to Expr (Lambda carries the type name + optional type params)
- Parse: `primitive Int;` → `Primitive (Lambda "Int" [] UNDEFINED (U 0))`
- Parse: `primitive Array(a:Type);` → `Primitive (Lambda "Array" [Var "a" Type UNDEFINED] UNDEFINED (U 1))`
- Add to traverseExpr, ppr

**Step 2.5.2: Process `primitive` in Pass 1 (environment building)**

Files: Pipeline.hs, State.hs

- Add `primitiveTypes :: Map Name Lambda` to InterpreterState
- When processing a Primitive declaration, register the type name
- Primitive types have no constructors (unlike SumType which adds constructors to the environment)
- The type name is added to the type environment so it can be referenced

**Step 2.5.3: Parse `intrinsic` as a body**

Files: Lexer.hs, Parser.hs, Surface.hs

- Add `intrinsic` to reserved words
- In function body position: `function foo(x:Int) : Int = intrinsic;` → body is `Intrinsic`
- In instance body position: `instance Num(Int) = intrinsic;` → all functions get `Intrinsic` body
- Add `| Intrinsic` to Expr
- Add to traverseExpr (identity), ppr

**Step 2.5.4: Intrinsic registry in the compiler**

Files: new Intrinsics.hs

- Create a module mapping `(functionName, typeName)` to evaluation behavior
- Start with: `("+", "Int")`, `("-", "Int")`, `("*", "Int")`, `("div", "Int")`, `("mod", "Int")`, `("==", "Int")`, `("<", "Int")`, etc.
- Each entry maps to a Haskell function: `\[CLMLIT (LInt a), CLMLIT (LInt b)] -> CLMLIT (LInt (a + b))`
- Similar for Float64, String (concat, length)

**Step 2.5.5: Intrinsic evaluation in interpreter**

Files: Interpreter.hs, CLM.hs

- Add `CLMINTRINSIC Name Name` to CLMExpr (function name, type name)
- When `intrinsic` body is encountered during CLM conversion, emit CLMINTRINSIC
- In evalCLM: `CLMINTRINSIC funcName typeName` → look up in intrinsic registry → apply to evaluated arguments
- For `instance Num(Int) = intrinsic`: each function in Num generates a CLMINTRINSIC

**Step 2.5.6: Create prelude.tl with primitive declarations**

Files: new prelude.tl, Main.hs

- Create prelude.tl with: `primitive Int; primitive Float64; primitive String; primitive Bool;` (Bool can remain user-defined OR become primitive — design choice)
- Load prelude.tl before base.tl in the REPL
- Move numeric algebra declarations to prelude.tl or a new numeric.tl

**Step 2.5.7: Integer and float literals in the REPL**

Files: Interpreter.hs

- `CLMLIT (LInt n)` already exists and evaluates to itself
- With `Num(Int) = intrinsic`, expressions like `3 + 4` should now evaluate:
  - Parse: `BinaryOp "+" (Lit (LInt 3)) (Lit (LInt 4))`
  - Desugar: `App (Id "+") [Lit (LInt 3), Lit (LInt 4)]`
  - The `+` dispatches through CLMIAP to the Int instance → CLMINTRINSIC "+" "Int"
  - Evaluate: `CLMINTRINSIC "+" "Int" [CLMLIT (LInt 3), CLMLIT (LInt 4)]` → `CLMLIT (LInt 7)`

**Test milestone:**
```
> 3 + 4
7
> 10 * 5 + 2
52
> 3.14 * 2.0
6.28
> 10 > 3
True
```

### Phase 2.6: Repr System ✅ COMPLETE

**Step 2.6.1: Parse `repr` declarations**

Files: Lexer.hs, Parser.hs, Surface.hs

- Add `repr`, `invariant` to reserved words
- Add `Repr Name Expr Bool [(Name, Lambda)] (Maybe Expr)` to Expr:
  - Name: user type name
  - Expr: repr type
  - Bool: is default
  - [(Name, Lambda)]: toRepr and fromRepr functions
  - Maybe Expr: optional invariant
- Parse the full `repr Nat as Int default where { ... };` syntax

**Step 2.6.2: Process `repr` in Pass 1**

Files: Pipeline.hs, State.hs

- Add `reprMap :: Map Name [(Expr, Bool, Lambda, Lambda, Maybe Expr)]` to InterpreterState
  - Key: user type name
  - Value: list of (repr type, isDefault, toRepr, fromRepr, invariant)
- Register repr declarations during environment building
- Validate: user type must exist, repr type must exist

**Step 2.6.3: Repr-aware pattern matching (future optimization)**

This is a compiler optimization pass, not needed for correctness. Deferred to codegen phase.

When the compiler knows `repr Nat as Int`:
- Pattern `Z` on a Nat value can compile to `== 0`
- Pattern `Succ(n)` can compile to `let n = x - 1 in ...`
- This is a Pass 2 or Pass 5 optimization

### Phase 2.7: Array Primitives ✅ COMPLETE

**Step 2.7.1: Declare Array in prelude**

```tulam
primitive Array(a:Type);
```

Uses the same `primitive` infrastructure from 2.5.1-2.5.2.

**Step 2.7.2: Array intrinsic functions**

Add to intrinsic registry:
- `length : Array(a) -> Int`
- `index : Array(a) -> Int -> a`
- `generate : Int -> (Int -> a) -> Array(a)`
- `arrayMap : (a -> b) -> Array(a) -> Array(b)`
- `arrayFold : (b -> a -> b) -> b -> Array(a) -> b`

In the interpreter (Haskell), arrays are represented as `Data.Vector` or lists for now. Efficient representation comes with codegen.

**Step 2.7.3: Array algebra instances**

```tulam
instance Functor(Array) = intrinsic;
instance Foldable(Array) = intrinsic;
```

Requires HKT (Phase 5) for full generality. Intermediate approach: hard-code `fmap` and `fold` as known functions for Array dispatch.

### Phase 2.8: SIMD Types ✅ COMPLETE (stubs — full SIMD requires codegen)

SIMD types are declared in the prelude but only become useful with native codegen. For the interpreter phase:

- Parse `primitive Vec4(a:Type);` etc.
- Register in primitive types
- SIMD operations evaluate to errors in interpreter: "SIMD operations require native compilation"
- Full implementation comes with the x86/LLVM codegen backend

---

## 12. Summary

### Design Principles

1. **No two-class type system**: Primitives are types, not magic. Users interact with them through the same algebras as any other type.
2. **No syntax noise**: No `#`, no `UNPACK`, no special operators for primitives. `3 + 4` just works.
3. **Opt-in optimization**: `repr` is separate, optional, and gradual. Add it when you need performance.
4. **Laws = optimization licenses**: The algebraic laws users write for correctness are exactly what the compiler needs for parallelism.
5. **Three levels of control**: Automatic (scalar code, compiler optimizes) → Guided (annotations, SIMD types) → Manual (explicit SIMD loops, device control).
6. **Open acceleration**: GPU/SIMD isn't hard-coded to Array. The `Bulk` algebra lets any type participate.

### New Keywords

| Keyword | Kind | Purpose |
|---------|------|---------|
| `primitive` | Declaration | Introduce a machine-provided type |
| `intrinsic` | Body | Compiler-provided implementation |
| `repr` | Declaration | Declare representation mapping |
| `invariant` | Inside repr | Representation validity constraint |
| `as` | Expression | Explicit repr cast |
| `match` | Expression | Pattern matching |
| `@device` | Annotation | Force execution target |
| `@parallel` | Annotation | Hint parallelism strategy |

---

## 13. Implementation Status (Phase 4)

This section documents what was actually implemented vs. the design above.

### 13.1 What Was Implemented

**Primitive types** (§1): The `primitive` keyword is fully implemented. `prelude.tl` declares `Int`, `Float64`, `String`, `Char`. Primitive types are registered in the type environment with no constructors. Parameterized primitives (`primitive Array(a:Type)`) are supported syntactically but not yet used.

**Intrinsic keyword** (§2): Both instance-level (`instance Num(Int) = intrinsic;`) and function-level (`function foo(x:Int) : Int = intrinsic;`) forms are implemented. When a whole-instance intrinsic is declared, the compiler extracts all function names from the structure definition and creates placeholder intrinsic lambdas for each.

**Intrinsic registry** (§2.4): Implemented in `src/Intrinsics.hs`. Keys are `"funcName\0typeName"` strings (reusing the instance key pattern from `State.hs`). Covers:
- **Int**: `+`, `-`, `*`, `negate`, `abs`, `fromInt`, `div`, `mod`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- **Float64**: `+`, `-`, `*`, `/`, `negate`, `abs`, `fromInt`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`

**Numeric algebras** (§3): `Num`, `Integral`, `Fractional` are defined in `prelude.tl` as standalone algebras (not extending Eq/Ord — see deviations below). Intrinsic instances for `Num(Int)`, `Integral(Int)`, `Num(Float64)`, `Fractional(Float64)` are declared in `prelude.tl`.

**Eq/Ord intrinsics**: Intrinsic instances for `Eq(Int)`, `Eq(Float64)`, `Ord(Int)`, `Ord(Float64)` are declared in `base.tl` (after the Eq/Ord structure definitions).

**Interpreter dispatch**: Intrinsic lookup happens in the CLMIAP evaluator, checked **before** normal instance lookup. This ensures intrinsics take priority over any user-defined instances.

**CLMLIT pretty-printing**: `CLMLIT (LInt 7)` now displays as `7` instead of the raw Haskell `show` form.

### 13.2 Deviations from Design

1. **No `CLMINTRINSIC` CLM node**: The design proposed a new `CLMINTRINSIC Name Name [CLMExpr]` constructor. Instead, intrinsic instances use `CLMLam [] CLMPRIMCALL` (same as existing primitives) and dispatch is handled directly in the interpreter via `lookupIntrinsic`. This is simpler and avoids changes to CLM traversal/beta-reduction.

2. **Num does not extend Eq/Ord**: The design shows `algebra Num(a:Type) extends Eq(a), Ord(a)`. This was not implemented because it creates a circular loading dependency: Num is in `prelude.tl` but Eq/Ord are in `base.tl`. Num is standalone for now. This can be revisited when the loading system supports forward declarations or single-file compilation.

3. **No polymorphic literals**: §4 describes polymorphic literal resolution (e.g., `42` could be any `Num(a) => a`). This is not implemented — integer literals are always `Int` and float literals are always `Float64`. Polymorphic literals require the type checker (Phase 11).

4. **No default implementations in Num**: The design shows `negate(x) = fromInt(0) - x` and `abs(x) = if x >= fromInt(0) then x else negate(x)` as defaults. These are not in the current `prelude.tl` because they would require the Eq/Ord instances to exist at algebra definition time. The intrinsic instances provide all operations directly.

5. **No `Floating` or `Bits` algebras**: Only `Num`, `Integral`, `Fractional` were implemented for MVP. `Floating` (sqrt, exp, sin, cos) and `Bits` (bitwise ops) are straightforward additions to the registry.

6. **No negative literal handling**: `-3` parses as `UnaryOp "-" (Lit (LInt 3))` → `App (Id "-") [LInt 3]`. This dispatches through CLMIAP with 1 arg, but `-` is registered as a binary op. A future fix should map unary minus to `negate`.

### 13.3 What's Not Yet Implemented

From this design document, the following remain for future phases:

- **§5 Repr system**: `repr`, `as`, `invariant` — scheduled for Phase 5
- **§6 Array and Bulk**: Requires HKT (Phase 7)
- **§7 SIMD types**: Requires native codegen
- **§8 GPU compute**: Requires Phases 4+7+10+11
- **§9 Convertible morphism for numeric types**: Morphism infrastructure exists; numeric Convertible instances are straightforward additions
- **§4 Polymorphic literals**: Requires type checker (Phase 11)
- **§3.4 Num → Monoid derivation**: Requires extends relationship and auto-derivation
