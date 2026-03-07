# tulam Language Reference

## Introduction

tulam (Functional Object-Oriented Low-level Language) is a type-theory based functional language compiler/interpreter written in Haskell. It targets JavaScript, .NET, and potentially x86 native code.

tulam is built on two primitives: **tuples** and **lambdas**. Everything else — sum types, product types, structures (typeclasses), records — is derived from these two building blocks.

### Building and Running

```bash
stack build              # Build the project
stack exec tulam          # Start the REPL (loads prelude.tl + base.tl automatically)
stack test               # Run tests
stack clean              # Clean build artifacts
```

The project uses Stack with Hpack. Edit `package.yaml` for dependency changes (not `tulam.cabal`).

---

## Getting Started: The REPL

When you run `stack exec tulam`, you enter the interactive REPL. It automatically loads `prelude.tl` (primitive types, numeric algebras) and then `base.tl` (Nat, Bool, Eq, Ord, etc.).

### REPL Commands

| Command           | Description                              |
|-------------------|------------------------------------------|
| `:load <file>`    | Load and compile a `.tl` source file   |
| `:list types`     | Show all defined types                   |
| `:list functions` | Show all defined functions               |
| `:env`            | Show the current environment             |
| `:all`            | Show everything (types, functions, etc.) |
| `:clm`            | Show CLM (Core List Machine) IR output   |
| `:quit`           | Exit the REPL                            |

You can also type expressions directly to evaluate them, or enter top-level declarations (terminated with `;`).

---

## Comments

tulam supports two styles of comments:

```
// This is a single-line comment

/* This is a
   multi-line comment */
```

Multi-line comments do **not** nest.

---

## Literals

### Integers

Integer literals produce values of type `Int` (a primitive machine integer) by default:

```
0
42
1000
```

With the `EuclideanDomain(Int)` intrinsic instance, arithmetic operations work natively: `3 + 4` evaluates to `7`.

The `fromInt` protocol enables polymorphic integer literals: when the expected type is known (e.g., `Int8`, `UInt32`), `fromInt` converts the literal to the target type. The AST supports width-specific literal types (`LInt8`, `LInt16`, `LInt32`, `LInt64`, `LWord8`, `LWord16`, `LWord32`, `LWord64`) for representing narrower integer values internally.

### Floating-Point Numbers

Decimal numbers with a fractional part produce values of type `Float64` by default:

```
3.14
0.5
100.0
```

With the `Floating(Float64)` intrinsic instance, float arithmetic works natively: `3.14 * 2.0` evaluates to `6.28`.

The `fromFloat` protocol enables polymorphic float literals: `Float32` values can be produced via `fromFloat` conversion. The AST includes `LFloat32` for 32-bit float literals.

### Strings

Double-quoted string literals:

```
"hello"
"world"
""
```

Standard escape sequences are supported (e.g., `\"`, `\\`, `\n`).

### Lists

Square brackets with comma-separated elements:

```
[1, 2, 3]
[True, False]
[]
```

### Tuples

Curly braces with comma-separated elements:

```
{1, 2, 3}
{True, Z}
```

Tuples are one of the two fundamental primitives in tulam.

### Vectors

Angle brackets with comma-separated elements:

```
<1, 2, 3>
<0.5, 1.0, 1.5>
```

---

## Identifiers and Naming Conventions

Identifiers start with a letter or underscore, followed by letters, digits, underscores, apostrophes, or `#`.

### Naming Rules

- **Types and constructors** must start with an **uppercase** letter: `Nat`, `Bool`, `Succ`, `True`
- **Functions and variables** must start with a **lowercase** letter: `plus`, `eq`, `not`, `x`
- The **`#` suffix** denotes built-in/primitive operations: `print#`, `concat#`, `+#`, `-#`

### Operators

Operators are sequences of symbolic characters. They can include standard symbols (`+`, `-`, `*`, `/`, `=`, `<`, `>`, etc.) as well as Unicode symbols. Operators can be defined as functions by wrapping them in parentheses:

```
function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
```

---

## Type Annotations

Type annotations are optional and use the `name:Type` syntax:

```
x:Nat           // x has type Nat
b:Bool          // b has type Bool
f:Type          // f is a type
```

Type annotations appear in function parameters, variable bindings, and return types.

### Rich Type Expressions

Type annotations support the full type expression syntax:

- **Simple names**: `Nat`, `Bool`, `a`
- **Type application**: `Vec(a, n)`, `Maybe(a)`, `Pair(Nat, Bool)`
- **Arrow types** (right-associative): `Nat -> Bool`, `a -> b -> c`, `(a -> b) -> c`
- **Universes**: `Type`, `Type1`, `Type2`, `Type3`
- **Parenthesized**: `(a -> b) -> c`

Arrow types represent non-dependent function types. They are right-associative, so `a -> b -> c` means `a -> (b -> c)`.

```
function apply(f: Nat -> Bool, x:Nat) : Bool = f(x);
function compose(f: b -> c, g: a -> b, x:a) : c = f(g(x));
```

### Universe Hierarchy

tulam has a universe hierarchy for classifying types:

| Name     | Universe Level | Description                     |
|----------|----------------|---------------------------------|
| `Type`   | `U 0`         | The type of ordinary values     |
| `Type0`  | `U 0`         | Alias for `Type`                |
| `Type1`  | `U 1`         | The type of types (kinds)       |
| `Type2`  | `U 2`         | The type of kinds               |
| `Type3`  | `U 3`         | Higher universe                 |

Most user code only needs `Type`. Higher universes are used for type-level programming (e.g., parameterized records and structures that take types as arguments).

---

## Sum Types (Algebraic Data Types)

Sum types define a type as a choice between multiple constructors. This is tulam's equivalent of Haskell's `data` declarations or Rust's `enum`.

### Syntax

```
type Name = Constructor1 | Constructor2(args);
```

Each constructor can optionally take arguments in parentheses. Arguments are comma-separated `name:Type` pairs.

### Examples

**Simple enumeration:**

```
type Bool = True | False;
```

**Recursive type:**

```
type Nat = Z | Succ(n:Nat);
```

`Nat` represents natural numbers. `Z` is zero, and `Succ(n)` is the successor of `n`. So `Succ(Succ(Z))` represents 2.

**Parameterized type:**

```
type ConstructorTag = ConstructorTag(n:Nat);
```

### How It Works

Under the hood, constructors create tagged tuples. Each constructor in a sum type gets an integer tag, which is used for pattern matching. This is essential for compilation to targets like .NET and JavaScript.

### GADTs (Generalized Algebraic Data Types)

Constructors can specify their own return type, which may be more specific than the parent type. This is done with a `: ReturnType` annotation after the constructor:

```
type Vec(a:Type, n:Nat) = VNil : Vec(a, Z) | VCons(head:a, tail:Vec(a, n)) : Vec(a, Succ(n));
```

Here `VNil` returns `Vec(a, Z)` (a vector of length zero) while `VCons` returns `Vec(a, Succ(n))` (a vector one longer). Without the `: ReturnType` annotation, the constructor's return type defaults to the parent type with its parameters (e.g., `Vec(a, n)`).

GADTs enable encoding type-level invariants directly in the type system, such as length-indexed vectors, balanced trees, and propositional equality.

### Propositional Equality (`PropEqT`)

The standard library defines propositional equality as a GADT:

```
type PropEqT(a:Type, x:a, y:a) = Refl : PropEqT(a, x, x);
```

The `Refl` constructor can only produce `PropEqT(a, x, x)` — both indices must be the same value. This is the type-theoretic equality witness: a value of type `PropEqT(a, x, y)` is proof that `x` and `y` are equal.

Note: This is distinct from `===` in law declarations, which is a syntactic marker for documentation. `PropEqT` is a first-class value type that can be constructed, pattern-matched, and passed as an argument.

---

## Records (Product Types)

Records are syntactic sugar for single-constructor sum types with named fields. They provide a convenient way to define product types.

### Basic Record

```
record Point = { x:Nat, y:Nat };
```

This desugars to:

```
type Point = Point(x:Nat, y:Nat);
```

### Parameterized Record

Records can take type parameters:

```
record Pair(a:Type, b:Type) = { fst:a, snd:b };
```

### Record Spread

You can include all fields from another record using the `..` spread syntax:

```
record Point3D = { ..Point, z:Nat };
```

This expands to include all fields from `Point` (`x:Nat`, `y:Nat`) plus the new field `z:Nat`. Spread fields are resolved during compilation when the environment is available.

### Dot-Access Syntax

Access record fields using dot notation:

```
function getX(p:Point) : Int = p.x;
function getY(p:Point) : Int = p.y;
```

Dot-access chains are left-associative: `a.b.c` accesses field `c` of the result of `a.b`. Dot-access works on any expression, including function results: `f(x).field`.

At compile time, field names are resolved to indices. If resolution fails, the interpreter resolves them at runtime.

### Named Construction

Construct records using named fields (in any order):

```
function mkPoint(a:Int, b:Int) : Point = Point { x = a, y = b };
function mkPointRev(a:Int, b:Int) : Point = Point { y = b, x = a };  // reordered
```

Fields are reordered to match the constructor's parameter order. All fields must be specified.

### Record Update

Create a modified copy of a record:

```
function setX(p:Point, newX:Int) : Point = p { x = newX };
```

Unmodified fields are copied from the original via dot-access. The constructor is inferred from the field names in the update.

### Named Field Patterns

Pattern match using named fields:

```
function isOrigin(p:Point) : Bool = match
    | Point { x = 0, y = 0 } -> True
    | p -> False;
```

Unspecified fields become wildcards. Field punning is supported: `{ x }` means `{ x = x }` (binds field to same-name variable).

---

## Primitive Types

Primitive types are machine-level types with no user-visible constructors. They are **declared**, not defined — the runtime provides their representation.

### Syntax

```
primitive TypeName;
primitive TypeName(params);
```

### Examples

```
primitive Int;
primitive Float64;
primitive String;
primitive Char;
```

Parameterized primitives (for future use):

```
primitive Array(a:Type);
```

### What `primitive` Means

A `primitive` declaration tells the compiler:

1. **No user-visible constructors** — you cannot pattern match on an Int's internal structure
2. **Target-specific representation** — the compiler knows how to represent this on each backend
3. **Literals may produce values of this type** — `42` produces an `Int`, `3.14` produces a `Float64`
4. **Operations must be declared explicitly** — no built-in operations exist until algebra instances provide them

### Currently Declared Primitives

The following primitives are declared in `prelude.tl` (loaded automatically):

**Core types:**

| Type | Description |
|------|-------------|
| `Int` | Machine integer (signed, platform-native word size) |
| `Float64` | 64-bit floating-point number |
| `String` | UTF-8 string |
| `Char` | Unicode character |

**Fixed-width signed integers:**

| Type | Description |
|------|-------------|
| `Int8` | 8-bit signed integer (-128 to 127) |
| `Int16` | 16-bit signed integer (-32768 to 32767) |
| `Int32` | 32-bit signed integer |
| `Int64` | 64-bit signed integer |

**Unsigned integers:**

| Type | Description |
|------|-------------|
| `UInt` | Machine-width unsigned integer |
| `UInt8` | 8-bit unsigned integer (0 to 255) |
| `UInt16` | 16-bit unsigned integer (0 to 65535) |
| `UInt32` | 32-bit unsigned integer |
| `UInt64` | 64-bit unsigned integer |
| `Byte` | Alias-like type for byte-level data (8-bit unsigned) |

**Floating-point types:**

| Type | Description |
|------|-------------|
| `Float32` | 32-bit floating-point number (single precision) |
| `Float64` | 64-bit floating-point number (double precision) |

**Parameterized primitives:**

| Type | Description |
|------|-------------|
| `Array(a:Type)` | Heap-allocated, variable-length array of elements |
| `Vec2(a:Type)` | 2-lane SIMD vector |
| `Vec4(a:Type)` | 4-lane SIMD vector |
| `Vec8(a:Type)` | 8-lane SIMD vector |
| `Vec16(a:Type)` | 16-lane SIMD vector |

---

## Intrinsic Implementations

The `intrinsic` keyword marks a function or instance as compiler-provided. It replaces the older `primop#` convention.

### Instance-Level Intrinsic

When applied to an entire instance, the compiler provides implementations for all functions in the algebra:

```
instance EuclideanDomain(Int) = intrinsic;
instance Eq(Int) = intrinsic;
instance Ord(Float64) = intrinsic;
```

### Function-Level Intrinsic

A single function can be marked as intrinsic:

```
function sqrt(x:Float64) : Float64 = intrinsic;
```

### Currently Available Intrinsics

**Int operations** (via `EuclideanDomain(Int)`, `Absolute(Int)`, `Eq(Int)`, `Ord(Int)`, `Bits(Int)`, `Group(Int)`):
- Arithmetic: `+`, `-`, `*`, `negate`, `zero`, `one` (from AdditiveSemigroup/AdditiveMonoid/AdditiveGroup through EuclideanDomain)
- Division: `div`, `mod` (from EuclideanDomain)
- Absolute: `abs`, `signum`
- Generic group: `combine`, `inverse`, `empty` (from Group(Int))
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- Bitwise: `.&.`, `.|.`, `xor`, `complement`, `shiftL`, `shiftR`, `bitSize`
- Conversion: `toFloat` (Int to Float64), `fromInt` (identity)

**Fixed-width integer operations** (Int8, Int16, Int32, Int64 — each has the same set):
- Arithmetic: `+`, `-`, `*`, `negate`, `zero`, `one`
- Division: `div`, `mod`
- Absolute: `abs`, `signum`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- Bitwise: `.&.`, `.|.`, `xor`, `complement`, `shiftL`, `shiftR`, `bitSize`
- Conversion: `fromInt` (from Int), `toInt` (to Int — where applicable)

**Unsigned integer operations** (UInt, UInt8, UInt16, UInt32, UInt64, Byte — each has the same set):
- Arithmetic: `+`, `-`, `*`, `negate`, `zero`, `one`
- Division: `div`, `mod`
- Absolute: `abs`, `signum`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- Bitwise: `.&.`, `.|.`, `xor`, `complement`, `shiftL`, `shiftR`, `bitSize`
- Conversion: `fromInt` (from Int)

**Float64 operations** (via `Floating(Float64)`, `Absolute(Float64)`, `Eq(Float64)`, `Ord(Float64)`, `AbelianGroup(Float64)`):
- Arithmetic: `+`, `-`, `*`, `/`, `negate`, `recip`, `zero`, `one` (from full additive+multiplicative chain through Floating)
- Absolute: `abs`, `signum`
- Generic group: `combine`, `inverse`, `empty` (from AbelianGroup(Float64))
- Floating math: `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `pow`, `pi`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- Conversion: `toInt` (Float64 to Int via truncation), `fromFloat` (identity), `fromInt` (Int to Float64)

**Float32 operations** (via `Floating(Float32)`, `Absolute(Float32)`, `Eq(Float32)`, `Ord(Float32)`):
- Arithmetic: `+`, `-`, `*`, `/`, `negate`, `recip`, `zero`, `one`
- Absolute: `abs`, `signum`
- Floating math: `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `pow`, `pi`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`
- Conversion: `fromFloat` (from Float64, narrowing), `fromInt` (from Int)

**Nat operations** (via `Semiring(Nat)`, `Monoid(Nat)`, `Eq(Nat)`, `Ord(Nat)`):
- Arithmetic: `+`, `*`, `zero`, `one` (from Semiring — no negation, Nat is not a Ring)
- Generic monoid: `combine`, `empty` (from Monoid(Nat))
- Helper functions: `plus`, `mult`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`

**String operations** (via `StringOps(String)`, `Eq(String)`, `Ord(String)`):
- String manipulation: `concat`, `length`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`

**Array operations** (via intrinsic functions on `Array(a)`):
- `length` — returns the number of elements
- `index` — access element by position
- `slice` — extract a sub-array

**SIMD Vec operations** (Vec2, Vec4, Vec8, Vec16 — via Lane algebra, stubs only):
- Lane operations are declared but return errors at runtime — they require native compilation
- Full SIMD support will be activated when the codegen backend is implemented

**Numeric conversions** (via `Convertible` morphism instances):
- All integer types can be converted to/from each other via `convert`
- `fromInt(n)` converts an `Int` to the target type (available on all numeric types)
- `fromFloat(x)` converts a `Float64` to `Float32` (narrowing) or is identity on `Float64`

### Unary Minus

The unary minus operator (`-x`) is desugared to `negate(x)`, which dispatches through the `AdditiveGroup` algebra (inherited by Ring). This means `-3` evaluates to `negate(3)` which returns `-3` for Int, and `-3.14` returns `-3.14` for Float64.

### Examples

```
> 3 + 4
7
> 10 * 5 + 2
52
> -3
-3
> negate(5)
-5
> 10 > 3
True
> 3 == 3
True
> sqrt(4.0)
2.0
> sin(0.0)
0.0
> pi
3.141592653589793
> 3 .&. 1
1
> complement(0)
-1
> concat("hello", " world")
"hello world"
> length("test")
4
> "abc" == "abc"
True
```

---

## Repr System (Representation Mappings)

The `repr` declaration defines a mapping between a user-defined type and a machine-level representation type. This bridges algebraic data types and primitive types, enabling conversions and (in future codegen) representation-aware optimizations.

### Syntax

```
repr UserType as ReprType [default] where {
    function toRepr(x:UserType) : ReprType = ...,
    function fromRepr(x:ReprType) : UserType = ...[,
    invariant(x:ReprType) = expr]
};
```

- `UserType` — the user-defined algebraic type being mapped
- `ReprType` — the target machine-level (usually primitive) type
- `default` — optional; marks this as the default repr for `UserType` when the target type is unambiguous
- `toRepr` — required; converts from `UserType` to `ReprType`
- `fromRepr` — required; converts from `ReprType` to `UserType`
- `invariant` — optional; a predicate over `ReprType` values expressing which values are valid representations

### Example

The base library declares `repr Nat as Int`:

```
repr Nat as Int default where {
    function toRepr(n:Nat) : Int =
        match
        | Z       -> 0
        | Succ(m) -> 1 + toRepr(m),
    function fromRepr(i:Int) : Nat = if i == 0 then Z else Succ(fromRepr(i - 1))
};
```

### Explicit Casting with `as`

Once a `repr` mapping exists, the `as` keyword performs explicit repr casts:

```
expr as TargetType
```

The compiler resolves the direction automatically:
- If `TargetType` is a user type (left-hand side of some `repr`) → calls `fromRepr(expr)`
- If `TargetType` is a repr type (right-hand side of some `repr`) → calls `toRepr(expr)`

**Examples:**

```
> (Succ(Succ(Z)) as Int)
2
> (5 as Nat)
Succ(Succ(Succ(Succ(Succ(Z)))))
```

The `toRepr` and `fromRepr` functions are also callable directly:

```
> toRepr(Succ(Succ(Z)))
2
> fromRepr(3)
Succ(Succ(Succ(Z)))
```

### How It Works

- `processBinding (Repr ...)` in Pass 1 registers `toRepr` and `fromRepr` as implicit-parameter wrapper lambdas so normal CLMIAP dispatch handles them.
- Each function is also stored as an instance lambda: `toRepr` keyed by the user type name, `fromRepr` keyed by the repr type name. This ensures type-directed dispatch finds the right implementation.
- `ReprCast` in `exprToCLM` checks `reprMap` to determine direction and emits the appropriate `CLMIAP` node.
- Multiple repr mappings for the same user type are supported (different target types). The `default` flag marks the preferred one for unambiguous casts.

### `as` is a Reserved Word

`as` cannot be used as an identifier. It is a postfix operator in expressions, lower precedence than function application.

---

## Functions

Functions are the primary way to define computations in tulam.

### Basic Function

```
function name(args) : ReturnType = body;
```

The return type annotation is optional.

**Simple function:**

```
function f(x) = [x, 18, 29];
```

**With type annotations:**

```
function typeOf(ex:tp) : Type = tp;
```

### Pattern Matching

Functions can use pattern matching with the `match` keyword. Each case is `| patterns -> expression`:

```
function eq(x:Nat, y:Nat) : Bool =
  match
  | Z, Z         -> True
  | Z, n         -> False
  | n, Z         -> False
  | Succ(m), Succ(n) -> eq(m, n);
```

The number of patterns in each case must match the number of function parameters.

**More examples:**

```
function not(b:Bool) : Bool =
  match
  | True  -> False
  | False -> True;

function plus(x:Nat, y:Nat) : Nat =
  match
  | Z, n       -> n
  | Succ(n), m -> Succ(plus(n, m));
```

### Inline Match Expression

You can also use `match` as an inline expression to match on a specific value:

```
match expr
| pat1 -> body1
| pat2 -> body2
```

This evaluates `expr` and matches the result against each pattern in order.

**Example:**

```
function describe(x:Nat) : String =
  match x
  | Z -> "zero"
  | Succ(n) -> "positive";
```

Patterns can be:
- **Variables** (`n`, `m`, `x`) — match anything and bind the value
- **Constructors** (`Z`, `True`) — match a specific constructor
- **Nested constructor applications** (`Succ(n)`, `Succ(Succ(Z))`) — match and destructure

### Operators as Functions

Operators can be defined as functions by wrapping the operator in parentheses:

```
function (==)(x, y:a) : Bool = not(x != y);
function (!=)(x, y:a) : Bool = not(x == y);
```

---

## Expressions

### Function Application

Call a function by name with arguments in parentheses:

```
plus(x, y)
Succ(Z)
eq(m, n)
not(b)
```

The function position can also be a parenthesized expression:

```
(f)(x, y)
```

### Binary Operators

Binary operators are written infix:

```
x + y
a * b
x == y
n +# m      // primitive addition
```

### Unary Operators

Prefix unary operators:

```
-x
```

### Operator Precedence

From highest to lowest precedence (all left-associative):

| Precedence | Operators              | Description          |
|------------|------------------------|----------------------|
| Highest    | `*`, `/`, `*#`, `/#`   | Multiplication, division |
| Medium     | `+`, `-`, `+#`, `-#`   | Addition, subtraction    |
| Lower      | Custom operators       | User-defined operators   |
| Lowest     | `==`                   | Equality comparison      |

The `#`-suffixed operators are primitive (built-in) variants.

### If/Then/Else

Conditional expressions:

```
if condition then expr1 else expr2
```

Both branches are required. The condition should evaluate to a `Bool`.

**Example:**

```
function testIf(b:Bool) : Nat = if b then Succ(Z) else Z;
```

### Let/In

Local bindings with `let ... in`:

```
let name = expr in body
```

Multiple bindings are separated by commas:

```
let x = expr1, y = expr2 in body
```

Bindings can have optional type annotations:

```
let x:Nat = Succ(Z) in plus(x, x)
```

**Example:**

```
function testLet(n:Nat) : Nat = let one = Succ(Z) in plus(n, one);
```

### Anonymous Lambdas

Anonymous (unnamed) functions are created with the backslash syntax:

```
\x -> expr
\x, y -> expr
\x:Nat -> plus(x, Succ(Z))
```

Anonymous lambdas are first-class values — they can be passed as arguments, returned from functions, and stored in data structures.

**Examples:**

```
> (\x -> plus(x, Succ(Z)))(Succ(Z))
Succ(Succ(Z))

> apply(\x -> not(x), True)
False
```

Anonymous lambdas desugar to `Function (Lambda "" args body UNDEFINED)` — a function with an empty name.

### First-Class Functions

Functions in tulam are first-class values. Named functions can be passed as arguments to other functions:

```
> apply(not, True)
False
> compose(not, not, True)
True
```

The standard library provides several combinators for working with functions:

| Combinator | Signature | Description |
|------------|-----------|-------------|
| `id` | `(x:a) : a` | Identity function |
| `const` | `(x:a, y:b) : a` | Returns first argument |
| `apply` | `(f:a, x:b) : c` | Apply function to argument |
| `compose` | `(f:b, g:a, x:a) : c` | Function composition: `f(g(x))` |
| `flip` | `(f:a, x:b, y:c)` | Flip first two arguments: `f(y, x)` |

### Parenthesized Expressions

Parentheses can be used for grouping:

```
(x + y) * z
```

---

## Structures (Typeclasses)

Structures are tulam's equivalent of Haskell's typeclasses. They define a set of functions that can be implemented for different types.

### Syntax

```
structure Name(args) = {
  function fn1(params) : ReturnType = defaultImpl,
  function fn2(params) : ReturnType = defaultImpl
};
```

### Categorical Keywords

tulam provides specialized keywords for classifying structures by their categorical role. Each has both a mathematical and a programmer-friendly alias:

| Math keyword | Friendly keyword | Meaning | Use when |
|---|---|---|---|
| `algebra` | `trait` | Single-type operations | Eq, Monoid, Ord, etc. |
| `morphism` | `bridge` | Multi-type relations | Convertible, Iso, etc. |
| `structure` | — | General-purpose (no alias) | Everything else |

Both forms are fully interchangeable:

```
// Math style:
algebra Semigroup(a:Type) = { function combine(x:a, y:a) : a };

// Programmer-friendly style (identical semantics):
trait Semigroup(a:Type) = { function combine(x:a, y:a) : a };

// Multi-type:
morphism Convertible(a:Type, b:Type) = { function convert(x:a) : b };
bridge Convertible(a:Type, b:Type) = { function convert(x:a) : b };
```

The compiler issues a warning (not an error) if:
- An `algebra`/`trait` has more or fewer than 1 type parameter
- A `morphism`/`bridge` has fewer than 2 type parameters

### Example

```
algebra Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y),
  law reflexivity(x:a) = (x == x) === True,
  law symmetry(x:a, y:a) = (x == y) === (y == x)
};
```

This defines an `Eq` algebra with two functions and two law declarations. Laws are desugared into proof-returning functions (see Law Declarations below).

### How Structures Work

When a structure is declared, its functions are transformed into functions with **implicit parameters**. For example, the `(==)` function from `Eq` becomes:

```
(==) [a:Type] (x:a, y:a) : Bool
```

The `[a:Type]` is an implicit parameter that gets resolved at compile time based on the types of the arguments. This means type-dependent dispatch happens during compilation, not at runtime.

### Type-Directed Dispatch

When a function's return type determines the correct dispatch (e.g., `toEnum(0) : Bool`), the argument types alone are insufficient. tulam uses **type-directed dispatch** to handle this:

1. During CLM conversion (Pass 4), `maybeAddTypeHint` wraps the body of functions with concrete return types in a `CLMTYPED` annotation.
2. The interpreter first attempts normal argument-based dispatch.
3. If dispatch fails (function returned unevaluated), the return type hint is used as a fallback to find the correct intrinsic or instance.
4. If dispatch succeeds but the result type doesn't match the declared return type, re-dispatch using the hint type is attempted.

This enables patterns like:
```
function testToEnumBool() : Bool = toEnum(0);       // → False
function testToEnumOrd() : Ordering = toEnum(2);     // → GreaterThan
function testFromEnumBool() : Int = fromEnum(True);  // → 1
```

---

## Law Declarations

Laws declare expected properties of structure functions. They use the `law` keyword as syntactic sugar, but are **desugared into real functions** returning `PropEqT` proof terms via the Curry-Howard correspondence: propositions are types, proofs are programs.

### Syntax

```
law name(params) = lawBody
```

Law bodies use two special operators:

| Operator | Meaning | Desugars to |
|----------|---------|-------------|
| `===` | Propositional equality | `PropEqT` return type |
| `==>` | Implication (right-associative) | Extra proof parameter |

### Desugaring Rules

A law is desugared into a function returning a `PropEqT` proof:

```
// Source:
law reflexivity(x:a) = (x == x) === True

// Desugars to:
function reflexivity(x:a) : PropEqT(_, x == x, True) = Refl
```

Implications (`==>`) become additional proof parameters:

```
// Source:
law transitivity(x:a, y:a, z:a) =
    ((x == y) == True) ==> ((y == z) == True) ==> ((x == z) === True)

// Desugars to:
function transitivity(x:a, y:a, z:a,
    __proof0: PropEqT(_, (x == y) == True, True),
    __proof1: PropEqT(_, (y == z) == True, True))
    : PropEqT(_, x == z, True) = Refl
```

A bare expression without `===` is treated as `expr === True`. A premise with `===` (e.g., `a === b ==> ...`) becomes `PropEqT(_, a, b)`.

### Examples

```
law reflexivity(x:a) = (x == x) === True;

law symmetry(x:a, y:a) = (x == y) === (y == x);

law transitivity(x:a, y:a, z:a) =
    ((x == y) == True) ==> ((y == z) == True) ==> ((x == z) === True);
```

Laws can only appear inside structure/algebra/morphism declarations. After desugaring, they become regular functions with implicit structure parameters, just like other structure functions.

---

## Structure Inheritance (`extends`)

A structure can inherit functions and laws from parent structures using `extends`.

### Syntax

```
algebra Child(a:Type) extends Parent1(a), Parent2(a) = {
  function childFunc(x:a) : a
};
```

### Behavior

- All functions and laws from parent structures are automatically included in the child
- The child can override inherited functions by declaring a function with the same name
- When an `instance` is declared for the child, its function implementations are **propagated to parent structures** as well, so dispatch works through both child and parent

### Example

```
algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) =
        combine(x, combine(y, z)) === combine(combine(x, y), z)
};

algebra Monoid(a:Type) extends Semigroup(a) = {
    function empty() : a
};

instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y),
    function empty() : Nat = Z
};
// combine is now available via both Semigroup(Nat) and Monoid(Nat)
```

---

## External Constraints (`requires`)

Structures and instances can declare external constraints using `requires`. These are validated at declaration time (the required structure must exist) but runtime resolution is deferred to the type checker.

### Syntax

```
morphism MonoidHom(a:Type, b:Type) requires Monoid(a), Monoid(b) = {
    function hom(x:a) : b
};

instance SomeStruct(Nat) requires Eq(Nat) = {
    function foo(x:Nat) : Bool = (x == Z)
};
```

---

## Instance Declarations

Instances provide implementations of structure functions for specific types.

### Syntax

```
instance StructureName(TypeArgs) = {
  function fn1(params) : ReturnType = implementation
};
```

Instances can also declare constraints:

```
instance StructureName(TypeArgs) requires Constraint1(Type), Constraint2(Type) = {
  function fn1(params) : ReturnType = implementation
};
```

Intrinsic instances provide all functions via the compiler:

```
instance StructureName(TypeArgs) = intrinsic;
```

### Examples

**Eq instance for Nat using a helper function:**

```
instance Eq(Nat) = {
  function (==)(x:Nat, y:Nat) : Bool = eq(x, y)
};
```

**Eq instance for Bool using pattern matching:**

```
instance Eq(Bool) = {
  function (==)(x:Bool, y:Bool) : Bool =
    match
    | True, True   -> True
    | False, False  -> True
    | True, False   -> False
    | False, True   -> False
};
```

When an instance is declared, its function implementations are added as new pattern match cases to the structure's functions, specialized for the given type arguments.

### Derived Instances

Instead of writing instance functions manually, you can derive them automatically using the `derive` keyword. This works for any algebra that defines a `derive { }` block:

```
instance Eq(Color) = derive;
instance Ord(Color) = derive;
instance Show(Color) = derive;
instance Hashable(Color) = derive;
```

When `derive` is used, the compiler looks up the algebra's `derive { }` block and instantiates its functions for the given type. See [Derive Blocks](#derive-blocks) below for how algebras define derivable implementations.

You can also derive instances inline with a type declaration using the `deriving` clause:

```
type Direction = North | South | East | West deriving Eq, Show;
```

This is equivalent to declaring the type and then writing separate `instance ... = derive;` declarations for each listed algebra.

---

## Classes (OOP)

tulam supports first-class OOP classes with single inheritance, dynamic method dispatch, and abstract/sealed modifiers. Classes are designed to map 1-1 to .NET, JavaScript, and C++ classes in codegen.

### Class Declaration

A class bundles data fields with methods:

```
class Animal(name:String, age:Int) = {
    function speak(self:Animal) : String = "...",
    function info(self:Animal) : String = self.name
};
```

- **Fields** are declared in the header parentheses (like record/constructor params)
- **Methods** are declared in the braces body (like structure declarations)
- All methods take an explicit **`self`** parameter — there is no implicit `this`
- Methods with no body are **abstract** (must be overridden by subclasses)
- Methods with a body are **concrete** (can be overridden, or inherited as-is)
- All methods are **virtual by default** (like Java/Kotlin)

### Construction

Objects are created with `ClassName.new(args)`:

```
let a = Animal.new("Rex", 5);
a.name       // "Rex" — field access
a.speak()    // "..." — method call
```

Arguments are ordered: inherited parent fields first, then own fields.

### Inheritance

Single inheritance via `extends`:

```
class Dog(breed:String) extends Animal = {
    override function speak(self:Dog) : String = "Woof!",
    function fetch(self:Dog, item:String) : String =
        self.name ++ " fetches " ++ item
};
```

- Subclass declares **only its own new fields** (parent fields are inherited automatically)
- `override` keyword is optional but checked — compiler verifies a parent method exists
- Inherited methods work unchanged on subclass instances
- Construction includes all fields: `Dog.new("Buddy", 3, "Labrador")` — parent fields (name, age) first, then own (breed)

Multi-level inheritance is supported:

```
class Puppy(toy:String) extends Dog = {
    override function speak(self:Puppy) : String = "Yip!"
};
// Puppy has fields: name (from Animal), age (from Animal), breed (from Dog), toy (own)
let p = Puppy.new("Max", 1, "Poodle", "ball");
p.name    // "Max"    — inherited from Animal (2 levels up)
p.breed   // "Poodle" — inherited from Dog (1 level up)
p.toy     // "ball"   — own field
p.speak() // "Yip!"   — overridden method
p.info()  // "Max"    — inherited from Animal
```

### Dynamic Dispatch

Method calls dispatch based on the **runtime type** of the object, not the static type:

```
function greet(a:Animal) : String = a.speak();
greet(Dog.new("Rex", 3, "Lab"))    // "Woof!" — dispatches to Dog.speak
greet(Cat.new("Whiskers", 2))      // "Meow!" — dispatches to Cat.speak
greet(Animal.new("Generic", 1))    // "..."   — dispatches to Animal.speak
```

The object carries its actual class tag (`ConsTag`). The interpreter looks up the class in `classDecls`, finds the method in the pre-merged `cmMethods` map (which includes inherited methods), and applies it.

### Abstract Classes

```
abstract class Shape(color:String) = {
    function area(self:Shape) : Float64,        // abstract — no body
    function describe(self:Shape) : String =    // concrete — has body
        "A " ++ self.color ++ " shape"
};
```

- Abstract classes **cannot be instantiated**: `Shape.new("red")` is a runtime error
- Abstract methods have no body and must be overridden by concrete subclasses
- Concrete methods are inherited as-is

### Sealed Classes

```
sealed class Result(a:Type) = {};
class Ok(value:a) extends Result(a) = {};
class Err(message:String) extends Result(a) = {};
```

`sealed` means all subclasses must be declared in the same source file. The compiler tracks source files via `cmSourceFile` in `ClassMeta` and emits an error if a child class extends a sealed parent from a different file. The compiler also warns about non-exhaustive pattern matches over sealed hierarchies (via `checkSealedExhaustiveness` after Pass 2).

### Method Modifiers

| Modifier | Meaning |
|----------|---------|
| (none) | Virtual method (can be overridden) |
| `override` | Overrides a parent method (compiler checks parent has it) |
| `final` | Prevents further override in subclasses |
| `static` | No `self` parameter, called on the class name |

### Super Constructor (for extern class extension)

When extending an external class, super constructor args specify parent initialization:

```
class MyButton(label:String) extends Button(label) = { ... };
```

For tulam-native inheritance, super constructor args are implicit (parent fields are passed through in construction order).

### Implementing Algebras

Classes can implement algebras (typeclasses) via the `implements` clause:

```
class Circle(radius:Float64) extends Shape implements Eq, Show = {
    function area(self:Circle) : Float64 = 3.14159 * self.radius * self.radius,
    function (==)(self:Circle, other:Circle) : Bool = self.radius == other.radius,
    function show(self:Circle) : String = "Circle(r=" ++ show(self.radius) ++ ")"
};
```

`implements Eq, Show` auto-generates the corresponding `instance Eq(Circle)` and `instance Show(Circle)` declarations. The compiler matches class method names against algebra function names (e.g., `(==)` matches `Eq`'s `(==)`, `show` matches `Show`'s `show`). If no matching methods are found but the algebra has a `derive` block, it falls back to auto-deriving.

### Dot Syntax

Dot notation resolves in order:
1. **Data field** (from class fields) — direct index access
2. **Class method** (from methods including inherited) — dynamic dispatch
3. **Record field** (for non-class types) — existing behavior

```
d.name          // field access
d.speak()       // method call (0 extra args)
d.fetch("ball") // method call (1 extra arg)
```

Method calls work with chaining: `Dog.new("Rex", 3, "Lab").speak()` — construction, then method call in one expression.

### Relation to Other Type Constructs

| Construct | Purpose | Polymorphism | Matching |
|-----------|---------|-------------|----------|
| **Sum types** | Closed discriminated unions | Parametric | Exhaustive |
| **Records** | Pure data (no methods) | Parametric | By field |
| **Classes** | Data + methods + inheritance | Subtype | By class tag (not exhaustive unless sealed) |
| **Algebras** | Ad-hoc polymorphism (typeclasses) | Parametric | N/A |

Classes and algebras are complementary: classes provide subtype polymorphism (inheritance), algebras provide ad-hoc polymorphism (typeclasses). A class can `implement` algebras.

### Internal Representation

- **Object**: `CLMCON (ConsTag "Dog" tag) [field0, field1, ...]` — no vtable, no hidden fields
- **Field layout**: parent fields first, own fields last, in declaration order
- **Method dispatch**: `CLMMCALL obj "methodName" args` — interpreter looks up ConsTag class name in `classDecls`, finds method in pre-merged `cmMethods` HashMap
- **Super calls**: `CLMSCALL obj "methodName" args` — resolves to parent class's method
- **Construction**: `CLMNEW "ClassName" args` — validates not abstract, builds CLMCON
- **Class metadata**: `ClassMeta` in `Environment.classDecls` stores fields, methods, hierarchy

---

## Reflection Primitives

tulam provides 6 intrinsic reflection primitives that expose the structure of values and types at runtime. These are the foundation for the derive system — deriving is NOT compiler magic, but pure tulam code powered by reflection.

### Value Reflection

These operate on any runtime value:

| Function | Signature | Description |
|----------|-----------|-------------|
| `tag` | `(x:a) : Int` | Constructor tag index (0-based). Primitives return 0. |
| `tagName` | `(x:a) : String` | Constructor name as string. Primitives return type name. |
| `arity` | `(x:a) : Int` | Number of fields in the constructor. Primitives return 0. |
| `field` | `(x:a, i:Int) : b` | Get the i-th field (0-indexed). Error if out of bounds. |

**Examples:**

```
> tag(True)
0
> tag(False)
1
> tagName(Just(42))
"Just"
> tagName(42)
"Int"
> arity(Just(42))
1
> field(Just(42), 0)
42
```

### Type Reflection

These operate on type names (as strings, since types are erased at runtime):

| Function | Signature | Description |
|----------|-----------|-------------|
| `numConstructors` | `(T:String) : Int` | Number of constructors in a sum type. |
| `constructorByIndex` | `(T:String, i:Int) : a` | Create a nullary constructor by tag index. |

**Examples:**

```
> numConstructors("Bool")
2
> numConstructors("Ordering")
3
> constructorByIndex("Bool", 0)
True
> constructorByIndex("Ordering", 2)
GreaterThan
```

### Structural Helpers

Built on the reflection primitives, these pure tulam functions (in `Core.Reflect`) provide generic implementations for common algebras:

| Function | Description |
|----------|-------------|
| `structuralEq(x, y)` | Compare constructor tags, then all fields recursively |
| `structuralCompare(x, y)` | Compare tags first, then fields lexicographically |
| `structuralShow(x)` | Constructor name + fields in parens |
| `structuralHash(x)` | Combine tag with field hashes |

---

## Derive Blocks

Algebras can include a `derive { }` block that provides generic implementations for automatic instance derivation. The derive block lives inside the algebra definition and uses reflection primitives to implement the algebra's functions structurally.

### Syntax

```
algebra Eq(a:Type) = {
    function (==)(x:a, y:a) : Bool,
    function (!=)(x:a, y:a) : Bool = not(x == y),

    derive {
        function (==)(x, y:a) : Bool = structuralEq(x, y),
        function (!=)(x, y:a) : Bool = not(structuralEq(x, y))
    }
};
```

The `derive { }` block appears after the algebra's function declarations (separated by a comma). It contains function definitions that will be instantiated when a user writes `instance AlgebraName(Type) = derive;`.

### Standard Library Derive Blocks

The following standard library algebras support `derive`:

- **Eq** — structural equality via `structuralEq`
- **Ord** — structural ordering via `structuralCompare`
- **Show** — structural string conversion via `structuralShow`
- **Hashable** — structural hashing via `structuralHash`

### Example

```
type Color = Red | Green | Blue;

instance Eq(Color) = derive;
instance Show(Color) = derive;

// Now these work:
// Red == Red       => True
// Red == Blue      => False
// show(Green)      => "Green"
```

Or equivalently with `deriving`:

```
type Color = Red | Green | Blue deriving Eq, Show;
```

---

## Effects

tulam tracks side effects using **row-polymorphic effect types**. Effects are declared as named sets of impure operations, and effect rows describe which side effects a computation may perform. Effect rows reuse the same row polymorphism machinery as records.

See `doc/EffectDesign.md` for the full design rationale and worked examples.

### Effect Declarations

```
effect Console = {
    function readLine() : String,
    function putStrLn(s:String) : Unit,
    function putStr(s:String) : Unit
};

effect FileIO = {
    function readFile(path:String) : String,
    function writeFile(path:String, content:String) : Unit
};

// Parameterized effects
effect State(s:Type) = {
    function get() : s,
    function put(x:s) : Unit,
    function modify(f: s -> s) : Unit
};
```

Effects are distinct from algebras: **algebras describe pure structure** (Eq, Ord, Monoid), **effects describe impure operations** (reading files, printing to console).

### Effect Row Types

```
Eff { console: Console, fileio: FileIO } Unit    // closed: exactly these effects
Eff { console: Console | r } Unit                // open: these effects + possibly more
Eff {} Unit                                       // pure: no effects (≅ plain value)
```

### Function Signatures with Effects

```
// Explicit effect annotation
function greet() : Eff { console: Console | r } Unit = action {
    name <- readLine(),
    putStrLn("Hello, " ++ name)
};

// Effect inference — compiler infers effect row from body
function greet() = action {
    name <- readLine(),
    putStrLn("Hello, " ++ name)
};

// Pure functions have no effect row
function add(x:Int, y:Int) : Int = x + y;
```

### Effect Composition

When effectful functions are composed, their effect rows are automatically merged via row unification:

```
function showConfig() : Eff { console: Console, fileio: FileIO | r } Unit = action {
    config <- readFile("app.cfg"),   // contributes fileio
    putStrLn(config)                 // contributes console
};
```

Open effect rows (`| r`) enable effect polymorphism — a function with fewer effects works in any context with more effects.

### Effect Handlers

Handlers provide implementations for effect operations and eliminate effect labels from the row:

```
handler StdConsole : Console = {
    function readLine() = intrinsic,
    function putStrLn(s) = intrinsic
};

// handle removes the console effect from the row
let result = handle greet() with StdConsole;
```

Built-in effects (Console, FileIO) have implicit default handlers — they execute directly without explicit `handle`.

---

## Actions (Do-Notation)

Actions provide syntactic sugar for sequencing effectful operations, similar to Haskell's `do` notation. An `action` block desugars into monadic `bind`/`let` chains.

### Syntax

```
action name(args) : ReturnType = {
    name <- effectfulExpr,       // monadic bind
    localName = pureExpr,        // let-binding
    effectfulExpr,               // execute for side effect
    lastExpr                     // return value
};
```

### Desugaring Rules

| Action Statement | Desugared Form |
|-----------------|----------------|
| `name <- expr, rest` | `bind(expr, \name -> rest)` |
| `name = expr, rest` | `let name = expr in rest` |
| `expr, rest` | `seq(expr, rest)` = `bind(expr, \_ -> rest)` |
| `expr` (last) | `expr` |

### Examples

```
// Hello world
action main() = {
    putStrLn("Hello, World!")
};

// Interactive program
action main() = {
    putStr("Name? "),
    name <- readLine(),
    putStrLn("Hello, " ++ name)
};

// File processing
action processFile(input:String, output:String) = {
    content <- readFile(input),
    processed = toUpper(content),
    writeFile(output, processed),
    putStrLn("Done")
};
```

### Legacy Action Syntax

The older action syntax (without `<-`) still works for pure sequencing:

```
action main = {
  one = Succ(Z),
  three = Succ(Succ(one)),
  res = plus(three, one),
  print#(res)
};
```

---

## Built-in Operations

### Intrinsic Operations (Recommended)

The preferred way to provide built-in operations is through `intrinsic` instances on primitive types. For example, `instance EuclideanDomain(Int) = intrinsic` provides `+`, `-`, `*`, `div`, `mod` etc. for the `Int` type through the standard algebra system. See the [Intrinsic Implementations](#intrinsic-implementations) section above.

### Legacy Primitive Operations

tulam also has legacy primitive operations identified by the `#` suffix:

| Operation  | Description                     |
|------------|---------------------------------|
| `print#`   | Print a value                   |
| `primop#`  | Generic primitive operation     |

The `#`-suffixed arithmetic operators (`+#`, `-#`, `*#`, `/#`) still exist in the operator precedence table for backward compatibility, but the recommended approach is to use the algebra-based operators (`+`, `-`, `*`, `/`) which dispatch through intrinsic instances.

---

## Type Checking

tulam includes a bidirectional type checker that validates expressions and reports type errors. The type checker runs as Pass 3 in the compilation pipeline, after case optimization and before CLM conversion.

### Permissive Mode (Default)

By default, the type checker operates in **permissive mode**: type errors are reported as warnings but do not halt compilation. This allows the interpreter to continue executing even when type issues are detected. To make type errors fatal, set the `strictTypes` flag in the interpreter state.

### Bidirectional Checking

The type checker uses two modes:

- **Inference (synthesis)**: Given an expression, the checker synthesizes its type. Used for literals, variable lookups, function applications, and constructor calls.
- **Checking (verification)**: Given an expression and an expected type, the checker verifies they are compatible. Used when a type annotation is present or when checking function arguments against parameter types.

### What Gets Checked

| Expression | Checking behavior |
|------------|-------------------|
| Literals (`42`, `3.14`, `"hello"`) | Inferred as `Int`, `Float64`, `String` respectively |
| Variables | Looked up in the type environment |
| Function application `f(x, y)` | Argument types checked against parameter types |
| Constructor application `Just(x)` | Arity and field types checked |
| Lambda `\x -> body` | Parameter and body types inferred/checked |
| Let bindings `let x = e in body` | Binding type inferred, body checked with extended environment |
| If/then/else | Condition checked as `Bool`, branches must have same type |
| Pattern matching | Patterns checked against scrutinee type, bodies must agree |
| Repr casts `e as T` | Repr relationship verified to exist |
| Record literals `{x = 1, y = 2}` | Field types inferred, produces record type |
| Record types `{x:Int, y:Bool}` | Validated as type-level construct |

### Structure Constraints

When a structure function (e.g., `==` from `Eq`) is called with concrete types, the type checker emits a `CStructure` constraint (e.g., `Eq(Nat)`) and verifies that a matching instance exists in the environment.

### Polymorphism

The type checker supports parametric polymorphism via `TForall`. When a polymorphic function is applied, its universally quantified type variables are instantiated with fresh unification variables that are then unified with the actual argument types. Generalization abstracts over free type variables at let-binding boundaries.

### Record Types and Row Polymorphism

Record types use row polymorphism for structural typing:

- **Record literals**: `{x = 1, y = True}` has type `{x:Int, y:Bool}`
- **Record types**: `{x:Int, y:Bool}` is a closed record type (no extra fields allowed)
- **Row variables**: Open records can accept additional fields (used internally for structural subtyping)

Record literals are parsed with `{name = value}` syntax. Record type annotations use `{name:Type}` syntax.

### Internal Type Representation

The type checker uses its own internal type representation (`Ty`) separate from the Surface AST:

| Type | Description |
|------|-------------|
| `TVar` | Unification variable (can be solved) |
| `TRigid` | Rigid variable (quantified, cannot be unified) |
| `TCon` | Type constructor (`Int`, `Bool`, `Nat`, etc.) |
| `TApp` | Type application (`Maybe Int`) |
| `TPi` | Dependent function type (Pi type) |
| `TSigma` | Dependent pair type (Sigma type) |
| `TId` | Identity/equality type (for `PropEqT`) |
| `TForall` | Universal quantification |
| `TRecord` | Record type with named fields |
| `TU` | Universe type (`U 0` = Type, `U 1` = Type1, etc.) |

Row types: `REmpty` (empty row), `RExtend` (label:type + rest), `RVar` (row variable), `RRigid` (rigid row variable).

### Unification

The type checker uses a HashMap-based substitution with occurs check. Unification resolves type variables by finding the most general substitution that makes two types equal. The occurs check prevents infinite types (e.g., `a = List(a)`).

---

## Module System

tulam has a module system for organizing code into separate files with controlled visibility.

### Module Declaration

Every source file can optionally declare its module name:

```
module Foo.Bar;
```

Module names use dot-separated paths that correspond to the file system layout. The module `Foo.Bar` would be located at `Foo/Bar.tl` relative to the library search path.

### Imports

Import declarations bring names from other modules into scope:

```
import Foo.Bar;                    // Import module, access via qualified names
import Foo.Bar (baz, Quux);       // Import specific names
import Foo.Bar hiding (internal); // Import everything except listed names
```

### Open

The `open` keyword brings all exported names from a module directly into scope (unqualified):

```
open Foo.Bar;
```

### Export

The `export` keyword controls which names are visible to importers:

```
export (foo, bar, MyType);
```

When no `export` declaration is present, all top-level names are exported by default.

### Visibility Modifiers

**`private`** — Marks a declaration as module-private (not exported even if no export list):

```
private function helper(x:Int) : Int = x + 1;
```

**`opaque`** — Exports a type name but hides its constructors:

```
opaque type Handle = MkHandle(Int);
```

Importers can use `Handle` as a type but cannot construct or pattern-match on `MkHandle`.

### Target-Specific Code

The `target` keyword enables conditional compilation for different backends:

```
target js {
    extern function alert(msg:String) : Unit;
};

target dotnet {
    extern function Console.WriteLine(msg:String) : Unit;
};
```

The `extern` keyword within target blocks declares foreign functions provided by the target platform.

### Module Resolution

The module system (`src/ModuleSystem.hs`) provides:

- **Path resolution**: Maps module paths to file system locations, searching `lib/` and project directories
- **Dependency graph**: Builds a directed graph of module imports
- **Cycle detection**: Reports errors for circular module dependencies
- **Topological sort**: Determines correct compilation order so dependencies are compiled before dependents

### Standard Library Layout

The `lib/` directory contains the modular standard library:

| Directory | Contents |
|-----------|----------|
| `lib/Algebra/` | Generic algebraic structures (Semigroup, Monoid, Group, etc.) |
| `lib/Core/` | Core types (Bool, Maybe, Either, List, Nat, Ordering) |
| `lib/Numeric/` | Numeric algebras and instances (Semiring through Floating) |
| `lib/HKT/` | Higher-kinded type structures (Functor, Applicative, Monad) |
| `lib/Morphism/` | Multi-type structures (Convertible, etc.) |
| `lib/Collection/` | Collection types and operations |
| `lib/SIMD/` | SIMD vector types and Lane algebra |
| `lib/Repr/` | Representation mappings |

---

## Standard Library (prelude.tl + base.tl)

> **Full API Reference:** See [doc/StandardLibrary.md](StandardLibrary.md) for auto-generated documentation of all 59 standard library modules, including every type, algebra, function, instance, and effect with doc comments.

The standard library is loaded automatically when you start the REPL. It consists of two files loaded in order:

1. **`prelude.tl`** — Primitive type declarations (all numeric types, String, Char, Array, Vec types)
2. **`base.tl`** — All algebras (with laws), types, instances, and utility functions

### Prelude (prelude.tl)

The prelude declares all primitive (machine-level) types:

```
// Core types
primitive Int;
primitive Float64;
primitive String;
primitive Char;

// Fixed-width signed integers
primitive Int8;
primitive Int16;
primitive Int32;
primitive Int64;

// Unsigned integers
primitive UInt;
primitive UInt8;
primitive UInt16;
primitive UInt32;
primitive UInt64;
primitive Byte;

// Additional float width
primitive Float32;

// Parameterized primitives
primitive Array(a:Type);
primitive Vec2(a:Type);
primitive Vec4(a:Type);
primitive Vec8(a:Type);
primitive Vec16(a:Type);
```

### Base Library (base.tl)

The base library provides all algebra definitions (with laws), core types, instances, and utility functions.

#### Algebraic Number Hierarchy

The numeric hierarchy is built on mathematically proper foundations using multiple extends. All algebras include equational laws expressed with `===` and `==>`.

**Layer 1: Generic single-operation structures**

```
Semigroup(combine) → Monoid(empty) → Group(inverse) → AbelianGroup(commutativity law)
```

**Layer 2: Named additive/multiplicative structures**

```
AdditiveSemigroup(+) → AdditiveMonoid(zero) → AdditiveGroup(negate,-) → AdditiveAbelianGroup
MultiplicativeSemigroup(*) → MultiplicativeMonoid(one) → MultiplicativeGroup(recip,/)
```

**Layer 3: Composed numeric structures (multiple extends)**

```
Semiring extends AdditiveMonoid, MultiplicativeMonoid       // +, *, zero, one
Ring extends AdditiveAbelianGroup, MultiplicativeMonoid      // + negate, -
CommutativeRing extends Ring                                 // + * commutativity
EuclideanDomain extends CommutativeRing                      // + div, mod
Field extends CommutativeRing, MultiplicativeGroup           // + recip, /
Floating extends Field                                       // + sqrt, sin, cos, ...
```

**Standalone algebras:** Absolute (abs, signum), Bits (.&., .|., xor, ...), StringOps (concat, length)

Example algebra definitions:

```
algebra Semigroup(a:Type) = {
    function combine(x:a, y:a) : a,
    law associativity(x:a, y:a, z:a) = combine(combine(x, y), z) === combine(x, combine(y, z))
};

algebra Group(a:Type) extends Monoid(a) = {
    function inverse(x:a) : a,
    law left_inverse(x:a) = combine(inverse(x), x) === empty,
    law right_inverse(x:a) = combine(x, inverse(x)) === empty
};

algebra Semiring(a:Type) extends AdditiveMonoid(a), MultiplicativeMonoid(a) = {
    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z,
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z,
    law annihilation(x:a) = zero * x === zero,
    law additive_commutativity(x:a, y:a) = x + y === y + x
};

algebra EuclideanDomain(a:Type) extends CommutativeRing(a) = {
    function div(x:a, y:a) : a,
    function mod(x:a, y:a) : a,
    law division(x:a, y:a) = div(x, y) * y + mod(x, y) === x
};

algebra Field(a:Type) extends CommutativeRing(a), MultiplicativeGroup(a) = {
    law multiplicative_inverse(x:a) = x * recip(x) === one
};
```

#### Comparison and Collection Algebras

```
algebra Eq(a:Type) = {
    function (==)(x,y:a) : Bool = not(x != y),
    function (!=)(x,y:a) : Bool = not(x == y),
    law reflexivity(x:a)  = (x == x) === True,
    law symmetry(x:a, y:a) = (x == y) === (y == x),
    law transitivity(x:a, y:a, z:a) = ((x == y) === True) ==> ((y == z) === True) ==> ((x == z) === True)
};

algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function (<)(x:a, y:a) : Bool,  function (>)(x:a, y:a) : Bool,
    function (<=)(x:a, y:a) : Bool, function (>=)(x:a, y:a) : Bool,
    law trichotomy(x:a, y:a)    = (x < y) === not(x >= y),
    law antisymmetry(x:a, y:a)  = ((x <= y) === True) ==> ((y <= x) === (x == y)),
    law transitivity(x:a, y:a, z:a) = ((x <= y) === True) ==> ((y <= z) === True) ==> ((x <= z) === True)
};
```

#### Higher-Kinded Type Algebras

```
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b),
    law identity(x:f(a)) = fmap(id, x) === x,
    law composition(f:b -> c, g:a -> b, x:f(a)) =
        fmap(compose(f, g), x) === fmap(f, fmap(g, x))
};

algebra Applicative(f:Type1) extends Functor(f) = { ... };

algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f: a -> m(b)) : m(b),
    function seq(x:m(a), y:m(b)) : m(b),
    law left_identity(x:a, f: a -> m(b))  = bind(pure(x), f) === f(x),
    law right_identity(x:m(a)) = bind(x, pure) === x,
    law associativity(x:m(a), f: a -> m(b), g: b -> m(c)) =
        bind(bind(x, f), g) === bind(x, \y -> bind(f(y), g))
};
```

#### Intrinsic Instances

```
instance EuclideanDomain(Int) = intrinsic;
instance Absolute(Int) = intrinsic;
instance Bits(Int) = intrinsic;
instance Floating(Float64) = intrinsic;
instance Absolute(Float64) = intrinsic;
instance StringOps(String) = intrinsic;
instance Eq(Int) = intrinsic;       instance Ord(Int) = intrinsic;
instance Eq(Float64) = intrinsic;   instance Ord(Float64) = intrinsic;
instance Eq(String) = intrinsic;    instance Ord(String) = intrinsic;
```

#### Core Types

### Nat (Natural Numbers)

```
type Nat = Z | Succ(n:Nat);
```

- `Z` — zero
- `Succ(n)` — successor of `n`

**Arithmetic:**

```
function plus(x:Nat, y:Nat) : Nat =
  match
  | Z, n       -> n
  | Succ(n), m -> Succ(plus(n, m));
```

**Equality:**

```
function eq(x:Nat, y:Nat) : Bool =
  match
  | Z, Z             -> True
  | Z, n             -> False
  | n, Z             -> False
  | Succ(m), Succ(n) -> eq(m, n);
```

### Bool

```
type Bool = True | False;
```

**Negation:**

```
function not(b:Bool) : Bool =
  match
  | True  -> False
  | False -> True;
```

### Eq Algebra

The `Eq` algebra provides overloaded equality with law declarations:

```
algebra Eq(a:Type) = {
  function (==)(x, y:a) : Bool = not(x != y),
  function (!=)(x, y:a) : Bool = not(x == y),
  law reflexivity(x:a) = (x == x) === True,
  law symmetry(x:a, y:a) = (x == y) === (y == x)
};

instance Eq(Nat) = {
  function (==)(x:Nat, y:Nat) : Bool = eq(x, y)
};

instance Eq(Bool) = {
  function (==)(x:Bool, y:Bool) : Bool =
    match
    | True, True   -> True
    | False, False  -> True
    | True, False   -> False
    | False, True   -> False
};
```

### Ordering and Ord

```
type Ordering = LessThan | Equal | GreaterThan;

algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function (<)(x:a, y:a) : Bool,
    function (>)(x:a, y:a) : Bool,
    function (<=)(x:a, y:a) : Bool,
    function (>=)(x:a, y:a) : Bool
};
```

Instances exist for `Nat` (user-defined), `Int`, `Float64`, and `String` (intrinsic).

### Semigroup and Monoid

```
algebra Semigroup(a:Type) = { function combine(x:a, y:a) : a };
algebra Monoid(a:Type) extends Semigroup(a) = { value empty : a };
```

### Maybe

```
type Maybe(a:Type) = Nothing | Just(val:a);
```

**Utility functions:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `isNothing` | `(m:Maybe(a)) : Bool` | True if Nothing |
| `fromMaybe` | `(def:a, m:Maybe(a)) : a` | Extract value or use default |

**Examples:**

```
> Just(Succ(Z))
Just(Succ(Z))
> isNothing(Nothing)
True
> fromMaybe(Z, Just(Succ(Z)))
Succ(Z)
> fromMaybe(Z, Nothing)
Z
```

### Either

```
type Either(a:Type, b:Type) = Left(val:a) | Right(val:b);
```

### List

```
type List(a:Type) = Nil | Cons(hd:a, tl:List(a));
```

**Utility functions:**

| Function | Signature | Description |
|----------|-----------|-------------|
| `head` | `(xs:List(a)) : Maybe(a)` | First element or Nothing |
| `tail` | `(xs:List(a)) : Maybe(List(a))` | All but first, or Nothing |
| `isEmpty` | `(xs:List(a)) : Bool` | True if Nil |

**Algebra instances:** `Semigroup(List(a))` and `Monoid(List(a))` are provided, with `combine` as list concatenation and `empty` as `Nil`.

**Examples:**

```
> head(Cons(Succ(Z), Nil))
Just(Succ(Z))
> head(Nil)
Nothing
> isEmpty(Nil)
True
> combine(Cons(Z, Nil), Cons(Succ(Z), Nil))
Cons(Z, Cons(Succ(Z), Nil))
```

### Convertible Morphism

```
morphism Convertible(a:Type, b:Type) = { function convert(x:a) : b };
```

Instances exist for `Convertible(Nat, Bool)` and `Convertible(Bool, Nat)`. The compiler automatically derives transitive compositions — given instances for `A→B` and `B→C`, it derives `A→C` (preferring direct instances over composed ones).

### Intrinsic Instances for Primitive Types

The base library includes intrinsic instances for Eq and Ord on primitive types:

```
instance Eq(Int) = intrinsic;
instance Eq(Float64) = intrinsic;
instance Eq(String) = intrinsic;
instance Ord(Int) = intrinsic;
instance Ord(Float64) = intrinsic;
instance Ord(String) = intrinsic;
```

### Utility Functions

```
function typeOf(ex:tp) : Type = tp;     // Returns the type of an expression
function consOf(ex:tp) : ConstructorTag = primop#;  // Returns the constructor tag
```

---

## Reserved Words

The following words are reserved and cannot be used as identifiers:

`type`, `function`, `if`, `then`, `else`, `in`, `action`, `structure`, `instance`, `let`, `case`, `of`, `where`, `exists`, `forall`, `record`, `algebra`, `trait`, `morphism`, `bridge`, `law`, `extends`, `requires`, `value`, `primitive`, `intrinsic`, `repr`, `invariant`, `as`, `default`, `match`, `module`, `import`, `open`, `export`, `private`, `opaque`, `hiding`, `target`, `extern`, `effect`, `handler`, `handle`, `derive`, `deriving`, `class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super`

The Unicode symbols `∃` and `∀` are also reserved (for future quantifier support).

### Reserved Operators

`;` `=` `,` `.` `..` `:` `->` `=>` `|` `?` `<:` `\` `===` `==>` `<-`

---

## Top-Level Declarations

All top-level declarations must be terminated with a semicolon (`;`):

```
module MyModule;                    // Module declaration (optional, must be first)
import Other.Module;                // Imports (after module declaration)
export (foo, bar);                  // Export list (optional)

type Bool = True | False;
function not(b:Bool) : Bool = match | True -> False | False -> True;
record Point = { x:Nat, y:Nat };
class Animal(name:String) = { function speak(self:Animal) : String = "..." };
class Dog(breed:String) extends Animal = { override function speak(self:Dog) : String = "Woof!" };
abstract class Shape(color:String) = { function area(self:Shape) : Float64 };
sealed class Result(a:Type) = {};
structure Eq(a:Type) = { ... };
instance Eq(Nat) = { ... };
action main = { ... };
private function helper(x:Int) : Int = x + 1;
opaque type Handle = MkHandle(Int);
```

Pattern match cases use `|` as a separator/prefix and do not require commas or semicolons between them.

---

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Sum type | `type Name = Con1 \| Con2(args);` |
| GADT constructor | `Con(args) : ReturnType` (inside sum type, separated by `\|`) |
| Arrow type | `a -> b`, `(a -> b) -> c` (in type position) |
| Type application | `Vec(a, n)`, `Maybe(a)` (in type position) |
| Record | `record Name = { field:Type };` |
| Record spread | `record Name = { ..Other, field:Type };` |
| Parameterized record | `record Name(a:Type) = { field:a };` |
| Dot-access | `expr.fieldName` |
| Named construction | `ConsName { field = expr, ... }` |
| Record update | `expr { field = newExpr, ... }` |
| Named field pattern | `ConsName { field = pat, ... }` |
| Function | `function name(args) : Type = body;` |
| Pattern match | `function f(x) = match \| pat -> expr \| ...;` |
| Inline match | `match expr \| pat -> body \| ...` |
| Operator function | `function (+)(x, y) = ...;` |
| If/then/else | `if cond then e1 else e2` |
| Let/in | `let x = e1, y = e2 in body` |
| Structure | `structure Name(a:Type) = { functions };` |
| Algebra/Trait | `algebra Name(a:Type) = { ... };` or `trait Name(a:Type) = { ... };` |
| Morphism/Bridge | `morphism Name(a:Type, b:Type) = { ... };` or `bridge ...` |
| Extends | `algebra Child(a:Type) extends Parent(a) = { ... };` |
| Requires | `morphism M(a,b) requires X(a), Y(b) = { ... };` |
| Law | `law name(params) = lhs === rhs` |
| Instance | `instance Name(Type) = { functions };` |
| Intrinsic instance | `instance Name(Type) = intrinsic;` |
| Derived instance | `instance Name(Type) = derive;` |
| Deriving clause | `type T = A \| B deriving Eq, Show;` |
| Class | `class Name(fields) = { function method(self:Name) : Type = body };` |
| Inheritance | `class Child(fields) extends Parent = { ... };` |
| Abstract class | `abstract class Name(fields) = { function method(self:Name) : Type };` |
| Sealed class | `sealed class Name = {};` |
| Method override | `override function speak(self:Dog) : String = "Woof!"` |
| Construction | `ClassName.new(args)` |
| Method call | `obj.method(args)` |
| Super call | `super.method(args)` (inside class methods) |
| Implements | `class C extends P implements Eq, Show = { ... };` |
| Derive block | `derive { function f(x:a) = ... }` (inside algebra) |
| Reflection | `tag(x)`, `tagName(x)`, `arity(x)`, `field(x, i)` |
| Primitive type | `primitive TypeName;` |
| Repr mapping | `repr UserType as ReprType [default] where { function toRepr(...) = ..., function fromRepr(...) = ... };` |
| Repr cast | `expr as TargetType` |
| Module declaration | `module Foo.Bar;` |
| Import | `import Foo.Bar;` or `import Foo.Bar (name1, name2);` |
| Open | `open Foo.Bar;` |
| Export | `export (name1, name2);` |
| Private | `private function f(x:Int) : Int = ...;` |
| Opaque type | `opaque type Handle = MkHandle(Int);` |
| Target block | `target js { extern function alert(msg:String) : Unit; };` |
| Value declaration | `value name : Type = expr` (inside structures/instances) |
| Effect declaration | `effect Name = { function op(args) : Type, ... };` |
| Effect row type | `Eff { label: Effect, ... \| r } ResultType` |
| Handler | `handler Name : Effect = { function op(...) = ..., ... };` |
| Handle expression | `handle expr with HandlerName` |
| Action (do-notation) | `action name() = { name <- expr, ... };` |
| Action (legacy) | `action name = { stmts };` |
| Anonymous lambda | `\x -> expr`, `\x:Nat, y -> expr` |
| Application | `f(x, y)` |
| List | `[1, 2, 3]` |
| Tuple | `{1, 2, 3}` |
| Record literal | `{x = 1, y = True}` |
| Record type | `{x:Int, y:Bool}` |
| Vector | `<1, 2, 3>` |
| Comment | `// line` or `/* block */` |
