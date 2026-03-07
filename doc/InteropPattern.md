# Algebra-Based Interop Pattern

## Overview

tulam's approach to native platform interop composes three existing language features into a single coherent pattern:

1. **Algebra** -- defines the behavioral contract (what operations a type supports)
2. **Repr** -- bridges data between representations (how to convert values)
3. **Target-qualified extern instances** -- maps algebra methods to native platform methods

This pattern avoids the need for a traditional FFI layer. Instead of writing glue code, the programmer defines a pure tulam algebra, implements it in pure tulam, and then optionally provides target-specific instances that the compiler can substitute when compiling for a particular backend. The compiler chooses the best implementation strategy: use the native platform method directly, convert data and delegate, or fall back to the pure tulam implementation.

This document uses **StringLike** as the primary worked example, but the pattern applies uniformly to collections, IO, date/time, concurrency, and any other domain where native platforms provide optimized implementations.

---

## Step 1: Define the Contract (Algebra)

The first step is always a pure tulam algebra that describes the behavioral interface. This algebra knows nothing about any platform -- it is a specification of what operations exist and what laws they obey.

```tulam
algebra StringLike(s:Type) = {
    /// Concatenate two string-like values.
    function concat(x:s, y:s) : s,

    /// Return the number of characters (not bytes).
    function charLength(x:s) : Int,

    /// Extract a substring by character index range [start, end).
    function substring(x:s, start:Int, end:Int) : s,

    /// Find the first occurrence of a pattern. Returns -1 if not found.
    function indexOf(x:s, pattern:s) : Int,

    /// Replace all occurrences of a pattern with a replacement.
    function replace(x:s, pattern:s, replacement:s) : s,

    /// Split by a delimiter.
    function split(x:s, delimiter:s) : List(s),

    /// Join a list of string-like values with a separator.
    function join(sep:s, parts:List(s)) : s,

    /// Convert to lowercase.
    function toLower(x:s) : s,

    /// Convert to uppercase.
    function toUpper(x:s) : s,

    /// Trim whitespace from both ends.
    function trim(x:s) : s,

    law concat_associativity(x:s, y:s, z:s) =
        concat(concat(x, y), z) === concat(x, concat(y, z)),

    law split_join_roundtrip(x:s, d:s) =
        join(d, split(x, d)) === x
};
```

Key points:

- The algebra is parameterized over a type `s`, not hard-coded to `String`.
- Laws document expected behavior and serve as optimization licenses.
- No mention of any platform, target, or representation. This is pure specification.

---

## Step 2: Pure Tulam Implementation (Reference Implementation)

Next, provide a pure tulam implementation of the algebra for the built-in `String` type. This implementation works in the interpreter and on every compilation target without any platform dependencies.

```tulam
instance StringLike(String) = {
    function concat(x:String, y:String) : String = strConcat(x, y),
    function charLength(x:String) : String = strLength(x),
    function substring(x:String, start:Int, end:Int) : String =
        strSubstring(x, start, end),
    function indexOf(x:String, pattern:String) : Int =
        strIndexOf(x, pattern),
    function replace(x:String, pattern:String, replacement:String) : String =
        strReplace(x, pattern, replacement),
    function split(x:String, delimiter:String) : List(String) =
        strSplit(x, delimiter),
    function join(sep:String, parts:List(String)) : String =
        strJoin(sep, parts),
    function toLower(x:String) : String = strToLower(x),
    function toUpper(x:String) : String = strToUpper(x),
    function trim(x:String) : String = strTrim(x)
};
```

This instance uses tulam's existing string intrinsics (`strConcat`, `strLength`, etc.) which are implemented in the interpreter's Haskell runtime. It is the reference implementation -- always correct, always available.

---

## Step 3: Native Platform Implementation (Target-Qualified Instances)

For each compilation target, provide a target-qualified instance that maps algebra methods directly to native platform methods. These instances are only compiled when targeting the specified platform.

### .NET Target

```tulam
target dotnet {
    instance StringLike(String) = {
        function concat(x:String, y:String) : String =
            System.String.Concat(x, y),
        function charLength(x:String) : Int =
            x.Length,
        function substring(x:String, start:Int, end:Int) : String =
            x.Substring(start, end - start),
        function indexOf(x:String, pattern:String) : Int =
            x.IndexOf(pattern),
        function replace(x:String, pattern:String, replacement:String) : String =
            x.Replace(pattern, replacement),
        function split(x:String, delimiter:String) : List(String) =
            toList(x.Split(delimiter)),
        function join(sep:String, parts:List(String)) : String =
            System.String.Join(sep, toArray(parts)),
        function toLower(x:String) : String = x.ToLowerInvariant(),
        function toUpper(x:String) : String = x.ToUpperInvariant(),
        function trim(x:String) : String = x.Trim()
    };
};
```

### JavaScript Target

```tulam
target js {
    instance StringLike(String) = {
        function concat(x:String, y:String) : String =
            x + y,
        function charLength(x:String) : Int =
            x.length,
        function substring(x:String, start:Int, end:Int) : String =
            x.substring(start, end),
        function indexOf(x:String, pattern:String) : Int =
            x.indexOf(pattern),
        function replace(x:String, pattern:String, replacement:String) : String =
            x.replaceAll(pattern, replacement),
        function split(x:String, delimiter:String) : List(String) =
            toList(x.split(delimiter)),
        function join(sep:String, parts:List(String)) : String =
            toArray(parts).join(sep),
        function toLower(x:String) : String = x.toLowerCase(),
        function toUpper(x:String) : String = x.toUpperCase(),
        function trim(x:String) : String = x.trim()
    };
};
```

### C++ / Native Target

```tulam
target native {
    instance StringLike(String) = {
        function concat(x:String, y:String) : String =
            std_string_append(x, y),
        function charLength(x:String) : Int =
            utf8_char_count(x),
        function substring(x:String, start:Int, end:Int) : String =
            utf8_substr(x, start, end),
        function indexOf(x:String, pattern:String) : Int =
            std_string_find(x, pattern),
        function replace(x:String, pattern:String, replacement:String) : String =
            std_string_replace_all(x, pattern, replacement),
        function split(x:String, delimiter:String) : List(String) =
            c_strtok_split(x, delimiter),
        function join(sep:String, parts:List(String)) : String =
            c_string_join(sep, parts),
        function toLower(x:String) : String = icu_to_lower(x),
        function toUpper(x:String) : String = icu_to_upper(x),
        function trim(x:String) : String = c_string_trim(x)
    };
};
```

The compiler sees three candidate instances for `StringLike(String)`:

1. The pure tulam instance (always available)
2. The target-qualified instance (available only when compiling for that target)

When compiling for a specific target, the target-qualified instance takes priority. The pure tulam instance remains the fallback.

---

## Step 4: Data Bridge (Repr Declarations)

When the native platform uses a different representation for a type, `repr` declarations bridge between the tulam type and the native representation. This is especially important for parameterized types and domain-specific types.

### Basic Repr

```tulam
repr Nat as Int default where {
    function toRepr(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + toRepr(m),
    function fromRepr(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(fromRepr(n - 1))
};
```

### Parameterized Repr (for collections)

```tulam
repr Array(Byte) as PackedBytes where {
    function toRepr(xs:Array(Byte)) : PackedBytes = packBytes(xs),
    function fromRepr(bs:PackedBytes) : Array(Byte) = unpackBytes(bs)
};
```

### Target-Specific Repr

Repr declarations can also be target-qualified when the representation mapping depends on the platform:

```tulam
target dotnet {
    repr List(a) as System.Collections.Generic.List(a) where {
        function toRepr(xs:List(a)) : System.Collections.Generic.List(a) =
            listToDotNetList(xs),
        function fromRepr(cs:System.Collections.Generic.List(a)) : List(a) =
            dotNetListToList(cs)
    };
};

target js {
    repr List(a) as Array(a) where {
        function toRepr(xs:List(a)) : Array(a) = listToArray(xs),
        function fromRepr(arr:Array(a)) : List(a) = arrayToList(arr)
    };
};
```

The compiler uses repr declarations to automatically insert conversion code at interop boundaries. When a tulam function returns `List(a)` but the .NET caller expects `System.Collections.Generic.List<A>`, the compiler inserts a `toRepr` call. When repr is zero-cost (the types are already the same representation internally), the conversion compiles away entirely.

---

## Compiler Optimization Strategy

The compiler has three strategies for dispatching algebra methods, chosen per call site based on what is available:

### Strategy 1: Use Native Directly (Zero Cost)

When the target-qualified instance exists and the data is already in native representation, the compiler emits a direct call to the native method. No conversion overhead.

```
charLength(s)  -->  s.Length        (on .NET, String is already System.String)
concat(x, y)   -->  x + y          (on JS, String is already a JS string)
```

This is the common case for primitive types like `String`, `Int`, and `Float64`, whose tulam types map directly to native types on all platforms.

### Strategy 2: Convert and Delegate

When the data needs conversion (a tulam type with a `repr` declaration), the compiler inserts `toRepr` before calling the native method and `fromRepr` on the result.

```
-- For a user type Text with repr Text as String:
charLength(t)  -->  charLength(toRepr(t))  -->  toRepr(t).Length   (on .NET)
```

The compiler fuses chains of `toRepr`/`fromRepr` when possible. If `toRepr` and `fromRepr` are no-ops (because the compiler already stores the value using the repr type), the conversion compiles away.

### Strategy 3: Use Pure Tulam (Fallback)

When no target-qualified instance exists, or when running in the interpreter, the compiler uses the pure tulam instance. This is always correct and always available.

```
charLength(s)  -->  strLength(s)    (interpreter intrinsic)
```

### Decision Procedure

At each call site, the compiler evaluates:

1. Is there a target-qualified instance for the current compilation target? If yes, prefer it.
2. Does the argument type need repr conversion? If yes, insert `toRepr`/`fromRepr` around the native call.
3. Otherwise, use the pure tulam instance.

For generic code (polymorphic over the algebra), the compiler generates a dispatch table that is resolved at monomorphization time. The same generic function `process [s] (x:s) : s requires StringLike(s)` may compile to native string methods on .NET and pure tulam calls in the interpreter, without the author of `process` knowing or caring.

---

## Applies To

This pattern is not specific to strings. It applies uniformly to every domain where native platforms provide optimized implementations.

### Strings

- **Algebra**: `StringLike(s)` -- concat, length, substring, indexOf, replace, split, join, case conversion, trim
- **Repr**: tulam `String` maps to `System.String` (.NET), JS `string`, `std::string` or `const char*` (native)
- **Native advantage**: Platform-optimized Unicode handling, regex engines, culture-aware operations

### Collections

- **Algebra**: `Collection(c)` -- insert, remove, lookup, size, iterate, fold
- **Repr**: `repr List(a) as System.Collections.Generic.List(a)` (.NET), `repr List(a) as Array(a)` (JS)
- **Native advantage**: Cache-friendly data structures, platform-specific hash maps, concurrent collections

### Ordered Collections

- **Algebra**: `Sortable(c)` -- sort, binarySearch, min, max
- **Repr**: `repr Array(a) as Span(a)` (.NET) for zero-copy slicing
- **Native advantage**: Introsort, TimSort, platform-tuned comparison routines

### IO and File System

- **Algebra**: `FileSystem(m)` -- readFile, writeFile, listDir, fileExists
- **Repr**: Paths as `String` or platform-specific path types
- **Native advantage**: Async IO, memory-mapped files, platform-specific permission models

### Date and Time

- **Algebra**: `Temporal(t)` -- now, addDays, diffDays, format, parse
- **Repr**: `repr DateTime as System.DateTime` (.NET), `repr DateTime as Date` (JS)
- **Native advantage**: Time zone databases, locale-aware formatting, leap second handling

### Concurrency

- **Algebra**: `Concurrent(m)` -- spawn, await, yield, cancel
- **Repr**: `repr Task(a) as System.Threading.Tasks.Task(a)` (.NET), `repr Task(a) as Promise(a)` (JS)
- **Native advantage**: Thread pools, work-stealing schedulers, platform-specific synchronization primitives

### Networking

- **Algebra**: `Socket(s)` -- connect, send, receive, close
- **Repr**: Platform-specific socket handles
- **Native advantage**: epoll/kqueue/IOCP, TLS integration, HTTP/2 and HTTP/3 stacks

### Serialization

- **Algebra**: `Serializable(a, fmt)` -- serialize, deserialize
- **Repr**: `repr JSON as String` for text formats, `repr Protobuf as Array(Byte)` for binary
- **Native advantage**: SIMD-accelerated JSON parsing (simdjson), platform-specific binary formats

### Regular Expressions

- **Algebra**: `Regex(r, s)` -- compile, match, findAll, replaceAll, groups
- **Repr**: `repr Pattern as System.Text.RegularExpressions.Regex` (.NET), JS `RegExp`
- **Native advantage**: JIT-compiled regex engines, platform-specific Unicode property tables

### Cryptography

- **Algebra**: `Hash(h)` -- digest, update, finalize; `Cipher(c)` -- encrypt, decrypt
- **Repr**: Platform-specific key and digest types
- **Native advantage**: Hardware-accelerated AES-NI, constant-time implementations, FIPS-certified libraries

---

## Relationship to Existing Designs

### InteropDesign.md

`InteropDesign.md` describes the mechanics of cross-platform interop: how `import ... target` works, how extern declarations expose platform types, how null/exception/callback mapping works, how subtyping and variance are handled.

This document describes the **pattern** that sits on top of those mechanics. Where InteropDesign.md says "the compiler loads metadata and makes extern types available," this document says "here is how you structure your tulam code to cleanly separate the contract from the platform implementation."

The two documents are complementary:

| Concern | InteropDesign.md | This document |
|---------|-----------------|---------------|
| How to import platform types | `import ... target dotnet` syntax, metadata resolution | Uses imports inside target-qualified instance bodies |
| How to call platform methods | Dot notation, method resolution order | Platform methods appear in target-qualified instance bodies |
| How to handle null/exceptions | `Maybe`/`Either` wrapping at boundaries | Wrapping happens inside target-qualified instance functions |
| How to bridge data types | Type mapping table (Section 6.1) | `repr` declarations formalize the mapping |
| How to write portable code | Not addressed | Algebra = portable contract, target instances = platform-specific |

### PrimitiveDesign.md

`PrimitiveDesign.md` describes how primitive types (`Int`, `Float64`, `String`, `Array`, SIMD vectors) interact with the algebra system via `intrinsic` instances, and how `repr` bridges user types to primitive representations.

This document generalizes that pattern. Where PrimitiveDesign.md focuses on machine-level primitives with compiler-provided intrinsics, this document extends the same algebra+repr+instance pattern to platform library interop.

The key connection is the `intrinsic` keyword:

- **PrimitiveDesign.md**: `instance Num(Int) = intrinsic;` -- the compiler provides the implementation using machine instructions
- **This document**: `target dotnet { instance StringLike(String) = { ... }; };` -- the platform provides the implementation using its own library methods

Both are "the implementation comes from outside tulam." The difference is where "outside" is: the CPU instruction set (intrinsic) vs. a platform runtime library (target-qualified instance).

### CategoricalDesign.md

`CategoricalDesign.md` defines the categorical vocabulary: `algebra` for single-type structures, `morphism` for multi-type relationships, `functor` for structure-preserving maps.

This document uses `algebra` as the contract mechanism. The choice of `algebra` vs `morphism` depends on the domain:

- `StringLike(s)` is an **algebra** -- it equips one type with operations
- `Convertible(a, b)` is a **morphism** -- it relates two types directionally
- `Serializable(a, fmt)` could be a **morphism** -- it relates a value type to a format type

The categorical vocabulary provides precision. An `algebra` instance is always for a single type, so the compiler knows that `StringLike(String)` is about `String` operations. A `morphism` instance relates two types, so `Convertible(List(a), System.Collections.Generic.List(a))` clearly describes a directional mapping.

---

## Complete Worked Example: StringLike

Putting it all together, here is the complete file structure for the StringLike algebra with full platform support.

### lib/Algebra/StringLike.tl (Pure Contract)

```tulam
/// String-like operations algebra.
///
/// Defines the behavioral contract for types that behave like strings.
/// Platform-independent -- no target-specific code.
module Algebra.StringLike;

algebra StringLike(s:Type) = {
    function concat(x:s, y:s) : s,
    function charLength(x:s) : Int,
    function substring(x:s, start:Int, end:Int) : s,
    function indexOf(x:s, pattern:s) : Int,
    function replace(x:s, pattern:s, replacement:s) : s,
    function split(x:s, delimiter:s) : List(s),
    function join(sep:s, parts:List(s)) : s,
    function toLower(x:s) : s,
    function toUpper(x:s) : s,
    function trim(x:s) : s,

    law concat_assoc(x:s, y:s, z:s) =
        concat(concat(x, y), z) === concat(x, concat(y, z)),
    law split_join(x:s, d:s) =
        join(d, split(x, d)) === x
};
```

### lib/Algebra/StringLike/Instances.tl (Pure Tulam Instance)

```tulam
/// Pure tulam instance of StringLike for String.
///
/// Uses intrinsic string operations. Works in the interpreter
/// and on all compilation targets as a fallback.
module Algebra.StringLike.Instances;
import Algebra.StringLike;

instance StringLike(String) = {
    function concat(x:String, y:String) : String = strConcat(x, y),
    function charLength(x:String) : Int = strLength(x),
    function substring(x:String, start:Int, end:Int) : String =
        strSubstring(x, start, end),
    function indexOf(x:String, pattern:String) : Int =
        strIndexOf(x, pattern),
    function replace(x:String, pattern:String, replacement:String) : String =
        strReplace(x, pattern, replacement),
    function split(x:String, delimiter:String) : List(String) =
        strSplit(x, delimiter),
    function join(sep:String, parts:List(String)) : String =
        strJoin(sep, parts),
    function toLower(x:String) : String = strToLower(x),
    function toUpper(x:String) : String = strToUpper(x),
    function trim(x:String) : String = strTrim(x)
};
```

### lib/Platform/DotNet/StringLike.tl (Target-Qualified Instance)

```tulam
/// .NET-specific StringLike instance.
///
/// Maps algebra methods to System.String methods for native performance
/// when compiling to .NET.
module Platform.DotNet.StringLike;
import Algebra.StringLike;
import System target dotnet;

target dotnet {
    instance StringLike(String) = {
        function concat(x:String, y:String) : String =
            System.String.Concat(x, y),
        function charLength(x:String) : Int = x.Length,
        function substring(x:String, start:Int, end:Int) : String =
            x.Substring(start, end - start),
        function indexOf(x:String, pattern:String) : Int =
            x.IndexOf(pattern),
        function replace(x:String, pattern:String, replacement:String) : String =
            x.Replace(pattern, replacement),
        function split(x:String, delimiter:String) : List(String) =
            toList(x.Split(delimiter)),
        function join(sep:String, parts:List(String)) : String =
            System.String.Join(sep, toArray(parts)),
        function toLower(x:String) : String = x.ToLowerInvariant(),
        function toUpper(x:String) : String = x.ToUpperInvariant(),
        function trim(x:String) : String = x.Trim()
    };
};
```

### User Code (Platform-Independent)

```tulam
/// User code that works everywhere -- interpreter, .NET, JS, native.
///
/// The compiler picks the best StringLike(String) instance for the target.

function shoutAndTrim(s:String) : String =
    trim(toUpper(s));

function csvLine(fields:List(String)) : String =
    join(",", fields);

/// Generic over any StringLike type.
function repeatN [s] (x:s, n:Int) : s requires StringLike(s) =
    match n
        | 0 -> x
        | _ -> concat(x, repeatN(x, n - 1));
```

When this code is compiled for .NET, calls to `trim`, `toUpper`, `join`, and `concat` resolve to `System.String` methods. When run in the interpreter, they resolve to the pure tulam intrinsics. The user code is identical in both cases.

---

## Design Principles

1. **Contract first.** Always define the algebra before any implementation. The algebra is the stable interface; implementations may change per platform.

2. **Pure fallback always exists.** Every algebra should have a pure tulam instance that works in the interpreter. Target-qualified instances are optimizations, not requirements.

3. **Repr is orthogonal.** Data bridging (`repr`) is separate from behavioral contracts (`algebra`). A type can have a `repr` mapping without any algebra, and an algebra without any `repr`. They compose when needed.

4. **Target blocks scope implementations, not contracts.** The `algebra` definition is never inside a `target` block. Only `instance` declarations and `repr` declarations go inside `target` blocks.

5. **The compiler decides.** The programmer writes portable code against the algebra. The compiler selects the best available implementation for the compilation target. No `#ifdef`, no conditional compilation in user code.

6. **Laws transfer.** When a target-qualified instance replaces a pure instance, the laws from the algebra still apply. If a native platform method violates a law (e.g., locale-dependent string comparison violating transitivity), the target-qualified instance should use the correct variant (e.g., `ToLowerInvariant` instead of `ToLower`).
