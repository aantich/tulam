# Multi-Backend Code Generation Design

## 1. Design Philosophy

tulam's codegen architecture follows the principle already embedded in the language design:

**All type and operation mappings flow through the algebra + repr system. The compiler hardcodes nothing.**

Every type mapping to a target platform is a `repr` declaration. Every operation mapping is an algebra/morphism instance qualified by `target`. The standard library provides sensible defaults, but everything is overridable by the user. This means:

- `repr Bool as boolean target js` + `instance Eq(Bool) target js = intrinsic` → Bool becomes JS boolean, `==` becomes `===`
- `repr Maybe(a) as Nullable(a) target dotnet` → Maybe becomes `T?` on .NET
- OR don't repr Maybe at all → it stays as an abstract class hierarchy. **User's choice.**
- `repr List(a) as Array(a) target js` → List becomes JS array. Or keep it as linked list ADT.

The compiler reads the repr map and algebra instances for the active target and emits accordingly. Pure tulam types without repr compile as pure functional data structures. Types with repr compile to native target representations.

### Core Principles

1. **Classes** → emit 1-1 to target classes (ES6, C#, C++). tulam classes CAN extend extern target classes.
2. **Algebras/morphisms** → emit via configurable dispatch strategy (dictionary passing, monomorphization, or hybrid)
3. **Sum types / records** → emit as pure functional structures OR as native types (controlled by repr)
4. **Lambdas** → emit natively (arrow functions on JS, delegates on .NET, closure-converted on C++)
5. **Primitives** → emit as target primitives via `repr` mappings
6. **Effects** → hybrid: explicit evidence-passing for custom effects, implicit for IO
7. **Cross-target** → a single tulam project can compile to multiple targets (like Kotlin Multiplatform)

---

## 2. Architecture Overview

```
Source (.tl) → Lexer/Parser → Surface AST
  → Pass 0:   After-parser desugaring
  → Pass 0.25: Action desugaring
  → Pass 0.5:  Record desugaring
  → Pass 1:   Environment building
  → Pass 1.5: Effect system setup
  → Pass 2:   Case optimization
  → Pass 3:   Type checking (bidirectional, row polymorphism)
  → Pass 4:   CLM conversion (Surface → CLM IR)
  → Pass 4.5: CLM optimization (eta, inline, constant fold, dead code)
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  → Pass 5:   Dispatch Resolution (CLM → Resolved CLM)    [SHARED]
  → Pass 6:   Target Emission                              [PER-TARGET]
               ├─→ JS Emitter    (Resolved CLM → .js + .d.ts)
               ├─→ .NET Emitter  (Resolved CLM → CIL/.dll)
               └─→ Native Pipeline
                    → Pass 6N.1: Closure Conversion
                    → Pass 6N.2: Monomorphization
                    → Pass 6N.3: ANF/CPS Conversion
                    → Pass 6N.4: C++ Emission (LLVM later)
```

### Compilation Model: Separate + Link-Time Optimization

Each module compiles independently (fast, incremental, cacheable — builds on our existing module cache system). A separate link/bundle step does cross-module inlining, dead code elimination, and monomorphization. Like C separate compilation.

A tulam project can target multiple backends simultaneously. Shared modules compile for all targets. Target-specific modules (using `target` blocks) compile only for their target.

### Why No Shared Lower IR for JS/.NET

CLM already maps almost 1-1 to both JS and CIL because:

1. **Closures**: JS has native closures. .NET has delegates/lambdas. No conversion needed.
2. **Dynamic dispatch**: JS has prototype chain. .NET has virtual methods. CLMMCALL maps directly.
3. **Pattern matching**: Both targets support switch/if chains.
4. **Constructors**: Tagged objects (JS) and sealed subclasses (.NET) are different syntax, same structure.

Pass 5 (dispatch resolution) is a shared CLM→CLM transform. Emitters are mostly pretty-printers.

### Why Native Needs Extra Lowering

C/C++ lacks closures, GC, runtime polymorphism on data, and generics. The native pipeline adds:

- **Closure conversion**: Explicit `{env*, fn*}` structs
- **Monomorphization**: Specialize polymorphic code per concrete type
- **Memory management**: Custom GC (separate design — see Section 7.10)

---

## 3. Key Design Decisions (Resolved)

All decisions from the design review, recorded for reference:

| # | Decision | Resolution | Rationale |
|---|---|---|---|
| 1 | **Type/operation mapping** | Via algebra + repr system, not hardcoded | Maximum flexibility. User controls what's native vs pure tulam. |
| 2 | **Effect threading** | Hybrid: explicit params for custom effects, implicit for IO | IO handler never changes. Custom effects need composable scoping. |
| 3 | **Compilation model** | Separate modules + link-time optimization | Fast incremental builds. Cross-module opt at link time. |
| 4 | **Tail call optimization** | Self-recursion → loops (v1). Mutual recursion → trampoline (later). | Correctness requirement for functional code. Self-recursion covers ~90%. |
| 5 | **Memory management (C++)** | Custom garbage collector (separate design project) | Essential for functional code. Too large to design inline — needs its own doc. |
| 6 | **Async model** | Defer to after v1 backends work | Avoid premature design. Ship sync evidence passing first. |
| 7 | **Dispatch strategy** | Target-dependent defaults + all 3 as compiler options | JS/.NET default: dictionary passing. C++ default: monomorphize. User can override. |
| 8 | **Reflection metadata** | Only when used (default). Always-emit as compiler option. | Zero overhead by default. Option for debugging/tooling. |
| 9 | **Stdlib distribution** | Both pre-compiled packages AND compile-from-source | Pre-compiled for production speed. Source for development freshness. |
| 10 | **Extern class extension** | Yes, from the start | Essential for GUI frameworks, game engines, etc. |
| 11 | **Debug info (v1)** | Line comments in generated code | Minimal effort. `// from MyModule.tl:42`. Full source maps/PDB/DWARF later. |
| 12 | **JS output format** | ES modules + TypeScript .d.ts generation | Tree-shakeable. tulam libraries consumable from TypeScript with type safety. |
| 13 | **.NET output format** | CIL (IL) directly via Mono.Cecil | Full control. No C# compiler dependency. |
| 14 | **C++ output format** | C++ source initially. LLVM backend later. | Portable, integrates with any toolchain. LLVM for optimization later. |
| 15 | **Missing target repr** | Warn + fall back to pure tulam representation | Graceful degradation. User can add repr to silence warning. |
| 16 | **Dispatch resolution** | Separate shared pass (Pass 5) | Clean separation from emitters. Easier testing. |
| 17 | **.NET overload resolution** | Require explicit disambiguation | Simpler, no surprising resolution. User annotates types when ambiguous. |
| 18 | **Cross-target compilation** | Yes, design for it from the start | Single project → multiple targets. Like Kotlin Multiplatform. |

---

## 4. Pass 5: Dispatch Resolution (Shared)

The most critical new pass. Transforms CLM's interpreter-centric dispatch into explicit operations.

### 4.1 What Gets Resolved

| CLM Input | Resolution | Output |
|---|---|---|
| `CLMIAP f [args]` (monomorphic) | Static instance lookup | `CLMAPP resolvedFn [args]` |
| `CLMIAP f [args]` (polymorphic) | Dictionary param or monomorphization (configurable) | Target-dependent |
| `CLMTYPED expr hint` | Resolve dispatch, drop wrapper | Resolved `expr` |
| `CLMPRIMCALL` | Replace with target prim marker | `CLMPrimOp opName [args]` |
| Intrinsic calls | Replace with prim ops via repr | `CLMPrimOp "add" [a,b]` |

### 4.2 What Passes Through Unchanged

| CLM Node | Why |
|---|---|
| `CLMMCALL obj method args` | Every target has native dynamic dispatch |
| `CLMSCALL obj method args` | Every target has super/base calls |
| `CLMNEW name args` | Every target has constructors |
| `CLMCON tag fields` | Tagged data — emitter handles representation per repr |
| `CLMLAM / CLMLamCases` | Emitter handles closure representation |
| `CLMCASE checks body` | Emitter handles pattern match syntax |
| `CLMHANDLE` | Effect compilation is target-specific |

### 4.3 Configurable Dispatch Strategy

Three strategies, all available as compiler options. Default depends on target.

**Option 1: Dictionary Passing** (default for JS/.NET)

Polymorphic calls receive a dictionary object containing the algebra/morphism methods:

```
-- Function: allEqual[a](xs:List(a)):Bool requires Eq(a)
-- Becomes: allEqual(dict$Eq, xs)

-- At call site with known type:
-- allEqual(EqInt, myList)  -- pass concrete dict
```

**Option 2: Monomorphization** (default for C++)

Specialize polymorphic code at each call site:

```
-- allEqual[Int](xs) → allEqual_Int(xs)  -- specialized, no dict
-- allEqual[String](xs) → allEqual_String(xs)
```

Falls back to dictionary passing when monomorphization fails (recursive polymorphism, existentials).

**Option 3: Hybrid (Monomorphize First, Dict Fallback)**

Try to specialize. If the call site has known concrete types → monomorphize. If polymorphic → dictionary pass. Best performance, available on all targets.

**Compiler flags:**
```
--dispatch=dict          # Always dictionary passing
--dispatch=mono          # Always monomorphize (fallback to dict)
--dispatch=hybrid        # Monomorphize when possible, dict otherwise
--dispatch=target        # Use target default (JS/.NET=dict, C++=mono)
```

### 4.4 Dictionary Representation

**Single-param algebra:**
```
-- algebra Eq(a:Type) = { function ==(x:a,y:a):Bool; function !=(x:a,y:a):Bool; };
EqDict = { eq :: (a, a) -> Bool, neq :: (a, a) -> Bool }
```

**Multi-param morphism:**
```
-- morphism Convertible(a:Type, b:Type) = { function convert(x:a):b; };
ConvertibleDict = { convert :: a -> b }
-- Keyed by ALL type params. Structurally identical to single-param.
```

**Structure `extends` (superclass embedding):**
```
-- algebra Ord(a) extends Eq(a) = { ... };
OrdDict = { super_Eq :: EqDict, compare :: (a,a) -> Ordering, ... }
-- Accessing Eq method inside Ord-constrained fn: dict_Ord.super_Eq.eq(x, y)
```

**`requires` constraints (separate dictionary):**
```
-- algebra Monad(m:Type1) requires Applicative(m) = { ... };
-- Function constrained by Monad receives TWO dict params:
doStuff(dict_Monad, dict_Applicative, x)
```

### 4.5 HKT Dictionary Handling (Functor, Applicative, Monad)

HKT algebras operate on type constructors (`Type1 = Type → Type`):

```tulam
algebra Functor(f:Type1) = {
    function fmap[a,b](fn: a -> b, x: f(a)) : f(b)
};
```

**JS**: HKT dictionaries work identically to normal dictionaries — JS doesn't care about kinds.

**.NET**: CIL cannot express `Functor<F>` where F is a type constructor. Strategy: monomorphize HKT-polymorphic code. A function using `Functor(f)` generically gets specialized per concrete type constructor at each call site.

**C++**: Same as .NET — monomorphize.

**Fallback** (when monomorphization is impossible): Use defunctionalization encoding — `IFunctor<FA, FB, A, B>` where `FA` is the concrete applied type.

### 4.6 New CLM Nodes for Resolved Code

```haskell
| CLMPrimOp Name [CLMExpr]              -- Target-native primitive operation
| CLMDictAccess Name CLMExpr            -- Access method from dictionary
| CLMBoundaryWrap BoundaryKind CLMExpr  -- Interop boundary wrapper
```

`CLMPrimOp` replaces intrinsic CLMIAP calls (guided by repr map):
- `CLMPrimOp "add" [a, b]` → JS: `a + b`, CIL: `add`, C++: `a + b`

`CLMBoundaryWrap` for interop:
- `NullToMaybe` / `MaybeToNull`
- `ExceptionToEither` / `EitherToException`

### 4.7 Tail Call Optimization (in Pass 5 or Pass 4.5)

**V1: Self-tail-recursion → while loops.**

Detection: a function whose body's last expression is a call to itself with updated args.

```
-- Before: function sum(n, acc) = if n == 0 then acc else sum(n-1, acc+n)
-- After:  while(true) { if (n===0) return acc; [n,acc]=[n-1,acc+n]; }
```

Applies to all targets uniformly. Essential correctness requirement — without it, recursive `map`/`foldl`/`filter` stack overflow on moderate inputs.

**Future: Mutual tail recursion → trampoline or state machine.** Added as a later optimization pass.

---

## 5. The Algebra + Repr Compilation Model

This is the core insight that drives everything. Rather than hardcoding type mappings, the compiler reads the repr map and algebra instance map for the active target.

### 5.1 How It Works

```tulam
-- In lib/Backend/JS/Native.tl (or similar target-specific stdlib module):
repr Int as number target js;
repr Float64 as number target js;
repr String as string target js;
repr Bool as boolean target js;
repr Char as string target js;

-- Algebra instances with target-specific intrinsic implementations:
instance Additive(Int) target js = intrinsic;   -- compiles (+) to JS +
instance Eq(Int) target js = intrinsic;          -- compiles (==) to JS ===
instance StringOps(String) target js = intrinsic; -- compiles concat to JS +
```

```tulam
-- In lib/Backend/DotNet/Native.tl:
repr Int as System.Int64 target dotnet;
repr Float64 as System.Double target dotnet;
repr String as System.String target dotnet;
repr Bool as System.Boolean target dotnet;
```

```tulam
-- In lib/Backend/LLVM/Native.tl:
repr Int as int64_t target native;
repr Float64 as double target native;
repr String as std::string target native;
repr Bool as bool target native;
```

### 5.2 What Happens When No Repr Exists

**Decision: Warn + fallback to pure tulam representation.**

If a type has no `repr` for the active target, the compiler emits a warning and compiles it as a normal tulam data structure (tagged object on JS, class hierarchy on .NET, tagged union on C++).

This means:
- User-defined types without repr → pure functional representation (always correct)
- Primitive types should have repr for all targets in the stdlib (warning catches omissions)
- Users can add repr for their own types to optimize specific target representations

### 5.3 Algebra ↔ Target Interface Mapping (`maps`)

```tulam
algebra Ord(a) maps IComparable(a) target dotnet;
```

Bidirectional:
- .NET types implementing `IComparable<T>` automatically get tulam `Ord` instances
- tulam types with `Ord` automatically implement `IComparable<T>` on .NET

Stored in `algebraMaps :: HashMap (Name, Target) Name` in Environment.

### 5.4 Target-Qualified Repr Extension

Extend `reprMap` in Environment to include target qualification:

```haskell
-- Current:
reprMap :: NameMap [(Name, Bool, Lambda, Lambda, Maybe Expr)]
-- Extended:
reprMap :: NameMap [(Name, Bool, Lambda, Lambda, Maybe Expr, Maybe Target)]
-- Nothing = all targets, Just target = specific target
```

Pass 5 filters `reprMap` by active target.

---

## 6. Pass 6: Target Emission

### 6.1 Shared Emitter Infrastructure

```haskell
-- src/Codegen.hs

data Target = TargetJS | TargetDotNet | TargetNative
  deriving (Eq, Show)

data CodegenConfig = CodegenConfig
  { cgTarget        :: Target
  , cgModuleName    :: String
  , cgDispatch      :: DispatchStrategy     -- Dict | Mono | Hybrid | TargetDefault
  , cgReflection    :: ReflectionMode       -- OnlyWhenUsed | AlwaysEmit
  , cgNullability   :: NullMode             -- Strict | Annotated | Unsafe
  , cgModuleFormat  :: ModuleFormat
  }

data DispatchStrategy = DispatchDict | DispatchMono | DispatchHybrid | DispatchTargetDefault
data ReflectionMode = ReflectOnlyUsed | ReflectAlways
data NullMode = NullStrict | NullAnnotated | NullUnsafe
```

### 6.2 Module-Level Emission

Each tulam module compiles to one target module:

| tulam | JS | .NET | C++ |
|---|---|---|---|
| `module Algebra.Ring` | `algebra/ring.js` + `.d.ts` | `Tulam.Algebra.Ring` (in .dll) | `tulam/algebra/ring.hpp` |

---

## 7. JavaScript Backend (`src/Codegen/JS.hs`)

### 7.1 Sum Types

```tulam
type Maybe(a:Type) = Nothing + Just * val:a;
```

**Without repr (pure tulam representation):**
```javascript
const Nothing = Object.freeze({ _tag: 0 });
const Just = (val) => ({ _tag: 1, _0: val });
```

**With `repr Maybe(a) as nullable target js`:**
```javascript
// Nothing → null, Just(val) → val
// Pattern matching adjusts: check null instead of _tag
```

The emitter checks the repr map and emits accordingly.

### 7.2 Pattern Matching

```javascript
// Without repr:
switch (x._tag) {
  case 0: return false;  // Nothing
  case 1: return true;   // Just
}

// With Bool repr as boolean:
if (x) { ... } else { ... }

// With Maybe repr as nullable:
if (x !== null) { ... } else { ... }
```

### 7.3 Records

```tulam
type Point = x:Int * y:Int;
```

```javascript
const Point = (x, y) => ({ x, y });
// Record update: const p2 = { ...p, x: 10 };
```

### 7.4 Functions and Lambdas

```javascript
function add(x, y) { return x + y; }   // top-level function
const double = (x) => x * 2;            // lambda
const add5 = (y) => add(5, y);          // partial application (inline closure)
```

All type annotations erased. JS closures handle capture natively.

### 7.5 Classes (1-1 Mapping)

```javascript
class Animal {
  constructor(name, age) { this.name = name; this.age = age; }
  speak() { throw new Error("Abstract method: speak"); }
  info() { return this.name + " (age " + show(this.age) + ")"; }
}

class Dog extends Animal {
  constructor(name, age, breed) { super(name, age); this.breed = breed; }
  speak() { return "Woof!"; }
}
```

tulam's explicit `self` → JS `this`. CLMMCALL → `obj.method()`. CLMSCALL → `super.method()`. CLMNEW → `new ClassName(args)`.

**Extern class extension**: tulam classes CAN extend extern JS classes:
```tulam
import { HTMLElement } from dom target js;
class MyWidget() extends HTMLElement = { ... };
```
```javascript
class MyWidget extends HTMLElement { ... }
```

### 7.6 Algebras and Morphisms

**Monomorphic (resolved at compile time):**
```javascript
// instance Eq(Int) target js = intrinsic;
// 1 == 2 → compiles directly to:
1 === 2
```

**Polymorphic (dictionary-passed, the JS default):**
```javascript
// function allEqual[a](xs:List(a)):Bool requires Eq(a)
function allEqual(dict$Eq, xs) {
  // uses dict$Eq.eq(x, y)
}
// Call site: allEqual({ eq: (a,b) => a === b, neq: (a,b) => a !== b }, myList)
```

**Multi-param morphisms — identical pattern:**
```javascript
// morphism Convertible(a, b): function stringify[a](x:a):String requires Convertible(a,String)
function stringify(dict$Convertible, x) { return dict$Convertible.convert(x); }
```

**HKT (Functor, Monad) — same as normal dicts on JS (JS doesn't care about kinds):**
```javascript
function liftA2(dict$Applicative, f, a, b) {
  return dict$Applicative.ap(dict$Applicative.fmap(f, a), b);
}
```

### 7.7 Effect System

**Hybrid approach:**

Custom effects → explicit evidence passing (compiler inserts parameters):
```javascript
// Custom handler for State effect:
function withRefState(init, body) {
  let ref = { value: init };
  const handler = { get: () => ref.value, put: (x) => { ref.value = x; } };
  return body(handler);
}

// Effectful function receives handler dict:
function increment(handler$State) {
  handler$State.put(handler$State.get() + 1);
}
```

IO effects → implicit (direct calls, no parameter threading):
```javascript
// IO operations emit directly:
console.log(msg);              // putStrLn
const line = prompt();         // readLine
```

### 7.8 Mutable References

```javascript
const r = { value: 0 };   // Ref(a) → { value: a }
r.value = 5;              // writeRef
const v = r.value;        // readRef

const arr = [1, 2, 3];    // MutArray → native JS array
arr.push(4);
```

### 7.9 Reflection Metadata

**Default (only when used):** Not emitted. `derive` instances are inlined at compile time.

**When user code uses reflection intrinsics OR `--reflect=always` flag:**
```javascript
// Add _type field to constructors:
const Just = (val) => ({ _tag: 1, _type: "Maybe", _0: val });

// Module-level metadata table:
const _meta = {
  Maybe: { constructors: [{ name: "Nothing", arity: 0 }, { name: "Just", arity: 1 }] }
};

function _tag(x) { return x._tag; }
function _tagName(x) { return _meta[x._type].constructors[x._tag].name; }
```

### 7.10 Module Output

ES modules + TypeScript declarations:

```javascript
// lib/Core/Maybe.js
export const Nothing = Object.freeze({ _tag: 0 });
export const Just = (val) => ({ _tag: 1, _0: val });
export function isJust(x) { return x._tag === 1; }
```

```typescript
// lib/Core/Maybe.d.ts
interface Nothing { readonly _tag: 0; }
interface Just<A> { readonly _tag: 1; readonly _0: A; }
type Maybe<A> = Nothing | Just<A>;
export declare const Nothing: Nothing;
export declare function Just<A>(val: A): Just<A>;
export declare function isJust<A>(x: Maybe<A>): boolean;
```

**Import mapping:**
```tulam
import Core.Maybe;           → import * as Maybe from './core/maybe.js';
import Core.Maybe (Just);    → import { Just } from './core/maybe.js';
import Core.Maybe as M;      → import * as M from './core/maybe.js';
```

### 7.11 Extern JS Interop

Boundary wrapping at call sites:
```javascript
// tulam: document.getElementById("root") : Maybe(Element)
const _raw = document.getElementById("root");
const result = _raw == null ? Nothing : Just(_raw);
```

### 7.12 Debug Info (v1)

Line comments mapping to source:
```javascript
// from Core/Maybe.tl:15
export function isJust(x) { return x._tag === 1; }
```

### 7.13 Target Blocks

```tulam
function hash(s:String) : Int = target | js -> hashCode_js(s) | dotnet -> s.GetHashCode();
```

Only the JS branch survives in JS compilation. `TargetSwitch` resolved before emission.

---

## 8. .NET Backend (`src/Codegen/DotNet.hs`)

**Output format: CIL (IL) directly via Mono.Cecil. No C# intermediate.**

### 8.1 Sum Types

```tulam
type Maybe(a:Type) = Nothing + Just * val:a;
```

**Without repr (pure tulam):**
Emits as abstract base class + sealed subclasses (CIL):
```
// Conceptual C# equivalent of emitted IL:
public abstract class Maybe<A> { public int Tag { get; } }
public sealed class Nothing<A> : Maybe<A> { ... Tag = 0 ... }
public sealed class Just<A> : Maybe<A> { public A Val; ... Tag = 1 ... }
```

**With `repr Maybe(a) as Nullable(a) target dotnet`:**
Emits as `T?` / `Nullable<T>`. Pattern matching adjusts to null checks.

### 8.2 Generics — Reified

.NET preserves generic type info at runtime. tulam generics map directly. No erasure needed (except HKT — see 8.6).

```
// tulam: function identity[a](x:a):a = x;
// CIL: static A Identity<A>(A x) { return x; }
```

### 8.3 Records

```tulam
type Point = x:Int * y:Int;
```

Emits as sealed class with readonly fields (CIL struct for small value types).

### 8.4 Classes (1-1 Mapping)

```tulam
class Animal(name:String, age:Int) = { ... };
class Dog(breed:String) extends Animal = { override function speak(self:Dog):String = "Woof!" };
```

Emits as CIL classes with virtual methods. tulam `self` → CIL `this`.

**Extern class extension:**
```tulam
import System.Windows.Forms target dotnet;
class MyButton() extends Button = { ... };
```
Emits `MyButton` as a CIL class extending `System.Windows.Forms.Button`.

### 8.5 Algebras → Interfaces (via `maps` or direct)

```tulam
algebra Eq(a:Type) = { function ==(x:a,y:a):Bool; function !=(x:a,y:a):Bool; };
```

Emits as CIL interface `IEq<A>` with methods.

**`maps` declaration:**
```tulam
algebra Ord(a) maps IComparable(a) target dotnet;
```
Bidirectional: .NET types with `IComparable<T>` get tulam `Ord` instances. tulam types with `Ord` implement `IComparable<T>` in CIL.

### 8.6 Morphisms → Multi-Param Interfaces

```tulam
morphism Convertible(a:Type, b:Type) = { function convert(x:a):b; };
```

Emits as `IConvertible<A, B>` interface in CIL.

### 8.7 HKT on .NET — Monomorphization

CIL cannot express `Functor<F>` where F is a type constructor. Strategy: monomorphize HKT-polymorphic code. Functions using `Functor(f)` generically get specialized per concrete type constructor.

Fallback for truly polymorphic HKT: defunctionalization encoding — `IFunctor<FA, FB, A, B>` where FA is the applied type.

### 8.8 Structure Inheritance

```tulam
algebra Ord(a) extends Eq(a) = { ... };
```

CIL: `interface IOrd<A> : IEq<A>` — interface inheritance maps 1-1.

### 8.9 `implements` — Class ↔ Algebra Bridge

```tulam
class MyList(items:Array(a)) implements Eq = { ... };
```

CIL: `class MyList<A> : IEq<MyList<A>>` — class directly implements the interface.

### 8.10 Effect System

Hybrid:
- Custom effects → evidence passing via interfaces
- IO effects → direct CLR API calls

### 8.11 Null and Exception Boundaries

**Calling .NET from tulam:**
```
// .NET method returns nullable → wrap in Maybe
var raw = obj.GetName();
var result = raw == null ? Nothing : Just(raw);
```

**Calling tulam from .NET:**
```
// tulam Either(String, Int) → .NET throws on Left, returns on Right
```

### 8.12 Overload Resolution

When extern .NET methods have multiple overloads, the compiler **requires explicit disambiguation** — user annotates parameter types. No implicit .NET-style overload resolution.

### 8.13 Debug Info (v1)

IL sequence points mapped to `.tl` source locations via line comments in the generated metadata. Full PDB later.

---

## 9. Native/C++ Backend (`src/Codegen/Native.hs`)

### 9.1 Architecture: Extra Lowering Passes

```
Resolved CLM
  → Pass 6N.1: Lambda Lifting + Closure Conversion
  → Pass 6N.2: Monomorphization (default dispatch strategy for native)
  → Pass 6N.3: Memory Management Annotation
  → Pass 6N.4: C++ Emission (LLVM backend added later)
```

### 9.2 Sum Types → Tagged Unions

```tulam
type Maybe(a:Type) = Nothing + Just * val:a;
```

**Without repr (monomorphized for `Maybe<int64_t>`):**
```cpp
struct Maybe_int {
    enum Tag { Nothing = 0, Just = 1 } tag;
    union {
        struct {} nothing;
        struct { int64_t val; } just;
    };
};
```

**With repr:** uses whatever the target type is.

### 9.3 Records → Structs

```cpp
struct Point { int64_t x; int64_t y; };
```

### 9.4 Pattern Matching → Switch

```cpp
switch (x.tag) {
    case Maybe_int::Nothing: return def;
    case Maybe_int::Just: return x.just.val;
}
```

Future optimization: decision trees for complex nested patterns.

### 9.5 Classes → C++ Classes (1-1)

```cpp
class Animal {
public:
    std::string name;
    int64_t age;
    Animal(std::string name, int64_t age) : name(std::move(name)), age(age) {}
    virtual ~Animal() = default;
    virtual std::string speak() const = 0;
    virtual std::string info() const { return name + " (age " + std::to_string(age) + ")"; }
};

class Dog : public Animal {
public:
    std::string breed;
    Dog(std::string name, int64_t age, std::string breed)
        : Animal(std::move(name), age), breed(std::move(breed)) {}
    std::string speak() const override { return "Woof!"; }
};
```

CLMMCALL → `obj->method()` (virtual dispatch). CLMSCALL → `Base::method()`. CLMNEW → allocation (managed by GC — see 9.10).

**Extern class extension:**
```tulam
import { QWidget } from qt target native;
class MyWidget() extends QWidget = { ... };
```
```cpp
class MyWidget : public QWidget { ... };
```

### 9.6 Closure Conversion (Pass 6N.1)

C++ function pointers cannot capture. Transform closures into explicit structs:

**V1: `std::function` (simple, correct):**
```cpp
std::function<int64_t(int64_t)> add5 = [](int64_t x) { return x + 5; };
// Capturing lambda:
auto adder = [x](int64_t y) { return x + y; };
```

**Future optimization: lambda lifting** — hoist closures to top-level functions with explicit env parameter. Avoids heap allocation.

### 9.7 Monomorphization (Pass 6N.2)

Default dispatch strategy for native. Specialize polymorphic code per concrete type:

```cpp
// tulam: identity[a](x:a):a = x;
int64_t identity_int(int64_t x) { return x; }
std::string identity_string(std::string x) { return x; }
```

Fallback (recursive polymorphism, existentials): vtable-based dispatch (like Rust `dyn Trait`).

### 9.8 Algebras → Vtable Structs (when not monomorphized)

```cpp
template<typename A>
struct Eq_vtable {
    bool (*eq)(const A&, const A&);
    bool (*neq)(const A&, const A&);
};
```

Usually eliminated by monomorphization. Only survives for truly polymorphic code.

### 9.9 Morphisms → Multi-Param Vtables

```cpp
template<typename A, typename B>
struct Convertible_vtable {
    B (*convert)(const A&);
};
```

### 9.10 Memory Management — Custom GC

**This is a separate design project** due to its complexity and far-reaching implications.

Preliminary direction: custom tracing garbage collector designed for functional workloads (immutable data, short-lived allocations, minimal mutation). Key considerations:
- Integration with C++ extern code (prevent GC from collecting extern objects)
- Support for deterministic finalization (file handles, connections)
- Low-pause or concurrent collection for interactive applications
- Compatibility with SIMD/GPU memory (pinned allocations)

**V1 fallback**: reference counting via `std::shared_ptr` until custom GC is designed. Correct for most functional code (cycles rare without mutation).

Dedicated design document: `doc/GCDesign.md` (to be written).

### 9.11 Effect System on C++

Hybrid:
- Custom effects → vtable pointers (evidence passing)
- IO effects → direct `std::cout`/`std::cin` calls

```cpp
struct ConsoleHandler {
    void (*putStrLn)(const std::string&);
    std::string (*readLine)();
};
```

### 9.12 SIMD/GPU Compilation

tulam's Vec/Lane types are designed for native compilation:

```cpp
#include <immintrin.h>
__m128 v = _mm_set_ps(4.0f, 3.0f, 2.0f, 1.0f);
__m128 w = _mm_set_ps(8.0f, 7.0f, 6.0f, 5.0f);
__m128 r = _mm_add_ps(v, w);
```

Lane algebra operations map 1-1 to SIMD intrinsics via repr + target-specific algebra instances.

### 9.13 Project Output

```
output/
  CMakeLists.txt
  tulam_runtime.hpp   (shared utilities, GC hooks, tagged union helpers)
  algebra/ring.hpp
  core/maybe.hpp
  main.cpp
```

Future: LLVM backend for better optimization without C++ intermediate.

---

## 10. Effect Compilation — Deep Dive

### 10.1 Hybrid Threading Model

**Custom effects** (State, Exception, user-defined): Compiler inserts handler dictionary parameters into effectful function signatures automatically. User code is unchanged.

```tulam
-- User writes:
function increment() : Unit with State(Int) = put(get() + 1);

-- Compiler transforms to (conceptually):
function increment(handler$State) = handler$State.put(handler$State.get() + 1);
```

**IO effects** (Console, FileIO): Implicit — emit direct target API calls. IO handler never changes within a program, so threading is unnecessary overhead.

```tulam
-- User writes:
putStrLn("hello");

-- Compiles directly to:
-- JS: console.log("hello")
-- .NET: Console.WriteLine("hello")  (CIL call)
-- C++: std::cout << "hello" << std::endl
```

### 10.2 Handler Composition

Nested `handle` blocks compose naturally — inner function receives all handler dicts:

```javascript
// JS output:
function body(handler$Console, handler$State) {
    handler$Console.putStrLn("count: " + handler$State.get());
    handler$State.put(handler$State.get() + 1);
}
```

### 10.3 Static Handler Optimization

When the handler is known at compile time, inline its implementation:

```tulam
handle { putStrLn("hello") } with SilentConsole;
-- Optimizes to: nothing (SilentConsole.putStrLn is a no-op)
```

### 10.4 Async Effects (Deferred)

Async compilation (JS Promise, .NET Task, C++ coroutines) is deferred to after v1 backends work. V1 ships with synchronous evidence passing for all effects.

---

## 11. Cross-Cutting Concerns

### 11.1 Name Mangling

```
tulam name        → JS name           → CIL name         → C++ name
==                → _op_eq            → OpEq              → op_eq
++                → _op_concat        → OpConcat          → op_concat
Core.Bool.not     → Core$Bool$not     → Tulam.Core.Bool   → Core__Bool__not
eq\0Int           → eq$Int            → EqInt             → eq__Int
eq\0Int\0String   → eq$Int$String     → EqIntString       → eq__Int__String
```

### 11.2 Entry Point

```tulam
action main() = { ... };
```

| Target | Entry point |
|---|---|
| **JS** | `export function main() { ... }` + optional auto-invocation |
| **.NET** | Static `Main` method in assembly entry point |
| **C++** | `int main(int argc, char** argv) { ... }` |

### 11.3 Error Handling

`CLMERR` nodes:
- JS: `throw new Error("message")`
- .NET: `throw new TulamException("message")` (CIL `throw`)
- C++: `throw std::runtime_error("message")`

### 11.4 Erased Features (No Codegen on Any Target)

- Universe hierarchy (U 0, U 1, U 2) — type-level only
- Arrow types (`a -> b`) — erased to CLMEMPTY at Pass 4
- Type annotations — type-level only
- Fixity declarations — parser-only
- Law declarations — test/verification only
- Repr invariants — verification only
- Module visibility (`private`, `export`, `opaque`) — compile-time enforcement
- Effect type annotations (`EffType`) — rows erased at CLM conversion
- Structure declarations themselves — only instances survive
- `derive` marker — resolved at compile time; generated instances survive

### 11.5 Standard Library Distribution

**Both** pre-compiled and source:
- **Pre-compiled**: Ship `tulam-stdlib` as npm package (JS), NuGet package (.NET), static lib (C++). Fast for production.
- **Source**: Compile from `lib/*.tl` for development. Always matches compiler version. Module cache system makes this fast.

---

## 12. Feature Compilation Matrix

Complete mapping of every surface language feature to every target. Type representations depend on repr map — entries below show the default stdlib repr.

### Data Types

| Feature | JS (default repr) | .NET (default repr) | C++ (default repr) |
|---|---|---|---|
| **Int** | `number` | `System.Int64` | `int64_t` |
| **Float64** | `number` | `System.Double` | `double` |
| **String** | `string` | `System.String` | `std::string` |
| **Bool** | `boolean` | `System.Boolean` | `bool` |
| **Sum types** | Tagged objects `{_tag, _0, ...}` | Abstract base + sealed subclasses | Tagged union struct |
| **Records** | Plain objects `{field: val}` | Sealed class / record struct | C struct |
| **Tuples** | Arrays `[a, b]` | `ValueTuple<A,B>` | `std::tuple<A,B>` |
| **Arrays** | JS arrays | `List<T>` or `T[]` | `std::vector<T>` |
| **Maybe** (with repr) | nullable | `Nullable<T>` / `T?` | `std::optional<T>` |
| **Maybe** (without repr) | `{_tag:0}` / `{_tag:1, _0:val}` | sealed class hierarchy | tagged union |

### Functions and Control Flow

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Top-level functions** | `function f(x) {...}` | Static method | `T f(T x) {...}` |
| **Lambdas** | `(x) => expr` | Delegate (Func<>/Action<>) | `std::function` or closure struct |
| **Partial application** | Inline closure | Delegate wrapper | Lambda or closure struct |
| **Pattern matching** | `switch(x._tag)` / if-chain | IL switch / branch | `switch(x.tag)` |
| **Tail recursion** | While loop (v1: self-recursion) | IL loop | While loop |

### Polymorphism and Dispatch

| Feature | JS (default: dict) | .NET (default: dict) | C++ (default: mono) |
|---|---|---|---|
| **Algebras** | Erased/inlined or dict obj | Interfaces (CIL) | Monomorphized or vtable |
| **Morphisms** | Erased/inlined or dict obj | Multi-param interfaces | Monomorphized or vtable |
| **HKT** | Dict obj (JS doesn't care about kinds) | Monomorphized (CIL lacks HKT) | Monomorphized |
| **Structure `extends`** | Dict with embedded super-dict | Interface inheritance | Vtable with super-pointer |
| **`requires`** | Extra dict params | Extra interface params | Extra vtable params |
| **`maps`** | N/A (JS has no interfaces) | Bidirectional algebra↔interface | Bidirectional algebra↔vtable |
| **`implements`** | Dict generated from class methods | Class implements interface | Class has static vtable |

### OOP (1-1 Mapping on All Targets)

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Classes** | ES6 `class` | CIL class | C++ `class` |
| **Inheritance** | `extends` | CIL extends | `: public Base` |
| **Extern extend** | Extend JS classes | Extend .NET classes | Extend C++ classes |
| **Method dispatch** | `obj.method()` | `callvirt` | `obj->method()` |
| **Super calls** | `super.method()` | CIL `call` base | `Base::method()` |
| **Abstract** | Throw in body | CIL abstract method | Pure virtual |
| **Sealed** | Convention | CIL sealed | `final` |
| **Downcast** | `instanceof` → Maybe | `isinst` → Maybe | `dynamic_cast` → Maybe |

### Effects

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Custom effects** | Dict param (explicit) | Interface param (explicit) | Vtable param (explicit) |
| **IO effects** | Direct `console.log` etc. | Direct CIL calls | Direct `std::cout` etc. |
| **State effect** | `{ value: x }` ref | Ref wrapper | Pointer/reference |
| **Async** | Deferred (v1: sync) | Deferred (v1: sync) | Deferred (v1: sync) |

### Interop

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Null boundary** | `== null ? Nothing : Just(x)` | `== null` CIL check | `std::optional` or null check |
| **Exception boundary** | `try/catch → Either` | CIL exception filter | `try/catch → Either` |
| **Extern metadata** | `.d.ts` files | Assembly metadata (Mono.Cecil) | Headers (libclang) |
| **Overload resolution** | N/A | Explicit disambiguation | Explicit disambiguation |
| **Target blocks** | Select `js` branch | Select `dotnet` branch | Select `native` branch |

### Mutable State

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Ref(a)** | `{ value: x }` | Ref wrapper class | GC-managed pointer |
| **MutArray(a)** | JS array | `List<T>` | `std::vector<T>` |

### Metadata and Reflection

| Feature | JS | .NET (CIL) | C++ |
|---|---|---|---|
| **Default** | Not emitted | Not emitted | Not emitted |
| **`--reflect=always`** | `_type`/`_tag` fields + table | Static metadata table | Static metadata table |
| **`tag(x)`** | `x._tag` | `x.Tag` | `x.tag` |
| **`tagName(x)`** | Table lookup | Table lookup | Table lookup |

---

## 13. Implementation Phases

### Phase A: Shared Infrastructure
1. New CLM nodes: `CLMPrimOp`, `CLMDictAccess`, `CLMBoundaryWrap`
2. `CodegenConfig` type in `src/Codegen.hs`
3. Target-qualified repr extension to reprMap
4. Dispatch resolution pass (Pass 5) — static resolution (monomorphic calls)
5. Name mangling utilities
6. Target selection in compiler flags / REPL command
7. Tail call detection (self-recursion → loop transform)
8. Cross-target project configuration

### Phase B: JS Backend (MVP)
1. JS emitter — sum types, records, functions, literals, pattern matching (repr-aware)
2. Class emission — 1-1 ES6 classes, including extern extends
3. Algebra resolution — monomorphic instances inlined via repr-guided prim ops
4. Module output — ES modules with import/export
5. TypeScript `.d.ts` generation
6. Effect compilation — hybrid (explicit custom, implicit IO)
7. Partial application — inline closures
8. Line comment debug info

### Phase C: .NET Backend (MVP)
1. CIL emitter via Mono.Cecil — sum types as class hierarchies, records
2. Generic emission — reified type parameters
3. Interface emission — algebras/morphisms → CIL interfaces
4. Class emission — 1-1 CIL classes, including extern extends
5. `implements` bridge — classes implement algebra interfaces
6. `maps` bridge — algebra ↔ .NET interface equivalence
7. Boundary wrappers — null→Maybe, exception→Either
8. Assembly packaging

### Phase D: Polymorphic Dispatch (All Targets)
1. Dictionary passing — polymorphic algebra/morphism calls
2. Superclass embedding — `extends` → embedded superclass dict
3. `requires` threading — separate dict parameters
4. HKT monomorphization — specialize Functor/Monad calls
5. Configurable dispatch strategy flags
6. Link-time cross-module optimization

### Phase E: Native/C++ Backend (MVP)
1. Closure conversion pass (Pass 6N.1)
2. Monomorphization pass (Pass 6N.2) — default dispatch for native
3. C++ emitter — sum types as tagged unions, records as structs
4. Class emission — 1-1 C++ classes with virtual dispatch
5. V1 memory: `shared_ptr` reference counting
6. Vtable generation for non-monomorphized algebras
7. CMake project scaffolding

### Phase F: Advanced Features
1. Custom GC design + implementation (separate project: `doc/GCDesign.md`)
2. SIMD emission — Vec/Lane → SSE/AVX intrinsics (C++ backend)
3. Extern metadata loading — `.d.ts` parser, Mono.Cecil integration, libclang
4. Async effect compilation — JS Promise, .NET Task, C++ coroutines
5. Source maps (JS), PDB (.NET), DWARF (C++) debug info
6. Decision tree pattern matching optimization
7. Mutual tail recursion → trampoline
8. LLVM backend for native (alternative to C++ emission)

### Phase G: Ecosystem
1. Pre-compiled stdlib packages (npm, NuGet, static lib)
2. Build system integration (tulam.toml project file)
3. Cross-target project support (shared + target-specific modules)
4. Tree shaking / dead code elimination across modules
5. Bundling (JS single-file output option)

---

## 14. Open Questions (Remaining)

1. **Custom GC design** for native backend — needs its own design document. Key: functional workload optimization, extern code integration, deterministic finalization.

2. **Async effect model** — deferred. When designed, must map naturally to JS Promise, .NET Task, C++20 coroutines.

3. **Incremental metadata loading** — for large .NET frameworks (all of `System.*`), loading all metadata eagerly is expensive. Lazy/on-demand resolution?

4. **String encoding consistency** — repr maps strings to target-native encoding (UTF-16 on JS/.NET, UTF-8 on C++). Should the StringLike algebra guarantee consistent semantics across targets, or accept target-specific behavior?

5. **Unsafe escape hatch** — should there be an `unsafe` block for direct pointer manipulation, unchecked casts? Essential for native, questionable for JS/.NET.

---

## 15. Relation to Other Design Documents

| Document | Relationship |
|---|---|
| `InteropDesign.md` | Import/extern/boundary system (Sections 7.11, 8.11, 9.5 implement it) |
| `ClassDesign.md` | Class 1-1 mapping (Sections 7.5, 8.4, 9.5 follow it) |
| `CategoricalDesign.md` | Algebra/morphism compilation (Section 4.4) implements the vocabulary |
| `PrimitiveDesign.md` | Repr system and SIMD (Sections 5, 9.12) follow it |
| `EffectDesign.md` | Effect compilation (Section 10) implements the effect system |
| `RecordDesign.md` | Record emission follows single-constructor desugaring |
| `ImplementationPlan.md` | Codegen phases (Section 13) extend Phase 14 |
| `GCDesign.md` | To be written — native memory management |
