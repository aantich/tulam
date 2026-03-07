# Interop Design: Native .NET, JavaScript, and C/C++ Support

## 1. Design Philosophy

tulam targets three backends — .NET (CLR), JavaScript, and native (x86 via C/C++) — with a single guiding principle:

**The user writes tulam. The compiler handles the rest.**

There is no FFI layer, no binding generators, no marshaling boilerplate. Interop with target platform libraries should feel as natural as using tulam's own types. The compiler has access to target metadata (assemblies, `.d.ts` files, C headers) and uses it to resolve types, methods, properties, and inheritance automatically.

### Core Tenets

1. **Zero-ceremony import**: `import System.Windows.Forms target dotnet;` — then just use the types.
2. **The compiler does the work**: method signatures, overload resolution, inheritance chains, generics — all resolved from target metadata at compile time.
3. **Tulam types at boundaries**: null becomes `Maybe`, exceptions become `Either`, callbacks become lambdas. The tulam programmer works with tulam types; the compiler translates at call boundaries.
4. **Explicit declarations only for edge cases**: override type mappings, disambiguate overloads, or declare untyped JS libraries.
5. **Algebras bridge the worlds**: .NET interfaces and JS protocols map to tulam algebras. Pure tulam code works with algebras; interop code works with extern types. Both coexist.

---

## 2. Import System

### 2.1 Target Imports

The primary mechanism for accessing external libraries:

```
import System.Collections.Generic target dotnet;
import System.Linq target dotnet;
import react target js;
import stdio target native;
```

**Semantics**: The compiler loads metadata for the specified namespace/module from the target platform's metadata source:

| Target | Metadata Source | What Gets Loaded |
|--------|----------------|-----------------|
| `dotnet` | Assembly metadata (Mono.Cecil / System.Reflection) | Classes, interfaces, structs, enums, methods, properties, events, generics, inheritance, attributes |
| `js` | TypeScript `.d.ts` declaration files, `package.json` `types` field | Exports, function signatures, class shapes, type aliases, generics |
| `native` | C/C++ header files (`.h`, `.hpp`), parsed via libclang or similar | Structs, functions, typedefs, enums, macros (as constants), `extern "C"` signatures |

### 2.2 Selective Imports

Import specific types to avoid namespace pollution:

```
import { List, Dictionary, HashSet } from System.Collections.Generic target dotnet;
import { useState, useEffect } from react target js;
import { printf, scanf } from stdio target native;
```

### 2.3 Aliased Imports

Rename to avoid conflicts with tulam types or for convenience:

```
import { List as DotNetList } from System.Collections.Generic target dotnet;
import System.Windows.Forms as WinForms target dotnet;
```

### 2.4 Target Metadata Resolver

A new compiler component, sitting before Pass 0 in the pipeline:

```
Source (.tl) → Target Metadata Resolver → Lexer/Parser → Surface AST → ...
```

The resolver:
1. Reads all `import ... target` declarations
2. Loads metadata from the appropriate source
3. Builds an **extern environment** — a map of fully-qualified type names to their shape (methods, properties, constructors, supertypes, generic parameters)
4. Makes this environment available to all subsequent passes (type checker, CLM conversion, codegen)

The extern environment is structurally identical to what manual `extern` declarations would produce — the resolver just automates population.

---

## 3. Extern Declarations (Power-User / Edge Cases)

Most users never write `extern`. But when needed:

### 3.1 Basic Extern Type

Declares that a type exists on the target platform:

```
extern class System.Windows.Forms.Button target dotnet;
```

The compiler still resolves all methods/properties from metadata. This is useful when you need to reference a type before importing its full namespace.

### 3.2 Aliased Extern

Short name for a fully-qualified type:

```
extern class Button = System.Windows.Forms.Button target dotnet;
```

### 3.3 Extern with Overrides

Override how the compiler sees specific methods — primarily for tulam-ifying the API:

```
extern class Dictionary(K:Type, V:Type) =
    System.Collections.Generic.Dictionary target dotnet {
    // Override: Get returns Maybe instead of throwing KeyNotFoundException
    method Get(key:K) : Maybe(V),
    // Override: indexer also returns Maybe
    method get(index:K) : Maybe(V)
};
```

Only the listed methods are overridden; everything else is still resolved from metadata.

### 3.4 Extern for Untyped JS

When no `.d.ts` is available, the user provides the shape:

```
extern type lodash target js {
    method chunk(arr:List(a), size:Int) : List(List(a)),
    method flatten(arr:List(List(a))) : List(a),
    method uniq(arr:List(a)) : List(a)
};
```

### 3.5 Extern for C Structs and Functions

```
extern struct Vec3 target native {
    field x : Float64,
    field y : Float64,
    field z : Float64
};

extern function printf(fmt:String, args:...) : Int target native;
```

### 3.6 Extern Syntax Summary

```
extern class <Name> [= <QualifiedName>] target <target> [extends <Supertype>] [{ <overrides> }];
extern type <Name> [= <QualifiedName>] target <target> [{ <members> }];
extern struct <Name> [= <QualifiedName>] target <target> [{ <fields> }];
extern function <Name>(<params>) : <ReturnType> target <target>;
```

**Keywords inside extern bodies:**
- `method <name>(<params>) : <ReturnType>` — instance method
- `static <name>(<params>) : <ReturnType>` — static method
- `property <name> : <Type>` — readable property
- `property <name> : <Type> [get, set]` — readable and writable property
- `field <name> : <Type>` — struct field (native target)
- `new(<params>)` — constructor
- `event <name> : <HandlerType>` — event (.NET) / event emitter (JS)

---

## 4. Subtyping

### 4.1 The Problem

.NET is built on class inheritance. JS uses prototype chains. C++ has class hierarchies. tulam's current type system is parametrically polymorphic with typeclasses (algebras) — there is no notion of "type A is a subtype of type B."

We need subtyping for extern types so that, e.g., a `Button` can be passed where a `Control` is expected.

### 4.2 Dual Subtyping Model

tulam uses two complementary subtyping mechanisms:

#### Nominal Subtyping (for extern types)

Established by the `extends` chain in the target metadata. The compiler reads this automatically from assemblies / `.d.ts` / headers.

```
// The compiler knows from .NET metadata:
//   Button extends Control extends ScrollableControl extends Component extends Object
// The user writes:
import System.Windows.Forms target dotnet;

function showControl(c:Control) : Unit = c.Show();
let b = Button.new("Click me");
showControl(b);  // OK: Button <: Control, known from metadata
```

When the type checker sees `Button` where `Control` is expected, it walks the extern environment's inheritance chain. If a path exists, an implicit upcast is inserted.

**Rules:**
- Upcasts (Button → Control) are implicit and free (zero runtime cost on .NET, just a reference)
- Downcasts (Control → Button) are explicit and checked: `expr as Button` returns `Maybe(Button)`
- No implicit downcasts — this prevents ClassCastException at compile time

#### Structural Subtyping (for tulam-native types and JS interop)

Uses the existing row polymorphism system. A value with "more" fields/methods can be passed where "fewer" are expected:

```
// Function accepting anything with an innerHTML property
function setContent(el:{innerHTML:String, ..}, content:String) : Unit =
    el.innerHTML = content;

// HTMLElement satisfies this structurally — no explicit declaration needed
```

This is the natural model for JS (duck typing) and for tulam's own record types.

#### When Each Applies

| Context | Subtyping Model | Mechanism |
|---------|----------------|-----------|
| Extern .NET classes/interfaces | Nominal | Inheritance chain from assembly metadata |
| Extern C++ classes | Nominal | Inheritance from headers |
| Extern JS classes (with `.d.ts`) | Nominal | `extends` from type declarations |
| JS duck typing / untyped | Structural | Row polymorphism |
| tulam records | Structural | Row polymorphism |
| tulam sum types | None (distinct types) | Use algebras for polymorphism |

### 4.3 Implicit Coercion Rules

When the type checker encounters a type mismatch where the actual type is a subtype of the expected type:

1. **Direct subtype**: `Button` where `Control` expected → insert upcast (zero-cost reference cast on .NET, no-op on JS)
2. **Through interface**: `List(T)` where `IEnumerable(T)` expected → insert interface coercion
3. **No transitive coercion through unrelated chains**: if `A <: B` and `C <: B`, `A` is NOT coercible to `C`
4. **Coercion depth limit**: the compiler walks at most N levels (configurable, default 10) to prevent pathological cases

### 4.4 Variance

Once generic extern types interact with subtyping, variance matters.

**Declaration-site variance** (like Kotlin/Scala):

```
// In .NET metadata, IEnumerable<out T> is covariant
// The compiler reads this and knows:
//   IEnumerable(Button) <: IEnumerable(Control)  -- covariant, OK
//   Action(Control) <: Action(Button)             -- contravariant, OK
//   List(Button) is NOT List(Control)             -- invariant, blocked
```

For tulam-native types, variance is declared explicitly when needed:

```
type Producer(+a:Type) = ...;   // covariant: Producer(Button) <: Producer(Control)
type Consumer(-a:Type) = ...;   // contravariant: Consumer(Control) <: Consumer(Button)
type Mutable(a:Type) = ...;     // invariant (default): no subtyping
```

The `+` and `-` annotations on type parameters follow Scala/Kotlin convention. Default is invariant (safe).

### 4.5 Downcast Syntax

Upcasts are implicit. Downcasts use `as` (which we already have for `repr`):

```
function getButton(c:Control) : Maybe(Button) = c as Button;
```

The `as` keyword is already reserved. For extern types, the compiler generates the appropriate runtime check:
- .NET: `obj is Button b ? Just(b) : Nothing` (C# equivalent)
- JS: `obj instanceof Button ? Just(obj) : Nothing`
- Native: static cast with RTTI check (C++) or disallowed (C)

This reuses our existing `as` / `ReprCast` infrastructure with an extended resolution strategy:
1. Check repr map (existing behavior)
2. Check extern subtype chain (new: downcast)
3. Error if neither applies

---

## 5. Method Call Syntax (Dot Notation)

### 5.1 Uniform Dot Syntax

Extern type methods use dot notation:

```
let b = Button.new("OK");
b.Text = "Cancel";
b.Show();
let w = b.Width;
```

This extends our existing `RecFieldAccess` to also resolve methods, not just record fields.

### 5.2 Resolution Order

When the compiler sees `expr.name(args)`:

1. **Record field access**: if `expr`'s type is a record with field `name`, access it
2. **Extern method/property**: if `expr`'s type is an extern type, look up `name` in the extern environment (including inherited methods from supertypes)
3. **Extension method** (future): look up `name` as a function whose first parameter matches `expr`'s type
4. **Error**: no such member

### 5.3 Static Methods and Constructors

```
let b = Button.new("text");          // constructor
let e = List.Empty;                  // static property (.NET)
let s = String.Format("{0}", val);   // static method
```

Static access uses the type name directly (not an instance). The compiler distinguishes instance vs static from metadata.

### 5.4 Property Access and Mutation

Properties desugar to getter/setter calls:

```
let t = b.Text;       // desugars to: b.get_Text()
b.Text = "hello";     // desugars to: b.set_Text("hello")
```

Mutation syntax (`b.Text = "hello"`) is only valid for:
- Extern type properties marked as settable
- Mutable record fields (when we add mutability)

This does NOT introduce general mutation into tulam — it's a controlled interop surface.

---

## 6. Type Mapping at Boundaries

### 6.1 Automatic Type Translations

The compiler automatically translates between tulam types and target types at interop boundaries:

| tulam Type | .NET Type | JS Type | C/C++ Type |
|-----------|-----------|---------|------------|
| `Int` | `System.Int64` or `System.Int32` (configurable) | `number` | `int64_t` / `int32_t` |
| `Float64` | `System.Double` | `number` | `double` |
| `String` | `System.String` | `string` | `const char*` (or `std::string`) |
| `Char` | `System.Char` | `string` (length 1) | `char` |
| `Bool` | `System.Boolean` | `boolean` | `bool` |
| `Unit` | `void` | `undefined` | `void` |
| `Maybe(T)` | `Nullable<T>` / `T?` | `T \| null \| undefined` | `std::optional<T>` / `T*` |
| `Either(E, T)` | thrown exception (E) / return (T) | thrown / return | error code / return |
| `List(T)` | `IEnumerable<T>` (lazy) or `List<T>` (strict) | `Array<T>` | `std::vector<T>` |
| `(A, B)` (tuple) | `System.ValueTuple<A,B>` | `[A, B]` | `std::tuple<A,B>` |
| `A -> B` (function) | `Func<A,B>` / `Action<A>` | `(a: A) => B` | `std::function<B(A)>` |
| tulam sum type | .NET class hierarchy (see 6.3) | tagged object | tagged union / `std::variant` |
| tulam record | .NET class with properties | JS object | C struct |

### 6.2 Null Handling

**Principle**: null does not exist in tulam. At every interop boundary, the compiler wraps/unwraps:

**Calling .NET from tulam:**
```
// .NET: public string GetName() — may return null
// tulam sees: method GetName() : Maybe(String)
let name = obj.GetName();  // name : Maybe(String)
match name
    | Just(n) -> use(n)
    | Nothing -> handleMissing();
```

**Calling tulam from .NET:**
```
// tulam: function find(x:Int) : Maybe(String)
// .NET sees: public string? Find(int x)
// Nothing → null, Just(v) → v
```

**Configuration**: The compiler can be configured for nullable-awareness level:
- **Strict (default)**: all reference-type returns from extern methods are `Maybe(T)`. Safe but verbose.
- **Annotated**: only `.NET` nullable-annotated (`T?`) returns become `Maybe(T)`; non-nullable (`T`) stay as `T`. Requires .NET 8+ nullable reference type annotations.
- **Unsafe**: no wrapping. Null values can crash at runtime. For performance-critical code.

```
// In project config or per-import
import System.Collections.Generic target dotnet [nullable = annotated];
```

### 6.3 Exception Handling

**Principle**: exceptions do not exist in tulam. They become `Either` at boundaries.

**Calling .NET from tulam:**
```
// .NET: public int Parse(string s) — throws FormatException
// tulam: two options depending on user preference

// Option A: auto-wrapping (safe, default)
let result = Int.Parse("42");  // result : Either(Exception, Int)

// Option B: explicit try
let result = try Int.Parse("42");  // result : Either(Exception, Int)

// Option C: asserting (user takes responsibility)
let result = Int.Parse!("42");  // result : Int — crashes on exception
```

**Calling tulam from .NET:**
```
// tulam: function safeDivide(x:Int, y:Int) : Either(String, Int)
// .NET sees: public int SafeDivide(int x, int y)  — throws Exception on Left
```

The exact syntax for try/`!` is open for refinement, but the principle is: the user chooses their error-handling style, and the compiler translates.

### 6.4 Callback and Lambda Interop

tulam functions are first-class. Target platforms have their own callable types:

```
// .NET event handler
import System.Windows.Forms target dotnet;
let b = Button.new("Click");
b.Click.Add(\sender, args -> print#("Clicked!"));
// tulam lambda → System.EventHandler delegate, automatic

// JS callback
import { setTimeout } from globals target js;
setTimeout(\() -> print#("done"), 1000);
// tulam lambda → JS arrow function, automatic

// C function pointer
extern function qsort(base:Ptr(a), n:Int, size:Int, cmp: (Ptr(a), Ptr(a)) -> Int) : Unit target native;
qsort(arr, len, 8, \a, b -> compare(deref(a), deref(b)));
// tulam lambda → C function pointer (requires closure conversion or static context)
```

---

## 7. Algebras as the Bridge

### 7.1 .NET Interfaces Map to Algebras

A .NET interface is a behavioral contract — exactly what an algebra is:

```
// System.IComparable<T> in .NET becomes:
// (auto-generated by the compiler from metadata)
algebra IComparable(a:Type) = {
    function CompareTo(x:a, other:a) : Int
};

// Any .NET class implementing IComparable<T> automatically gets an instance
// instance IComparable(System.String) — from metadata
```

This means tulam code can work with .NET interfaces polymorphically:

```
function sortBy [T] (items:List(T)) : List(T) requires IComparable(T) = ...;
// Works with any .NET type that implements IComparable<T>
```

### 7.2 Two-Way Algebra Mapping

| Direction | Mechanism |
|-----------|-----------|
| .NET interface → tulam algebra | Auto-generated from metadata. Instances auto-discovered. |
| tulam algebra → .NET interface | When compiling to .NET, algebras become interfaces. Instances become `class : IAlgebra` implementations. |
| JS protocol → tulam algebra | From `.d.ts` interfaces. Structural matching. |
| tulam algebra → JS | Algebras become objects with method properties (duck typing). |
| C "interface" (vtable) → tulam algebra | From header patterns. Manual declaration likely needed. |
| tulam algebra → C | Algebras become vtable structs with function pointers. |

### 7.3 Mapping Existing tulam Algebras to Platform Types

The user can declare equivalences:

```
// Tell the compiler: tulam's Ord is equivalent to .NET's IComparable
algebra Ord(a:Type) maps IComparable(a) target dotnet;

// Now any .NET type with IComparable automatically gets an Ord instance
// And any tulam type with Ord automatically implements IComparable on .NET
```

This is the power move — it means the tulam standard library (Eq, Ord, Functor, Monad, etc.) seamlessly maps to platform conventions, and platform types seamlessly participate in tulam abstractions.

---

## 8. Mutability and Reference Semantics

### 8.1 The Problem

Extern types are often mutable. A .NET `List<T>` has `.Add()`, `.Remove()`, etc. A JS object's properties can be reassigned. tulam is functional — it has no mutation.

### 8.2 Approach: Controlled Mutability at the Interop Surface

Mutation is **only** allowed on extern types, and **only** through the target's own methods/properties:

```
let list = System.Collections.Generic.List.new();
list.Add(1);       // OK: mutation via extern method
list.Add(2);       // OK: mutation via extern method
let count = list.Count;  // OK: read property
```

This is NOT general-purpose mutation. You cannot write:

```
let x = 5;
x = 6;          // ERROR: tulam values are immutable
```

The rule is simple: **extern values follow extern rules. tulam values follow tulam rules.**

### 8.3 Action Blocks for Imperative Interop

When sequencing multiple mutations, use `action` blocks (already in the language):

```
action setupUI() = {
    let form = Form.new("My App");
    let btn = Button.new("Click me");
    btn.Location = Point.new(10, 10);
    btn.Size = Size.new(100, 30);
    form.Controls.Add(btn);
    btn.Click.Add(\s, e -> form.Text = "Clicked!");
    form.ShowDialog();
};
```

### 8.4 Mutable References (Future)

For tulam-native mutable state (rare, but needed for some algorithms):

```
// Possible future syntax — NOT part of initial implementation
let ref x = Ref.new(0);    // x : Ref(Int)
Ref.set(x, 5);             // mutation through Ref API
let v = Ref.get(x);        // v = 5
```

This is deferred. Extern mutability covers 95% of practical needs.

---

## 9. Generics Mapping

### 9.1 Per-Target Generic Strategy

| Target | Generics Strategy | Notes |
|--------|------------------|-------|
| .NET | Reified generics | `List(Int)` → `List<int>`. .NET preserves type info at runtime. Direct mapping. |
| JS | Type erasure | `List(Int)` → `Array`. No runtime type info. Generics are compile-time only. |
| Native | Monomorphization | `List(Int)` → `List_Int` (specialized code per type instantiation). Like C++ templates / Rust generics. |

### 9.2 Generic Extern Types

When importing a generic .NET type:

```
import System.Collections.Generic target dotnet;

let dict = Dictionary.new();  // Dictionary(String, Int) inferred from usage
dict.Add("x", 1);
dict.Add("y", 2);
let val = dict.Get("x");     // val : Maybe(Int)
```

The compiler instantiates the generic with tulam types, mapping them through the type translation table (Section 6.1).

### 9.3 Higher-Kinded Type Interop

tulam supports HKT (`Functor(f:Type1)`). Target platforms generally do not. The mapping:

- **To .NET**: HKT algebras compile to generic interfaces with extra type parameters. `Functor(List)` becomes an interface with methods parameterized over the element type.
- **To JS**: HKT algebras compile to objects with generic method properties. Type safety is compile-time only.
- **To Native**: Monomorphized. Each HKT instantiation generates specialized code.

---

## 10. Target-Specific Code

### 10.1 Target Blocks

When you need different implementations per target:

```
function fastHash(s:String) : Int = target
    | dotnet -> s.GetHashCode()
    | js     -> hashCode_js(s)
    | native -> hashCode_murmur3(s);
```

This is pattern-match-like syntax on the compilation target. Only one branch is compiled.

### 10.2 Target-Conditional Declarations

```
target dotnet {
    instance Convertible(List(a), System.Collections.Generic.List(a)) = {
        function convert(xs:List(a)) : System.Collections.Generic.List(a) =
            let result = System.Collections.Generic.List.new() in
            forEach(\x -> result.Add(x), xs);
    };
};
```

### 10.3 Platform Modules

By convention, target-specific code lives in separate files:

```
src/
  core/           -- pure tulam, platform-independent
    base.tl
    prelude.tl
  dotnet/         -- .NET-specific
    collections.tl
    forms.tl
  js/             -- JS-specific
    dom.tl
    react.tl
  native/         -- C/C++-specific
    memory.tl
    simd.tl
```

The build system selects the appropriate platform modules based on the compilation target.

---

## 11. Compilation Strategy per Target

### 11.1 .NET (CLR)

```
tulam sum type    → .NET abstract base class + subclass per constructor
tulam record      → .NET class with readonly properties
tulam algebra     → .NET interface
tulam instance    → .NET class implementing interface (+ static dispatch table)
tulam function    → .NET static method (or instance method if first param is 'self')
tulam lambda      → System.Func<> / System.Action<> delegate
tulam pattern match → if/else chain or switch on constructor tag
constructor tags  → integer discriminator field on base class
```

**Example:**
```
// tulam:
type Maybe(a:Type) = Nothing | Just(val:a);

// .NET output:
public abstract class Maybe<A> {
    public int Tag { get; }
}
public sealed class Nothing<A> : Maybe<A> {
    public Nothing() { Tag = 0; }
}
public sealed class Just<A> : Maybe<A> {
    public A Val { get; }
    public Just(A val) { Tag = 1; Val = val; }
}
```

### 11.2 JavaScript

```
tulam sum type    → tagged objects { _tag: 0, ... }
tulam record      → plain JS objects { field1: val1, ... }
tulam algebra     → not emitted (compile-time only, structural)
tulam instance    → dispatch table object (or inlined)
tulam function    → JS function / arrow function
tulam lambda      → JS arrow function
tulam pattern match → switch on _tag or if/else chain
```

**Example:**
```
// tulam:
type Maybe(a:Type) = Nothing | Just(val:a);

// JS output:
const Nothing = () => ({ _tag: 0 });
const Just = (val) => ({ _tag: 1, val });
```

### 11.3 Native (C/C++)

```
tulam sum type    → tagged union (struct with tag + union of payloads)
tulam record      → C struct
tulam algebra     → vtable struct (struct of function pointers)
tulam instance    → static vtable instance
tulam function    → C function
tulam lambda      → closure struct + function pointer (closure conversion)
tulam pattern match → switch on tag
```

**Example:**
```
// tulam:
type Maybe(a:Type) = Nothing | Just(val:a);

// C output:
typedef struct {
    int tag;
    union {
        struct {} nothing;
        struct { void* val; } just;  // or monomorphized concrete type
    };
} Maybe;
```

---

## 12. Worked Example: Full .NET Interop

A complete example showing how a tulam program uses .NET's Windows Forms:

```
// main.tl — a simple GUI application

import System.Windows.Forms target dotnet;
import System.Drawing target dotnet;

function createCounter() : Form = {
    let form = Form.new("Counter");
    form.Size = Size.new(300, 200);

    let label = Label.new();
    label.Text = "0";
    label.Location = Point.new(100, 50);
    label.AutoSize = True;
    form.Controls.Add(label);

    let count = Ref.new(0);

    let btn = Button.new();
    btn.Text = "Increment";
    btn.Location = Point.new(90, 100);
    btn.Click.Add(\sender, args -> {
        Ref.set(count, Ref.get(count) + 1);
        label.Text = toString(Ref.get(count));
    });
    form.Controls.Add(btn);

    form
};

action main() = {
    Application.EnableVisualStyles();
    Application.Run(createCounter());
};
```

**What the compiler does:**
1. `import System.Windows.Forms target dotnet` → loads assembly metadata for all types in the namespace
2. `Form.new("Counter")` → resolves `Form` constructor taking `String`, emits `new Form("Counter")`
3. `form.Size = Size.new(300, 200)` → resolves `Size` property setter, `Size` constructor
4. `btn.Click.Add(\sender, args -> ...)` → resolves `Click` event, converts tulam lambda to `EventHandler` delegate
5. `Ref.get(count) + 1` → tulam arithmetic, emitted as .NET `int` addition
6. All null-returning methods are wrapped in `Maybe` (though `Form`, `Button`, `Label` constructors never return null)

---

## 13. Worked Example: Full JS Interop

```
// app.tl — a simple web app

import { document, console, setTimeout } from globals target js;
import { createElement, useState } from react target js;

function Counter() = {
    let (count, setCount) = useState(0);

    createElement("div", {},
        createElement("p", {}, "Count: " ++ toString(count)),
        createElement("button",
            { onClick = \e -> setCount(\c -> c + 1) },
            "Increment"
        )
    )
};

action main() = {
    let root = document.getElementById("root");
    ReactDOM.render(createElement(Counter, {}), root);
};
```

---

## 14. Worked Example: Full C Interop

```
// math_native.tl — SIMD vector math

import { malloc, free, memcpy } from stdlib target native;
import { printf } from stdio target native;

extern struct Vec4f target native {
    field x : Float64,
    field y : Float64,
    field z : Float64,
    field w : Float64
};

extern function simd_add_vec4f(a:Vec4f, b:Vec4f) : Vec4f target native;
extern function simd_dot_vec4f(a:Vec4f, b:Vec4f) : Float64 target native;

function normalize(v:Vec4f) : Vec4f = {
    let len = sqrt(simd_dot_vec4f(v, v));
    Vec4f.new(v.x / len, v.y / len, v.z / len, v.w / len)
};

action main() = {
    let a = Vec4f.new(1.0, 2.0, 3.0, 0.0);
    let b = Vec4f.new(4.0, 5.0, 6.0, 0.0);
    let c = simd_add_vec4f(a, b);
    printf("Result: (%f, %f, %f)\n", c.x, c.y, c.z);
};
```

---

## 15. Implementation Phases

### Phase 1: Foundation
- New AST nodes: `Import`, `Extern`, `MethodCall`, `PropertyAccess`, `StaticAccess`
- Target metadata resolver stub (hardcoded extern environments for testing)
- Dot notation syntax in parser
- Extern environment in `InterpreterState`

### Phase 2: .NET Backend
- Assembly metadata reader (via Mono.Cecil or equivalent Haskell binding)
- Nominal subtype checker (walks extern extends chain)
- CLM → .NET IL code generation (or C# source emission as intermediate step)
- Null → Maybe wrapping at call boundaries
- Exception → Either wrapping
- Type mapping table (Section 6.1)

### Phase 3: JS Backend
- `.d.ts` parser (or leverage TypeScript compiler API)
- CLM → JS source emission
- Structural subtyping integration with row polymorphism
- Lambda → arrow function compilation
- Module system (ES modules, CommonJS)

### Phase 4: Native Backend
- C header parser (libclang bindings or simplified parser)
- CLM → C source emission (or direct x86 via LLVM)
- Closure conversion for lambdas passed as function pointers
- Monomorphization pass for generics
- Memory management strategy (reference counting, GC, or regions)

### Phase 5: Algebra Bridge
- Auto-generation of algebras from .NET interfaces
- `maps` declarations for algebra↔interface equivalence
- Instance auto-discovery from extern type metadata
- Two-way: tulam algebras emit as target interfaces/protocols

### Phase 6: Variance and Advanced Generics
- Declaration-site variance (`+`/`-` annotations)
- Variance checking in subtype judgments
- Generic type mapping per target (reified, erased, monomorphized)
- HKT compilation strategies

---

## 16. Open Questions

1. **Memory management for native target**: Reference counting (Swift/Rust-like), tracing GC (like Go/OCaml), or region-based (like Cyclone)? This significantly affects the native interop story.

2. **Async/await**: .NET has `Task<T>`, JS has `Promise<T>`. Should tulam have a native async construct, or model it purely through monads (`IO(a)`, `Async(a)`)?

3. **Overload resolution**: .NET has complex overload rules. When multiple methods match, how does the compiler choose? Use .NET's own rules? Or require disambiguation?

4. **Inheritance for tulam-defined types**: Should tulam types be able to `extend` extern classes? E.g., `type MyButton extends Button = { ... }`. This would let tulam types participate in .NET class hierarchies.

5. **Unsafe escape hatch**: Should there be an `unsafe` block for direct pointer manipulation, unchecked casts, and other low-level operations? Essential for native target, questionable for .NET/JS.

6. **Incremental metadata loading**: For large .NET frameworks (e.g., all of `System.*`), loading all metadata eagerly is expensive. Lazy/on-demand resolution?

7. **Cross-target compilation**: Can a single tulam program target multiple backends simultaneously? E.g., shared core logic compiled to both .NET and JS (for server + client).

---

## 17. Algebra-Based Interop Pattern

The general interop mechanism is formalized as a three-part pattern: **algebra** (contract) + **repr** (data bridge) + **target-qualified extern instances** (native method binding). This pattern applies to strings, collections, IO, date/time, concurrency, and any domain where native platforms have optimized implementations.

The string library (`lib/String/`) serves as the first complete worked example:
- `StringLike` algebra defines universal string operations
- `Str` type provides pure tulam reference implementation
- Target-specific instances (future) will map to native strings (.NET `System.String`, JS `string`, C++ `std::string`)
- `repr Str as System.String target dotnet` (future) bridges data between representations

See [doc/InteropPattern.md](InteropPattern.md) for the full design document describing this pattern in detail.

## 18. Relation to Existing Design Documents

| Document | Relationship |
|----------|-------------|
| `CategoricalDesign.md` | Algebras/morphisms become the bridge between tulam and target platform interfaces (Section 7) |
| `PrimitiveDesign.md` | `primitive` is the predecessor of `extern`. Intrinsics extend to extern method dispatch. SIMD/GPU plans feed into native target (Section 14) |
| `RecordDesign.md` | Records with row polymorphism enable structural subtyping for JS interop (Section 4.2) |
| `ImplementationPlan.md` | Interop phases should be integrated after the current Phase 8 (type checker completion) |
| `LanguageReference.md` | New keywords (`import`, `extern`, `target`) and syntax to be documented |
