# Record System Design for tulam

## Core Insight: Functions Are Values

In tulam, everything is tuples + lambdas. A lambda is a value. Therefore a record with function fields is just a record — no special mechanism needed. This unifies three concepts that other languages treat separately:

| Concept | Other languages | tulam |
|---------|----------------|-------|
| Data record | `struct`, `data class` | `type` with value fields (implicit constructor) |
| Typeclass dictionary | `interface`, `trait` | `structure`/`algebra` (record parameterized over types, with laws) |
| Object | `class` | `type` with function fields (explicit `self` parameter), or `class` |

All three are products with named projections. The keyword tells you what *kind* of product and what extra semantics the compiler should track.

---

## 1. Named Records (Nominal)

### Declaration

```tulam
type Point = x:Nat * y:Nat;
```

> **Note**: The `record` keyword has been removed. Records are now declared with `type` using implicit constructors. When the right-hand side starts with a lowercase field name (not an uppercase constructor), the constructor inherits the type name. Fields are separated by `*`.

This creates:
- A nominal type `Point`
- A constructor `Point` that takes named fields
- Projection functions `x` and `y` (or field access via `.x`, `.y`)

### Parameterized records

```tulam
type Pair(a:Type, b:Type) = fst:a * snd:b;
type Tagged(a:Type) = tag:String * value:a;
```

### Construction syntax

```tulam
let p = Point { x = Z, y = Succ(Z) };
// or positional (when unambiguous):
let p = Point(Z, Succ(Z));
```

Named construction `Point { x = Z, y = Succ(Z) }` is preferred — it's self-documenting and order-independent. Positional construction `Point(Z, Succ(Z))` works as sugar when the record has a single constructor (which all records do).

### Field access

```tulam
p.x       // Succ(Z)
p.fst     // for Pair
```

### Compilation

A record desugars to a single-constructor sum type:

```tulam
type Point = x:Nat * y:Nat;
// compiles identically to:
type Point = Point * x:Nat * y:Nat;
```

Records go through the existing pipeline — constructor extraction, field access via `CLMFieldAccess`, pattern matching. No new CLM nodes needed.

---

## 2. Functions as Fields

Since functions are first-class values, any record can contain function fields:

```tulam
type Counter = count:Nat * step:Nat;

function increment(self:Counter) : Counter =
    Counter { count = plus(self.count, self.step), step = self.step };

function reset(self:Counter) : Counter =
    Counter { count = Z, step = self.step };
```

### Why explicit `self` (not implicit `this`)

tulam is a functional language. Methods are just functions that happen to take a record as an argument. There is no hidden state, no implicit `this`, no mutation. This keeps the language honest:

```tulam
// These are equivalent:
increment(myCounter)       // function call style
myCounter.increment        // if we add method syntax later, it's sugar for the above
```

### Functions stored inside the record

Functions can also be fields of the record itself, creating "object-like" records:

```tulam
type Widget = label:String * render:(Widget -> String) * onClick:(Widget -> Widget);

// Different widgets can have different render/onClick behavior:
let button = Widget {
    label = "OK";
    render(self) = concat#("<button>", self.label, "</button>");
    onClick(self) = self { label = "Clicked!" }  // record update
};
```

This is dynamic dispatch — the function is stored in the record, not resolved at compile time. Useful when you want runtime polymorphism. For compile-time dispatch, use structures/algebras instead.

---

## 3. Record Extension (Spread)

### In declarations

```tulam
type Point = x:Nat * y:Nat;
type Point3D = ..Point * z:Nat;
// expands to: x:Nat * y:Nat * z:Nat
```

`..Name` spreads all fields of the named type. Fields can be overridden:

```tulam
type FloatPoint = ..Point * x:Float * y:Float;
// fields x and y overridden from Nat to Float
```

### Multiple spread

```tulam
type HasName = name:String;
type HasAge  = age:Nat;
type Person  = ..HasName * ..HasAge * email:String;
// expands to: name:String * age:Nat * email:String
```

Field conflicts (same name, different type from two spreads) are a compile-time error.

### Compilation

Spread is resolved at parse time. `type Point3D = ..Point * z:Nat;` looks up Point's fields and expands to a flat record. The result is a standard single-constructor sum type. No runtime overhead.

---

## 4. Record Update Syntax

```tulam
let p = Point { x = Z, y = Succ(Z) };
let p2 = p { x = Succ(Z) };
// p2 = Point { x = Succ(Z), y = Succ(Z) }
```

`p { field = newValue }` creates a **new** record with the specified fields changed and all others copied. This is purely functional — `p` is unchanged.

### Compilation

Record update desugars to a new constructor call with field access for unchanged fields:

```tulam
p { x = Succ(Z) }
// desugars to:
Point(Succ(Z), p.y)
```

---

## 5. Anonymous Records (Structural Typing) — Future

### The concept

Anonymous records don't need a named type declaration. The type IS the record:

```tulam
function origin() : {x:Nat, y:Nat} = {x = Z, y = Z};
```

### Row polymorphism

The `..` in type position is a **row variable** — it captures "whatever other fields exist":

```tulam
// Works on ANY record with at least field "name" of type String
function greet(thing : {name:String, ..}) : String =
    concat#("Hello, ", thing.name);

// All of these work:
greet({name = "World"});
greet({name = "Alice", age = Succ(Z)});
greet(Person {name = "Bob", email = "bob@example.com"});
```

`{name:String, ..}` means "has at least `name:String`, may have more fields." The `..` is the row variable.

### Row polymorphism in function types

```tulam
// Extract a field from any record that has it
function getX(p : {x:a, ..}) : a = p.x;

// Transform a field in place
function mapX(f:a -> b, p:{x:a, ..r}) : {x:b, ..r} = p { x = f(p.x) };
```

In `mapX`, the row variable `r` is named — the output record has the same "rest" fields as the input, but `x` changes type from `a` to `b`.

### Compatibility between named and anonymous records

A named record `Point` with fields `{x:Nat, y:Nat}` is compatible with the anonymous type `{x:Nat, y:Nat, ..}`:

```tulam
type Point = x:Nat * y:Nat;
let p = Point { x = Z, y = Z };
greet_x(p);  // works — Point has field x
```

Named records are subtypes of anonymous records with matching fields. This bridges the nominal and structural worlds.

### Why defer structural typing

Row polymorphism requires:
1. Row type variables in the type representation
2. Row unification in the type checker (unifying `{x:a, ..r}` with `{x:Nat, y:Bool}` yields `a=Nat, r={y:Bool}`)
3. Evidence passing for field access (the compiler must know the offset of `x` in an arbitrary record)

None of this exists yet. Named records compile to known-layout constructors and work today. Structural records need the type checker. However, the Package A syntax migration has eliminated the parsing ambiguity that previously made anonymous record types problematic — since `{ }` is now exclusively used for records/declarations, `{x:Nat, y:Nat}` in type position is unambiguously an anonymous record type.

---

## 6. The `..` Syntax — Unified Design

One symbol, multiple related uses:

| Context | Syntax | Meaning |
|---------|--------|---------|
| Record definition | `{ ..Point, z:Nat }` | Spread: include Point's fields |
| Record value | `p { x = 5 }` | Update: copy p, change x |
| Type position | `{x:a, ..}` | Row variable: at least x, maybe more |
| Named row variable | `{x:a, ..r}` | Named row: binds remaining fields to r |

All uses relate to "the rest of the fields" — spread includes them, update preserves them, row variables abstract over them.

---

## 7. Relationship to Structures

Structures and records are the same underlying concept at different levels:

```tulam
// A record: product of VALUES with named fields
type Point = x:Nat * y:Nat;

// A structure: product of FUNCTIONS parameterized over types, with laws
algebra Monoid(a:Type) = {
    empty : a;                        // can be a value field!
    combine(x:a, y:a) : a;
    law assoc(x:a,y:a,z:a) = combine(x,combine(y,z)) === combine(combine(x,y),z)
};

// An instance: a record VALUE satisfying the structure
instance Monoid(Nat) = {
    empty = Z;
    combine(x:Nat, y:Nat) : Nat = plus(x, y)
};
```

Note that `empty` in Monoid is a value field, not a function — structures can mix values and functions. An instance dictionary is literally a record whose fields are the implementations.

This unification means:
- Records, structures, and instances all share the same `{ field:Type, ... }` declaration syntax
- `{ }` is exclusively used for records and declarations — sum types use `|` syntax and pattern matching uses `match |`, so there is no ambiguity
- The compiler for structures already handles named fields, construction, field access
- Row polymorphism on records will eventually enable structural constraints too

---

## 8. Pattern Matching on Records

Records support pattern matching just like sum types:

```tulam
type Point = x:Nat * y:Nat;

function isOrigin(p:Point) : Bool =
    match
    | Point(Z, Z) -> True
    | p -> False;

// Or with named fields:
function isOrigin(p:Point) : Bool =
    match
    | Point { x = Z, y = Z } -> True
    | _ -> False;
```

Since records desugar to single-constructor sum types, positional pattern matching already works through the existing CaseOf/ExpandedCase/CLMCASE pipeline. Named field patterns are sugar for positional patterns in field-declaration order.

---

## 9. Summary

### Syntax disambiguation (Package A migration)

The Package A syntax migration resolves all previous `{ }` ambiguity concerns:

| Construct | Old syntax | New syntax |
|-----------|-----------|------------|
| Sum types | `type Bool = { True, False }` | `type Bool = True + False;` |
| Sum types (intermediate) | `type Bool = True \| False;` | `type Bool = True + False;` |
| Constructor fields | `Just(val:a)` | `Just * val:a` |
| Records | `record Point = { x:Int, y:Int };` | `type Point = x:Int * y:Int;` |
| Pattern matching | `{ {pat} -> expr, ... }` | `match \| pat -> expr \| ...` |
| Records/declarations | `{ field:Type, ... }` | `{ field:Type, ... }` (for NTuples, declaration blocks) |

Sum types use `+` to separate constructors and `*` to introduce named fields. The `record` keyword has been removed — use `type` with implicit constructors instead. `{ }` is used for NTuples, declaration blocks (structure/instance/class bodies), and where-clauses. Pattern matching uses the `match` keyword with `|`-separated arms. `match` is a reserved keyword.

### Feature roadmap

| Feature | Phase | Depends on |
|---------|-------|------------|
| Named records (nominal) | Phase 2 (next) | Nothing — desugars to sum types |
| Parameterized records | Phase 2 (next) | Nothing — same as parameterized sum types |
| Functions as fields | Phase 2 (next) | Nothing — functions are values |
| Record spread in declarations (`..Name`) | Phase 2 (next) | Named records |
| Record construction `Name { f = v }` | Phase 2 (next) | Named records |
| Record update `p { f = v }` | Phase 3 | Named records, field access |
| Anonymous record types `{x:a, y:b}` | Phase 6+ | Type checker (no disambiguation issue — `{ }` is exclusively records) |
| Row polymorphism `{x:a, ..}` | Phase 7+ | Type checker, anonymous records |
| Named row variables `{x:a, ..r}` | Phase 7+ | Row polymorphism |
| Method syntax `p.method(args)` | TBD | Type checker (for resolution) |
