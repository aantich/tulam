# Surface Language Completion — Full Dependent Type Theory

This document specifies the redesigned surface syntax for tulam's type system, unifying sum types, product types, dependent telescopes, existentials, and quantifiers under a clean algebraic syntax using `+` and `*`.

**Core design invariant**: tulam represents all products as **flat n-tuples** (`CLMCON tag [fields]`) with O(1) field access. There are no nested binary pairs anywhere — not at runtime, not in the surface language, not conceptually. Dependent products are **telescopes**: flat sequences of bindings where later types may depend on earlier values. The nested `TSigma` in the type checker is purely an internal encoding detail; it never surfaces to users and erases to flat `CLMCON` at CLM conversion.

---

## 1. Unified Type Definition Syntax

All data types are defined with `type`, using `+` for sums (coproducts) and `*` for products. The `record` keyword is removed.

### 1.1 Enums (Nullary Constructors)

```tulam
type Unit = Unit;
type Bool = True + False;
type Color = Red + Green + Blue;
```

### 1.2 Single-Variant Types (Records)

When a type body starts with a lowercase field name (no uppercase constructor), the constructor name defaults to the type name:

```tulam
type Point = x:Float64 * y:Float64;
// Constructor is implicitly named Point
// Construct: Point(3.0, 4.0) or Point.new(3.0, 4.0)

type Person = name:String * age:Int * active:Bool;
```

### 1.3 Sum Types with Fields

Each variant in a multi-variant sum MUST start with an uppercase constructor name:

```tulam
type Maybe(a:Type) = Nothing + Just * val:a;
type Either(a:Type, b:Type) = Left * val:a + Right * val:b;
type List(a:Type) = Nil + Cons * head:a * tail:List(a);

type Shape = Circle * center:Point * radius:Float64
           + Rect * topLeft:Point * bottomRight:Point;

type Expr = Lit * v:Int
          + Add * l:Expr * r:Expr
          + Neg * e:Expr;
```

### 1.4 Type Composition

Combining existing types with `+` (flat sum — inlines all variants):

```tulam
type Shape = Circle + Rect + Triangle;
// Equivalent to defining Shape with all constructors from Circle, Rect, Triangle

type NumericOrString = Int + Float64 + String;
```

Extending an existing type with `*` (field concatenation):

```tulam
type LabeledPoint = Point * label:String;
// Flat: fields of Point + the label field
// Error on field name collision
```

### 1.5 GADTs

Constructor-specific return types use `: ReturnType` at the end of a variant.

**Disambiguation rule**: `lowercase:Type` = named field. `: UppercaseName(...)` after all fields = GADT return type annotation.

```tulam
type Vec(a:Type, n:Nat) =
    VNil : Vec(a, Z)
  + VCons * head:a * tail:Vec(a, n) : Vec(a, Succ(n));

type TExpr(a:Type) =
    TLitI * n:Int : TExpr(Int)
  + TLitB * b:Bool : TExpr(Bool)
  + TAdd * x:TExpr(Int) * y:TExpr(Int) : TExpr(Int);

type PropEq(a:Type, b:Type) = Refl : PropEq(a, a);

type SafeOption(a:Type, present:Bool) =
    SomeVal * x:a : SafeOption(a, True)
  + NoVal : SafeOption(a, False);
```

### 1.6 Dependent Telescopes

Fields scope left-to-right — each field name is available to all fields to its right:

```tulam
type SizedVec(a:Type) = n:Nat * elems:Vec(a, n);

type Matrix(a:Type) = rows:Nat * cols:Nat * data:Vec(Vec(a, cols), rows);

type PositiveInt = x:Int * proof:IsPositive(x);

// Sum of dependent products
type ParseResult(a:Type) =
    Ok * val:a * rest:String
  + Err * pos:Nat * msg:String;
```

### 1.7 Function Types in Fields

Within a type definition, field types extend until the next bare (unparenthesized) `*`, `+`, or `;`. Arrow types inside fields need no parens. Products/sums inside a field type need parens:

```tulam
// Arrow in field — no parens needed
type Handler = name:String * f:Int -> Bool;
// f has type (Int -> Bool)

// Dependent Pi in field — no parens needed
type DepHandler = name:String * f:(n:Nat) -> Vec(Int, n);

// Product INSIDE a field type — needs parens
type Transform = f:(Int * Bool) -> String * name:String;
```

---

## 2. Quantifiers

### 2.1 Forall (Universal Quantification)

`forall` / `∀` is a prefix binder in type expressions. Desugars to nested `Pi (Just name) (U 0) body`:

```tulam
// In type annotations
val id : forall a. a -> a;
val const : forall a b. a -> b -> a;

// Unicode
val id : ∀ a. a -> a;

// With kind annotations
val mapMaybe : forall (a:Type)(b:Type). (a -> b) -> Maybe(a) -> Maybe(b);

// Rank-2 type (in function parameter)
function applyToInt(f: forall a. a -> a) : Int = f(42);

// Type alias
type NatTrans(f:Type -> Type, g:Type -> Type) = forall a. f(a) -> g(a);
```

**Binder syntax**: bare name `a` (defaults to kind `Type`) or kinded `(a:Kind)`.

### 2.2 Exists (Existential Quantification)

`exists` / `∃` is a prefix binder that introduces a hidden type variable. Requires a new AST node `Exists [(Name, Expr)] Expr`:

```tulam
// Existential type
type Showable = exists (a:Type). val:a * show:(a -> String);

// Simple existential
type Hidden = exists (a:Type). a;

// Multi-variable
type BiHidden = exists (a:Type)(b:Type). f:(a -> b) * val:a;
```

**Construction**: No `pack` keyword needed. Construct existential values directly; the type annotation drives witness inference:

```tulam
val s : Showable = {val = 42, show = showInt};
// Compiler infers witness type = Int from the value 42
// Checks: 42 : Int, showInt : Int -> String ✓
// Runtime: CLMCON tag [42, <showInt>] — witness type erased
```

**Elimination**: Field access works directly. The compiler skolemizes the hidden type variable and enforces that it doesn't escape:

```tulam
s.show(s.val)    // : String — fine, hidden type used consistently, doesn't escape
s.val            // ERROR: type is existentially bound, cannot escape
```

**Optional explicit `unpack`** for clarity or naming the type variable:

```tulam
unpack s as (a, x) in x.show(x.val)
// a = fresh skolem variable (the hidden type)
// x = the payload with type: val:a * show:(a -> String)
// Result must not mention a
```

---

## 3. Type Expressions (Annotations)

`*` and `+` work as type operators in annotation positions with standard algebraic precedence.

### 3.1 Precedence (Loosest to Tightest)

| Level | Operator | Associativity | Example |
|---|---|---|---|
| 1 | `forall`/`exists` | Prefix (extends rightward) | `forall a. a -> a` |
| 2 | `->` | Right | `a -> b -> c` = `a -> (b -> c)` |
| 3 | `+` | Left | `A + B + C` = `(A + B) + C` |
| 4 | `*` | Left | `A * B * C` = `(A * B) * C` |
| 5 | Type application | Left | `Maybe(Int)`, `Vec(a, n)` |
| 6 | Atoms | — | `Int`, `Bool`, `(...)`, `Type` |

### 3.2 Examples

```tulam
val f : Int * Bool -> String;         // (Int * Bool) -> String
val g : Int -> Int * Bool;            // Int -> (Int * Bool)
val h : Int * Bool + String * Char;   // (Int * Bool) + (String * Char)
val k : a -> b * c + d;              // a -> ((b * c) + d)
val m : forall a. a -> a * a;         // forall a. (a -> (a * a))
```

### 3.3 Two Parsing Contexts

**Type definitions** (`type X = ...`): `*` and `+` are field/variant separators. Field types extend until the next bare `*`/`+`/`;`. This means arrows inside field types need no parens.

**Type expressions** (annotations, function params, standalone types): `*` and `+` are type operators with the precedence table above. Parens for grouping as needed.

Both contexts produce the same internal representation. The difference is ergonomic: type definitions have named fields with `:`, type expressions are anonymous.

---

## 4. Removed / Changed Syntax

| Old Syntax | New Syntax | Notes |
|---|---|---|
| `type Bool = True \| False;` | `type Bool = True + False;` | `\|` reserved for pattern matching only |
| `Just(val:a)` | `Just * val:a` | Fields use `*`, not constructor parens |
| `record Point = (x:Float64, y:Float64);` | `type Point = x:Float64 * y:Float64;` | `record` keyword removed |
| `record Point = {x:Float64, y:Float64};` | `type Point = x:Float64 * y:Float64;` | Braces not needed for type defs |
| `pack(Type, value)` | `value : ExistentialType` | No `pack` keyword — witness inferred from annotation |

**Unchanged syntax:**
- Pattern matching: `match | pat1 -> expr1 | pat2 -> expr2` (pipes stay)
- Function definitions: `function f(x:Int, y:Bool) : Int = ...;` (commas stay for params)
- NTuple values: `{1, 2, 3}` and `{x=1, y=2}` (commas stay for values)
- Structure/instance/class declaration blocks: `{ ... }` with `;` separators
- Algebras, morphisms, classes: no change (declaration blocks, not data composition)

---

## 5. New Reserved Keywords

| Keyword | Purpose | Status |
|---|---|---|
| `exists` / `∃` | Existential quantification | Already reserved, needs parser rule |
| `forall` / `∀` | Universal quantification | Already reserved, needs parser rule |
| `unpack` | Explicit existential elimination (optional) | New — add to Lexer.hs |
| `Level` | Type of universe levels | New — add to Lexer.hs |

**Removed keywords:** `record`

---

## 6. Grammar Summary

### Type Definition Grammar

```
type_def       = 'type' TypeName type_params? '=' type_body ';'
type_params    = '(' param (',' param)* ')'
param          = name ':' type_expr

type_body      = variant ('+' variant)*

variant        = UpperName field* gadt_ret?       -- explicit constructor
               | named_field ('*' named_field)*   -- implicit constructor (single variant only)

field          = '*' name ':' field_type

named_field    = name ':' field_type

field_type     = type expression, parsed until bare *, +, :UpperName, or ;
                 (parentheses/brackets protect inner * and +)

gadt_ret       = ':' UpperName '(' type_expr (',' type_expr)* ')'
```

### Type Expression Grammar (Annotations)

```
type_expr      = forall_type | exists_type | arrow_type

forall_type    = ('forall' | '∀') binders '.' type_expr
exists_type    = ('exists' | '∃') binders '.' type_expr
binders        = binder+
binder         = name | '(' name ':' type_expr ')'

arrow_type     = sum_type ('->' arrow_type)?          -- right-assoc
               | dep_pi
dep_pi         = '(' name ':' type_expr ')' '->' arrow_type

sum_type       = product_type ('+' product_type)*     -- left-assoc

product_type   = app_type ('*' app_type)*             -- left-assoc

app_type       = atom ('(' type_expr (',' type_expr)* ')')?

atom           = name
               | universe                              -- Type, U(level), etc.
               | '(' type_expr ')'
```

### Expression-Level Additions

```
unpack_expr    = 'unpack' expr 'as' '(' name ',' name ')' 'in' expr
```

---

## 7. Internal Representation

### AST Changes (Surface.hs)

**New AST node:**
```haskell
| Exists [(Name, Expr)] Expr    -- exists (a:Type)(b:Kind). body
```

**No new AST node for `forall`** — desugars to `Pi (Just name) (U 0) body`.

**No new AST node for `unpack`** — desugars to `LetIn` with field access projections in a later pass.

**No `pack` AST node** — existential values are plain NTuples with type annotations driving witness inference.

### Type Definition Parsing

`+`/`*` in type definitions parse into the SAME AST as today:
- `type Bool = True + False;` → `SumType` with `Constructors [trueLam, falseLam]`
- `type Point = x:Float64 * y:Float64;` → `SumType` with one constructor, params = `[Var "x" Float64, Var "y" Float64]`
- `type Shape = Circle * r:Float64 + Rect * w:Float64 * h:Float64;` → `SumType` with two constructors

### Type Expression Parsing

`*`/`+` in type annotations produce:
- `Int * Bool` → `NTuple [(Nothing, Id "Int"), (Nothing, Id "Bool")]` (anonymous product) — OR if we want to distinguish type-level products: fold into nested `TSigma Nothing` via a new surface node
- `Int + Bool` → new surface node or desugar to sum type reference

**Decision**: For type annotations, `A * B` maps to a product type and `A + B` maps to a sum type. These can be represented as type-level applications of built-in type constructors `_*_` and `_+_`, or as new Expr nodes. Implementation will determine the cleanest approach.

---

## 8. Implementation Phases

### Phase A: Core Syntax Change (+/* for Type Definitions)

**Priority**: Critical — this is the foundation. All other phases build on this.

1. **Parser**: Replace `|` with `+`, constructor parens with `*` fields in `pSumType`/`pConstructor`
2. **Parser**: Add implicit constructor name for single-variant types
3. **Parser**: Add `pForall`/`pExists` in type expression parser
4. **Lexer**: Add `unpack`, `Level` to reserved words. Remove `record`.
5. **Migration**: Update all `lib/*.tl`, `tests/programs/*.tl`, `parsertests.tl`
6. **Tests**: Comprehensive parser tests for all new syntax forms

### Phase B: Type-Level * and + in Annotations

1. **Parser**: Add `*` (product) and `+` (sum) as type operators in `concreteType` with correct precedence
2. **AST**: Determine representation for anonymous products/sums in type expressions
3. **Type checker**: Handle product/sum types from annotations

### Phase C: Existentials

1. **AST**: Add `Exists` node to Surface.hs
2. **Parser**: Wire `exists`/`∃` into type expression parser
3. **Parser**: Add `unpack` expression parser
4. **Type checker**: Existential introduction (infer witness from annotation), elimination (skolemize + escape check)
5. **Pipeline**: Existential values → `CLMCON` with type witness erasure

### Phase D: Level Polymorphism

1. **Parser**: `Level` keyword, `U(levelExpr)`, `pLevelExpr`
2. **Type checker**: Level unification variables, constraint solving
3. **Pipeline**: Level parameter erasure

### Phase E: Identity Types

1. **Standard library**: Define `Id` as a GADT
2. **Type checker**: Verify normalization handles equality proofs
3. **Standard library**: `subst`, `cong`, `sym`, `trans` as functions

---

## 9. Interaction Matrix

| | Algebras | Morphisms | Classes | Effects | Pattern Match |
|---|---|---|---|---|---|
| **+/\* syntax** | No change (declaration blocks) | No change | No change | No change | `\|` stays for patterns |
| **Forall** | Structure methods already implicit-forall | Same | Same | Same | No impact |
| **Exists** | Can hide algebra-constrained values | Orthogonal | Orthogonal | Orthogonal | Field access with skolemization |
| **Levels** | Future: level-polymorphic algebras | Same | Same | Same | No impact |

Algebras, morphisms, classes, and effects use `{ }` declaration blocks with `;` separators. They are **not** changed by the `+`/`*` syntax — these are declaration blocks containing signatures, laws, implementations, and modifiers, not data type compositions.

---

## 10. Summary

| Feature | Syntax | Replaces |
|---|---|---|
| Sum types | `A + B + C` | `A \| B \| C` |
| Constructor fields | `Ctor * f1:T1 * f2:T2` | `Ctor(f1:T1, f2:T2)` |
| Records | `type R = f1:T1 * f2:T2;` | `record R = {f1:T1, f2:T2};` |
| GADTs | `Ctor * f:T : RetType(...)` | `Ctor(f:T) : RetType(...)` |
| Type composition | `type S = A + B;` | (new) |
| Type extension | `type S = A * f:T;` | (new) |
| Dependent telescope | `type S = n:Nat * v:Vec(a,n);` | (new) |
| Forall | `forall a. T` / `∀ a. T` | (new) |
| Exists | `exists (a:Type). T` / `∃ (a:Type). T` | (new) |
| Existential values | `{val, ops} : ExistsType` | (no `pack` needed) |
| Existential elimination | `unpack e as (a, x) in body` (optional) | (new) |
| Product type annotation | `Int * Bool` | (new) |
| Sum type annotation | `Int + Bool` | (new) |

**Removed**: `record` keyword, `|` in type definitions (stays in pattern matching).

**Unchanged**: function params (`,`), value tuples (`{,}`), declaration blocks (`{;}`), pattern matching (`| pat -> expr`), algebras, morphisms, classes, effects.
