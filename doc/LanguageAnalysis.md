# tulam Surface Language Analysis

## Condensed Language Reference

### Declarations (top-level, semicolon-terminated)

| Construct | Syntax | Purpose |
|-----------|--------|---------|
| **Sum type** | `type Bool = True + False;` | Algebraic data type with tagged constructors |
| **Parameterized type** | `type Maybe(a:Type) = Nothing + Just * x:a;` | Generic ADT |
| **GADT** | `type Vec(a:Type, n:Nat) = VNil : Vec(a,Z) + VCons * h:a * t:Vec(a,n) : Vec(a, Succ(n));` | Constructors with refined return types |
| **Deriving** | `type Color = Red + Blue deriving show, eq;` | Auto-generate algebra instances |
| **Record** | `type Point = x:Int * y:Int;` | Implicit constructor (single-constructor product type) |
| **Record spread** | `type Point3D = ..Point * z:Int;` | Include fields from another record |
| **Primitive** | `primitive Int;` | Machine-level type, no constructors |
| **Function** | `function f(x:Int) : Int = x + 1;` | Named function |
| **Pattern match fn** | `function f(x:Nat) : Bool = match \| Z -> True \| Succ(n) -> False;` | Function with case arms |
| **Implicit params** | `function f [a:Type] (x:a) : a = x;` | Brackets for implicit type params |
| **Operator as fn** | `function (+)(x:Nat, y:Nat) : Nat = ...;` | Define operator |
| **Where clause** | `function f(x:Int) : Int = g(x) where { function g(y:Int) : Int = y + 1 };` | Local definitions |
| **Action** | `action main() : Action = { putStrLn("hello") };` | IO/imperative block |
| **Action (monadic)** | `action main() = { x <- readLine(), putStrLn(x) };` | With `<-` bind syntax |
| **Value** | `value empty : a` / `value pi = 3.14;` | Named constant (abstract or concrete) |
| **Algebra/Trait** | `algebra Eq(a:Type) = { function (==)(x:a, y:a) : Bool };` | Single-type typeclass |
| **Morphism/Bridge** | `morphism Convertible(a:Type, b:Type) = { function convert(x:a) : b };` | Multi-type typeclass |
| **Structure** | `structure Monad(m:Type1) = { ... };` | General typeclass |
| **Instance** | `instance Eq(Int) = { function (==)(x:Int, y:Int) : Bool = ... };` | Typeclass implementation |
| **Instance intrinsic** | `instance Eq(Int) = intrinsic;` | Compiler-provided impl |
| **Instance derive** | `instance Show(Color) = derive;` | Auto-derived impl |
| **Class (OOP)** | `class Animal(name:String) = { function speak(self:Animal) : String = ... };` | OOP class with methods |
| **Abstract class** | `abstract class Shape() = { ... };` | Cannot instantiate |
| **Sealed class** | `sealed class Result() = { ... };` | Subclasses in same module only |
| **Inheritance** | `class Dog(breed:String) extends Animal = { ... };` | Single inheritance |
| **Implements** | `class Foo() implements Eq, Show = { ... };` | Auto-generate algebra instances |
| **Method modifiers** | `override`, `final`, `static` before method | Control dispatch |
| **Repr** | `repr Nat as Int default where { function toRepr(...) = ..., function fromRepr(...) = ... };` | Representation mapping |
| **Effect** | `effect Console = { function readLine() : String, function putStrLn(s:String) : Unit };` | Effect declaration |
| **Handler** | `handler StdConsole : Console = { function readLine() = intrinsic };` | Effect handler impl |
| **Fixity** | `infixl 6 (+), (-);` | Operator precedence/associativity |
| **Law** | `law reflexivity(x:a) = (x == x) === True;` | Equational law (inside structures) |
| **Module** | `module Algebra.Ring;` | Module declaration |
| **Import** | `import Algebra.Ring;` / `import Algebra.Ring (Field);` / `import Algebra.Ring hiding (x);` / `import Algebra.Ring as R;` | Import |
| **Export/Open** | `export M; open M;` | Re-export, open module |
| **Private** | `private function helper() = ...;` | Visibility |
| **Opaque** | `opaque type Handle = Int;` | Hide representation |
| **Target** | `target dotnet { ... };` / `target \| dotnet -> e \| js -> e` | Platform-specific code |

### Expressions

| Construct | Syntax | Purpose |
|-----------|--------|---------|
| **Application** | `f(x, y)` | Function call |
| **Binary op** | `x + y` | Operator application (Pratt parser) |
| **Unary minus** | `-x` | Negation |
| **If/then/else** | `if cond then e1 else e2` | Conditional (desugars to match) |
| **Let/in** | `let x = 1, y = 2 in x + y` | Local bindings (desugars to lambda) |
| **Anonymous lambda** | `\x -> x + 1` / `\x:Int, y:Int -> x + y` | Lambda expression |
| **Inline match** | `match expr \| pat -> body \| pat -> body` | Pattern match expression (desugars to lambda app) |
| **Handle** | `handle expr with handler` | Run effectful code |
| **Action block** | `action { x <- readLine() }` | Monadic block expression |
| **Dot access** | `p.x` / `obj.method(args)` | Field access / method call |
| **Record construct** | `Point { x = 1, y = 2 }` | Named construction |
| **Record update** | `p { x = 5 }` | Functional update |
| **Repr cast** | `expr as Type` | Representation conversion |
| **Operator as value** | `(+)` | Operator section |

### Literals

| Literal | Syntax |
|---------|--------|
| **Int** | `42` |
| **Float** | `3.14` |
| **String** | `"hello"` |
| **Char** | `'A'`, `'\n'` |
| **Array** | `[1, 2, 3]` |
| **NTuple (positional)** | `{1, 2, 3}` |
| **NTuple (named)** | `{x = 1, y = 2}` |
| **Vector** | `<1, 2, 3>` |

### Types

| Type | Syntax |
|------|--------|
| **Simple** | `Int`, `Bool` |
| **Application** | `Maybe(a)`, `Vec(a, n)` |
| **Arrow** | `Int -> Bool` (right-associative) |
| **Dependent Pi** | `(x:Nat) -> Vec(a, x)` |
| **Tuple type** | `{Int, Bool}` |
| **Record type** | `{x:Int, y:Bool}` |
| **Open record** | `{x:Int, ..}` |
| **Effect type** | `Eff { console: Console \| r } Int` |
| **Universe** | `Type` (= `U 0`), `Type1`, `Type2`, `Type3` |

### Pattern Matching

- **Constructor**: `Succ(n)` / `Just(x)`
- **Literal**: `0`, `'A'`, `"hi"`
- **Wildcard**: `_` (lowercase variable that's unused)
- **Named fields**: `Point { x = Z, y = _ }` / `Point { x }` (punning)
- **Multi-arg**: `| pat1, pat2 -> expr` (in function-level match)

### Reserved Keywords

`type`, `function`, `if`, `then`, `else`, `in`, `action`, `structure`, `instance`,
`let`, `where`, `exists`, `forall`,
`algebra`, `trait`, `morphism`, `bridge`, `law`, `extends`, `requires`,
`value`, `primitive`, `intrinsic`,
`repr`, `invariant`, `as`, `default`,
`match`,
`module`, `import`, `open`, `export`, `private`, `opaque`, `hiding`,
`target`, `extern`,
`effect`, `handler`, `handle`,
`derive`, `deriving`,
`class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super`,
`infixl`, `infixr`, `infix`

### Reserved Operators

`;`, `=`, `,`, `.`, `..`, `:`, `->`, `=>`, `|`, `?`, `<:`, `\`, `===`, `==>`, `<-`

---

## Inconsistencies & Design Issues

### 1. ~~Tuple syntax `{...}` is overloaded~~ RESOLVED

**Resolution**: Tuples and record literals are now unified under a single `NTuple` AST node with optional field names. `{1, 2}` is a positional NTuple, `{x = 1, y = 2}` is a named NTuple. The `{...}` overloading is now *intentional* — it's all NTuples, some with names. `RecordType` (`{x:Int, ..}`) remains separate as it's a type-level concept with row polymorphism. Pattern synonyms `Tuple`, `RecordLit`, and `LTuple` provide full backward compatibility.

**Status**: Implemented. Cache version bumped to v5.

### 2. `structure`/`algebra`/`trait` — redundant keywords

`algebra` and `trait` are synonyms (both produce `SAlgebra`). `morphism` and `bridge` are synonyms (both `SMorphism`). `structure` is the general form. This means there are **5 keywords for 3 concepts**. Users will be confused about which to use.

**Severity**: Low-medium. Intentional (categorical vs OOP vocabulary), but the duplication should be documented clearly or one set should be preferred.

### 3. `action` keyword does double duty

- Top-level: `action main() = { ... };` — declares an IO procedure
- Expression: `action { x <- readLine() }` — monadic do-block expression

These are conceptually different things sharing the same keyword.

**Severity**: Low. Context disambiguates (top-level has a name, expression-level doesn't).

### 4. ~~Comma as separator inside `{ }` for multiple contexts~~ **RESOLVED**

**Resolved**: Semicolons now separate declarations inside `{ }` blocks (structures, instances, classes, effects, handlers, where-clauses, derive blocks, action blocks). Commas are reserved for data separators only (args, tuples, record fields, let bindings). This eliminates the ambiguity of `{f(x,y), g(z)}`.

### 5. `class` (OOP) vs `structure`/`algebra` (typeclass) — two type abstraction systems

The language has both Haskell-style typeclasses (`structure`/`algebra`/`trait`) and OOP classes (`class`). They have different syntax, different runtime dispatch, different inheritance models. The `implements` keyword bridges them, but it's essentially two parallel hierarchies.

**Severity**: Medium. Intentional design decision (needed for .NET/JS interop), but the overlap can confuse new users.

### 6. `as` keyword is overloaded

- `import M as R` — module alias
- `expr as Type` — repr cast / downcast
- `repr Nat as Int` — representation declaration

Three very different meanings in different contexts.

**Severity**: Low-medium. Contextually unambiguous, but could confuse when reading code.

### 7. Constructor naming rule vs identifier rules

Constructors and types must start uppercase, functions/variables must start lowercase. But `identifier` in the parser accepts both — the enforcement is scattered (some places use `uIdentifier`, others `identifier`). Structure/algebra names use `identifier` (lowercase allowed), while types/classes use `uIdentifier`. This means `algebra eq(...)` is technically valid syntax.

**Severity**: Low. Works in practice. Could add a validation pass if desired.

### 8. ~~Semicolons terminate top-level declarations but commas separate members~~ **RESOLVED**

**Resolved**: Semicolons are now used consistently as declaration separators everywhere — both top-level and inside `{ }` blocks. `function f() = ...; function g() = ...` works the same at top level and inside structures/instances/classes.

### 9. `match` at function level vs expression level — different arity

Function-level match: `function f(x, y) = match | p1, p2 -> ...` — matches against all params.
Expression-level match: `match expr | p -> ...` — matches against one scrutinee.

There's no way to do a multi-scrutinee inline match. The function-level syntax also doesn't allow the scrutinee to be something other than the function params.

**Severity**: Low. Multi-scrutinee inline match is rare. Could be addressed with tuple matching if needed.

### 10. `value` declarations at top level vs inside structures

`value` is meaningful inside structures (abstract constant) and instances (concrete constant). At top level, `value pi = 3.14;` works but the relationship to `function pi() : Float64 = 3.14;` is unclear — one is a value, the other a nullary function.

**Severity**: Low. Could clarify documentation or restrict `value` to structure/instance context.

### 11. Anonymous lambda syntax `\x -> ...` vs everything else

The language uses ML-style `function` declarations but Haskell-style `\` for anonymous lambdas. This is fine but slightly inconsistent — one might expect `fn x -> ...` or `fun x -> ...` to match the ML flavor.

**Severity**: Low. `\` is well-known from Haskell/ML and works fine.

### 12. No `else`-less `if` for statements

`if/then/else` always requires both branches. This is correct for an expression language, but inside `action` blocks where statements dominate, an `if`-without-`else` (meaning "do nothing") would be natural.

**Severity**: Low. Can use `if cond then doThing() else ()` as workaround.

### 13. `deriving` on sum types vs `instance X(T) = derive;`

Two ways to request auto-derivation: `type T = ... deriving show;` and `instance Show(T) = derive;`. They do the same thing.

**Severity**: Low. `deriving` is syntactic sugar that expands to `instance ... = derive;`. Having both is convenient.

### 14. `extends` means different things in different contexts

- In `algebra`/`structure`: inherits operations from parent algebra (typeclass inheritance)
- In `class`: inherits fields and methods from parent class (OOP inheritance)

Same keyword, very different semantics.

**Severity**: Medium. Could confuse users working with both systems. The underlying concept (inheritance) is similar enough that this may be acceptable.

### 15. Structural records `{x:Int}` vs nominal records `type R = x:Int * y:Int` — **RESOLVED**

**Resolution**: Structural subtyping implemented. Nominal records (e.g., `Point`) can now unify with structural record types (e.g., `{x:Int, y:Int}`) via `expandNominalToRow` in the type checker. A `Point(3, 4)` value can be passed to a function expecting `{x:Int, y:Int}`. Additionally, sum-of-records now works: `type Shape = Rect + Circle` inherits fields from existing record declarations, so constructors carry the same data.

### 16. `#` suffix convention is legacy but still in the lexer

The `#` suffix (e.g., `print#`) is documented as "legacy" and replaced by `intrinsic`, but identifiers can still contain `#`. Unclear if this should be deprecated/removed or kept for backward compatibility.

**Severity**: Low. Harmless to keep, but should be documented as deprecated.

### 17. `exists`/`forall`/`∃`/`∀` are reserved but unused

These are in the reserved names list but have no parser rules. They're presumably reserved for future sigma/dependent type syntax, but currently they just prevent users from using these as identifiers.

**Severity**: Low. Forward-looking reservation is fine, but should be documented.

### 18. `=>` is a reserved operator but unused

The `=>` operator is reserved but has no parser rule. Presumably reserved for future use (type constraints? fat arrow lambdas?), but currently just blocks the symbol.

**Severity**: Low. Same as #17.

### 19. `?` is a reserved operator but unused

Reserved but has no parser rule. Possibly intended for optional types or nullable types.

**Severity**: Low. Same as #17.

### 20. `<:` is a reserved operator but unused

Reserved but has no parser rule. Presumably intended for subtype constraints.

**Severity**: Low. Same as #17.
