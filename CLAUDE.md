# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

tulam is a type-theory based functional language compiler/interpreter written in Haskell. It targets JavaScript, .NET, and potentially x86 native. The language is built on two primitives: **tuples** and **lambdas** — everything else (sum types, product types, structures/typeclasses) is derived from these.

## Build & Run

```bash
stack build              # Build
stack exec tulam         # Run the REPL (loads lib/ modules automatically)
stack test               # Run tests (test/Spec.hs)
stack clean              # Clean build artifacts
```

The project uses Stack with Hpack (`package.yaml` generates `tulam.cabal`). Edit `package.yaml` for dependency changes, not `tulam.cabal` directly.

## REPL Commands

Once in the REPL: `:load <file>`, `:list types`, `:list functions`, `:env`, `:all`, `:clm`, `:quit`.

## Architecture

### Compilation Pipeline (7 passes)

```
Source (.tl) → Lexer/Parser → Surface AST (Expr/Lambda)
  → Pass 0: After-parser desugaring (BinaryOp→App, IfThenElse→match, LetIn→lambda, etc.)
  → Pass 1: Environment Building (types, constructors, lambdas, primitives, instances, intrinsics)
  → Pass 2: Case Optimization (beta reduction, pattern expansion)
  → Pass 3: Type Checking (bidirectional type checker with row polymorphism, permissive by default)
  → Pass 4: CLM Conversion (Surface AST → Core List Machine IR, intrinsic → CLMPRIMCALL)
  → Pass 5: Code Generation (target-specific output)
```

### Key Modules

| Module | Role |
|--------|------|
| `src/Surface.hs` | Surface AST: `Expr`, `Lambda`, `Var`, `ConsTag`, `ClassInfo`, `ClassModifier` types. Core data structures for the entire compiler. |
| `src/Parser.hs` | Parsec-based parser. Grammar rules for sum types, records, functions, structures. |
| `src/Lexer.hs` | Tokenizer. Reserved keywords, operators, identifier rules. |
| `src/Pipeline.hs` | All compilation passes. Environment building, case optimization, CLM conversion. |
| `src/CLM.hs` | Core List Machine IR. Simply-typed intermediate form using explicit n-lists. Beta reduction, lambda application. Class nodes: `CLMMCALL`, `CLMSCALL`, `CLMNEW`. |
| `src/State.hs` | Interpreter state and monad stack. Environment maps (types, constructors, topLambdas, clmLambdas, classDecls). `ClassMeta` stores class hierarchy. |
| `src/Interpreter.hs` | CLM evaluator. Pattern matching resolution, expression evaluation, intrinsic dispatch. |
| `src/Intrinsics.hs` | Intrinsic function registry. Maps `(funcName, typeName)` to Haskell evaluation functions for Int/Float64 arithmetic and comparison. |
| `src/TypeCheck.hs` | Bidirectional type checker with row polymorphism. Internal types (`Ty`: TVar, TRigid, TCon, TApp, TPi, TSigma, TId, TForall, TRecord, TU), row types (REmpty, RExtend, RVar, RRigid), unification engine, structure constraint resolution. Permissive mode by default (errors are warnings). |
| `src/Logs.hs` | Error handling with source location tracking. |
| `src/MetadataResolver.hs` | Stub interface for codegen-time extern class metadata resolution (.NET/JS/native). No-op in interpreter. |
| `app/Main.hs` | REPL loop (Haskeline), file loading, interactive command dispatch. |

### Monad Stack

```haskell
type InputTState = InputT IntState           -- Haskeline layer
type IntState = StateT InterpreterState LambdaLoggerMonad  -- State layer
type LambdaLoggerMonad = LoggerMonadIO LogPayload          -- Logging layer
```

### Key Design Decisions

- **Constructor tags**: Integer tags on tuples for discriminated union pattern matching (needed for .NET/JS targets, unlike GHC's tagless approach). Sum types use `+`-separated constructors (`A + B + C`), with `*` for named fields (`Just * val:a`). Pattern matching uses `match | pat -> expr` syntax.
- **Implicit parameters**: Structure (typeclass) functions generate implicit-parameter functions, e.g. `(+) [a:Type] (x:a,y:a):a`. Instance declarations expand the case body.
- **Type-dependent functions execute at compile time only**: Only value-dependent simply-typed functions survive to runtime; type-dependent dispatch is resolved during compilation.
- **Expression traversal**: `traverseExpr` (pure) and `traverseExprM` (monadic/stateful) for AST transformations throughout passes.
- **Universe hierarchy**: `U 0` = `Type` (types of values), `U 1` = `Type1` (kinds / type constructors), `U 2` = `Type2`, etc. `pattern Type = U 0` provides backward compatibility. **Cumulativity**: `U n` unifies with `U m` when `n ≤ m` (types lift upward). Type-level computation uses `evalCLMPure` (pure CLM evaluator reusing `applyCLMLam`/`resolveCases`). `normalizeTy` in TypeCheck.hs normalizes after dependent substitution and on unification failure. Future: explicit Level polymorphism (Option C).
- **Categorical structure vocabulary**: `algebra` (single-type structure, e.g. Monoid), `morphism` (multi-type structure, e.g. Convertible), with `functor` and `natural` planned for when HKT support lands. Monad, Category, Arrow are standard library structures, not keywords. See `doc/CategoricalDesign.md` for full design.
- **Primitive types**: `primitive` keyword declares machine-level types with no constructors (Int, Float64, String, Char). Operations come through algebra instances.
- **Intrinsics**: `intrinsic` keyword marks compiler-provided implementations. Works at instance level (`instance Num(Int) = intrinsic;`) or function level. The intrinsic registry in `src/Intrinsics.hs` maps `(funcName, typeName)` to Haskell evaluation functions. Intrinsics are checked before normal instance lookup in CLMIAP dispatch.
- **Repr system**: `repr UserType as ReprType` declares a representation mapping. Stored in `reprMap` in `InterpreterState`. `toRepr`/`fromRepr` are registered as implicit-param wrapper lambdas + instance lambdas. `expr as Type` (`ReprCast`) resolves direction in `exprToCLM` using the repr map.
- **Pi types (dependent functions)**: `a -> b` (non-dependent) and `(x:A) -> B(x)` (dependent) parse in all type positions via `pTypeArrow` (right-associative). `Pi (Maybe Name) Expr Expr` in AST; `pattern ArrowType a b = Pi Nothing a b` for backward compat. Erased to `CLMEMPTY` at CLM conversion. `lamToTy` preserves param names as `TPi (Just name)`. `inferApp` substitutes bound variables on dependent application. Pattern synonyms `ExprConsTagCheck`/`ExprLiteralCheck` map to unified `PatternGuard PatternCheck Expr`. `Prim`/`PrimCall` removed (use `Function`+`Intrinsic`).
- **Type checker**: Bidirectional type checking (infer/check) with row polymorphism for records. Internal type representation (`Ty`) is separate from Surface AST. Unification uses HashMap-based substitution with occurs check. Structure constraints (`CStructure`) are emitted for implicit-param functions and resolved against `instanceLambdas`. Permissive mode (default): type errors are warnings; `strictTypes` flag makes them fatal. `NTuple [(Maybe Name, Expr)]` unifies tuples and record literals — positional `{1,2}` and named `{x=1,y=2}`. Pattern synonyms `Tuple`, `RecordLit`, `LTuple` for backward compat. `RecordType` remains separate for structural record types with open rows.
- **Class system (OOP)**: First-class classes with single inheritance, dynamic method dispatch, and abstract/sealed modifiers. `ClassDecl Lambda ClassInfo` in Surface AST. Objects are plain `CLMCON` (no vtable) — method dispatch is tag-based via `ClassMeta.cmMethods` in Environment. `CLMMCALL obj methodName args` dispatches by looking up the object's ConsTag class name in `classDecls`, finding the method in the pre-merged methods HashMap (includes inherited methods). Fields layout is parent-first, own-last. `extends` for single inheritance. `abstract` prevents instantiation. `sealed` restricts subclasses to same source file; `cmSourceFile` tracks origin; `checkSealedExhaustiveness` warns on incomplete pattern matches. `implements` auto-generates algebra instances from class methods (matching by name), falls back to derive blocks. `override`/`final`/`static` method modifiers. `targetImports` stores `import ... target ...` declarations for codegen-time extern class resolution (stub in `MetadataResolver.hs`). Designed for 1-1 mapping to .NET/JS/C++ classes in codegen. See `doc/ClassDesign.md`.

### Design Documents

- `doc/ClassDesign.md` — First-class OOP: classes, inheritance, extern subclassing, 1-1 codegen mapping
- `doc/InteropDesign.md` — .NET/JS/C++ interop: imports, type mapping, null/exception handling
- `doc/CategoricalDesign.md` — Categorical type system design (algebras, morphisms, functors, monads, categories, arrows)
- `doc/ConcurrencyDesign.md` — Effect-based structured concurrency: async/await, channels, STM, actors, parallel combinators
- `doc/ImplementationPlan.md` — 14-phase incremental implementation roadmap from current state to full categorical type system
- `doc/SigmaDesign.md` — Sigma types & telescopes: dependent pairs, existentials, relationship to existing systems
- `doc/SigmaCoreChanges.md` — Sigma types: detailed CLM and TypeChecker changes required
- `doc/PrimitiveDesign.md` — Primitive types, intrinsics, repr system, SIMD, GPU acceleration design
- `doc/RecordDesign.md` — Record system design (nominal, structural, spread, functions-as-fields)
- `doc/LanguageReference.md` — Full language reference documentation
- `doc/backends/LLVMBackendDesign.md` — LLVM native code backend: evaluation strategy, memory representation, Perceus RC, monomorphization, unboxing, defunctionalization, SIMD. 1700-line comprehensive design with comparison tables vs GHC/OCaml/MLton/Lean/Koka/Rust.
- `doc/backends/LazinessAndThreadingDesign.md` — Laziness treatment (Lazy(a) type, ~syntax, demand analysis, corecursion detection) and multi-threading options (biased RC, STM, green threads, fork-join). Options exploration, no final decisions. Includes §18 phased strategy (single-threaded first → OS threads → full concurrency).
- `doc/backends/PhaseA1_Design.md` — Detailed implementation design for Phase A.1 (LLVM backend foundation): LIR types, CLM→LIR lowering, LLVM IR emission, C runtime, memory representation, build pipeline, test strategy, implementation order.

### Data Files

- `lib/` — Consolidated standard library (13 files). REPL loads all modules from here via `loadModuleTree "lib/Base.tl"`.
  - `lib/Prelude.tl` — Primitive type declarations only (Int, Float64, String, Char, Int8-64, UInt8-64, Float32, Byte, Array). Loaded first.
  - `lib/Core.tl` — Pure algebraic data types: Bool, Unit, Ordering, Nat, Maybe, Either, List, Pair, NonEmpty, Tree, combinators (id, const, compose, flip, apply). No algebra imports.
  - `lib/Algebra.tl` — All algebra definitions: Semigroup, Monoid, Group, Additive, Multiplicative, Ring, Field, Floating, Absolute, Bits, StringOps, Eq, Ord, Show, Bounded, Enum, Hashable, Lattice, StringExt, StringLike.
  - `lib/Morphism.tl` — Multi-type structures: Convertible, Embed, Iso.
  - `lib/Instances.tl` — ALL instance declarations: reflection intrinsics, Core type instances, numeric intrinsic instances, Show/Bounded/Enum/Hashable/Lattice instances, numeric conversions.
  - `lib/Categorical.tl` — Category theory: Category, Arrow, Functor, Applicative, Monad + instances for Maybe/List.
  - `lib/Collection.tl` — Container algebras: Foldable, Sized, Searchable, Filterable, Buildable, Indexable, Traversable, Sortable, Zippable, Sliceable + Array instances.
  - `lib/Effect.tl` — Effect declarations and handlers: Console, FileIO, Exception, State.
  - `lib/Mutable.tl` — Mutable references: Ref, MutArray.
  - `lib/SIMD.tl` — SIMD vector types and Lane algebra.
  - `lib/Repr.tl` — Representation mappings: Nat as Int.
  - `lib/Str.tl` — Str type: UTF-8 encoded immutable strings, codec, algebra instances, StringLike, FromString.
  - `lib/Base.tl` — Umbrella module that re-exports all 11 submodules.
- `parsertests.tl` — Parser test cases.

## Language Conventions

- Type and constructor names must start with a capital letter.
- `#` suffix denotes legacy built-in operations (e.g., `print#`). New approach uses `intrinsic` keyword instead.
- **Separator rule**: Semicolons = declaration/statement separators (top-level, structure/instance/class/effect/handler/where/derive/action bodies). Commas = data separators (args, tuples, record fields, let bindings). Trailing semicolons are allowed in declaration blocks.
- Sum types use `+` to separate constructors and `*` to introduce named fields: `type Bool = True + False;`, `type Maybe(a:Type) = Nothing + Just * val:a;`. Implicit constructor: `type Point = x:Float64 * y:Float64;` creates constructor named `Point`.
- Pattern matching uses `match` keyword with pipes: `match | pat1 -> expr1 | pat2 -> expr2`. Inline form: `match expr | pat -> body`.
- `match` is a **reserved word**.
- `{ }` braces are used for NTuples (positional `{1,2}` and named `{x=1,y=2}`), record types, and declaration blocks (structures, instances, repr where-clauses).
- `structure` = typeclasses, `instance` = typeclass instances. `record` keyword is removed — use `type` with implicit constructors instead.
- `primitive` = machine-level type declaration, `intrinsic` = compiler-provided implementation body.
- `algebra`/`trait` = single-type structures, `morphism`/`bridge` = multi-type structures.
- `repr UserType as ReprType [default] where { ... }` = representation mapping between a user type and a primitive type. Requires `toRepr` and `fromRepr` function definitions. Optional `invariant`.
- `expr as Type` = explicit repr cast. `as` is a **reserved word** and cannot be used as an identifier. Direction (toRepr vs fromRepr) is resolved automatically from the repr map.
- `default` is a reserved word (used in `repr` declarations to mark the preferred mapping).
- `match` is a reserved word (used for pattern matching expressions).
- `class Name(fields) = { methods }` = OOP class with fields and methods. Methods take explicit `self` parameter.
- `class Child(fields) extends Parent = { ... }` = single inheritance. Child declares only own fields; parent fields are inherited.
- `abstract class` = cannot be instantiated; `sealed class` = all subclasses must be in same module.
- `override`, `final`, `static` = method modifiers inside class body. `super` = reserved for parent method calls.
- `forall a. T` / `∀ a. T` = universal quantification in type annotations. Desugars to `Pi (Just name) (U 0) body`.
- `exists (a:Type). body` / `∃ (a:Type). body` = existential quantification in type definitions. Binders become type params.
- `expr : Type` = expression-level type annotation. Produces `Typed expr type` AST node.
- `unpack e as (a, x) in body` = existential elimination. `unpack` is a **reserved word**.
- `A * B` and `A + B` in type annotations = type-level product/sum operators. Precedence: `forall`/`exists` < `->` < `+` < `*` < type application.
- Two type parser contexts: `concreteType` (full, with `+`/`*` as operators), `fieldType` (stops at bare `+`/`*` which are separators in type defs).
- `ClassName.new(args)` = construction. `obj.method(args)` = method call (dynamic dispatch). `obj.field` = field access.
- `class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super` are reserved words.
- `infixl N (op);` / `infixr N (op);` / `infix N (op);` = operator fixity declarations. N is precedence 0-9. Default for unknown ops is `infixl 9`. `infixl`, `infixr`, `infix` are reserved words.
- `fn(x) = expr` = anonymous lambda (ML-style). Mirrors named function syntax: `fn(x:Int, y:Int) : Int = x + y`. Supports `fn(x) = match | pat -> body` for pattern matching. `fn` is a **reserved word**.
- Per-function `requires` constraints: `function fold(xs:c(a)) : a requires Monoid(a);` — constraints checked at call site by the type checker. Multiple constraints comma-separated: `requires Monoid(a), Ord(b)`. Stored in `Lambda.lamRequires`.
