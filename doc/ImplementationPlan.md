# tulam Implementation Plan

Incremental roadmap from current state to categorical type system. Each step is testable independently and unlocks the next.

---

## Current State Summary (Updated after Effect Handler Runtime)

**Package A Syntax Migration (completed):**
- Sum types now use `type Bool = True | False;` instead of `type Bool = { True, False };` — curly braces freed for records exclusively.
- Pattern matching now uses `match` keyword: `match | Pat -> expr | ...` instead of `{ {Pat} -> expr, ... }`.
- Inline match expression supported: `match expr | Pat -> body | ...`.
- `match` is now a reserved keyword.

**Module System (completed):**
- New keywords: `module`, `import`, `open`, `export`, `private`, `opaque`, `hiding`, `target`, `extern`
- New AST nodes: `ModuleDecl`, `Import`, `Open`, `Export`, `PrivateDecl`, `OpaqueTy`, `TargetBlock`, `TargetSwitch`, `ArrayLit`
- New types: `ImportSpec`, `Visibility`, `ModulePath`
- New module: `src/ModuleSystem.hs` — path resolution, dependency graph, cycle detection, topological sort
- `ModuleEnv` added to `State.hs` for tracking module state
- `lib/` directory created with modular stdlib layout: `Algebra/`, `Core/`, `Numeric/`, `HKT/`, `Morphism/`, `Collection/`, `SIMD/`, `Repr/`

**Separate Compilation — Milestones 1-3 (completed):**
- Milestone 1: Module loading wired up — `loadModuleTree` recursively resolves dependencies from `lib/`, loads Prelude first, then all 34 modules in topological order
- Milestone 2: Code dedup — shared loading functions (`loadFileQuiet`, `loadModuleTree`, `resolveAllDeps`, `runParseOnly`) centralized in `src/ModuleSystem.hs`. `Main.hs` and `Spec.hs` import from single source of truth.
- Milestone 3: Module-scoped environments — per-module `publicNames`/`privateNames` tracking via environment snapshots. `private` keyword removes names from global env after module loading. `filterVisibleNames` implements `ImportAll`, `ImportOnly`, `ImportHiding` filtering. `loadedModules` HashMap stores all module envs for downstream visibility queries.
- Next: Milestone 4 (`.tli` interface files for faster startup)

**Primitive Type Expansion (completed):**
- New literal types in AST: `LInt8`, `LInt16`, `LInt32`, `LInt64`, `LWord8`, `LWord16`, `LWord32`, `LWord64`, `LFloat32`
- New primitive types in `prelude.tl`: `Int8`, `Int16`, `Int32`, `Int64`, `UInt`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Float32`, `Byte`, `Array(a:Type)`, `Vec2(a:Type)`, `Vec4(a:Type)`, `Vec8(a:Type)`, `Vec16(a:Type)`
- `Intrinsics.hs` completely refactored with generic helpers (`mkBinOp`, `mkUnaryOp`, `mkCmpOp`, `mkCompare`, `mkConst`)
- All numeric types have full operation sets: arithmetic, comparison, bitwise, transcendentals (for floats)
- `fromInt`/`fromFloat` protocol for polymorphic literals
- Convertible morphism instances for numeric conversions between all types
- Array primitive with `length`, `index`, `slice` operations
- SIMD Vec types with Lane algebra (stubs returning errors — need native compilation)
- `CLMARRAY` CLM node added for array representation

**What works:** Parsing sum types, functions with pattern matching, structures (typeclasses), instance declarations, actions, records with spread syntax, if/then/else, let/in, algebra/morphism/trait/bridge keywords, structure inheritance (extends), value declarations, law declarations, primitive type declarations, intrinsic instances, **anonymous lambdas** (`\x -> expr`), **first-class functions** (functions as values, passed to/returned from other functions), **common ADTs** (Maybe, Either, List with utility functions and Semigroup/Monoid instances), **combinators** (id, const, apply, compose, flip), **automated morphism composition** (transitive instances auto-derived with cycle detection), **parameterized type instances** (e.g., `Semigroup(List(a))`), **unary minus** (`-3` dispatches to `negate`), **Floating math** (sqrt, sin, cos, tan, exp, log, asin, acos, atan, pow, pi), **Bits algebra** (.&., .|., xor, complement, shiftL, shiftR, bitSize for Int), **String operations** (concat, length, Eq/Ord for String), **Int↔Float64 conversions** (toFloat, toInt, recip), **repr system** (`repr UserType as ReprType [default] where { ... }` + `expr as Type` casting), **higher-kinded types** (Functor, Applicative, Monad algebras on Type1 with instances for Maybe and List — `fmap(not, Just(True))` → `Just(False)`, `bind(Just(x), f)` → `f(x)`, List monad flatMap), **module system** (module/import/export/open/private/opaque with dependency resolution), **expanded numeric primitives** (Int8-64, UInt8-64, Float32 with full intrinsic support), **arrays** (Array(a) with length/index/slice), **SIMD type declarations** (Vec2/4/8/16 with Lane algebra stubs). Environment building handles types, constructors, lambdas, instance-specialized functions, primitive types, intrinsic expansion, morphism composition, repr registration, and module resolution. Case optimization expands patterns into constructor tag checks. CLM conversion produces simply-typed IR, including repr cast direction resolution and CLMARRAY. Interpreter evaluates: function application (including first-class), constructor matching, field access, literals, action sequences, pattern match cases, **implicit parameter dispatch** (CLMIAP) with intrinsic-first lookup (including nullary intrinsics) and partial type inference for HKT dispatch, partial application (CLMPAP), and lambda values (CLMLAM).

**Phases 1–9 + Phase 9.5 + Phase 9.6 + Phase 10.1–10.2 (Reflection + Derive) + Phase 10.3 (Type-Directed Dispatch) + Phase 11 Type Checker + Module System + Primitive Type Expansion + Phase 13.3 (Type-Test Patterns + Downcast) + Phase 13.6 (TC Subtyping) + Minimal Definition Checking + Parameterized Derive + Requires Enforcement + Effect Handler Runtime COMPLETE.** 720 automated hspec tests all passing.

**Parameterized Derive (completed):**
- `derive` on parameterized types (Maybe, List, Pair, Triple, etc.) works via reflection — `structuralEq`/`structuralCompare`/`structuralShow` recurse into fields using CLMIAP dispatch which infers concrete types at runtime.
- Nested parameterized types also work: `Just(Just(1)) == Just(Just(1))` dispatches through multiple layers.
- `deriving` syntax on parameterized type declarations supported.
- Test file: `tests/programs/P30_DeriveParam.tl` (20 tests)

**Requires Constraint Enforcement (completed):**
- `validateRequiresForInstance` in Pipeline.hs checks that required structure instances exist when processing instance declarations.
- Structure-level `requires` constraints are propagated to instance declarations with type parameter substitution (e.g., `algebra Foo(a) requires Eq(a)` + `instance Foo(MyType)` → checks `Eq(MyType)` exists).
- Instance-level explicit `requires` also validated.
- Warnings emitted (consistent with permissive mode) when required instances are missing.
- Test file: `tests/programs/P31_Requires.tl` (6 tests)

**Type-Directed Dispatch (Phase 10.3 completed):**
- `CLMTYPED` wrapper annotates CLMIAP bodies with return type hints during CLM conversion (Pass 4)
- `maybeAddTypeHint` in Pipeline.hs wraps non-implicit-param function bodies that have concrete return types
- Interpreter evaluates CLMTYPED with three strategies: (1) fallback when dispatch fails and args are values, (2) result-type-check when dispatch succeeds but return type mismatches hint, (3) passthrough when dispatch succeeds and matches
- `extractTypeNameFromHint`, `resultMatchesHint`, `tryHintInstance`, `isValue` helpers in Interpreter.hs
- Enables: `toEnum(0) : Bool` → False, `toEnum(2) : Ordering` → GreaterThan, `fromEnum(True) : Int` → 1
- Resolves the generic `mapM`/`sequence` base case dispatch limitation (return-type-dependent `pure`)
- Test file: `tests/programs/P17_TypeDirected.tl` (5 tests)

**Intrinsic Completeness (Phase 9.5 completed):**
- Char literal parsing (`'A'`, `'\n'`, etc.), Char Eq/Ord intrinsics
- Show intrinsics for all primitive types (Int, Float64, String, Char, Int8-64, UInt8-64, Float32, Byte)
- Bounded intrinsics (minBound/maxBound for all fixed-width types, Char, Byte)
- Enum intrinsics (succ/pred/toEnum/fromEnum for Int and Char)
- Hashable intrinsics (hash for Int, String, Char)
- Extended string ops: charAt, substring, indexOf, trim, toUpper, toLower, startsWith, endsWith, replace, parseInt, parseFloat (StringExt algebra)
- Array ops: set, push, arrayConcat, reverse, range (ArrayOps algebra). `[...]` syntax now creates `ArrayLit`/`CLMARRAY`.
- error/panic function, FileIO runtime (readFile, writeFile, appendFile, fileExists)
- CLMAPP IO fallback dispatch for unresolved function IDs

**Effect System (Phase 9 completed):**
- Row-polymorphic effects with `effect`/`handler`/`handle` keywords
- Action blocks with `<-` bind syntax, desugared to bind chains (Pass 0.5)
- `TEffect Row Ty` in type checker, effect rows erased at CLM conversion
- IO intrinsics: `putStrLn`, `putStr`, `readLine` via `dispatchIOIntrinsic`
- Standard library: `lib/Effect/Console.tl`, `lib/Effect/FileIO.tl`, `lib/Effect/State.tl`, `lib/Effect/Exception.tl`
- Record system with dot-access, named construction, record update, named field patterns (Pass 1.5)

**Real Programs (Phase 9.6 completed):**
- Array HOFs via closure-aware dispatch: `dispatchArrayHOF` in Interpreter.hs handles `fmap`, `filter`, `foldl`, `foldr`, `generate` when args include closures + arrays
- `Functor(Array)` instance + `ArrayHOF` algebra (filter/foldl/foldr/generate) in `lib/Collection/Array.tl`
- Universal Show fallback: `show` on any `CLMCON` value reflects constructor name + recursively shows fields; `show` on `CLMARRAY` shows `[elem1, elem2, ...]`
- Monadic traversal: `mapMMaybe`, `foldMMaybe`, `sequenceMaybe`, `forMMaybe`, `mapM_` in `lib/HKT/Traverse.tl`
- Note: Generic `mapM`/`foldM`/`sequence` previously needed type-directed dispatch (value-based dispatch can't infer target monad at base case). Phase 10.3 adds CLMTYPED annotations that resolve this. Specialized Maybe versions still available as explicit alternatives.

**Safety & Diagnostics:**
- `_contEval` capped at 10000 iterations, `evalCLM` at depth 1000, CLMERR propagation
- Verbose/trace flags: `:s verbose on/off`, `:s trace on/off`, `timedPass` for per-pass timing
- `betaReduceCLM` respects variable shadowing (fixed nested if/else capture bug)
- `lookupCLMInstancePrefix` partitions direct vs composed instances (fixed morphism dispatch ambiguity)

**Minimal Definition Checking (Automatic Validation):**
- Algebra/morphism instances are automatically validated for completeness
- When an instance doesn't provide enough functions, the compiler warns or errors
- Default implementations in algebras are propagated to instances using fixpoint resolution
- Functions with no default body that aren't provided → error "required (no default)"
- Mutual defaults where none from the cycle are provided → warning "consider providing at least one"
- Self-sufficient default groups (mutual recursion with base cases) are resolved automatically
- Intrinsic and derive instances bypass validation
- **Implementation**: `validateMinimalDefinition` in Pipeline.hs (Pass 1) checks instance-provided functions BEFORE default propagation. `propagateDefaults` (Pass 1) uses `fixpointResolve` with two phases: (1) iterative resolution via dependency graph, (2) self-sufficient group resolution when iterative phase stalls. `collectIdRefs` in Surface.hs extracts Id references for dependency analysis. Example: `Ord.tl` now has mutual defaults where `compare` defaults from comparison operators, and operators default from `compare`, resolved when at least one is provided.

**Remaining gaps:**
1. ~~**No State/Exception effect runtime**~~ — **RESOLVED**: Effect handler runtime implemented. `CLMHANDLE` node, handler stack, `dispatchHandlerOp`, `dispatchEffectSequencing`. Standard library handlers: RefState, SilentConsole, DefaultException, StdFileIO.
2. **Type checker permissive by default**: errors are warnings; `strictTypes` flag makes them fatal
3. **SIMD Vec operations are stubs** (need native compilation backend)
4. **No codegen backend**: interpreter only, no JS/.NET/x86 output
5. **Array HOFs for non-Int types**: foldl/filter type signatures use Int; needs polymorphic dispatch
6. **Missing string ops**: split, join not yet implemented
7. **TC pattern variable warnings**: type checker emits spurious "Unbound variable" warnings for pattern match binders in catch-all arms (~95 warnings in stdlib)

---

## Phase 1: Make Structures Actually Work

**Goal:** `structure Eq(a:Type)` + `instance Eq(Nat)` produces a working `==` function that dispatches on type.

This is the single most important missing piece. Everything in the categorical design depends on structures working end-to-end.

### Step 1.1: Parse `instance` declarations

**Files:** Lexer.hs, Parser.hs

`instance` is not currently a reserved word. Add it to the lexer, then add a parser rule:

```tulam
instance Eq(Nat) = {
    function ==(x:Nat, y:Nat) : Bool = eq(x,y),
    function !=(x:Nat, y:Nat) : Bool = not(eq(x,y))
};
```

Parser produces a new Expr variant or reuses Structure with a flag. Simplest: add `Instance Lambda [Expr]` to Expr -- the Lambda carries the structure name + type args, the [Expr] carries the function implementations.

**Surface.hs changes:** Add `| Instance Lambda [Expr]` to Expr. Add to traverseExpr, ppr.

**Test:** Parse `instance Eq(Nat) = { ... };` and see it in `:all` output.

### Step 1.2: Process `instance` in environment building (Pass 1)

**Files:** Pipeline.hs, State.hs

When we encounter `instance Eq(Nat)`:
1. Look up the structure `Eq` in the environment
2. Look up the implicit-parameter function `==` that was created from the structure
3. Add a new case to its PatternMatches body: `CaseOf [Var "a" Type (Id "Nat")] (Function implFunction)`
4. Update the lambda in the environment

This is the mechanism already sketched in the structure processing code -- `fixStr` in Pipeline.hs creates the initial `CaseOf [] (Function l)` with empty cases. Instance processing *adds* concrete cases.

**Test:** Load base.tl with uncommented `instance Eq(Nat)`. Check that `==` function in `:list functions` now has a `| Nat -> ...` case alongside the default.

### Step 1.3: Complete interpreter evaluation

**Files:** Interpreter.hs

Fill in the missing cases in `evalCLM`:
- `CLMLIT` -- return the literal as-is
- `CLMPROG` -- evaluate expressions in sequence, return last
- `CLMCASE` -- evaluate constructor tag checks, return body if all true
- `CLMIAP` -- resolve implicit type parameter, look up the specialized function, apply

Start with CLMLIT and CLMPROG (trivial), then CLMCASE (needed for pattern matching to work in the evaluator), then CLMIAP (needed for structure dispatch).

**Test:** In REPL, evaluate `not(True)` and get `False`. Evaluate `plus(Succ(Z), Succ(Z))` and get `Succ(Succ(Z))`. Then with instances working, evaluate `==(Z, Z)` and get `True`.

### Step 1.4: Uncomment and fix base.tl instances

**Files:** base.tl

Uncomment the `instance Eq(Nat)` and `instance Eq(Bool)` declarations. Verify they load and work.

Add a simple test:
```tulam
instance Eq(Nat) = {
    function ==(x:Nat, y:Nat) : Bool = eq(x,y)
};
```

**Test:** `:load base.tl` succeeds, `==(Z, Z)` evaluates to `True`, `==(Z, Succ(Z))` evaluates to `False`.

**Milestone: Structures work end-to-end.** This is the foundation for everything that follows.

### Phase 1 Learnings & Implementation Notes

**Completed.** Key decisions and discoveries:

1. **Instance storage**: Instance functions are stored separately from top-level lambdas in `instanceLambdas` (Surface) and `clmInstances` (CLM) maps, keyed by `"funcName\0typeName"`. This avoids fighting with the CaseOf/PatternMatches mechanism which is designed for value-level constructor matching, not type-level dispatch.

2. **Type inference from values**: CLMIAP dispatch infers the type from constructor tags on the evaluated arguments. `CLMCON (ConsTag "Z" 0) _` -> look up "Z" in constructors -> get `lamType` -> extract type name "Nat". Also handles literal types (Int, Float, String, Char).

3. **Case optimization scope**: Instance lambda functions with pattern matching (like `instance Eq(Bool)`) MUST go through `caseOptimizationPass` (Pass 2). Initially missed this -- instance functions were stored but their patterns weren't expanded, causing runtime errors.

4. **Interactive desugaring**: The `afterparse` pass (BinaryOp->App desugaring) only runs on file loads. Interactive REPL expressions need manual desugaring in `processInteractive`. Note: `traverseExpr f` transforms children but NOT the root node -- must apply `f` to root separately.

5. **What the plan got right**: The step ordering was correct. Instance parsing -> processing -> interpreter -> base.tl was the right sequence. The "instance processing adds cases to PatternMatches" approach from the original plan was wrong though -- direct instance maps turned out to be cleaner.

6. **What to watch for next**: The implicit-param function `== [a:Type]` still exists with its default body. Currently the default path (no instance found) falls through to the general function's `PatternMatches` body, which has only a default case that returns another function. This works but is fragile. Future: type checker should validate instance coverage.

---

## Phase 2: Basic Language Completeness

**Goal:** Fill in the missing basic constructs so the language is usable for non-trivial programs.

See also: `doc/RecordDesign.md` for full record system design rationale.

### Step 2.1: Parse and desugar `if/then/else`

**Files:** Surface.hs, Parser.hs, Pipeline.hs

Add `IfThenElse Expr Expr Expr` to the `Expr` type. This keeps parsing clean and puts desugar logic in the pipeline where it belongs.

**Surface.hs:**
- Add `| IfThenElse Expr Expr Expr` to `Expr`
- Add `traverseExpr f (IfThenElse c t e) = IfThenElse (f c) (f t) (f e)`
- Add `ppr (IfThenElse c t e) = "if " ++ ppr c ++ " then " ++ ppr t ++ " else " ++ ppr e`

**Parser.hs:**
- Add `pIfThenElse` parser using existing reserved words `if`, `then`, `else`
- Add `try pIfThenElse` to `pFactor` before `pApp`

```haskell
pIfThenElse :: Parser Expr
pIfThenElse = do
    reserved "if"
    cond <- pExpr
    reserved "then"
    e1 <- pExpr
    reserved "else"
    e2 <- pExpr
    return $ IfThenElse cond e1 e2
```

**Pipeline.hs — afterparse desugaring:**
```haskell
afterparse (IfThenElse cond thenE elseE) =
    App (Function (Lambda "" []
        (PatternMatches
            [ CaseOf [Var "" (Id "Bool") (Id "True")]  thenE defaultSI
            , CaseOf [Var "" (Id "Bool") (Id "False")] elseE defaultSI
            ]) EMPTY)) [cond]
```

The desugared form feeds into existing Pass 2 case optimization, which expands it into constructor tag checks. No CLM or interpreter changes needed.

**Test:**
```
> if True then Succ(Z) else Z
Succ(Z)
> if False then Succ(Z) else Z
Z
```

### Step 2.2: Parse `let/in` expressions

**Files:** Surface.hs, Parser.hs, Pipeline.hs

Add `LetIn [(Var, Expr)] Expr` to `Expr` — list of `(variable, value)` bindings and a body expression.

**Surface.hs:**
- Add `| LetIn [(Var, Expr)] Expr`
- Add traverseExpr: apply f to binding values and body
- Add ppr

**Parser.hs:**
```haskell
pLetIn :: Parser Expr
pLetIn = do
    reserved "let"
    bindings <- sepBy1 pLetBinding (reservedOp ",")
    reserved "in"
    body <- pExpr
    return $ LetIn bindings body

pLetBinding :: Parser (Var, Expr)
pLetBinding = do
    nm <- identifier
    tp <- optionMaybe (reservedOp ":" >> concreteType)
    reservedOp "="
    val <- pExpr
    return (Var nm (maybe UNDEFINED id tp) UNDEFINED, val)
```

Add `try pLetIn` to `pFactor` before `pApp`.

**Pipeline.hs — afterparse desugaring:**
```haskell
-- let x = e1, y = e2 in body  =>  (\x -> (\y -> body)(e2))(e1)
afterparse (LetIn [(v, val)] body) =
    App (Function (Lambda "" [v] body EMPTY)) [val]
afterparse (LetIn ((v,val):rest) body) =
    App (Function (Lambda "" [v] (LetIn rest body) EMPTY)) [val]
```

Nested lets desugar to nested lambda applications. The recursive case gets caught by the next afterparse traversal.

**Test:**
```
> let one = Succ(Z) in plus(one, one)
Succ(Succ(Z))
> let a = Succ(Z), b = Succ(Succ(Z)) in plus(a, b)
Succ(Succ(Succ(Z)))
```

### Step 2.3: Named records with functions as fields

**Files:** Lexer.hs, Parser.hs

Records are products with named fields. They can contain any values, including functions. They desugar to single-constructor sum types, so the existing pipeline handles them with no changes.

**Lexer.hs:** Add `"record"` to reserved words.

**Parser.hs:**
```haskell
pRecord :: Parser Expr
pRecord = do
    reserved "record"
    name <- identifier
    -- optional type parameters
    tparams <- optionMaybe (parens (sepBy1 pTypeParam (reservedOp ",")))
    reservedOp "="
    fields <- braces (sepBy1 pRecordField (reservedOp ","))
    -- Desugar: record Foo = { x:A, y:B }  =>  type Foo = Foo(x:A, y:B);
    let vars = map fieldToVar fields
    let consLam = Lambda name vars EMPTY (Id name)
    let tpVars = maybe [] id tparams
    return $ SumType (Lambda name tpVars (Tuple [Function consLam]) (U 0))

pRecordField :: Parser (Name, Expr)
pRecordField = do
    nm <- identifier
    reservedOp ":"
    tp <- concreteType
    return (nm, tp)

fieldToVar :: (Name, Expr) -> Var
fieldToVar (nm, tp) = Var nm tp UNDEFINED
```

Add `try pRecord` to `pDef` before `pSumType`.

**Test:**
```tulam
record Point = { x:Nat, y:Nat };
record Pair(a:Type, b:Type) = { fst:a, snd:b };
```
```
> :load test.tl
> Point(Z, Succ(Z))
Point(Z, Succ(Z))
> let p = Point(Succ(Z), Z) in p.x
Succ(Z)
```

### Step 2.4: Record spread in declarations

**Files:** Parser.hs, Pipeline.hs

Allow `..Name` in record field lists to include all fields from another record.

**Parser.hs:** In `pRecordField`, handle the `..` prefix:
```haskell
pRecordField :: Parser (Either Name (Name, Expr))
pRecordField =
    try (reservedOp ".." >> identifier >>= return . Left)   -- spread
    <|> (do nm <- identifier; reservedOp ":"; tp <- concreteType; return $ Right (nm, tp))
```

**Pipeline.hs — resolve spread in Pass 0 or afterparse:**
When a spread `..Point` is encountered, look up `Point` in the parsed types, extract its constructor fields, and splice them into the field list. Duplicate field names from later in the list override spread fields.

**Test:**
```tulam
record Point = { x:Nat, y:Nat };
record Point3D = { ..Point, z:Nat };
// expands to: type Point3D = Point3D(x:Nat, y:Nat, z:Nat);
```

### Step 2.5: Named record construction syntax

**Files:** Parser.hs

Allow `Name { field = value, ... }` as an alternative to positional `Name(value, ...)`.

**Parser.hs:**
```haskell
pNamedConstruction :: Parser Expr
pNamedConstruction = do
    nm <- identifier
    fields <- braces (sepBy1 pFieldAssign (reservedOp ","))
    -- reorder fields to match declaration order, produce positional App
    return $ NamedRecord nm fields

pFieldAssign :: Parser (Name, Expr)
pFieldAssign = do
    nm <- identifier
    reservedOp "="
    val <- pExpr
    return (nm, val)
```

This needs a new `NamedRecord Name [(Name, Expr)]` constructor in Surface.hs that desugars in Pass 0/afterparse to a positional constructor call after looking up the field order from the environment.

Alternative: defer named construction to Phase 3 and only support positional `Point(Z, Z)` in Phase 2. Named construction requires environment access during desugaring which adds complexity.

**Test:**
```
> Point { x = Z, y = Succ(Z) }
Point(Z, Succ(Z))
> Point { y = Succ(Z), x = Z }    // order doesn't matter
Point(Z, Succ(Z))
```

### Step 2.6: Record update syntax

**Files:** Parser.hs, Pipeline.hs

Allow `expr { field = newValue }` to create a copy with fields changed.

**Parser.hs:** Parse `pExpr { field = val, ... }` as `RecordUpdate Expr [(Name, Expr)]`.

**Pipeline.hs — desugar:** Given record type info, expand to positional constructor call:
```tulam
p { x = Succ(Z) }
// desugars to:
Point(Succ(Z), p.y)   // changed fields from update, unchanged from field access
```

**Note:** This requires knowing the record's type to enumerate fields. Options:
- Require type annotation: `(p:Point) { x = Succ(Z) }`
- Infer from context (needs type checker)
- Defer to Phase 4+ when we have basic type inference

**Recommendation:** Defer record update to a later phase. It needs type info to know which fields to copy. Steps 2.1-2.4 give us records that work; update is convenience sugar.

### Phase 2 ordering and dependencies

```
Step 2.1 (if/then/else)        <- independent, simple desugar
Step 2.2 (let/in)              <- independent, simple desugar
Step 2.3 (named records)       <- independent, desugars to sum types
Step 2.4 (record spread)       <- depends on 2.3
Step 2.5 (named construction)  <- depends on 2.3, needs environment access
Step 2.6 (record update)       <- DEFER to later phase (needs type info)
```

Recommended order: **2.1 → 2.2 → 2.3 → 2.4 → 2.5(optional)**

Steps 2.1 and 2.2 are quick wins. Step 2.3 is the core record feature. Step 2.4 (spread) is valuable for record extension. Step 2.5 (named construction) can be deferred if environment access during desugaring proves complex. Step 2.6 (update) is deferred.

**Milestone: Language has conditionals, local bindings, records with functions-as-fields, and record extension. Usable for non-trivial programs.**

**STATUS: COMPLETE.** All record features implemented (dot-access, named construction, record update, named field patterns). Record desugaring runs in Pass 1.5. See `tests/programs/P11_Records.tl` for 11 tests covering all features.

---

## Phase 3: Structure Inheritance (`extends`)

**Goal:** `algebra Ord(a) extends Eq(a)` inherits all Eq methods and can add new ones.

### Step 3.1: Add `extends` to parser

**Files:** Lexer.hs, Parser.hs

Add `extends` as reserved word. Modify structure/algebra parsing:

```tulam
structure Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering,
    function <(x:a, y:a) : Bool = ...
};
```

Parser stores the parent reference in the Structure/Lambda representation. Options:
- Add a field to `Lambda`: `lamExtends :: [Expr]`
- Or store it in a wrapper: `Structure Lambda [Name] [Expr]` -- third field is parent structures

### Step 3.2: Process inheritance in Pass 1

**Files:** Pipeline.hs

When building environment for a structure with `extends`:
1. Look up parent structure
2. Copy all parent functions into child (with the child's type parameters)
3. Allow child to override parent functions
4. When instantiating child, automatically satisfy parent constraint

### Step 3.3: Instance inheritance

When `instance Ord(Nat)` is declared, the compiler should also require/verify `instance Eq(Nat)` exists (or generate an error).

**Test:**
```tulam
structure Eq(a:Type) = { function ==(x:a,y:a):Bool };
structure Ord(a:Type) extends Eq(a) = { function <(x:a,y:a):Bool };
instance Eq(Nat) = { ... };
instance Ord(Nat) = { ... };
-- Ord(Nat) instance inherits == from Eq(Nat)
```

**Milestone: Structure inheritance works. Foundation for the algebra hierarchy (Semigroup -> Monoid -> Group).**

---

## Phase 4: Primitive Types and Intrinsics

**Goal:** Machine-level types (Int, Float64, String) work with native performance. `3 + 4` evaluates to `7`.

See `doc/PrimitiveDesign.md` for full design rationale.

### Step 4.1: Parse `primitive` declarations

**Files:** Lexer.hs, Parser.hs, Surface.hs

- Add `primitive` to reserved words in Lexer.hs
- Add `| Primitive Lambda` to Expr (Lambda carries type name + optional type params)
- Parse: `primitive Int;` → `Primitive (Lambda "Int" [] UNDEFINED (U 0))`
- Parse: `primitive Array(a:Type);` → `Primitive (Lambda "Array" [Var "a" Type UNDEFINED] UNDEFINED (U 1))`
- Add to traverseExpr (identity), ppr

### Step 4.2: Process `primitive` in Pass 1

**Files:** Pipeline.hs, State.hs

- Add `primitiveTypes :: Map Name Lambda` to InterpreterState
- When processing a Primitive declaration, register the type name in primitiveTypes
- Primitive types have no constructors (unlike SumType)
- The type name is added to the type environment so it can be referenced in type positions

### Step 4.3: Parse `intrinsic` as function/instance body

**Files:** Lexer.hs, Parser.hs, Surface.hs

- Add `intrinsic` to reserved words
- Add `| Intrinsic` to Expr
- In function body position: `function foo(x:Int) : Int = intrinsic;` → body is `Intrinsic`
- In instance body position: `instance Num(Int) = intrinsic;` → functions get `Intrinsic` body
- Add to traverseExpr (identity), ppr

### Step 4.4: Intrinsic registry

**Files:** new src/Intrinsics.hs

Create a module mapping `(functionName, typeName)` to Haskell evaluation functions:

```haskell
type IntrinsicFn = [CLMExpr] -> Maybe CLMExpr

intrinsicRegistry :: Map (Name, Name) IntrinsicFn
intrinsicRegistry = Map.fromList
    [ (("+", "Int"),  \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ CLMLIT (LInt (a + b)))
    , (("-", "Int"),  \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ CLMLIT (LInt (a - b)))
    , (("*", "Int"),  \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ CLMLIT (LInt (a * b)))
    , (("div", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ CLMLIT (LInt (a `div` b)))
    , (("mod", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ CLMLIT (LInt (a `mod` b)))
    , (("==", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a == b))
    , (("<",  "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a < b))
    , ((">",  "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a > b))
    , (("<=", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a <= b))
    , ((">=", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a >= b))
    , (("!=", "Int"), \[CLMLIT (LInt a), CLMLIT (LInt b)] -> Just $ boolToCLM (a /= b))
    -- Float64
    , (("+", "Float64"), \[CLMLIT (LFloat a), CLMLIT (LFloat b)] -> Just $ CLMLIT (LFloat (a + b)))
    , (("*", "Float64"), \[CLMLIT (LFloat a), CLMLIT (LFloat b)] -> Just $ CLMLIT (LFloat (a * b)))
    -- etc.
    ]
```

### Step 4.5: CLM representation and interpreter evaluation

**Files:** CLM.hs, Interpreter.hs, Pipeline.hs

- Add `| CLMINTRINSIC Name Name [CLMExpr]` to CLMExpr (funcName, typeName, args)
- When `intrinsic` body is encountered during CLM conversion (Pass 4), emit CLMINTRINSIC
- In evalCLM: evaluate args, look up in intrinsic registry, apply
- For `instance Num(Int) = intrinsic`: each function in the Num algebra generates a separate CLMINTRINSIC entry

### Step 4.6: Create prelude.tl

**Files:** new prelude.tl, Main.hs

- Create prelude.tl:
  ```tulam
  primitive Int;
  primitive Float64;
  primitive String;
  primitive Char;
  primitive Byte;
  ```
- Load prelude.tl before base.tl in the REPL
- Add Num, Integral, Eq, Ord intrinsic instances for Int in prelude.tl (or a new numeric.tl)

### Step 4.7: Literal type inference for dispatch

**Files:** Interpreter.hs

- `inferTypeFromExpr` already returns `"Int"` for `CLMLIT (LInt _)` and `"Float"` for `CLMLIT (LFloat _)` — rename Float to Float64 for consistency
- CLMIAP dispatch for `+` applied to `[3, 4]` → infers type "Int" → looks up instance → finds CLMINTRINSIC

**Test milestone:**
```
> 3 + 4
7
> 10 * 5 + 2
52
> 3 > 2
True
> 3 == 3
True
```

**Milestone: Machine integers and floats work with native performance. Arithmetic expressions evaluate correctly.**

### Phase 4 Learnings & Implementation Notes

**Completed.** Key decisions and discoveries:

1. **No new CLM node needed**: The plan proposed `CLMINTRINSIC` as a new CLM node, but intrinsic dispatch was implemented directly in the CLMIAP evaluator by checking `lookupIntrinsic` before normal instance lookup. This is simpler — intrinsic instances register as CLMLam with `CLMPRIMCALL` body (same as existing primitives), but the interpreter short-circuits them via the intrinsic registry.

2. **Intrinsic registry**: `src/Intrinsics.hs` maps `"funcName\0typeName"` keys to `[CLMExpr] -> Maybe CLMExpr` functions. Covers Int and Float64 arithmetic (`+`, `-`, `*`, `div`, `mod`, `/`), comparison (`==`, `!=`, `<`, `>`, `<=`, `>=`, `compare`), and utility functions (`negate`, `abs`, `fromInt`).

3. **Loading order matters**: `prelude.tl` (primitive types + Num/Integral/Fractional algebras + their intrinsic instances) loads before `base.tl` (Nat, Bool, Eq, Ord + their Nat/Bool instances + Eq/Ord intrinsic instances for Int/Float64). Intrinsic instances for Eq/Ord must be in `base.tl` because Eq and Ord are defined there.

4. **Intrinsic instance expansion**: When `instance Num(Int) = intrinsic` is encountered in Pass 1, the compiler looks up the Num structure, extracts all function/value names, and creates placeholder `Lambda fn [] Intrinsic UNDEFINED` entries for each. These are then propagated to parents via the existing `propagateToParent` mechanism.

5. **Bool/Ordering constructor tags**: Intrinsic comparison functions must produce `CLMCON (ConsTag "True" 0) []` / `CLMCON (ConsTag "False" 1) []` matching base.tl's `type Bool = True | False;` tag ordering (True=0, False=1). Similarly, `compare` returns `LessThan`=0, `Equal`=1, `GreaterThan`=2.

6. **CLMPAP handler added**: Partial application evaluation was missing from the interpreter. Added as part of this phase since it's needed for some numeric expression evaluation paths.

7. **Float type name fix**: `inferTypeFromExpr` returned `"Float"` for float literals but the intrinsic registry uses `"Float64"`. Fixed to return `"Float64"`.

8. **Num standalone for MVP**: The plan considered `Num extends Eq, Ord` but this creates a circular dependency between prelude.tl and base.tl. For now, Num is a standalone algebra without extends.

### Phase 4 Expanded Intrinsics (Completed)

Extended the intrinsic registry and prelude with additional algebras:

1. **Unary minus fix**: `afterparse (UnaryOp "-" e)` now desugars to `App (Id "negate") [afterparse e]` instead of `App (Id "-") [e]`. This maps `-3` to `negate(3)` which dispatches through the existing `negate` intrinsic.

2. **Floating algebra**: `Floating(a) extends Fractional(a)` in prelude.tl with `sqrt`, `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `pow`, and `value pi`. Intrinsic instance for Float64. All use `floatUnaryOp` / `floatBinOp` helpers.

3. **Bits algebra**: `Bits(a)` in prelude.tl with `.&.`, `.|.`, `xor`, `complement`, `shiftL`, `shiftR`, `value bitSize`. Intrinsic instance for Int. Uses `Data.Bits` import in Intrinsics.hs. Note: `.&.` and `.|.` must NOT be reserved operators — they need to parse as regular operators via `parens operator` for function declarations.

4. **StringOps algebra**: `StringOps(a)` in prelude.tl with `concat` and `length`. Plus `Eq(String)` and `Ord(String)` intrinsic instances in base.tl (since Eq/Ord are defined there). String comparison and ordering intrinsics added to registry.

5. **Nullary intrinsic dispatch fix**: `findAnyInstanceWithType` added to State.hs — returns both the type name and CLM lambda for nullary instance lookup. Interpreter's nullary CLMIAP branch now checks `lookupIntrinsic funcNm typNm` before applying the CLM lambda. This fixes `pi` and `bitSize` which are nullary `value` declarations with intrinsic bodies.

6. **Conversion intrinsics**: `toFloat` (Int→Float64), `toInt` (Float64→Int via truncate), `recip` (Float64) added to registry.

---

## Phase 4+: Parallel Features (Anonymous Lambdas, ADTs, Tests, Morphism Composition)

**Goal:** Fill in practical language features independent of the primitive types work: anonymous lambdas, first-class functions, common ADTs, automated tests, and morphism auto-composition.

### Completed Features

**A. Anonymous Lambdas + First-Class Functions** ✅
- Parser: `pAnonLambda` parses `\x -> expr` (and `\x:Type, y -> expr`), added to `pFactor`
- Pipeline: Unknown function IDs in `exprToCLM` produce `CLMAPP (CLMID nm) args` (fallthrough, not error), enabling first-class function application
- Interpreter: Generic `CLMAPP` eval checks if function resolved to `CLMLAM` and applies it
- base.tl combinators: `id`, `const`, `apply`, `compose`, `flip`

**B. Automated Test Suite** ✅
- hspec dependency added to `package.yaml`
- 45 tests in `test/Spec.hs`: parser (3), pipeline integration (8), end-to-end evaluation (23), combinators (4), ADTs (11)
- Tests cover: Nat ops, Bool, Monoid, morphisms, if/else, let/in, Int intrinsics, Maybe/List operations

**C. Common ADTs (Maybe, List, Either)** ✅
- Types: `Maybe(a)`, `Either(a,b)`, `List(a)` with constructors
- Utilities: `isNothing`, `fromMaybe`, `head`, `tail`, `isEmpty`
- Instances: `Semigroup(List(a))`, `Monoid(List(a))`
- Fix: `lookupTypeOfConstructor` and `extractTypeName` now handle parameterized types like `List(a)`

**D. Automated Morphism Composition** ✅
- `composeMorphismInstances` in Pipeline.hs auto-derives transitive morphism instances for 2-param morphisms
- `findMorphismInstances` in State.hs finds all existing instance pairs
- Cycle detection: skips identity loops and prefers direct instances over composed ones
- e.g., given `Convertible(Nat, Bool)` + `Convertible(Bool, Nat)`, auto-derives `Convertible(Nat, Nat)` and `Convertible(Bool, Bool)`

### Phase 4+ Learnings

1. **First-class functions require fallthrough**: The `exprToCLM` error for unknown function names must become a `CLMAPP (CLMID nm) args` to allow variables bound to functions to be applied.

2. **Parameterized type instances**: Instance registration needed `extractTypeName` to handle `App (Id "List") [Id "a"]` (not just `Id "Nat"`). Similarly, `lookupTypeOfConstructor` needed to match `App (Id nm) _` for constructors whose return type is parameterized (e.g., `Cons` returns `List(a)`).

3. **Morphism composition is Surface-level**: Composed lambdas are generated as `Lambda funcNm [Var "__x" ...] (App (Id funcNm) [App (Id funcNm) [Id "__x"]]) ...` — normal Surface AST that flows through the standard pipeline passes.

4. **Test infrastructure**: Tests use `evalStateT (execStateT setup emptyIntState) initLogState` to run the full monad stack, and `evalStateT (evalStateT (evalExprM input) st) initLogState` for evaluation. The `loadFileQuiet` helper suppresses stdout output.

---

## Phase 5: Repr System ✅ COMPLETE

**Goal:** User-defined types can declare representation mappings to primitives for optimization.

See `doc/PrimitiveDesign.md` §5 for full design.

### Step 5.1: Parse `repr` declarations ✅

**Files:** Lexer.hs, Parser.hs, Surface.hs

- Added `repr`, `invariant`, `as`, `default` to reserved words in Lexer.hs
- Added `| Repr Name Expr Bool [Expr] (Maybe Expr)` to Expr in Surface.hs
  - Name: user type name
  - Expr: repr type
  - Bool: is default
  - [Expr]: toRepr and fromRepr function definitions
  - Maybe Expr: optional invariant expression
- Parse: `repr Nat as Int default where { ... };`
- Added traverseExpr and ppr cases for Repr

### Step 5.2: Process `repr` in Pass 1 ✅

**Files:** Pipeline.hs, State.hs

- Added `reprMap :: Map Name [(Name, Bool, Lambda, Lambda, Maybe Expr)]` to InterpreterState
  - Key: user type name
  - Value: list of (repr type name, isDefault, toRepr, fromRepr, invariant)
- Added helper functions: `addRepr`, `lookupRepr`, `getDefaultRepr`, `lookupReprPair`, `isReprTarget`
- `processBinding (Repr ...)` in Pipeline.hs: registers toRepr/fromRepr as implicit-param wrapper lambdas + instance lambdas keyed by `[userTypeName]` (toRepr) and `[reprTypeName]` (fromRepr)

### Step 5.3: `as` expression for explicit repr casts ✅

**Files:** Lexer.hs, Parser.hs, Surface.hs, Pipeline.hs

- Added `| ReprCast Expr Expr` to Expr; added traverseExpr and ppr cases
- `as` added as a reserved word; postfix parsing: `pExpr` tries `reserved "as" >> concreteType` after reading the base expression
- `exprToCLM env (ReprCast e (Id targetType))` resolves direction:
  - if `targetType` is a key in `reprMap` (user type) → emit `CLMIAP "fromRepr" [clmE]`
  - if `targetType` is a value in `reprMap` (repr type) → emit `CLMIAP "toRepr" [clmE]`
  - otherwise → `CLMERR` with message

### Step 5.4: Repr-aware optimizations (future)

Deferred to codegen. When the compiler knows `repr Nat as Int`:
- Pattern `| Z` compiles to `== 0`
- Pattern `| Succ(n)` compiles to `let n = x - 1 in ...`
- Operations forwarded through repr to intrinsic implementations

### Phase 5 Learnings & Implementation Notes

**Completed.** Key decisions and discoveries:

1. **Instance lambda keying**: `toRepr` is keyed by the user type name (so `CLMIAP "toRepr" [natValue]` dispatches correctly), `fromRepr` by the repr type name (so `CLMIAP "fromRepr" [intValue]` dispatches correctly).

2. **Implicit-param wrapper**: Both `toRepr` and `fromRepr` are registered as top-level implicit-param lambdas (`Lambda nm [Var "a" (Implicit Type) UNDEFINED] (PatternMatches [...]) ...`) so CLMIAP dispatch is triggered. Only the first registration is kept if multiple repr declarations define the same function name.

3. **Direction resolution at CLM time**: `ReprCast` does not desugar in `afterparse` (it only recurses into children). Direction is resolved in `exprToCLM` using `reprMap` — no ambiguity because a type cannot simultaneously be a user-type key and a repr-type value (they are distinct sets).

4. **`as` is a postfix operator**: In `pExpr`, after parsing the base expression, an optional `try (reserved "as" >> concreteType)` creates `ReprCast` if present. This keeps it lower precedence than function application.

5. **base.tl repr declaration**:
   ```tulam
   repr Nat as Int default where {
       function toRepr(n:Nat) : Int = match
           | Z -> 0
           | Succ(m) -> 1 + toRepr(m),
       function fromRepr(i:Int) : Nat = if i == 0 then Z else Succ(fromRepr(i - 1))
   };
   ```

6. **7 new tests**: toRepr(Z)=0, toRepr(Succ(Succ(Z)))=2, fromRepr(0)=Z, fromRepr(3)=Succ(Succ(Succ(Z))), cast `Succ(Succ(Z)) as Int`=2, cast `5 as Nat`=Succ(Succ(Succ(Succ(Succ(Z))))), roundtrip fromRepr(toRepr(Succ(Succ(Z))))=Succ(Succ(Z)). Total: 52 tests.

**Milestone: Representation mappings declared and usable. Bridge between user types and machine types.**

---

## Phase 6: Categorical Keywords and Morphism Composition ✅ COMPLETE

**Goal:** `algebra` and `morphism` work as refined synonyms for `structure` with additional compiler knowledge. `Convertible` morphism enables type conversions.

All steps completed across Phases 3 and 4+:
- **Step 6.1** ✅: algebra/morphism parameter count validation (Phase 3)
- **Step 6.2** ✅: `Convertible` morphism with `Convertible(Nat, Bool)` and `Convertible(Bool, Nat)` instances (Phase 3)
- **Step 6.3** ✅: Automatic morphism composition with cycle detection (Phase 4+)

**Milestone: Categorical vocabulary validated. Type conversions work through morphisms. Auto-composition derives transitive instances.**

---

## Phase 7: Higher-Kinded Types

**Goal:** Type constructors (`Maybe`, `List`) can be passed as parameters to structures. Required for Functor, Monad, Bulk, etc.

**Key discovery:** The existing dispatch mechanism *almost already works*. When `instance Functor(Maybe)` stores key `"fmap\0Maybe"`, and `fmap(f, Just(3))` is called, `Just(3)` evaluates to `CLMCON (ConsTag "Just" 1) [...]`, `lookupTypeOfConstructor "Just"` returns `"Maybe"`, and the key matches. The parser already handles `Type1`, arrow types (`a -> b`), and type application (`f(a)`).

**There is exactly one critical blocker:** `inferTypeNames` in Interpreter.hs requires ALL args to have inferable types. For `fmap(f, Just(3))`, arg `f` is a function — its type can't be inferred. So `inferTypeNames` returns `Nothing` and dispatch fails.

### Step 7.1: Fix `inferTypeNames` for partial type inference

**Files:** Interpreter.hs (lines 250-253)

Change from requiring ALL args to collecting whatever types are inferable:
```haskell
inferTypeNames env exs =
    let types = catMaybes $ Prelude.map (inferTypeFromExpr env) exs
    in if Prelude.null types then Nothing else Just types
```

For `fmap(f, Just(3))`: types = `["Maybe"]` (from second arg only). Key `"fmap\0Maybe"` matches. Safe because dispatch tries keys in order — multi-param exact, prefix, single-param fallback.

### Step 7.2: Handle `ArrowType` and `Implicit` in `exprToCLM`

**Files:** Pipeline.hs — add before catch-all:
```haskell
exprToCLM _ (ArrowType _ _) = CLMEMPTY  -- type-level, erased at runtime
exprToCLM _ (Implicit _) = CLMEMPTY     -- type-level wrapper, erased at runtime
```

### Step 7.3: Define Functor/Applicative/Monad + instances in base.tl

**Files:** base.tl

```tulam
algebra Functor(f:Type1) = { function fmap(g: a -> b, x:f(a)) : f(b) };
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a),
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f: a -> m(b)) : m(b),
    function then(x:m(a), y:m(b)) : m(b)
};

instance Functor(Maybe) = { ... };
instance Functor(List) = { ... };
instance Monad(Maybe) = { pure, bind, ap, then };
instance Monad(List) = { pure, bind, ap, then };
```

### Step 7.4: Future — Array and Bulk algebra instances

With HKT, we can define Functor/Foldable instances for Array and the Bulk algebra (deferred until Array is implemented).

**Test:**
```
> fmap(not, Just(True))                  # → Just(False)
> fmap(not, Nothing)                     # → Nothing
> bind(Just(3), \x -> Just(x))           # → Just(3)
> fmap(not, Cons(True, Cons(False, Nil)))  # → Cons(False, Cons(True, Nil))
```

**Milestone: Higher-kinded types work. Functor, Applicative, Monad defined and instantiated for Maybe and List.**

### Phase 7 Learnings

- The existing dispatch mechanism needed only a 3-line change to `inferTypeNames` — collect whatever types are inferable instead of requiring ALL args
- `ArrowType` and `Implicit` Surface AST nodes needed `CLMEMPTY` cases in `exprToCLM` to avoid hitting the error catch-all
- `then` is a reserved word (from if/then/else) — renamed monadic sequencing to `seq`
- When intrinsic dispatch finds the intrinsic but args aren't fully evaluated (fewer inferred types than args), must defer (return CLMIAP unevaluated) rather than error — allows re-evaluation once args resolve
- Extracted `dispatchInstance` helper in Interpreter.hs for reuse from both intrinsic-fallback and non-intrinsic paths
- `CLMPatternCheck` type replaced old `CLMConsTagCheck` tuples — `consTagCheckToCLM` became `patternCheckToCLM` returning `CLMCheckTag`/`CLMCheckLit`
- `pure(True)` can't dispatch without type context (no constructor arg to infer monad from) — expected without type checker

---

## Phase 8: Arrow Types and Function Type Syntax

**Goal:** Parse `a -> b` as a type, required for Functor's `fmap` signature and for expressing function types in general.

### Step 8.1: Parse arrow types

**Files:** Parser.hs

In type positions, `a -> b` should parse to a function type:

```tulam
function apply(f: Nat -> Bool, x:Nat) : Bool = f(x);
```

Parser needs `concreteType` to handle `->` as an infix type operator.

### Step 8.2: Represent function types

ArrowType Expr Expr already exists in Surface.hs. Ensure it's usable in all type positions.

**Milestone: Function types expressible in type positions. Functor's fmap signature is parseable.**

---

## Phase 9: Row-Polymorphic Effect System ✅ COMPLETE

**Goal:** Side effects tracked via row-polymorphic effect types. `action` blocks desugar into monadic `bind` chains. Effects are inferred, composable, and erased at runtime.

See `doc/EffectDesign.md` for the full design document.

**Status:** All 6 steps completed + **handler runtime added**. Effect declarations, action blocks, type checking with `TEffect Row Ty`, handlers, standard library effects (Console, FileIO, State, Exception). IO intrinsics (`putStrLn`, `putStr`, `readLine`) working via `dispatchIOIntrinsic`. 10 P12 effect tests + 7 P32 effect handler runtime tests passing.

**Effect Handler Runtime (completed):**
- `HandlerDecl` AST node gained `[Var]` params field for parameterized handlers (e.g., `RefState(init:a)`)
- `CLMHANDLE` CLM node added: carries body, effect name, let bindings, and handler ops
- Handler stack in `InterpreterState` — dynamic scoping of handler dictionaries
- Pipeline CLM conversion for `handle expr with handler` — resolves handlers, substitutes params
- Interpreter: push handler, eval body, pop; `dispatchHandlerOp` checks stack before IO intrinsics
- `dispatchEffectSequencing`: fallback for `seq`/`bind` when handler stack is active (simple function application)
- Standard library handlers: `RefState` (State), `SilentConsole` (Console), `DefaultException` (Exception), `StdFileIO` (FileIO)
- Cache version bumped from 1 to 2
- Test file: `tests/programs/P32_EffectHandlers.tl` (7 tests)

### Step 9.1: Effect Declaration Parsing (Phase A)

Add `effect`, `handler`, `handle` to reserved words. Parse effect declarations:

```tulam
effect Console = {
    function readLine() : String,
    function putStrLn(s:String) : Unit
};
```

- New AST node: `EffectDecl Name [(Name, [Expr], Expr)]`
- Store in `effectDecls :: HashMap Name EffectDecl` in Environment
- Effect operations registered as available functions

### Step 9.2: Action Block Desugaring (Phase B)

Parse `action` blocks with `<-` for monadic bind:

```tulam
action main() = {
    name <- readLine(),
    greeting = "Hello, " ++ name,
    putStrLn(greeting)
};
// Desugars to: bind(readLine(), \name -> let greeting = ... in bind(putStrLn(greeting), \_ -> pure(())))
```

- New AST nodes: `ActionBlock [ActionStmt]`, `ActionStmt = ActionBind Name Expr | ActionLet Name Expr | ActionExpr Expr`
- New pipeline pass (Pass 0.5): `ActionBlock` → nested `bind`/`let` applications
- `<-` operator parsed in action block context

### Step 9.3: Effect Type Checking (Phase C)

Add `TEffect Row Ty` to internal type representation. Reuse existing row machinery (`RExtend`, `REmpty`, `RVar`, `rowExtract`, `unifyRows`, `rowSubst`):

```tulam
Eff { console: Console, fileio: FileIO } Unit     // closed row
Eff { console: Console | r } Unit                  // open row (effect-polymorphic)
```

- Infer effect rows from function bodies (each effect operation adds its label)
- Merge effect rows via `unifyRows` when composing effectful expressions
- Open rows (`| r`) enable subsumption — fewer effects usable where more expected
- Effect rows erased in Pass 4 (CLM conversion) — zero runtime overhead

### Step 9.4: Effect Handlers (Phase D) ✅ RUNTIME COMPLETE

```tulam
handler StdConsole : Console = {
    function readLine() = intrinsic,
    function putStrLn(s) = intrinsic
};

// Parameterized handler with let bindings
handler RefState(init:a) : State = {
    let state = newRef(init),
    function get() = readRef(state),
    function put(x) = writeRef(state, x)
};

let result = handle greet() with StdConsole;
handle action { x <- get(), put(x + 1) } with RefState(0);
```

- v1: Built-in effects have implicit default handlers (just execute) ✅
- v2: Explicit `handle...with` for testing/mocking ✅
- Handler application removes one effect label from the row ✅
- Parameterized handlers with `[Var]` params on `HandlerDecl` ✅
- `CLMHANDLE` CLM node with body, effect name, let bindings, ops ✅
- Handler stack in `InterpreterState` for dynamic scoping ✅
- `dispatchHandlerOp` checks handler stack before IO intrinsic fallback ✅
- `dispatchEffectSequencing` for seq/bind in handler context ✅

### Step 9.5: Interop Effect Inference (Phase E)

Auto-assign effect labels from extern metadata namespaces:

| Extern Source | Effect Label |
|---|---|
| `System.IO.*`, `stdio.h` | `fileio: FileIO` |
| `System.Net.*`, `fetch` | `net: NetworkIO` |
| `System.Console` | `console: Console` |

Sandboxing: `import UntrustedLib with { deny NetworkIO }` — type checker rejects functions with denied effect labels.

### Step 9.6: Standard Library Effects (Phase F)

Create `lib/Effect/` modules: Console, FileIO, State, Exception, Random, NetworkIO, Async. Add intrinsic implementations for core effect operations. Update `lib/Base.tl`.

**Milestone: Full effect system works. Side effects are tracked, composable, testable, and erased at runtime. Monad transformers not needed.**

---

## Phase 10: Standard Library, Deriving, and Category/Arrow

**Goal:** Rich standard library with categorical structures and a language-native deriving mechanism.

### Step 10.1: Core algebras ✅ COMPLETE

Semigroup, Monoid, Group already exist (Phase 3). Instances for primitive types use `intrinsic`.

### Step 10.2: Reflection Primitives and Derive System ✅ COMPLETE

**Key design insight:** `deriving` is NOT compiler magic — it's pure tulam code powered by reflection primitives. The `derive { }` block lives inside algebra definitions, and `instance X(T) = derive;` instantiates it.

#### 10.2.1: Reflection Primitives (6 intrinsics in `lib/Core/Reflect.tl`)

**Value reflection** (runtime, operates on any value):
- `tag(x:a) : Int` — constructor tag index (0-based), primitives return 0
- `tagName(x:a) : String` — constructor name, primitives return type name
- `arity(x:a) : Int` — number of fields, primitives return 0
- `field(x:a, i:Int) : b` — get i-th field (0-indexed)

**Type reflection** (operates on type name strings):
- `numConstructors(T:String) : Int` — number of constructors in a sum type
- `constructorByIndex(T:String, i:Int) : a` — create nullary constructor by tag index

**Structural helpers** (pure tulam functions built on primitives):
- `structuralEq(x, y)`, `structuralCompare(x, y)`, `structuralShow(x)`, `structuralHash(x)`

Implementation: `dispatchReflectionIntrinsic` in `src/Interpreter.hs`, wired into CLMIAP dispatch chain after IO intrinsics and before array HOFs.

#### 10.2.2: Derive Blocks in Algebras

**Chosen syntax:** `derive { }` block inside algebra definitions + `instance X(T) = derive;` + `type T = ... deriving A, B;`

```tulam
algebra Eq(a:Type) = {
    function (==)(x:a, y:a) : Bool,
    function (!=)(x:a, y:a) : Bool = not(x == y),

    derive {
        function (==)(x, y:a) : Bool = structuralEq(x, y),
        function (!=)(x, y:a) : Bool = not(structuralEq(x, y))
    }
};

// Usage:
instance Eq(Color) = derive;
type Direction = North | South | East | West deriving Eq, Show;
```

**Standard algebras with derive blocks:** Eq, Ord, Show, Hashable.

**Implementation:**
- `derive`/`deriving` reserved words in Lexer.hs
- `Derive` expr variant + `structDerive` field on `StructInfo` in Surface.hs
- `parseStructMembers` custom parser handles comma-separated items + trailing derive block
- Pipeline processes `[Derive]` instances by looking up algebra's derive block and instantiating functions
- `pSumTypeWithDeriving` expands to SumType + Instance declarations

**Tests:** 38 new hspec tests (21 reflection primitives, 11 structural helpers, 8 derive system).

#### 10.2.3: (Original design — superseded by 10.2.2)

The original design proposed `deriveFunctor(Type)` compile-time functions. This was superseded by the `derive { }` block approach which keeps derive logic inside algebra definitions — cleaner and more discoverable.

```tulam
// SUPERSEDED — kept for historical reference
// This is a compile-time function: it takes a Type1 and produces an instance body
// The compiler evaluates this during Pass 1 (environment building)
function deriveFunctor(T:Type1) = {
    // For each constructor of T:
    //   - nullary constructors: pass through unchanged
    //   - fields of type 'a' (the functor param): apply g
    //   - fields of type T(a) (recursive): apply fmap(g, field)
    //   - other fields: pass through unchanged
    // Returns: function fmap(g, x) = match | pattern match cases...
};
```

The actual implementation requires compile-time evaluation of type reflection primitives. This means the interpreter needs to handle these intrinsics during Pass 1 when processing `derive` expressions. The key insight is that **this is the same mechanism as intrinsic dispatch** — the compiler already evaluates intrinsics, we just need intrinsics that inspect type structure.

#### 10.2.4: What can be auto-derived?

| Structure | Derivation rule |
|-----------|----------------|
| `Eq` | Compare constructor tags, then recursively compare fields (requires `Eq` on field types) |
| `Ord` | Compare constructor tags (by position), then lexicographic field comparison |
| `Functor` | Map function over fields of type `a`, recurse on fields of type `F(a)`, pass through others |
| `Foldable` | Collect fields of type `a`, recurse on fields of type `F(a)` |
| `Show` | Print constructor name + fields recursively |

#### 10.2.5: Natural Transformations as Library Functions

Natural transformations (e.g., `safeHead : List ~> Maybe`) don't need a keyword. They're just parametrically polymorphic functions:

```tulam
// This IS a natural transformation — the type signature says it all
function safeHead(xs:List(a)) : Maybe(a) = match
    | Nil -> Nothing
    | Cons(x, rest) -> Just(x);

// When we have a type checker, it can verify parametricity (doesn't inspect 'a')
// Until then, it's just a regular polymorphic function
```

The `~>` notation is useful documentation but provides no runtime benefit without a type checker. If desired later, `~>` can be added as a type-level operator that the type checker interprets as "parametrically polymorphic functor morphism."

### Step 10.3: Category and Arrow as library structures

```tulam
algebra Category(arr:Type2) = {
    function id() : arr(a, a),
    function compose(f:arr(b,c), g:arr(a,b)) : arr(a,c)
};

algebra Arrow(arr:Type2) extends Category(arr) = {
    function arr(f: a -> b) : arr(a, b),
    function first(f:arr(a,b)) : arr({a,c}, {b,c})
};
```

### Step 10.4: Kleisli derivation

Automatically derive `Category(Kleisli(m))` from `Monad(m)`.

### Step 10.5: SIMD types and Lane algebra

Declare SIMD primitives and the Lane algebra in standard library:

```tulam
primitive Vec2(a:Type);
primitive Vec4(a:Type);
primitive Vec8(a:Type);
primitive Vec16(a:Type);

instance Num(Vec4(Float32)) = intrinsic;
instance Num(Vec8(Float32)) = intrinsic;

algebra Lane(v:Type1) = {
    function splat(x:a) : v(a),
    function extract(xs:v(a), i:Int) : a,
    function hsum(xs:v(a)) : a requires Num(a),
    function hmin(xs:v(a)) : a requires Ord(a),
    function hmax(xs:v(a)) : a requires Ord(a),
    function blend(mask:v(Int32), xs:v(a), ys:v(a)) : v(a),
    function lanes(xs:v(a)) : Int
};

instance Lane(Vec4) = intrinsic;
instance Lane(Vec8) = intrinsic;
```

**Milestone: Full standard library. Deriving mechanism, categorical vocabulary, SIMD types, numeric hierarchy all available.**

---

## Phase 11: Type Checker (Pass 3) ✅ COMPLETE

**Goal:** Actually enforce types, universe levels, and structure laws.

### Step 11.1: Basic bidirectional type checking ✅

Implemented in new module `src/TypeCheck.hs`. Bidirectional type checking with two modes:
- **`infer`** (synthesis): given an expression, produce its type
- **`check`** (verification): given an expression and expected type, verify they match

Internal type representation (`Ty`):
- `TVar Name` — type variables (unification variables)
- `TRigid Name` — rigid type variables (quantified, cannot be unified away)
- `TCon Name` — type constructors (Int, Bool, Nat, etc.)
- `TApp Ty Ty` — type application (Maybe Int, List Bool)
- `TPi Name Ty Ty` — dependent function type (Pi type)
- `TSigma Name Ty Ty` — dependent pair type (Sigma type)
- `TId Ty Ty Ty` — identity/equality type (for PropEqT)
- `TForall Name Ty` — polymorphic type (∀a. T)
- `TRecord [(Name, Ty)]` — record type with named fields
- `TU Int` — universe type (U 0 = Type, U 1 = Type1, etc.)

Row types for structural record typing:
- `REmpty` — empty row
- `RExtend Name Ty Row` — row extension (label:type + rest)
- `RVar Name` — row variable (for polymorphism)
- `RRigid Name` — rigid row variable

Covers: literals, variables, function application, constructor application, lambda abstractions, let bindings, if/then/else, pattern matching, repr casts, record literals, record types.

### Step 11.2: Unification engine ✅

HashMap-based substitution with occurs check. Unifies:
- Type variables with concrete types
- Type constructors (must match)
- Type application (component-wise)
- Arrow/Pi types (contravariant in domain, covariant in codomain)
- Record types (field-wise)
- Row types (structural matching with extension)

### Step 11.3: Structure constraint checking ✅

`CStructure Name [Ty]` constraints emitted for implicit-parameter functions. Resolved against `instanceLambdas` in the interpreter state. When `(==)(x:Nat, y:Nat)` is called, the checker emits `CStructure "Eq" [TCon "Nat"]` and verifies an `Eq(Nat)` instance exists.

### Step 11.4: Polymorphism ✅

`TForall` for universal quantification. `instantiate` replaces bound variables with fresh unification variables. `generalize` abstracts over free type variables with deduplication (no duplicate `∀` bindings).

### Step 11.5: Pipeline integration ✅

`typeCheckPass` inserted as Pass 3 in the compilation pipeline (between case optimization and CLM conversion). Pipeline order:
- Pass 0: After-parser desugaring
- Pass 1: Environment building
- Pass 2: Case optimization
- **Pass 3: Type checking** (new)
- Pass 4: CLM conversion
- Pass 5: Code generation

**Permissive mode (default):** Type errors are reported as warnings, not fatal errors. The `strictTypes` flag in `InterpreterState` can be set to make type errors fatal. This allows the existing pipeline to continue working while the type checker matures.

### Step 11.6: Record types in Surface AST ✅

Added `RecordLit [(Name, Expr)]` and `RecordType [(Name, Expr)]` to the Surface AST (`Expr`). Parsed with `{name = val}` for literals and `{name:Type}` for type annotations. These integrate with the row-polymorphic type checker.

### Phase 11 Learnings & Implementation Notes

**Completed.** Key decisions and discoveries:

1. **Permissive by default**: Making type errors warnings (not fatal) was essential for incremental adoption. The existing test suite (95 tests) continued passing while the type checker was added. The `strictTypes` flag exists for future strict mode.

2. **Row polymorphism for records**: Record types use row types (`REmpty`, `RExtend`, `RVar`, `RRigid`) for structural subtyping. `{x:Int, y:Bool}` is a closed record; open records use row variables for extensibility.

3. **Constraint generation vs. resolution**: Structure constraints (`CStructure`) are generated during type checking and resolved against the existing instance maps. This reuses the instance infrastructure from Phase 1 rather than building a separate constraint solver.

4. **Surface AST additions**: `RecordLit` and `RecordType` were added to `Expr` in `Surface.hs` for record literal construction and record type annotations. These provide first-class record support beyond the `record` declaration sugar.

5. **Test suite growth**: 141 tests total (up from 95), covering type inference, type checking, constraint resolution, record types, and error reporting. All passing.

**Milestone: Type errors are caught at compile time (as warnings in permissive mode, fatal in strict mode). Bidirectional type checking with row polymorphism and structure constraint resolution.**

---

## Phase 9.5: Intrinsic Completeness and Runtime Foundations ✅ COMPLETE

**Goal:** Fill the gap between declared `= intrinsic` instances and actual Haskell backing. Make the interpreter capable of running real programs with I/O, string manipulation, and error handling.

**Completed:** All steps implemented. 47 new tests (387 total). Key additions:

### What was implemented:

1. **Char foundation**: Parser support for char literals (`'A'`, `'\n'`), Eq/Ord intrinsics, `lib/Numeric/Char.tl`
2. **Show intrinsics**: All primitive types (Int, Float64, String, Char, Int8-64, UInt8-64, Float32, Byte). Show(String) uses Haskell-style quotes.
3. **Bounded intrinsics**: minBound/maxBound for all fixed-width types, Char, Byte
4. **Enum intrinsics**: succ/pred/toEnum/fromEnum for Int and Char
5. **Hashable intrinsics**: hash for Int (identity), String (FNV-1a), Char (ord)
6. **Extended string ops**: charAt, substring, indexOf, trim, toUpper, toLower, startsWith, endsWith, replace, parseInt, parseFloat. New `StringExt` algebra + instance.
7. **Array ops (pure)**: set, push, arrayConcat, reverse, range. New `ArrayOps` algebra + instance.
8. **error/panic**: `error(msg)` → CLMERR, dispatched via `dispatchIOIntrinsic`
9. **FileIO runtime**: readFile, writeFile, appendFile, fileExists in `dispatchIOIntrinsic`
10. **CLMAPP IO fallback**: `evalCLM` for CLMAPP tries `dispatchIOIntrinsic` on unresolved function IDs
11. **Parser**: `[1,2,3]` now creates `ArrayLit` (→ `CLMARRAY`) instead of `Lit (LList ...)`

### Known dispatch limitations (resolved in Phase 10.3):
- `toEnum` for Char/Bool/Ordering: **resolved** — type-directed dispatch uses return type annotation from CLM conversion
- `range` for Array: **resolved** — same mechanism (return type hint enables dispatch to correct intrinsic/instance)
- `pure(Nil)` in Maybe context: **resolved** — CLMTYPED wrapper carries return type, enabling correct monad dispatch

### Files modified:
- `src/Intrinsics.hs` — charEntries, showEntries, boundedEntries, enumEntries, hashEntries, stringExtEntries, expanded arrayEntries. Exports `boolToCLM`.
- `src/Interpreter.hs` — error, readFile, writeFile, appendFile, fileExists in dispatchIOIntrinsic. CLMAPP IO fallback.
- `src/Parser.hs` — charVal parser, ArrayLit for `[...]`, char in pInsideLeftPattern
- `lib/Numeric/Char.tl` — **new** (Char Eq/Ord instances)
- `lib/Algebra/StringExt.tl` — **new** (StringExt algebra)
- `lib/Collection/Array.tl` — ArrayOps algebra + instance
- `lib/Numeric/String.tl` — StringExt instance
- `lib/Instances/Show.tl` — Show for fixed-width types
- `lib/Base.tl` — exports StringExt, Numeric.Char
- `tests/programs/P13_Intrinsics.tl` — **new** (32 test functions)
- `test/Spec.hs` — P13 describe block (47 tests)

---

## Phase 9.7: Managed Mutability (Ref/MutArray)

**Goal:** Enable algorithms that need mutable state without exposing raw memory.

**Depends on:** Phase 9 (effect system) — mutation operations return `Eff {state: State} a`.

### Step 9.7.1: Ref Primitive

```tulam
primitive Ref(a:Type);

// All operations are effectful:
function newRef(x:a) : Eff {state: State} Ref(a) = intrinsic;
function readRef(r:Ref(a)) : Eff {state: State} a = intrinsic;
function writeRef(r:Ref(a), x:a) : Eff {state: State} Unit = intrinsic;
function modifyRef(r:Ref(a), f:a -> a) : Eff {state: State} Unit = intrinsic;
```

Interpreter: `Ref` backed by `IORef` via the Haskell runtime. `CLMREF Int` in CLM (index into a ref table in `InterpreterState`).

### Step 9.7.2: MutArray Primitive

```tulam
primitive MutArray(a:Type);

function newMutArray(n:Int, x:a) : Eff {state: State} MutArray(a) = intrinsic;
function readAt(xs:MutArray(a), i:Int) : Eff {state: State} a = intrinsic;
function writeAt(xs:MutArray(a), i:Int, x:a) : Eff {state: State} Unit = intrinsic;
function freeze(xs:MutArray(a)) : Eff {state: State} Array(a) = intrinsic;
function thaw(xs:Array(a)) : Eff {state: State} MutArray(a) = intrinsic;
```

### Step 9.7.3: Raw Memory (native backend only, future)

Only needed for x86/LLVM backend. Not needed for interpreter or JS/.NET targets.

```tulam
// Only available with `import unsafe Memory;`
primitive Ptr(a:Type);
function malloc(n:Int) : Eff {mem: UnsafeMemory} Ptr(Byte) = intrinsic;
function free(p:Ptr(a)) : Eff {mem: UnsafeMemory} Unit = intrinsic;
function peek(p:Ptr(a)) : Eff {mem: UnsafeMemory} a = intrinsic;
function poke(p:Ptr(a), x:a) : Eff {mem: UnsafeMemory} Unit = intrinsic;
```

Gated by `UnsafeMemory` effect — sandboxable at module boundaries.

---

## Phase 13: First-Class Class System

**Goal:** Native OOP classes with single inheritance, dynamic dispatch, sealed classes, and transparent extern class subclassing. 1-1 mapping to .NET/JS/C++ classes in codegen. See `doc/ClassDesign.md` for the full design.

### Phase 13.1: Core Class System (Milestone 1)

**New keywords:** `class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super`

**Files changed:**
- `Lexer.hs` — reserve keywords
- `Surface.hs` — `ClassDecl Lambda ClassInfo`, `ClassInfo`, `ClassModifier`, `MethodModifier` types; add `lamMod` to `Lambda` (defaults `MNone`); update `traverseExpr`, `ppr`
- `Parser.hs` — `pClassDecl` production (fields in header, methods in body, extends, implements)
- `State.hs` — `ClassMeta` data type, `classDecls :: NameMap ClassMeta` + `classTagCounter :: !Int` in `Environment`; helpers: `lookupClass`, `isSubclassOf`, `lookupParentMethod`, `resolveDot`, `allocClassTag`
- `Pipeline.hs` Pass 1 — `processBinding (ClassDecl ...)`: resolve parent hierarchy, build `ClassMeta` with merged fields/methods, register constructor + type, update parent's `cmChildren`
- `CLM.hs` — `CLMMCALL CLMExpr Name [CLMExpr]`, `CLMSCALL CLMExpr Name [CLMExpr]`, `CLMNEW Name [CLMExpr]` nodes; update `traverseCLMExpr`, `descendCLM`, `freeVarsCLM`, `ppr`
- `Pipeline.hs` Pass 4 — `exprToCLM` rules: `App (RecFieldAccess ...) args` on class type → `CLMMCALL`; `Dog.new(args)` → `CLMNEW`; `super.method(args)` → `CLMSCALL`; field access on class → `CLMFieldAccess` with resolved index
- `Interpreter.hs` — `evalCLM` for `CLMNEW` (build `CLMCON`, validate not abstract), `CLMMCALL` (tag-based dynamic dispatch via `cmMethods`), `CLMSCALL` (parent method lookup)
- `CaseOptimization.hs` — iterate over `classDecls` method CLMLams alongside `topLambdas`
- `CLMOptimize.hs` — update `descendCLM` for new nodes (treat as opaque like `CLMIAP`)

**Object representation:** `CLMCON (ConsTag "Dog" tag) [field0, field1, ...]` — no vtable in object. Fields laid out parent-first, own-last. Method dispatch via `classDecls` lookup on runtime ConsTag.

**Test:** `tests/programs/P20_Classes.tl` — class declaration, construction, field access, method call, single inheritance, override, super calls, abstract method error

### Phase 13.2: Sealed Classes + Implements + Extern Plumbing (Milestone 2) ✅ DONE

**Completed.** 488 tests passing.

**Sealed classes:**
- `cmSourceFile :: String` added to `ClassMeta` — tracks which file declared each class
- `isSealedClass`, `getAllSubclasses` helpers in `State.hs`
- Sealed validation in `processBinding (ClassDecl ...)` step 8.5: child of sealed parent must be in same source file
- `checkSealedExhaustiveness` in `CaseOptimization.hs`: warns when sealed class pattern match is missing branches
- **Test:** `tests/programs/P21_SealedClasses.tl` (5 tests)

**Implements clause:**
- `generateClassAlgebraInstance` in `Pipeline.hs`: matches class methods to algebra function names, registers as instance lambdas
- Falls back to algebra's derive block if no matching methods found
- Reuses `propagateToParent` + `composeMorphismInstances` for parent structure inheritance
- **Test:** `tests/programs/P22_ClassAlgebras.tl` (5 tests)

**Extern class plumbing:**
- `targetImports :: [(ModulePath, Name)]` in `Environment` — stores `import ... target ...` declarations
- `processBinding (Import path _ (Just tgt), _)` stores target imports
- `src/MetadataResolver.hs` — stub interface for codegen-time resolution (no-op in interpreter)
- Extern classes not available in interpreter (by design); will be populated by codegen backends
- **Test:** `tests/programs/P23_ExternClasses.tl` (3 tests)

### Phase 13.3: Type-Test Patterns + Downcast (Milestone 3) ✅ DONE

- Type-test patterns: `match a | Dog(_, _, breed) -> ... | Cat() -> ... | _ -> ...` — works via function-level pattern matching (class constructors registered in constructors map with ConsTag)
- Safe downcast via `as`: `obj as ClassName` → `Maybe(ClassName)`. Extends `ReprCast` CLM conversion to emit `CLMIAP "__downcast" [CLMLIT targetClass, obj]` when target is a class. `dispatchDowncastIntrinsic` in Interpreter.hs checks `isSubclassOf` and returns `Just(obj)` or `Nothing`
- TC: `infer (ReprCast e tp)` returns `TApp (TCon "Maybe") [targetTy]` when target is a class type
- **Test:** `tests/programs/P24_ClassSubtyping.tl` (9 tests: pattern matching, subtype passing, safe downcast success/failure)

### Phase 13.4: Extern Class Resolution (Milestone 4)

- Implement actual metadata resolvers in `MetadataResolver.hs` for each target
- .NET: read assembly metadata via Mono.Cecil → auto-populate `ClassMeta` in `classDecls`
- JS: parse `.d.ts` TypeScript declaration files
- Native: parse C/C++ headers via libclang
- Subclassing extern classes works automatically (processBinding already handles it)
- Super calls across tulam/extern boundary

### Phase 13.6: Type Checker Integration ✅ DONE

**Completed:**
- `tcWarnOrFail` helper respects `tcMode` (strict=fatal, relaxed=warning)
- Error context tracking (`WithContext`, `tcWithContext`) for better error messages
- `TForall` alpha-renaming in unification (proper structural comparison with rigid vars)
- `TRigid` unification case (same-name rigid vars unify)
- Catch-all warnings in `infer`, `exprToTy`, unbound var emission
- Reduced `exprToTy` catch-all surface (declaration forms, compound App, Function, ConTuple)
- Recursive function self-binding in `inferLambda`
- `checkTopLevel (ClassDecl ...)`: field type validation, method body checking with `self` in scope
- Override signature checking (type comparison of overridden methods vs parent)
- Constructor arity checking for `ClassName.new(args)` (via `lookupClass`/`cmAllFields`)
- Abstract class instantiation prevention
- Implements contract validation (checks algebra method list against class methods, respects derive blocks)
- **Subtype checking**: `subtype :: Ty -> Ty -> TC ()` walks class hierarchy via `isSubclassOf`. `SubtypeMismatch` error variant. Wired into `check` subsumption fallback (unify first, then subtype).
- **Implicit upcast**: `check` accepts subclass where superclass expected — no runtime coercion needed (Dog and Animal share CLMCON representation)
- **Class downcast type inference**: `infer (ReprCast e tp)` returns `Maybe(ClassName)` when target is a class
- 628 tests passing

### Key Dependencies
- Phase 10 (Reflection + Derive) — needed for `derive` on classes
- Phase 11 (Type Checker) — needed for subtype judgments, sealed exhaustiveness
- Module System — needed for sealed class validation (same-module constraint)

---

## Phase 14: Interop and Codegen Backends (Future)

**Goal:** Target-specific code generation using the class system for 1-1 OOP mapping.

### Phase 14.1: .NET Backend
- Assembly metadata reader (Mono.Cecil) → `ClassMeta` entries for imported .NET types
- CLM → C# source emission (intermediate) or direct IL emission
- `CLMNEW` → `new`, `CLMMCALL` → virtual call, `CLMSCALL` → `base.method()`
- Type mapping table (tulam Int → System.Int64, Maybe → Nullable, etc.)
- Null → Maybe wrapping, exception → Either wrapping at boundaries

### Phase 14.2: JavaScript Backend
- `.d.ts` parser → `ClassMeta` entries
- CLM → JS source emission (ES6 classes)
- Module system (ES modules / CommonJS)
- Structural subtyping via row polymorphism for duck-typed JS

### Phase 14.3: C++ Backend
- Header parser (libclang) → `ClassMeta` entries
- CLM → C++ source emission
- Memory management strategy (unique_ptr / shared_ptr / GC)
- Closure conversion for lambdas passed as function pointers
- Monomorphization for generics

See `doc/InteropDesign.md` for the full interop design (type mapping, null handling, exception handling, callback interop, generics mapping, target-specific code blocks).

---

## Phase 12: GPU Compute Model (Future)

**Goal:** Transparent GPU acceleration for bulk operations on primitive types.

See `doc/PrimitiveDesign.md` §8 for the type-system-level design. A separate `doc/GPUDesign.md` document will detail:

- Memory model: CPU↔GPU data transfer, caching, aliasing rules
- Kernel generation: how `fmap` over `Array(Float32)` becomes a GPU kernel
- Law-driven optimization: how algebraic laws license parallelism transformations
- `@device` and `@parallel` annotation processing
- Target backends: CUDA, Metal, Vulkan Compute, WebGPU

### Key Dependencies
- Phase 4 (primitive types, intrinsics) — needed for GPU-friendly types
- Phase 7 (HKT) — needed for Functor/Bulk algebra definitions
- Phase 10 (SIMD, Lane algebra) — needed for CPU-side vectorization
- Phase 11 (type checker) — needed to verify GPU compatibility constraints

---

## Dependency Graph (Updated)

```
Phase 1: Structures Work             ✅ COMPLETE
    |
    v
Phase 2: Language Completeness       ✅ COMPLETE (if/else, let/in, records)
    |
    v
Phase 3: extends + algebra/morphism  ✅ COMPLETE
    |
    v
Phase 4: Primitives + Intrinsics    ✅ COMPLETE (primitive, intrinsic, Num, 3+4=7)
    |
    +---> Phase 4+: Parallel Features  ✅ COMPLETE (lambdas, ADTs, tests, composition)
    |
    v
Phase 5: Repr System                ✅ COMPLETE (repr, as, representation mappings)
    |
    v
Phase 6: Convertible Morphism       ✅ COMPLETE (type conversions, morphism composition)
    |
    v
Phase 7: Higher-Kinded Types        ✅ COMPLETE (Functor, Applicative, Monad for Maybe/List)
    |
    +---> Phase 8: Arrow Types       ✅ COMPLETE (a -> b in type positions)
    |         |
    |         v
    +---> Phase 9: Effect System      (effects, action desugaring, handlers, interop)
              |
              v
          Phase 10: Std Library      ✅ PARTIAL (Reflection + Derive DONE; Category, Arrow remaining)
              |
              v
          Phase 11: Type Checker     ✅ COMPLETE (bidirectional, row polymorphism, permissive mode)
              |
              v
          Module System              ✅ COMPLETE (module/import/export/open/private/opaque)
              |
              v
          Primitive Expansion        ✅ COMPLETE (Int8-64, UInt8-64, Float32, Array, Vec2/4/8/16)
              |
              v
          Phase 13: Class System     (first-class OOP: classes, inheritance, extern subclassing)
              |
              v
          Phase 14: Interop + Codegen (metadata resolver, .NET/JS/C++ backends)
              |
              v
          Phase 12: GPU Compute      (requires codegen backend + SIMD activation)
```

Phase 7 (HKT / Kinds) is complete. Functor, Applicative, Monad work for Maybe and List. Phase 8 (Arrow Types) is complete — `a -> b` parses in all type positions, is represented as `ArrowType Expr Expr` in the AST, and is erased to `CLMEMPTY` at CLM conversion (type-level only). Arrow types are used in Functor/Monad signatures in base.tl. Phase 11 (Type Checker) is complete — bidirectional type checking with row polymorphism, permissive mode by default. Module System is complete — `module`, `import`, `open`, `export`, `private`, `opaque` keywords with `ModuleSystem.hs` providing path resolution, dependency graph construction, cycle detection, and topological sort. `lib/` directory contains modular stdlib layout. Primitive Type Expansion is complete — all integer widths (Int8-64, UInt8-64), Float32, Array(a), Vec2/4/8/16(a) with full intrinsic coverage via refactored generic helpers. 226 tests. Phase 9 (Row-Polymorphic Effect System) is the next step — see `doc/EffectDesign.md` for full design.

### Algebraic Hierarchy Refactor (Post-Phase 7)

The numeric hierarchy was refactored to use proper mathematical foundations with **multiple extends**:
- Generic chain: Semigroup→Monoid→Group→AbelianGroup
- Additive chain: AdditiveSemigroup(+)→AdditiveMonoid(zero)→AdditiveGroup(negate,-)→AdditiveAbelianGroup
- Multiplicative chain: MultiplicativeSemigroup(*)→MultiplicativeMonoid(one)→MultiplicativeGroup(recip,/)
- Composed (multiple extends): Semiring extends AdditiveMonoid+MultiplicativeMonoid, Ring extends AdditiveAbelianGroup+MultiplicativeMonoid, CommutativeRing extends Ring, EuclideanDomain extends CommutativeRing, Field extends CommutativeRing+MultiplicativeGroup, Floating extends Field

New instances: Semiring(Nat), Group(Int), AbelianGroup(Float64). Nat now supports `+` and `*` operators. Interpreter fixed to re-evaluate `applyCLMLam` results (was returning unevaluated nested calls). 90 tests, all passing.

### Vector Literal Design Note (`<1, 2, 3>` syntax)

The `<1, 2, 3>` angle-bracket syntax currently parses to `Lit (LVec [exprs])` but has no CLM representation or interpreter support. The design decision: **vector literals should be overloaded** (like numeric literals), with context determining whether they become:

1. **Array literal** — `Array(a)`, heap-allocated, variable-length bulk data
2. **SIMD vector literal** — `Vec4(Float32)` etc., register-width, for math-heavy inner loops

This requires the type checker (Phase 11) to resolve. Implementation deferred until then. For now, `LVec` is parsed and stored but not evaluable.

---

## Phase 14: Pure String Library + Algebra-Based Interop Pattern (COMPLETE)

**Goal:** Build a pure-tulam string library that replaces primitive String with a proper byte-array-backed Str type, demonstrating the algebra-based interop pattern where all string operations flow through algebras rather than hardcoded intrinsics.

**Test count:** 669 (was 628 before Phase 14)

### Phase 14.1: Pure Tulam String Library (COMPLETE)

New standard library modules for string processing:
- `lib/Algebra/StringLike.tl` — Universal string contract: 5 core methods + ~25 default methods derived from them
- `lib/String/Utf8.tl` — UTF-8 codec implemented in pure bitwise math (no intrinsics)
- `lib/String/Str.tl` — `Str` record type (byte array + cached length)
- `lib/String/Instances.tl` — `StringLike(Str)` 5 core method implementations
- `lib/String/Ops.tl` — `charCount`, `nthChar` (generic StringLike operations)
- `lib/String/Algebras.tl` — `Eq`/`Ord`/`Semigroup`/`Monoid` instances for `Str`
- `lib/String/FromString.tl` — `FromString` algebra + `Str` instance
- `lib/Repr/Char.tl` — `repr Char as Int` (disabled: key collision with `Repr.Nat`)
- `tests/programs/P25_Strings.tl` — 28 tests

**Interpreter fixes:**
- `dispatchInstance` evaluation: found-instance branches must `evalCLM` results (not catch-all — `CLMTYPED` needs unevaluated form)
- Multi-type single-param dispatch: must try all arg types, not just first (algebra methods may have non-algebra-type first params)
- Byte intrinsics added for low-level operations

### Phase 14.2: FromString Algebra + Literal Desugaring (COMPLETE)

String literal desugaring transforms `"hello"` into `Str` constructor calls at compile time:
- `src/Pipeline.hs` — String literal desugaring pass (Pass 0.25): `"hello"` becomes `Str([0x68,...], 5)`
- `src/Interpreter.hs` — IO bridge: `putStrLn`/`putStr` decode `Str` byte arrays to Haskell strings
- `src/State.hs` — `newStrings` flag in `CurrentFlags`
- `app/Main.hs` — `:s newstrings on/off` toggle
- `src/ModuleSystem.hs` — Pass 0.25 integrated into pipeline
- `tests/programs/P26_StringDesugar.tl` — 10 tests

**Design decision:** String literal desugaring produces `Str` constructor directly (not `fromStringLiteral`) to avoid type-directed dispatch complexity at this stage.

### Phase 14.3: Parameterized Repr (COMPLETE)

Extends the repr system to support parameterized types (e.g., `repr Array(Int) as ...`):
- `src/Parser.hs` — `pReprUserType` allows type expressions like `Array(Int)`
- `src/Surface.hs` — Repr AST: `Name` changed to `Expr` for user type position
- `src/State.hs` — `mkReprKey` builds qualified keys (`Array\0Int`)
- `src/Pipeline.hs` — `processBinding` uses `mkReprKey`, `exprToCLM` `ReprCast` uses `mkReprKey`
- `tests/programs/P27_ParamRepr.tl` — 3 tests

**Key insight:** Parameterized repr uses qualified keys with `\0` separator (same pattern as instance keys).

### Phase 14.4: Documentation (COMPLETE)

- `doc/InteropPattern.md` — New design document describing the algebra-based interop pattern
- `doc/ImplementationPlan.md` — This phase added
- `doc/LanguageReference.md` — Updated with new string library and parameterized repr

### Key Learnings

- **dispatchInstance must evalCLM results** in found-instance branches (not catch-all — `CLMTYPED` needs unevaluated form for type-directed dispatch)
- **Single-param dispatch must try all arg types**, not just the first argument (algebra methods may have non-algebra-type first params, e.g., `charAt(s:Str, i:Int)`)
- **String literal desugaring produces Str constructor directly** rather than going through `fromStringLiteral` to avoid type-directed dispatch complexity
- **Parameterized repr uses qualified keys** with `\0` separator, following the same pattern as multi-param instance keys

### Key Dependencies
- Phase 4 (primitive types, intrinsics) — byte-level operations
- Phase 5 (repr system) — parameterized repr extension
- Phase 9.7 (managed mutability) — `MutArray` for efficient byte array building
- Phase 10 (reflection + derive) — derive blocks for `Eq`/`Ord`/`Show` on `Str`

---

## Phase 15: Operator Fixity and Precedence ✅ COMPLETE

Replaced the hard-coded 3-level `buildExpressionParser` with a Pratt parser and `infixl`/`infixr`/`infix` fixity declarations.

### What was done:
- **Surface.hs**: Added `Assoc`, `OperatorFixity` types; `FixityDecl` expr constructor
- **State.hs**: Added `fixityTable` to `Environment`, `defaultFixities`, `lookupFixity`/`addFixity` helpers; updated `Binary Environment`, `mergeEnvironment`, `initialEnvironment`
- **Lexer.hs**: Reserved `infixl`, `infixr`, `infix`
- **Parser.hs**: Pratt parser (`prattExpr`/`prattLoop`/`pPrefixExpr`), `pFixityDecl`; removed `buildExpressionParser`
- **Pipeline.hs**: `processBinding` case for `FixityDecl` (env build + cache restore)
- **Standard library**: Fixity declarations in `Algebra/Additive.tl`, `Multiplicative.tl`, `Eq.tl`, `Ord.tl`, `Bits.tl`, `Lattice.tl`
- **Tests**: 16 new tests (P33_Fixity.tl + parser + interactive), total 736

### Default fixity table:
| Prec | Assoc | Operators |
|------|-------|-----------|
| 9 | left | (unknown ops) |
| 7 | left | `*`, `/` |
| 6 | left | `+`, `-` |
| 5 | right | `++` |
| 4 | none | `==`, `!=`, `<`, `>`, `<=`, `>=` |
| 3 | right | `.&.`, `/\` |
| 2 | right | `.\|.`, `\/` |

---

## Effort Estimates (Updated)

| Phase | Status | Complexity | Key Challenge |
|-------|--------|-----------|--------------|
| 1 | ✅ COMPLETE | Medium-High | Instance processing and CLMIAP evaluation |
| 2 | ✅ COMPLETE | Low-Medium | Parser additions + desugaring |
| 3 | ✅ COMPLETE | Medium | Inheritance resolution, extends, value decls |
| 4 | ✅ COMPLETE | Medium | Intrinsic registry, CLMIAP dispatch, prelude.tl |
| 4+ | ✅ COMPLETE | Medium | Lambdas, ADTs, tests, morphism composition |
| 5 | ✅ COMPLETE | Low-Medium | Parser for repr syntax, `as` expression |
| 6 | ✅ COMPLETE | Low | Convertible morphism, composition (mostly library code) |
| 7 | ✅ COMPLETE | Low | Only needed 3-line fix to inferTypeNames + 2-line exprToCLM addition |
| 8 | ✅ COMPLETE | Medium | Arrow type parsing interacts with expression parsing |
| 9 | Planned | Medium | Desugaring is mechanical but needs correct scoping |
| 10 | Partial | Low-Medium | SIMD stubs done, Category/Arrow still needed |
| 11 | ✅ COMPLETE | Very High | Bidirectional checking, row polymorphism, constraint resolution |
| Module System | ✅ COMPLETE | High | Module resolution, dependency graph, cycle detection, topological sort, visibility |
| Prim Expansion | ✅ COMPLETE | High | 15+ new primitive types, refactored intrinsic registry, numeric conversions, 226 tests |
| 13 | Planned | High | Class hierarchy, dynamic dispatch, extern integration |
| 14 | Future | Very High | Codegen backends (.NET/JS/C++), metadata readers |
| 12 | Future | Very High | GPU codegen, memory model, kernel fusion |
