# Core Language Design — Universe-Polymorphic CoC

This document describes the core language enhancements for tulam's type system,
covering Level polymorphism, GADT support, full Sigma/Pi types, and the path
toward a self-hosting type checker.

## Architecture Summary

tulam's core is built on two primitives: **tuples** (flat n-ary products) and
**lambdas** (functions). At the CLM (Core List Machine) level, both sums and
products are `CLMCON tag [fields]` — a flat n-tuple with a discriminating tag.
There is no distinction between sums and products at the core level; the
difference is purely surface syntax.

## Universe Hierarchy

```
Level = LConst Int | LVar Name | LSucc Level | LMax Level Level

U(0) = Type      -- types of values (Int, Bool, Maybe(Int))
U(1) = Type1     -- types of type constructors (Maybe : Type -> Type)
U(2) = Type2     -- kinds of kind constructors
...

Cumulativity: U(n) ≤ U(m) when n ≤ m
```

**Level polymorphism** (defined in `Surface.hs`):
- `LConst n` — concrete level
- `LVar name` — level variable (for universe-polymorphic definitions)
- `LSucc l` — successor level
- `LMax l1 l2` — maximum of two levels
- Smart constructors `levelSucc`, `levelMax` normalize concrete levels eagerly
- `levelLeq`, `levelEq` return `Maybe Bool` (Nothing when variables are involved)
- `substLevel` substitutes level variables

**Cumulativity in type checker** (`TypeCheck.hs`):
- `unify' (TU l1) (TU l2)` succeeds when `levelLeq l1 l2 == Just True`
- Functions defined at level 0 automatically work at type level (level 1)

## Pi Types (Dependent Functions)

```
Pi (Maybe Name) Expr Expr

-- Non-dependent: Int -> Bool  ≡  Pi Nothing (Id "Int") (Id "Bool")
-- Dependent:     (n:Nat) -> Vec(a, n)  ≡  Pi (Just "n") (Id "Nat") (App (Id "Vec") [Id "a", Id "n"])
```

- Backward compat: `pattern ArrowType a b = Pi Nothing a b`
- Type checker: `TPi (Maybe Name) Ty Ty` with proper shadowing in `substTyVar`
- Alpha-renaming in unification: `TPi (Just n1)` vs `TPi (Just n2)` creates fresh rigid var
- Dependent application: `inferApp (TPi (Just name) paramTy retTy)` substitutes bound var

## Sigma Types (Dependent Pairs / Telescopes)

```
TSigma (Maybe Name) Ty Ty

-- Non-dependent pair: (Int, Bool)  ≡  TSigma Nothing (TCon "Int") (TCon "Bool")
-- Dependent:          (x:Nat, Vec(a,x))  ≡  TSigma (Just "x") (TCon "Nat") (TApp ...)
```

At runtime, tulam uses **flat n-tuples** (CLMCON with i-indexing), not nested
binary pairs. This gives O(1) field access. The nested `TSigma` representation
is only at the type level; it erases to flat tuples at CLM conversion.

## GADT Support

GADTs (Generalized Algebraic Data Types) allow constructors to specify return
types more specific than the parent type:

```
type Vec(a:Type, n:Nat) =
    VNil : Vec(a, Z)
  + VCons * head:a * tail:Vec(a, n) : Vec(a, Succ(n));
```

### Parser Support
- `pConstructor` parses optional `: ReturnType` after constructor args
- Constructor's `Lambda.lamType` stores the GADT-specific return type
- Default (non-GADT) return type is the parent type applied to its params

### Type Checker Support

**Constructor inference** (`infer (ConTuple ...)`):
1. Look up the parent sum type from the constructor's return type
2. Instantiate type params with fresh unification variables
3. Unify param types (with fresh vars) against actual arg types
4. Return the instantiated GADT return type (now partially resolved)

**Pattern match refinement** (`gadtRefine` in `TypeCheck.hs`):
1. Extract constructor tags from `ExpandedCase` pattern checks
2. Look up the constructor and its parent sum type
3. Instantiate parent's type params with fresh unification vars
4. Convert the constructor's GADT return type using the fresh vars
5. Unify the instantiated return type with the scrutinee's type
6. Extract refinements: type param name → resolved type
7. Apply refinements to `varTypes` in the branch body via `applyGADTRefinements`

**Example**: Matching `VNil` against scrutinee `v : Vec(a, n)`:
- VNil's return type: `Vec(a, Z)` (instantiated: `Vec(?0, Z)`)
- Unify with `Vec(a, n)` → `?0 = a`, plus `Z` unified with `n`
- Refinement: `n → Z` (applied in branch body)

### Runtime
No runtime overhead — pattern matching is pure tag comparison at CLM level.
GADT refinement is entirely compile-time.

## Type-Level Normalization

`normalizeTy` in `TypeCheck.hs` evaluates type-level function applications using
`evalCLMPure` from `CLM.hs`. This reuses the same evaluation logic as runtime
(applyCLMLam, resolveCases), just without IO/effects.

Key: `lambdaToCLMLam`/`exprToCLMTC` convert Surface AST to CLM on-the-fly during
type checking (since clmLambdas don't exist yet at Pass 3).

## Type Constructors as Type-Level Functions

Parameterized types are registered as type-level lambdas in `topLambdas` during
Pass 1 (environment building). For example:

```
type Maybe(a:Type) = Nothing + Just * val:a;
```

Registers `Maybe` as a lambda: `Maybe(a:Type) : Type = Maybe(a)` in topLambdas.
This enables kind checking via `evalCLMPure`/`normalizeTy`.

## Algebraic Structure of Types

At each universe level, types form a semiring:
- `(+)` = coproduct/sum (disjoint union)
- `(*)` = product/telescope (dependent pair)
- `(→)` = exponential/Pi (dependent function)
- `0` = Void (empty type)
- `1` = Unit (singleton type)

## Self-Hosting Type Checker (Future)

The long-term goal is to express the type checker as a tulam program itself.
This requires:
- `Ty(l:Level) : U(l+1)` — universe-indexed type representation
- `Expr : Type` — expression representation as a tulam data type
- Quote/reflect mechanism between meta-level and object-level
- Full evaluation of type-level functions at arbitrary universe levels (already working via `evalCLMPure`)

---

## Design Notes: Future Enhancements

### Positivity Checking (Planned)

**Purpose**: Ensure inductive type definitions are well-founded by checking that
the type being defined only appears in strictly positive positions in constructor
arguments.

**Why needed**: Without positivity checking, one can define non-terminating types:
```
type Bad = MkBad(f: Bad -> Bool);  // 'Bad' in negative position
```

**Approach**: During Pass 1 (environment building), after parsing a sum type,
check each constructor's parameter types. The type being defined must NOT appear:
- As the domain of a function type (negative position)
- Under an odd number of function arrows

**Implementation plan**:
1. Add `checkPositivity :: Name -> [Lambda] -> Either String ()` in Pipeline.hs
2. Walk each constructor param type, tracking polarity (positive/negative)
3. Report error if the type name appears in negative position
4. Can be a warning initially (consistent with permissive mode)

### Termination Checking (Planned)

**Purpose**: Ensure recursive functions terminate, which is required for a
consistent type theory (otherwise `U(n)` doesn't mean what it should).

**Why needed**: Without termination checking, any type can be inhabited:
```
function loop() : Void = loop();  // non-terminating, inhabits Void
```

**Approach**: Structural recursion checking — verify that recursive calls are
made on structurally smaller arguments.

**Implementation plan**:
1. During type checking (Pass 3), identify recursive functions
2. For each recursive call, check that at least one argument is structurally
   decreasing (a subterm of the corresponding parameter)
3. Use the constructor structure: matching `Succ(n)` then calling `f(n)` is
   structurally decreasing
4. Can be a warning initially; strict mode makes it fatal
5. Advanced: sized types or well-founded recursion for more expressiveness

### Explicit Level Polymorphism (Planned — Option C)

**Purpose**: Allow functions to be explicitly polymorphic over universe levels.

```
function id[l:Level](a:U(l), x:a) : a = x;
```

**Implementation plan**:
1. Add `TLevel` constructor to Ty (done)
2. Add `LVar` to Level (done)
3. Parse level parameters in function signatures (surface language change)
4. Instantiate level variables during type application
5. Level constraint solving during unification
