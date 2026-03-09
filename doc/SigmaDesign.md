# Sigma Types & Telescopes Design for tulam

## Motivation

tulam's type system is built on two primitives: **tuples** (products) and **lambdas** (functions). We already have full Pi type support — dependent function types `(x:A) -> B(x)` — where the return type can depend on the argument value. Sigma types are the **categorical dual**: dependent product types `(x:A, B(x))` where the type of a later component depends on the value of an earlier one.

Together, Pi and Sigma form the complete foundation of dependent type theory. Pi gives us **universal quantification** ("for all x of type A, there exists a B(x)"). Sigma gives us **existential quantification** ("there exists an x of type A such that B(x) holds").

### What Sigma Types Unlock

1. **Dependent pairs / refinement types** — values paired with evidence about them
2. **Existential types** — hiding implementation details behind an interface
3. **Abstract data types** — true encapsulation without OOP classes
4. **Length-indexed collections** — compile-time size safety
5. **Proof-carrying data** — values that carry type-level evidence, erased at runtime
6. **Heterogeneous collections** — lists of different types with a common interface (no base class needed)

---

## Design Principle: Telescopes, Not Pairs

### The Problem with Binary Sigma

Classical dependent type theory defines Sigma as a binary operation: `Σ(x:A). B(x)` — exactly two components. Multi-field dependent products require nesting:

```
Σ(x:A). Σ(y:B(x)). C(x,y)
```

This creates the same nested-pair problem that Haskell's GHC Core suffers from:
- Field access becomes O(depth) — chasing nested pairs
- Pattern matching generates nested case trees
- Codegen must reconstruct flat layouts from nested structure
- Everything is indirect (pointer chains)

### tulam's Core List Machine (CLM)

tulam's CLM was designed explicitly to avoid this. The comment at the top of `CLM.hs` says it all:

> "Core List Machine — we use **n-lists for tuples explicitly** instead of lambda-applications."

All data values in CLM are flat:
```haskell
CLMCON ConsTag [CLMExpr]  -- flat list of fields
```

A 10-field constructor is `CLMCON tag [f1, f2, ..., f10]` — O(1) field access via `CLMFieldAccess (name, index) expr`. This maps directly to .NET objects, JS objects, and C structs. It's one of tulam's key architectural advantages over Haskell.

### The Solution: Telescopes

A **telescope** is a flat sequence of bindings where each entry's type can depend on the values of all preceding entries:

```
(x₁:A₁, x₂:A₂(x₁), x₃:A₃(x₁,x₂), ..., xₙ:Aₙ(x₁,...,xₙ₋₁))
```

This is the dependent generalization of flat n-tuples. It matches CLM perfectly:

| Concept | Surface | CLM (runtime) |
|---------|---------|---------------|
| Non-dependent tuple | `(a, b, c)` | `CLMCON tag [v1, v2, v3]` |
| Non-dependent record | `{ x:Int, y:Bool }` | `CLMCON tag [v1, v2]` |
| Dependent telescope | `(n:Nat, v:Vec(Int,n))` | `CLMCON tag [v1, v2]` — **same!** |
| Existential | `exists (a:Type). {val:a}` | `CLMCON tag [witness, v1]` — **same!** |

**At runtime, telescopes are identical to flat tuples/records.** Dependencies are enforced at type-check time and erased at runtime. No new CLM nodes needed. No nested pairs. Our architecture stays clean.

### Binary Sigma as Sugar

The traditional Sigma notation `(x:A) * B(x)` is available as syntactic sugar for a 2-element telescope. It desugars internally to the flat form:

```
(x:A) * B(x)           ≡  telescope (x:A, _:B(x))
A * B                   ≡  telescope (_:A, _:B)         -- non-dependent pair
(x:A) * (y:B(x)) * C   ≡  telescope (x:A, y:B(x), _:C) -- flat, not nested!
```

The canonical internal representation is always the flat telescope.

---

## Relationship to Existing Systems

### What Changes, What Doesn't

| System | Impact | Reason |
|--------|--------|--------|
| **Algebras** (`algebra`) | **None** | Algebras provide implicit-parameter dispatch. Orthogonal to dependent data. |
| **Morphisms** (`morphism`) | **None** | Same dispatch mechanism, different arity. Orthogonal. |
| **Classes** (`class`) | **None** | OOP nominal types with inheritance. Different abstraction level. |
| **Records** (`type` with implicit constructor) | **Extended** | Records gain the ability to have dependent fields. Non-dependent records unchanged. |
| **Repr** (`repr`) | **None** | Static compiler-known type mapping. Existentials hide at value level instead. |
| **Effects** (`effect`) | **None** | Row-polymorphic effects are orthogonal. |
| **Module system** | **None** | Compile-time file organization. Existentials are runtime values, not modules. |
| **Sum types** (`type`) | **None** | Tagged unions with pattern matching. Orthogonal to dependent products. |
| **Pi types** | **Completed** | Sigma is the dual — same infrastructure, different direction. |

### Algebras Are NOT Sigma Types

An algebra `Monoid(a)` says: *"for any type `a`, if you have a Monoid instance, these operations exist."* Operations are implicit parameters, resolved at call sites. The type `a` is visible to the consumer.

An existential `exists (a:Type). MonoidOps(a)` says: *"here is a hidden type `a` together with monoid operations for it."* Operations are explicit in the value. The type `a` is hidden from the consumer.

| | Algebra | Existential |
|---|---|---|
| Type visibility | Visible (parametric) | Hidden |
| Operations | Implicit dispatch | Explicit in value |
| Usage | Ad-hoc polymorphism | Data abstraction |
| Resolution | Compile-time instance lookup | Runtime: use what's packed |

These are complementary, not overlapping. They can compose: an algebra-constrained function can `pack` a value into an existential.

### Classes vs. Existentials

OOP classes provide dynamic dispatch through nominal subtyping: `Dog extends Animal` means `Dog` IS-A `Animal` with known fields and vtable dispatch.

Existentials provide abstraction through structural hiding: `exists (a:Type). Showable(a)` means "some type I can show, but I don't know which." No class hierarchy needed.

They compose but don't conflict: a class method could return an existential, and an existential's operations could internally use class dispatch.

### Repr vs. Existentials

`repr Nat as Int` is a static, compiler-known bidirectional mapping. Both sides are always visible to the compiler. It's a compile-time optimization.

An existential hides the witness type from the consumer at the type level. The producer knows the concrete type; the consumer only has the interface.

No overlap. `repr` stays for representation mapping. Existentials are for information hiding.

### Module System vs. "First-Class Modules"

tulam's module system (`module`, `import`, `open`, `export`, `private`) is a **compile-time** organizational system controlling visibility, namespacing, caching, and dependency tracking. It operates on files and produces `CompiledModule` artifacts with hash-based dependency validation.

Existential types that bundle operations with hidden types are sometimes called "first-class modules" in PL literature. **These are entirely different things.** Existentials are runtime values — you can pass them to functions, store them in lists, return them. They interact with the module system the same way any other value does (they can be exported, imported, made private). No conflict.

---

## Type-Level Architecture

### Internal Type Representation (TypeCheck.hs)

The type checker already has the binary Sigma type:

```haskell
data Ty
  = ...
  | TPi (Maybe Name) Ty Ty      -- (x:A) -> B(x)
  | TSigma (Maybe Name) Ty Ty   -- (x:A) * B(x)
  | TRecord Row                  -- {x:A, y:B, ..r}
  | ...

-- Pattern synonyms
pattern TArrow a b = TPi Nothing a b       -- non-dependent arrow
pattern TProd a b  = TSigma Nothing a b     -- non-dependent product
```

### Telescope Representation Strategy

We have three options for representing telescopes in the type checker:

**Option 1: New TTelescope constructor**
```haskell
| TTelescope [(Name, Ty)]  -- each Ty can reference earlier Names
```
Pro: Direct, clean. Con: New constructor → must update every function that matches on Ty.

**Option 2: Extend TRecord rows with dependency**
```haskell
-- TRecord (RExtend "n" (TCon "Nat") (RExtend "v" (TApp (TCon "Vec") [TCon "Int", TRigid "n"]) REmpty))
-- The "v" field's type references "n" — already expressible!
```
Pro: Reuses existing infrastructure. Row unification, extraction already work. Con: Rows are unordered conceptually (Remy-style), but telescopes are ordered. Need ordering discipline.

**Option 3: Nested TSigma (classical)**
```haskell
-- TSigma (Just "n") (TCon "Nat") (TSigma (Just "v") (TApp ...) ...)
```
Pro: Already works with all existing code. Con: Nested — exactly what we want to avoid at CLM level. But: types are only in the checker, not runtime. Nesting at type level doesn't affect runtime flatness.

**Chosen: Option 3 (nested TSigma) at type level, flat CLMCON at runtime.**

Rationale:
- TSigma already has full support in the type checker: unification, substitution, occurs check, normalization, free variable collection, display.
- The nesting is purely in the type checker — it doesn't affect CLM representation. Types are erased at runtime.
- Dependent substitution through nested TSigma works identically to how TPi chains work for curried functions.
- `exprToTy` already folds `Tuple [Expr]` into nested `TSigma`: `foldr1 TProd tys` (TypeCheck.hs line 282). Extending this to dependent telescopes is natural.
- Avoids adding a new Ty constructor and updating 15+ pattern match sites.

The key insight: **nesting at type level is fine because types don't exist at runtime.** Our design principle (flat n-lists) applies to CLM values, not to the type checker's internal representation.

### Telescope Type Formation

A surface telescope `(x:A, y:B(x), z:C(x,y))` translates to nested TSigma:

```
TSigma (Just "x") A
  (TSigma (Just "y") B(x)
    C(x,y))
```

This mirrors how a multi-argument Pi `(x:A) -> (y:B(x)) -> C(x,y)` is:

```
TPi (Just "x") A
  (TPi (Just "y") B(x)
    C(x,y))
```

The duality is exact.

### Existential Quantification

An existential `exists (a:Type). T(a)` is a Sigma type where the first component is a type and the consumer cannot project out the witness:

```
exists (a:Type). { val:a, show:a -> String }

-- Type-level representation:
TSigma (Just "a") (TU 0)
  (TRecord (RExtend "val" (TRigid "a")
    (RExtend "show" (TArrow (TRigid "a") (TCon "String"))
      REmpty)))
```

The `exists` keyword adds a **scoping restriction**: the bound variable `a` is existentially quantified and cannot escape the scope of `unpack`. This is enforced by the type checker (skolemization), not by a new type constructor.

---

## CLM Representation (Runtime)

### No New CLM Nodes

Dependent pairs/telescopes compile to the same CLM as non-dependent tuples:

```
-- Surface: val v : (n:Nat) * Vec(Int, n) = (Succ(Succ(Z)), [1, 2]);
-- CLM:     CLMCON (ConsTag "_Pair" 0) [CLMCON (ConsTag "Succ" 1) [...], CLMARRAY [...]]

-- Surface: val s : exists (a:Type). { val:a } = pack(Int, { val = 42 });
-- CLM:     CLMCON (ConsTag "_Exists" 0) [CLMID "Int", CLMLIT (LInt 42)]
--          (or with type erasure: CLMCON (ConsTag "_Exists" 0) [CLMEMPTY, CLMLIT (LInt 42)])
```

Field access uses existing `CLMFieldAccess`:
```
-- fst(pair) → CLMFieldAccess ("", 0) pair
-- snd(pair) → CLMFieldAccess ("", 1) pair
-- record.field → CLMFieldAccess ("field", idx) record
```

### Type Erasure

Type-level components of existentials (the witness type) can be:
1. **Erased to CLMEMPTY** — when the witness is never inspected at runtime (common case)
2. **Kept as a tag** — when runtime type dispatch is needed (e.g., heterogeneous collections with downcast)

The default is erasure. The Pipeline (Pass 4) determines erasability during CLM conversion.

### Pattern Matching

Matching on telescopes/dependent pairs uses existing `CLMCASE` + `CLMFieldAccess`:

```
-- match v | (n, xs) -> body
-- Compiles to:
-- CLMLamCases [("v", CLMEMPTY)]
--   [CLMCASE [] (body with n → CLMFieldAccess("",0) v, xs → CLMFieldAccess("",1) v)]
```

No new pattern matching infrastructure needed. The case optimization pass already handles positional destructuring via `CLMFieldAccess`.

---

## Type Checker Updates Required

### 1. Dependent Substitution in TSigma (Already Works)

`substTyVar` already handles TSigma:
```haskell
substTyVar n r (TSigma mn a b) = TSigma mn (substTyVar n r a) (substTyVar n r b)
```

However, this doesn't respect **shadowing** — if the TSigma binds the same name, the inner type shouldn't be substituted. This needs fixing:

```haskell
-- CURRENT (line 561): no shadowing check
substTyVar n r (TSigma mn a b) = TSigma mn (substTyVar n r a) (substTyVar n r b)

-- NEEDED: respect shadowing (same as TPi should)
substTyVar n r (TSigma mn a b)
  | mn == Just n = TSigma mn (substTyVar n r a) b  -- don't substitute in body (shadowed)
  | otherwise    = TSigma mn (substTyVar n r a) (substTyVar n r b)
```

Note: TPi may have the same shadowing bug — verify and fix both simultaneously.

### 2. Sigma Introduction (Construction Checking)

When type-checking the construction of a dependent pair, the type of the second component must be checked **after substituting** the value of the first:

```
-- Checking: (e1, e2) against TSigma (Just "x") A B(x)
-- 1. check e1 : A
-- 2. infer actual type of e1 → get concrete a1
-- 3. substitute: B' = B[x := a1]
-- 4. check e2 : B'
```

This requires a new case in the `check` function (or in `infer` for tuple literals with expected type propagation):

```haskell
check (Tuple [e1, e2]) (TSigma (Just n) a b) =
    check e1 a `tcBind` \_ ->
    infer e1 `tcBind` \a1ty ->
        let b' = substTyVar n a1ty b
        in check e2 b'

-- For telescopes with 3+ elements, fold left:
check (Tuple (e:es)) (TSigma (Just n) a rest) =
    check e a `tcBind` \_ ->
    infer e `tcBind` \eTy ->
        let rest' = substTyVar n eTy rest
        in check (Tuple es) rest'
```

### 3. Sigma Elimination (Projection Typing)

When projecting from a dependent pair, the type of the second projection depends on the first value:

```
-- v : (x:A) * B(x)
-- fst(v) : A                     ← straightforward
-- snd(v) : B(fst(v))             ← dependent! type references the first component
```

For field access:
```haskell
inferFieldAccess ("", 0) (TSigma _ a _) = a
inferFieldAccess ("", 1) (TSigma (Just n) _ b) =
    -- b mentions n; substitute n with "fst of this value"
    -- At type level, this may remain symbolic until evaluation
    substTyVar n (projectFirst theExpr) b
```

This is the trickiest part. When the first component isn't statically known, the second component's type contains a free variable. Options:
- **Normalize** using `normalizeTy` after substitution (already implemented for Pi)
- **Leave symbolic** and resolve during later unification
- **Require annotation** when the first component isn't concrete

### 4. Existential Introduction (pack)

`pack(witness, body)` creates an existential. Type checking:

```
-- pack(WitnessType, value) : exists (a:Type). T(a)
-- 1. Check WitnessType : Type (it's a type)
-- 2. Substitute a := WitnessType in T(a) → T(WitnessType)
-- 3. Check value : T(WitnessType)
-- 4. Return type: exists (a:Type). T(a)  (witness hidden!)
```

### 5. Existential Elimination (unpack)

`unpack e as (a, x) in body` opens an existential:

```
-- e : exists (a:Type). T(a)
-- In body: a is a fresh rigid (skolem) variable, x : T(a)
-- The type of body must NOT mention a (a cannot escape its scope)
```

Type checking:
```
-- 1. infer e → exists (a:Type). T(a)
-- 2. Create fresh rigid variable α for a
-- 3. In body's scope: bind x : T(α)
-- 4. Infer body → resultTy
-- 5. Check: α does NOT appear in resultTy (escape check)
-- 6. Return resultTy
```

The escape check uses existing `occursIn` / `freeTyVars` machinery.

### 6. Unification Updates

Current TSigma unification (line 450-451) ignores the binding name:
```haskell
unify' (TSigma _ a1 r1) (TSigma _ a2 r2) =
    unify a1 a2 `tcBind` \_ -> unify r1 r2
```

For dependent Sigma, we should alpha-rename (like TForall does):
```haskell
unify' (TSigma mn1 a1 r1) (TSigma mn2 a2 r2) =
    unify a1 a2 `tcBind` \_ ->
    case (mn1, mn2) of
        (Just n1, Just n2) | n1 /= n2 ->
            -- Alpha-rename: replace both with fresh rigid
            let fresh = "__sigma_" ++ show (nextVar st)
                r1' = substTyVar n1 (TRigid fresh) r1
                r2' = substTyVar n2 (TRigid fresh) r2
            in unify r1' r2'
        _ -> unify r1 r2
```

### 7. Normalization

`normalizeTy` already handles TSigma structurally (line 647):
```haskell
go (TSigma mn a b) = TSigma mn (go a) (go b)
```

For dependent telescopes, normalization should substitute known concrete values:
```haskell
go (TSigma (Just n) a b)
    | isConcreteTy (go a) = TSigma (Just n) (go a) (substAndNormalize n (go a) b)
    | otherwise = TSigma (Just n) (go a) (go b)
```

This parallels how `inferApp` normalizes after dependent Pi substitution.

---

## Pipeline (Pass 4) Changes

### Sigma/Telescope → CLM Conversion

Currently, `Pi _ _ _` erases to `CLMEMPTY` (it's a type, not a value). Sigma values ARE values — they must produce `CLMCON`:

```haskell
-- In exprToCLM:
exprToCLM env (SigmaPair e1 e2) =
    CLMCON (ConsTag "_Pair" 0) [exprToCLM env e1, exprToCLM env e2]

exprToCLM env (Pack witnessExpr bodyExpr) =
    CLMCON (ConsTag "_Exists" 0) [exprToCLM env witnessExpr, exprToCLM env bodyExpr]
    -- Note: witnessExpr may be erased to CLMEMPTY if unused at runtime

exprToCLM env (Unpack scrutinee (tyVar, valVar) body) =
    -- Desugar to let-binding with field access
    let clmScrut = exprToCLM env scrutinee
    in simultBetaReduce (Map.fromList [
          (tyVar,  CLMFieldAccess ("", 0) clmScrut),
          (valVar, CLMFieldAccess ("", 1) clmScrut)
       ]) (exprToCLM env body)
```

### Type Erasure Pass

A sub-pass in Pipeline could mark type-level components for erasure:
```
-- If field 0 of an existential is never accessed at runtime → erase to CLMEMPTY
-- This is an optimization; correctness doesn't depend on it
```

---

## Summary of Required Changes

### Core AST (CLM) — Minimal Changes
1. **No new CLMExpr constructors** — telescopes are `CLMCON`, field access is `CLMFieldAccess`
2. **ConsTag conventions**: `"_Pair"` for anonymous dependent pairs, `"_Exists"` for existentials (or reuse the declared type name)
3. **evalCLMPure**: may need to handle type-level Sigma evaluation (constructing/projecting dependent pairs during type normalization)

### Type Checker — Moderate Changes
1. **Fix shadowing** in `substTyVar` for TSigma (and TPi)
2. **Sigma introduction rule**: check dependent pair construction with substitution
3. **Sigma elimination rule**: project with dependent type substitution
4. **Existential introduction** (pack): check witness, substitute, hide
5. **Existential elimination** (unpack): skolemize, scope, escape check
6. **Unification alpha-renaming** for named TSigma
7. **Normalization** of dependent TSigma after substitution

### Pipeline — Small Changes
1. **CLM conversion** for new surface AST nodes (SigmaPair, Pack, Unpack)
2. **Type erasure** optimization for existential witness types

### Surface AST — Deferred
Surface syntax, parser changes, and new AST constructors will be designed separately.

---

## Future Extensions

### Dependent Records (Phase 2)
Extend record declarations (implicit constructor `type`) to allow field dependencies:
```
type SizedArray(a:Type) = n:Nat * elems:Array(a) | length(elems) == n;
```
This reuses the telescope machinery — each field is a telescope entry.

### Algebra + Existential Integration (Phase 3)
Bridge between algebra dispatch and existential packing:
```
function packShowable[a:Type](x:a) : (exists (t:Type). {val:t, show:t -> String}) requires Show(a) =
    pack(a, { val = x, show = show });
```

### Evidence Erasure (Phase 4)
Compile-time proofs that are erased at runtime:
```
type Even = (n:Nat) * IsEven(n);
// IsEven evidence is erased — zero runtime cost
```

### Propositional Equality Integration (Phase 5)
Using `TId` (already in TypeCheck.hs) with Sigma for GADTs:
```
type TypedExpr(t:Type) =
    | LitI(n:Int, Id(Type, t, Int))
    | LitB(b:Bool, Id(Type, t, Bool));
```
