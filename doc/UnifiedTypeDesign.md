# Unified Type System Redesign: Eliminating the Ty/Expr Split

## Table of Contents

1. [Problem Statement](#1-problem-statement)
2. [Design Principles](#2-design-principles)
3. [Changes to Expr](#3-changes-to-expr)
4. [Eliminating Ty](#4-eliminating-ty)
5. [Type Checker Rewrite Strategy](#5-type-checker-rewrite-strategy)
6. [Row Types in Expr](#6-row-types-in-expr)
7. [Unification on Expr](#7-unification-on-expr)
8. [Type Annotation Propagation](#8-type-annotation-propagation)
9. [Impact on Other Passes](#9-impact-on-other-passes)
10. [Migration Plan](#10-migration-plan)
11. [What This Enables](#11-what-this-enables)

---

## 1. Problem Statement

### 1.1 The Current Architecture (Broken)

The type system has two parallel representations for types:

```
Surface.hs:    Expr   — 60 constructors, the surface language
TypeCheck.hs:  Ty     — 14 constructors, internal type checker representation
```

Conversion is **one-way and lossy**:
```
exprToTy :: Expr -> TC Ty     -- exists (38+ cases, hardcoded name list)
tyToExpr :: Ty -> Expr        -- DOES NOT EXIST
```

This means:
- Type checker results **cannot flow back** to the AST
- CLM nodes have **no type annotations** (types are erased)
- Monomorphization is impossible without re-inferring types
- The language claims universe polymorphism but has two incompatible type worlds

### 1.2 What the Language Should Be

tulam is designed as a dependently-typed language where **types are values**:
```
Type : Type1 : Type2 : ...    (universe hierarchy)
(x:A) -> B(x)                 (dependent functions)
```

In such a language, there is ONE term language for everything: values, types, kinds, sorts. The type checker is a normalizer + unifier on that single representation. This is how Agda, Lean, and Idris work.

### 1.3 What We Have Instead

```haskell
-- TypeCheck.hs — a SEPARATE type world
data Ty
  = TVar TyVar        -- NO Expr equivalent
  | TRigid Name       -- maps to Id name (lossy)
  | TCon Name         -- maps to Id name (lossy, same as TRigid!)
  | TApp Ty [Ty]      -- maps to App (Id name) args
  | TPi (Maybe Name) Ty Ty  -- maps to Pi (Maybe Name) Expr Expr (OK)
  | TSigma (Maybe Name) Ty Ty  -- maps to NTuple (awkward)
  | TId Ty Ty Ty      -- maps to App (Id "PropEq") [t,a,b]
  | TForall Name Ty   -- maps to... nothing? (created by generalize)
  | TRecord Row       -- maps to RecordType (but Row has no Expr equivalent)
  | TEffect Row Ty    -- maps to EffType (but Row...)
  | TU Level          -- maps to U Int (lossy: Level vs Int)
  | TLevel            -- NO Expr equivalent
  | TLit Literal      -- maps to Lit Literal (OK)
```

50+ functions operate on `Ty`. These all need to operate on `Expr` instead.

---

## 2. Design Principles

### 2.1 One Term Language

Every type, kind, sort, value, and computation is an `Expr`. The type checker operates on `Expr` directly. No `Ty` type exists.

### 2.2 MetaVars in Expr

Unification variables (currently `TVar Int`) become a constructor in `Expr`:
```haskell
| Meta !Int          -- Unification metavariable (created during type checking)
```

This is the standard approach in elaboration-based type checkers (Agda, Lean, Idris all have metavariables in their core term language).

### 2.3 Normalization = Evaluation

Type checking IS evaluation (at the type level). `normalizeTy` already does this via `evalCLMPure`. In the unified system, the type checker normalizes `Expr` directly — no conversion to CLM needed for type-level computation because `Expr` IS the computation language.

### 2.4 Annotations Persist

When the type checker infers a type, it annotates the AST node with a `Typed expr type` wrapper (or stores it in a side-table keyed by node identity). These annotations survive through to CLM conversion and beyond.

---

## 3. Changes to Expr

### 3.1 New Constructors

```haskell
data Expr =
    -- ... existing constructors unchanged ...

    -- NEW: Unification metavariable (type checking only)
    | Meta !Int

    -- NEW: Row type constructors (for structural records)
    | RowEmpty                    -- closed row: no more fields
    | RowExtend Name Expr Expr   -- field extension: {name:Type | rest}
    -- (RowVar is just Meta, RowRigid is just Id)
```

That's it. **Three new constructors.**

### 3.2 Constructors That Become Unnecessary

| Old Ty | Expr Equivalent | Notes |
|--------|-----------------|-------|
| `TVar n` | `Meta n` | Direct replacement |
| `TRigid name` | `Id name` | Already works (rigid = bound name in scope) |
| `TCon name` | `Id name` | Already works (type constructor = identifier) |
| `TApp ty args` | `App expr args` | Already works |
| `TPi mn a b` | `Pi mn a b` | Already works (1:1) |
| `TSigma mn a b` | `NTuple [(mn, a), ...]` or new `Sigma mn a b` | See §3.3 |
| `TId t a b` | `App (Id "PropEq") [t, a, b]` | Already works |
| `TForall n t` | `Pi (Just n) (U 0) t` | Forall = Pi over Type |
| `TRecord row` | `RecordType` with row Expr | See §3.4 |
| `TEffect row ty` | `EffType rowExpr ty` | Row becomes Expr |
| `TU level` | `U level` but with `Level` not `Int` | See §3.5 |
| `TLevel` | `Id "Level"` or dedicated constructor | Minor |
| `TLit lit` | `Lit lit` | Already works (1:1) |

### 3.3 Sigma Types

Currently `TSigma` exists in `Ty` but `Expr` uses `NTuple` for both value-level tuples and type-level products. This is OK for now — `NTuple` with two elements is a sigma type. But for clarity, we should add:

```haskell
| Sigma (Maybe Name) Expr Expr   -- Dependent pair type: (x:A) * B(x)
```

This mirrors `Pi` and makes the AST self-documenting. `NTuple` remains for value-level tuples.

### 3.4 Record Types with Row Variables

Currently `RecordType [(Name, Expr)] Bool` uses a Bool for "is open". In the unified system, open rows are represented structurally:

```haskell
-- Closed: RecordType [("x", Int), ("y", Bool)] False
--   → RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty)

-- Open: RecordType [("x", Int)] True
--   → RowExtend "x" (Id "Int") (Meta 42)   -- Meta is the row variable
```

`RecordType` can remain as syntactic sugar that desugars to `RowExtend` chains during type checking.

### 3.5 Universe Levels

Change `U Int` to `U Level`:
```haskell
| U Level    -- Universe: U (LConst 0) = Type, U (LConst 1) = Type1, etc.
```

This is already partially done in CLM (`CLMU Level`). The Surface `Expr` just needs to catch up.

### 3.6 Summary of Expr Changes

```haskell
data Expr =
    -- CHANGED:
    | U Level                    -- was: U Int
    | Sigma (Maybe Name) Expr Expr  -- NEW (was only in Ty)

    -- NEW (for type checking):
    | Meta !Int                  -- Unification metavariable
    | RowEmpty                   -- Closed row
    | RowExtend Name Expr Expr   -- Row field extension

    -- everything else unchanged
```

**Total: 3 new constructors, 1 changed constructor, 1 new constructor for clarity.**

---

## 4. Eliminating Ty

### 4.1 Delete List

Once the type checker operates on `Expr`, delete from TypeCheck.hs:
- `data Ty` (all 14 constructors)
- `data Row` (all 4 constructors)
- `exprToTy` (the 38+ case bridge function)
- `lamToTy` (replaced by working directly on Lambda)
- `tyToCLM` and `clmToTy` (no longer needed — Expr IS the language)
- All `replaceTVar`, `replaceTVarRow`, `substTyVarRow` — replaced by `traverseExpr`-based substitution
- Pattern synonyms: `TArrow`, `TProd` — replaced by `ArrowType`, `Sigma`

### 4.2 Keep (but rewrite to use Expr)

All these functions keep their semantics but operate on `Expr` instead of `Ty`:

| Function | Current Signature | New Signature |
|----------|-------------------|---------------|
| `unify` | `Ty -> Ty -> TC ()` | `Expr -> Expr -> TC ()` |
| `unify'` | `Ty -> Ty -> TC ()` | `Expr -> Expr -> TC ()` |
| `bind` | `TyVar -> Ty -> TC ()` | `Int -> Expr -> TC ()` |
| `occursIn` | `TyVar -> Ty -> Bool` | `Int -> Expr -> Bool` |
| `applySubst` | `Ty -> TC Ty` | `Expr -> TC Expr` |
| `substTyVar` | `Name -> Ty -> Ty -> Ty` | `Name -> Expr -> Expr -> Expr` |
| `freeTyVars` | `Ty -> [TyVar]` | `Expr -> [Int]` |
| `freeRigidVars` | `Ty -> [Name]` | `Expr -> [Name]` |
| `instantiate` | `Ty -> TC Ty` | `Expr -> TC Expr` |
| `generalize` | `Ty -> TC Ty` | `Expr -> TC Expr` |
| `infer` | `Expr -> TC Ty` | `Expr -> TC Expr` |
| `check` | `Expr -> Ty -> TC ()` | `Expr -> Expr -> TC ()` |
| `inferApp` | `Ty -> [Expr] -> TC Ty` | `Expr -> [Expr] -> TC Expr` |
| `normalizeTy` | `Environment -> Ty -> Ty` | `Environment -> Expr -> Expr` |
| `tyToName` | `Ty -> Name` | `Expr -> Name` |

### 4.3 TCState Changes

```haskell
-- BEFORE:
data TCState = TCState
  { nextVar      :: !Int
  , substitution :: HashMap Int Ty    -- metavar → Ty
  , rowSubst     :: HashMap Int Row   -- row var → Row
  , constraints  :: [Constraint]
  , tcErrors     :: [TCError]
  , tcMode       :: TCMode
  }

-- AFTER:
data TCState = TCState
  { nextMeta     :: !Int
  , substitution :: HashMap Int Expr  -- metavar → Expr (unified!)
  -- rowSubst is gone — row variables are just Metas
  , constraints  :: [Constraint]
  , tcErrors     :: [TCError]
  , tcMode       :: TCMode
  }
```

Row variables and type variables share the same `Meta` namespace. `rowSubst` is eliminated.

### 4.4 TCEnv Changes

```haskell
-- BEFORE:
data TCEnv = TCEnv
  { varTypes    :: HashMap Name Ty
  , tyVarScope  :: [Name]
  , envCompiler :: Maybe Environment
  , tcContext   :: [String]
  }

-- AFTER:
data TCEnv = TCEnv
  { varTypes    :: HashMap Name Expr  -- Expr instead of Ty
  , tyVarScope  :: [Name]
  , envCompiler :: Maybe Environment
  , tcContext   :: [String]
  }
```

---

## 5. Type Checker Rewrite Strategy

### 5.1 The Key Insight

Most of the type checker's logic doesn't care whether it's operating on `Ty` or `Expr`. The unification algorithm is:
1. Apply substitution
2. Match constructors
3. Bind metavariables
4. Recurse on children

This works the same regardless of the representation. The 50+ functions are mostly mechanical — same algorithm, different data constructors.

### 5.2 Unification on Expr

The current `unify'` has ~40 pattern cases on `Ty`. The new one has pattern cases on `Expr`:

```haskell
unify' :: Expr -> Expr -> TC ()
-- Meta variable binding (was TVar)
unify' (Meta v) t = bind v t
unify' t (Meta v) = bind v t

-- Rigid/skolem variables (was TRigid)
-- Uppercase identifiers are type constructors, lowercase are rigid vars
-- We distinguish by checking if the name is in tyVarScope
unify' (Id a) (Id b) | a == b = pure ()

-- Universe cumulativity (was TU)
unify' (U l1) (U l2) | levelLeq l1 l2 = pure ()
unify' (U l1) (U l2) | levelLeq l2 l1 = pure ()

-- Pi types (was TPi) — already identical
unify' (Pi mn1 a1 b1) (Pi mn2 a2 b2) = do
    unify a1 a2
    -- alpha-rename if names differ
    case (mn1, mn2) of
        (Just n1, Just n2) | n1 /= n2 -> do
            fresh <- freshRigidName
            unify (substName n1 (Id fresh) b1) (substName n2 (Id fresh) b2)
        _ -> unify b1 b2

-- Sigma types (NEW — was TSigma)
unify' (Sigma mn1 a1 b1) (Sigma mn2 a2 b2) = ... -- same as Pi

-- Type application (was TApp)
unify' (App f1 args1) (App f2 args2) = do
    unify f1 f2
    zipWithM_ unify args1 args2

-- Record rows
unify' (RowExtend l1 t1 r1) row2 = do
    (t2, r2) <- rowExtract l1 row2
    unify t1 t2
    unify r1 r2
unify' RowEmpty RowEmpty = pure ()

-- Literals at type level
unify' (Lit l1) (Lit l2) | l1 == l2 = pure ()

-- PropEq: Id(A, x, y)
unify' (App (Id "PropEq") [t1,a1,b1]) (App (Id "PropEq") [t2,a2,b2]) = do
    unify t1 t2; unify a1 a2; unify b1 b2

-- Effect types (via EffType constructor)
unify' (EffType r1 t1) (EffType r2 t2) = do
    unify r1 r2; unify t1 t2

-- Failure
unify' t1 t2 = tcError $ "Cannot unify " ++ show t1 ++ " with " ++ show t2
```

### 5.3 Substitution Application

```haskell
applySubst :: Expr -> TC Expr
applySubst (Meta v) = do
    st <- tcGet
    case Map.lookup v (substitution st) of
        Just e  -> applySubst e   -- chase chains
        Nothing -> pure (Meta v)
applySubst (Pi mn a b) = Pi mn <$> applySubst a <*> applySubst b
applySubst (Sigma mn a b) = Sigma mn <$> applySubst a <*> applySubst b
applySubst (App f args) = App <$> applySubst f <*> mapM applySubst args
applySubst (RowExtend n t r) = RowExtend n <$> applySubst t <*> applySubst r
applySubst (EffType r t) = EffType <$> applySubst r <*> applySubst t
applySubst (U l) = pure (U l)
applySubst e = pure e  -- Id, Lit, RowEmpty, UNDEFINED, etc. are ground
```

### 5.4 Inference Returns Expr

```haskell
infer :: Expr -> TC Expr
infer (Lit (LInt _))    = pure (Id "Int")
infer (Lit (LFloat _))  = pure (Id "Float64")
infer (Lit (LString _)) = pure (Id "String")
infer (Lit (LChar _))   = pure (Id "Char")
infer (Id name)          = lookupVarType name >>= instantiate
infer (App f args)       = do
    fTy <- infer f
    inferApp fTy args
infer (Function lam)     = inferLambda lam
infer (Typed e ty)       = do
    check e ty
    pure ty
-- ... etc.
```

### 5.5 No More exprToTy

The entire `exprToTy` function (38+ cases, hardcoded type name list) disappears. When the type checker encounters a type expression, it just... uses it as an `Expr`. Because types ARE expressions.

Where `exprToTy` was doing real work (like converting `UNDEFINED` to a fresh metavar), that logic moves into `infer`/`check`:

```haskell
-- In Lambda type checking:
inferLambda lam = do
    paramTypes <- forM (params lam) $ \(Var n tp _) ->
        case tp of
            UNDEFINED -> Meta <$> freshMeta  -- was: exprToTy UNDEFINED → freshTyVar
            _         -> pure tp             -- type annotation IS the type
    retTy <- case lamType lam of
        UNDEFINED -> Meta <$> freshMeta
        _         -> pure (lamType lam)
    -- ... check body against retTy
```

---

## 6. Row Types in Expr

### 6.1 Representation

Row types are now built from `Expr` constructors:

```haskell
-- {x:Int, y:Bool}  (closed record type)
RecordType [("x", Id "Int"), ("y", Id "Bool")] False
-- desugars during type checking to:
RowExtend "x" (Id "Int") (RowExtend "y" (Id "Bool") RowEmpty)

-- {x:Int, ..r}  (open record type)
RecordType [("x", Id "Int")] True
-- desugars during type checking to:
RowExtend "x" (Id "Int") (Meta 42)    -- Meta 42 is the row variable
```

### 6.2 Row Unification

Row unification works exactly as before (Remy-style), but on `Expr`:

```haskell
rowExtract :: Name -> Expr -> TC (Expr, Expr)
rowExtract label (RowExtend l t r)
    | label == l = pure (t, r)
    | otherwise  = do
        (t', r') <- rowExtract label r
        pure (t', RowExtend l t r')
rowExtract label (Meta v) = do
    fieldTy <- Meta <$> freshMeta
    restRow <- Meta <$> freshMeta
    bind v (RowExtend label fieldTy restRow)
    pure (fieldTy, restRow)
rowExtract label RowEmpty = tcError $ "Field " ++ label ++ " not found in closed row"
rowExtract label other = tcError $ "Expected row type, got " ++ show other
```

---

## 7. Unification on Expr

### 7.1 Handling Non-Type Expr Constructors

The current `Ty` only has type-level constructors. `Expr` has 60 constructors including value-level ones (`Function`, `Statements`, `IfThenElse`, etc.). Do we need unification cases for all of them?

**No.** Unification only runs on **type-level expressions**. Value-level constructors like `Function`, `Statements`, `ArrayLit` will never appear in type position during normal type checking. If they do (e.g., in dependent types where a value appears at type level), they're handled by normalization first.

The unification function needs cases for:
- `Meta` — binding
- `Id` — name equality
- `Pi`, `Sigma` — structural with alpha-renaming
- `App` — head + args
- `U` — level comparison
- `Lit` — equality (for type-level literals)
- `RowEmpty`, `RowExtend` — row unification
- `EffType` — structural
- `RecordType` — desugar to row, then unify
- `NTuple` — desugar to sigma chain
- Catch-all for value-level constructors: normalize first, then compare

That's ~15 cases vs the current ~40 cases on `Ty` (because many `Ty` cases were redundant distinctions like `TCon` vs `TRigid` vs `TForall` that all become just `Id` or `Pi`).

### 7.2 Normalization Before Unification

The current two-phase approach stays:
```haskell
unify :: Expr -> Expr -> TC ()
unify t1 t2 = do
    t1' <- applySubst t1
    t2' <- applySubst t2
    unify' t1' t2' `catchTC` \_ -> do
        -- Normalize (evaluate type-level functions) and retry
        env <- getCompilerEnv
        let t1n = normalize env t1'
            t2n = normalize env t2'
        unify' t1n t2n
```

But now `normalize` works on `Expr` directly — no `tyToCLM`/`clmToTy` round-trip needed. It can evaluate `App (Id "add") [Lit (LInt 2), Lit (LInt 3)]` → `Lit (LInt 5)` at the type level.

---

## 8. Type Annotation Propagation

### 8.1 The Goal

After type checking, every binding in the AST has a known type. This information must be available to:
- Monomorphization pass (to resolve implicit dispatch)
- CLM conversion (to annotate CLM nodes)
- LLVM backend (to generate correct native types)

### 8.2 Approach: Annotated Environment

After Pass 3, store inferred types back into the `Environment`:

```haskell
data Environment = Environment {
    -- ... existing fields ...
    , inferredTypes :: HashMap Name Expr   -- NEW: name → inferred type (fully resolved)
}
```

At the end of `typeCheckPass`, apply the final substitution to all inferred types and persist them:

```haskell
typeCheckPass = do
    -- ... run type checking ...
    -- Persist resolved types
    let resolvedTypes = Map.map (applyFinalSubst finalSubst) allInferredTypes
    modifyEnv $ \env -> env { inferredTypes = resolvedTypes }
```

### 8.3 CLM Type Annotations

`CLMExpr` gains an optional type annotation:

```haskell
data CLMExpr =
    -- ... existing constructors ...
    | CLMANNOT CLMExpr Expr   -- NEW: annotated expression with its inferred type
```

Or simpler: the CLM conversion pass consults `inferredTypes` when needed. The monomorphization pass uses it to resolve CLMIAP dispatch statically.

---

## 9. Impact on Other Passes

### 9.1 Pass 0 (Afterparse)

No change. Works on `Expr`, doesn't touch types.

### 9.2 Pass 1 (Environment Building)

No change. Stores `Lambda` values with their source-level type annotations.

### 9.3 Pass 2 (Case Optimization)

No change. Works on `Expr`, doesn't touch types.

### 9.4 Pass 3 (Type Checking) — REWRITTEN

Major rewrite. Operates on `Expr` directly, no `Ty` conversion. Persists results to `inferredTypes` in `Environment`.

### 9.5 Pass 3.5 (Monomorphization) — NEW

New pass between type checking and CLM conversion. Uses `inferredTypes` to:
1. Resolve `CLMIAP` dispatch statically
2. Specialize intrinsic calls to direct operations
3. Error on unresolvable polymorphic dispatch (for native backend)

### 9.6 Pass 4 (CLM Conversion)

Minor changes: consult `inferredTypes` when generating CLM. Can annotate CLM nodes with types for the backend.

### 9.7 Pass 5+ (Backends)

LLVM backend can use type annotations on CLM to:
- Choose correct LIR types (i64 vs double vs ptr)
- Generate specialized code instead of boxed everything

### 9.8 Interpreter

The interpreter uses the existing runtime dispatch path and is unaffected by the type checker rewrite. `CLMIAP` dispatch still works for interpreted execution.

---

## 10. Migration Plan

### Phase 1: Add New Constructors to Expr (small, safe)

1. Add `Meta !Int`, `RowEmpty`, `RowExtend Name Expr Expr`, `Sigma (Maybe Name) Expr Expr` to `Expr`
2. Change `U Int` to `U Level`
3. Add `Binary` instances, update `traverseExpr`, update `traverseExprM`
4. Add these to `ppr` (pretty printing)
5. **All existing tests must still pass** — new constructors are additive

### Phase 2: Rewrite Type Checker Core (the big one)

1. Change `TCState.substitution` from `HashMap Int Ty` to `HashMap Int Expr`
2. Delete `rowSubst` (merged into `substitution`)
3. Change `TCEnv.varTypes` from `HashMap Name Ty` to `HashMap Name Expr`
4. Rewrite `unify`/`unify'` to pattern-match on `Expr` instead of `Ty`
5. Rewrite `applySubst` on `Expr`
6. Rewrite `bind`, `occursIn` on `Expr`
7. Rewrite `infer` to return `Expr` instead of `Ty`
8. Rewrite `check` to take `Expr` instead of `Ty`
9. Delete `exprToTy` entirely
10. Delete `lamToTy` (inline the logic into `inferLambda`)
11. Delete `tyToCLM`, `clmToTy` (no longer needed)
12. Delete `data Ty` and `data Row`

### Phase 3: Persist Type Information

1. Add `inferredTypes :: HashMap Name Expr` to `Environment`
2. At end of Pass 3, apply final substitution and store
3. Update CLM conversion to consult inferred types
4. Update cache serialization for new Environment field

### Phase 4: Monomorphization Pass

1. New pass between Pass 3 and Pass 4
2. Uses `inferredTypes` to resolve implicit dispatch
3. Rewrites `hasImplicit` applications to direct calls
4. Enables LLVM backend to compile real tulam programs

### Phase 5: Cleanup

1. Remove `data Ty` from exports
2. Remove all `Ty`-related pattern synonyms (`TArrow`, `TProd`)
3. Update doc strings throughout
4. Update CLAUDE.md and design docs

---

## 11. What This Enables

### 11.1 Monomorphization (Immediate Goal)

With types persisted on every binding, the monomorphization pass can:
- See that `x` has type `Int` → resolve `+(x, y)` to `add\0Int`
- See that `f` returns `Bool` → resolve `show(f(x))` to `show\0Bool`
- Propagate types through function calls using the inferred type map

### 11.2 True Universe Polymorphism

With `Expr` as the single representation, a function that works at any universe level is just a function that takes a `U level` argument. No separate handling needed.

### 11.3 Type-Level Computation Without CLM Round-Trip

Currently `normalizeTy` converts `Ty → CLMExpr`, evaluates, converts back. With unified `Expr`, type-level computation is just evaluation of `Expr` — the same evaluator works at all levels.

### 11.4 Better Error Messages

Types are `Expr`, which has `SourceInfo` tracking. Type errors can point to the exact source location of the conflicting type annotation.

### 11.5 Correct CLM Type Annotations for Native Backend

CLM nodes can carry `Expr` types. The LLVM backend reads these to generate correct native types (i64 for Int, double for Float64, ptr for heap objects) instead of defaulting everything to i64.

### 11.6 Future: Full Dependent Types

The unified representation is the correct foundation for:
- Type-level pattern matching
- Dependent elimination (match on value, refine type)
- Inductive families
- Universe polymorphic functions

All of which require types and values to live in the same world.

---

## Appendix A: Ty → Expr Mapping (Complete Reference)

| Ty Constructor | Expr Replacement | Conversion Notes |
|----------------|------------------|------------------|
| `TVar n` | `Meta n` | Direct replacement |
| `TRigid name` | `Id name` | Rigid vars are just identifiers in scope |
| `TCon name` | `Id name` | Type constructors are just identifiers |
| `TApp (TCon n) args` | `App (Id n) args` | Type application = function application |
| `TPi mn a b` | `Pi mn a b` | 1:1 identical |
| `TSigma mn a b` | `Sigma mn a b` | New constructor |
| `TId t a b` | `App (Id "PropEq") [t, a, b]` | Encode as application |
| `TForall n t` | `Pi (Just n) (U (LConst 0)) t` | Forall = Pi over Type |
| `TRecord (REmpty)` | `RowEmpty` wrapped in context | Direct |
| `TRecord (RExtend n t r)` | `RowExtend n t r` | Direct |
| `TRecord (RVar v)` | `Meta v` | Row var = metavar |
| `TRecord (RRigid n)` | `Id n` | Rigid row var = identifier |
| `TEffect row ty` | `EffType row ty` | 1:1, row is now Expr |
| `TU level` | `U level` | 1:1 with Level |
| `TLevel` | `Id "Level"` | Or add dedicated constructor |
| `TLit lit` | `Lit lit` | 1:1 identical |

## Appendix B: Function Count Estimate

| Category | Functions to Rewrite | Difficulty |
|----------|---------------------|------------|
| Unification (`unify`, `unify'`, etc.) | 5 | Medium — pattern cases change from Ty to Expr |
| Substitution (`applySubst`, etc.) | 3 | Easy — mechanical replacement |
| Occurs check | 2 | Easy — walk Expr instead of Ty |
| Polymorphism (`instantiate`, `generalize`, etc.) | 5 | Medium — same logic, different types |
| Variable substitution | 3 | Easy — use `traverseExpr` |
| GADT refinement | 5 | Medium — depends on unification |
| Bidirectional checking (`infer`, `check`) | 2 | Large — most cases, but logic unchanged |
| Application (`inferApp`, `inferLambda`) | 2 | Medium |
| Row operations | 3 | Easy — same algorithm on Expr rows |
| Normalization | 2 | Medium — simplifies (no CLM round-trip) |
| Environment/lookup | 3 | Easy — change Ty to Expr in maps |
| Top-level / constraints | 3 | Easy |
| **TOTAL** | **~38 functions** | ~2 days of focused work |

Most functions are mechanical rewrites (same algorithm, different constructor names). The hard parts are `infer` (50+ cases) and `unify'` (~15 cases on Expr vs ~40 on Ty), but the logic is unchanged.
