# Sigma Types: Core AST (CLM) & Type Checker Changes

This document focuses exclusively on the internal changes required in CLM and TypeCheck.hs to fully support Sigma types, telescopes, and existentials. Surface language syntax is designed separately.

---

## Part 1: CLM (Core List Machine) Changes

### 1.1 No New CLMExpr Constructors

The fundamental insight: **dependent pairs are flat tuples at runtime**. The CLM representation is unchanged:

```haskell
-- These already exist and are sufficient:
CLMCON ConsTag [CLMExpr]           -- flat constructor tuple (used for sigma values)
CLMFieldAccess (Name, Int) CLMExpr -- O(1) field projection (used for sigma elimination)
CLMEMPTY                           -- type erasure marker (used for erased witnesses)
```

A telescope `(n:Nat, xs:Vec(Int,n), p:IsSorted(xs))` at runtime is just:
```
CLMCON (ConsTag "T" 0) [natValue, vecValue, proofValue]
```

Existentials `exists (a:Type). {val:a}` at runtime:
```
CLMCON (ConsTag "E" 0) [CLMEMPTY, actualValue]
                         ^^^^^^^^ erased type witness
```

### 1.2 ConsTag Conventions for Anonymous Sigma Values

When the surface language creates a dependent pair without an explicit type name, Pipeline needs a ConsTag. Conventions:

| Construction | ConsTag | Fields |
|---|---|---|
| Named record `Point(x,y)` | `ConsTag "Point" 0` | `[x, y]` |
| Named existential type | User-chosen ConsTag | `[witness, ...fields]` |
| Anonymous pair `(a, b)` | `ConsTag "_Tup2" 0` | `[a, b]` |
| Anonymous triple `(a,b,c)` | `ConsTag "_Tup3" 0` | `[a, b, c]` |
| Anonymous dependent pair | `ConsTag "_Tup2" 0` | `[a, b]` — same as non-dependent |

**Key**: dependent vs. non-dependent is invisible at CLM level. The ConsTag is the same. Only the type checker knows about the dependency.

### 1.3 evalCLMPure — Type-Level Sigma Evaluation

`evalCLMPure` (CLM.hs, lines 323-353) currently handles function application and constructor evaluation. For type-level Sigma computation, it needs to handle:

1. **Constructing type-level pairs**: Already works — `CLMCON` is a value, `evalCLMPure` preserves it.
2. **Projecting from type-level pairs**: Already works — `CLMFieldAccess` on `CLMCON` is handled (lines 348-351).
3. **Substitution through type-level Sigma**: Already works — `simultBetaReduce` substitutes in all sub-expressions including `CLMCON` fields.

**No changes needed in evalCLMPure.** The pure evaluator already handles everything Sigma needs at the type level.

### 1.4 applyCLMLam — No Changes

Dependent pair construction/destruction doesn't involve lambda application. `applyCLMLam` is unchanged.

### 1.5 simultBetaReduce — No Changes

Already substitutes through all CLMExpr constructors including `CLMCON` and `CLMFieldAccess`. Sigma values are just `CLMCON` with flat field lists.

### 1.6 Pattern Matching on Sigma Values

Pattern matching on telescopes uses existing infrastructure:

```haskell
-- match v | (n, xs) -> body
-- Compiles to (in Pipeline Pass 4):

-- Step 1: bind variable to scrutinee field accesses
-- n  → CLMFieldAccess ("", 0) v
-- xs → CLMFieldAccess ("", 1) v

-- Step 2: substitute into body
-- simultBetaReduce (Map.fromList [("n", CLMFieldAccess ("",0) v), ("xs", CLMFieldAccess ("",1) v)]) body
```

The CaseOptimization pass already does exactly this for constructor patterns — `caseTransformApp2` generates `CLMFieldAccess` nodes for positional destructuring (CaseOptimization.hs, lines 109-130).

**No changes needed in CaseOptimization.hs** — sigma destructuring patterns compile to the same form as constructor patterns.

### 1.7 CLM Summary

| Component | Changes Required |
|---|---|
| `CLMExpr` data type | None |
| `CLMLam` | None |
| `applyCLMLam` | None |
| `simultBetaReduce` | None |
| `evalCLMPure` | None |
| `resolveCases` | None |
| `CLMFieldAccess` | None (already handles named + indexed) |
| ConsTag conventions | Documentation only — `_TupN` for anonymous tuples |

**Total CLM changes: zero lines of code.** The existing architecture handles Sigma types natively.

---

## Part 2: Type Checker (TypeCheck.hs) Changes

This is where all the real work happens. The type checker must enforce dependent typing discipline that CLM doesn't need to know about.

### 2.0 Current State Audit

TSigma is already threaded through the entire type checker:

| Function | Line | Current TSigma handling | Adequate? |
|---|---|---|---|
| `applySubst` | 353-356 | Applies substitution to both components | Yes |
| `occursIn` | 408 | Checks both components | Yes |
| `unify'` | 450-451 | Unifies components, **ignores name** | **No — needs alpha-renaming** |
| `substTyVar` | 561 | Substitutes in both components, **no shadowing** | **No — needs shadowing check** |
| `freeTyVars` | 595 | Collects from both components | **No — should exclude bound var** |
| `replaceTVar` | 617 | Replaces in both components | Yes |
| `normalizeTy` | 647 | Normalizes both components | **No — should substitute known values** |
| `isConcreteTy` | 749 | Checks both components | Yes |
| `showTy` | 1396-1397 | Displays both forms | Yes |
| `exprToTy` | 276-282 | Folds tuples into nested TProd | **No — needs dependent form** |

### 2.1 Fix: substTyVar Shadowing

**Current** (line 561):
```haskell
substTyVar n r (TSigma mn a b) = TSigma mn (substTyVar n r a) (substTyVar n r b)
```

**Problem**: If the Sigma binds the same name we're substituting, the body `b` should NOT be substituted (the inner binding shadows the outer one).

**Fix**:
```haskell
substTyVar n r (TSigma mn a b)
  | mn == Just n = TSigma mn (substTyVar n r a) b     -- shadowed: only substitute in domain
  | otherwise    = TSigma mn (substTyVar n r a) (substTyVar n r b)
```

**Also fix TPi** (should already have this, verify line ~559):
```haskell
substTyVar n r (TPi mn a b)
  | mn == Just n = TPi mn (substTyVar n r a) b
  | otherwise    = TPi mn (substTyVar n r a) (substTyVar n r b)
```

**Impact**: Correctness fix. Without shadowing, `substTyVar "x" Int (TSigma (Just "x") Nat (TRigid "x"))` would incorrectly produce `TSigma (Just "x") Nat Int` instead of `TSigma (Just "x") Nat (TRigid "x")`.

### 2.2 Fix: freeTyVars Should Respect Binding

**Current** (line 595):
```haskell
freeTyVars (TSigma _ a b) = freeTyVars a ++ freeTyVars b
```

**Problem**: A named Sigma `TSigma (Just "x") A B` binds `x` in `B`. It should NOT report `x` as free in `B`.

**Fix**:
```haskell
freeTyVars (TSigma (Just n) a b) = freeTyVars a ++ filter (/= n) (freeTyVars b)
freeTyVars (TSigma Nothing a b)  = freeTyVars a ++ freeTyVars b
```

**Also fix TPi** (verify):
```haskell
freeTyVars (TPi (Just n) a b) = freeTyVars a ++ filter (/= n) (freeTyVars b)
freeTyVars (TPi Nothing a b)  = freeTyVars a ++ freeTyVars b
```

**Impact**: Required for escape checking in existentials. If `freeTyVars` reports a bound variable as free, the escape check will incorrectly reject valid programs.

### 2.3 Fix: Unification with Alpha-Renaming

**Current** (lines 450-451):
```haskell
unify' (TSigma _ a1 r1) (TSigma _ a2 r2) =
  unify a1 a2 `tcBind` \_ -> unify r1 r2
```

**Problem**: Two Sigma types with different binding names should unify if they're alpha-equivalent:
```
TSigma (Just "x") Nat (TRigid "x")  ≡  TSigma (Just "y") Nat (TRigid "y")
```

**Fix** (mirroring TForall unification at lines 461-470):
```haskell
unify' (TSigma mn1 a1 r1) (TSigma mn2 a2 r2) =
  unify a1 a2 `tcBind` \_ ->
  case (mn1, mn2) of
    (Just n1, Just n2) | n1 /= n2 ->
      tcGet `tcBind` \st ->
        let freshName = "__sigma_" ++ show (nextVar st)
            rigid = TRigid freshName
        in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
          let r1' = substTyVar n1 rigid r1
              r2' = substTyVar n2 rigid r2
          in unify r1' r2'
    _ -> unify r1 r2
```

**Also fix TPi** (line 448 — currently ignores names too):
```haskell
unify' (TPi mn1 a1 r1) (TPi mn2 a2 r2) =
  unify a1 a2 `tcBind` \_ ->
  case (mn1, mn2) of
    (Just n1, Just n2) | n1 /= n2 ->
      tcGet `tcBind` \st ->
        let freshName = "__pi_" ++ show (nextVar st)
            rigid = TRigid freshName
        in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
          let r1' = substTyVar n1 rigid r1
              r2' = substTyVar n2 rigid r2
          in unify r1' r2'
    _ -> unify r1 r2
```

### 2.4 Sigma Introduction (Dependent Pair Construction)

When checking a tuple against a dependent Sigma type, substitute the actual first-component value into the second-component's type:

```haskell
-- New case in `check`:
check expr@(Tuple [e1, e2]) ty@(TSigma (Just n) a b) =
    -- Check first component against domain type
    check e1 a `tcBind` \_ ->
    -- Infer actual type of first component for substitution
    infer e1 `tcBind` \e1Ty ->
        -- Substitute the first component's type into the codomain
        let b' = substTyVar n e1Ty b
        in tcAsk `tcBind` \tenv ->
            let b'' = case envCompiler tenv of
                    Just env -> normalizeTy env b'
                    Nothing  -> b'
            in applySubst b'' `tcBind` \b''' ->
                check e2 b'''

-- For telescopes (3+ elements), fold through the chain:
check (Tuple (e:es)) (TSigma (Just n) a rest) =
    check e a `tcBind` \_ ->
    infer e `tcBind` \eTy ->
        let rest' = substTyVar n eTy rest
        in tcAsk `tcBind` \tenv ->
            let rest'' = case envCompiler tenv of
                    Just env -> normalizeTy env rest'
                    Nothing  -> rest'
            in applySubst rest'' `tcBind` \rest''' ->
                check (Tuple es) rest'''

-- Non-dependent still works as before:
check (Tuple [e1, e2]) (TSigma Nothing a b) =
    check e1 a `tcBind` \_ -> check e2 b
```

**Pattern**: This mirrors exactly how `inferApp` handles TPi (TypeCheck.hs lines ~1050-1065). Substitute the bound variable with the actual argument type, normalize, continue checking.

### 2.5 Sigma Elimination (Projection Typing)

When inferring the type of a field access on a Sigma-typed value:

```haskell
-- In inferFieldAccess (or wherever RecFieldAccess / field access is typed):
inferProjection :: Expr -> Ty -> Int -> TC Ty

-- First projection: straightforward
inferProjection _ (TSigma _ a _) 0 = tcPure a

-- Second projection: dependent — type references first component
inferProjection scrutinee (TSigma (Just n) a b) 1 =
    -- The type of the second component is b[n := fst(scrutinee)]
    -- At the type level, we represent fst(scrutinee) as a rigid variable
    -- or as the actual inferred type of the first projection
    infer (RecFieldAccess ("", 0) scrutinee) `tcBind` \fstTy ->
        let b' = substTyVar n fstTy b
        in tcAsk `tcBind` \tenv ->
            let b'' = case envCompiler tenv of
                    Just env -> normalizeTy env b'
                    Nothing  -> b'
            in applySubst b''

-- Non-dependent:
inferProjection _ (TSigma Nothing _ b) 1 = tcPure b

-- Deep telescope access (index > 1): peel layers
inferProjection scrutinee (TSigma (Just n) a rest) idx | idx > 0 =
    infer (RecFieldAccess ("", 0) scrutinee) `tcBind` \fstTy ->
        let rest' = substTyVar n fstTy rest
        in inferProjection scrutinee rest' (idx - 1)
```

**Note**: For named field access on telescopes that map to records, the field name resolves to an index, then the same logic applies.

### 2.6 Existential Introduction (pack)

`pack` creates an existential value. The type checker must:
1. Verify the witness type is well-formed
2. Substitute the witness into the body type
3. Check the body against the substituted type
4. Return the existential type with the witness **hidden**

```haskell
-- pack(WitnessType, bodyValue) : exists (a:Type). T(a)
--
-- Assuming surface AST node: Pack witnessExpr bodyExpr expectedTy
-- where expectedTy = TSigma (Just "a") (TU 0) bodyTy

checkPack :: Expr -> Expr -> Ty -> TC Ty
checkPack witnessExpr bodyExpr (TSigma (Just tyVarName) (TU level) bodyTy) =
    -- 1. Convert witness expression to a type
    exprToTy witnessExpr `tcBind` \witnessTy ->
    -- 2. Check witness is a valid type at the right universe level
    check witnessExpr (U level) `tcBind` \_ ->
    -- 3. Substitute witness type into body type
    let bodyTy' = substTyVar tyVarName witnessTy bodyTy
    in tcAsk `tcBind` \tenv ->
        let bodyTy'' = case envCompiler tenv of
                Just env -> normalizeTy env bodyTy'
                Nothing  -> bodyTy'
        in applySubst bodyTy'' `tcBind` \bodyTy''' ->
        -- 4. Check body against substituted type
        check bodyExpr bodyTy''' `tcBind` \_ ->
        -- 5. Return the EXISTENTIAL type (witness hidden)
        tcPure (TSigma (Just tyVarName) (TU level) bodyTy)

checkPack _ _ ty = tcFail (OtherError $ "pack requires existential type, got: " ++ showTy ty)
```

### 2.7 Existential Elimination (unpack)

`unpack` opens an existential. The bound type variable becomes a rigid/skolem variable that cannot escape:

```haskell
-- unpack e as (tyVar, valVar) in body
--
-- e : exists (a:Type). T(a)
-- In body: a is rigid (skolem), valVar : T(a)
-- Result type must NOT contain a

checkUnpack :: Expr -> Name -> Name -> Expr -> TC Ty
checkUnpack scrutinee tyVarName valVarName body =
    -- 1. Infer type of scrutinee
    infer scrutinee `tcBind` \scrutTy ->
    applySubst scrutTy `tcBind` \scrutTy' ->
    case scrutTy' of
        TSigma (Just boundName) (TU _) innerTy -> do
            -- 2. Create fresh rigid (skolem) variable for the witness type
            tcGet `tcBind` \st ->
                let freshRigid = "__exists_" ++ show (nextVar st)
                in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
                    -- 3. Substitute the bound type variable with the rigid variable
                    let innerTy' = substTyVar boundName (TRigid freshRigid) innerTy
                    in
                    -- 4. Type-check body with tyVarName → rigid, valVarName → innerTy'
                    tcLocal (\tenv -> tenv {
                        tcScope = Map.insert tyVarName (TU 0)
                                $ Map.insert valVarName innerTy'
                                $ tcScope tenv
                    }) (infer body) `tcBind` \resultTy ->
                    applySubst resultTy `tcBind` \resultTy' ->
                    -- 5. ESCAPE CHECK: rigid variable must not appear in result type
                    if freshRigid `elem` rigidVarsIn resultTy'
                        then tcFail (OtherError $
                            "Existential type variable '" ++ tyVarName ++
                            "' escapes its scope in type: " ++ showTy resultTy')
                        else tcPure resultTy'
        _ -> tcFail (OtherError $ "unpack requires existential type, got: " ++ showTy scrutTy')

-- Helper: collect rigid variable names from a type
rigidVarsIn :: Ty -> [Name]
rigidVarsIn (TRigid n) = [n]
rigidVarsIn (TPi _ a b) = rigidVarsIn a ++ rigidVarsIn b
rigidVarsIn (TSigma _ a b) = rigidVarsIn a ++ rigidVarsIn b
rigidVarsIn (TApp f args) = rigidVarsIn f ++ concatMap rigidVarsIn args
rigidVarsIn (TRecord row) = rigidVarsInRow row
rigidVarsIn (TEffect row t) = rigidVarsInRow row ++ rigidVarsIn t
rigidVarsIn (TForall _ t) = rigidVarsIn t
rigidVarsIn (TId t a b) = rigidVarsIn t ++ rigidVarsIn a ++ rigidVarsIn b
rigidVarsIn _ = []

rigidVarsInRow :: Row -> [Name]
rigidVarsInRow REmpty = []
rigidVarsInRow (RExtend _ t r) = rigidVarsIn t ++ rigidVarsInRow r
rigidVarsInRow (RRigid n) = [n]
rigidVarsInRow (RVar _) = []
```

### 2.8 Normalization Enhancement

**Current** (line 647):
```haskell
go (TSigma mn a b) = TSigma mn (go a) (go b)
```

**Enhanced**: When normalizing a dependent Sigma where the first component is concrete, substitute into the second component before normalizing:
```haskell
go (TSigma (Just n) a b) =
    let a' = go a
    in if isConcreteTy a'
       then TSigma (Just n) a' (go (substTyVar n a' b))
       else TSigma (Just n) a' (go b)
go (TSigma Nothing a b) = TSigma Nothing (go a) (go b)
```

**Rationale**: Parallels how Pi-type normalization works — after dependent substitution, normalize the result. Enables the type checker to reduce `(x:Nat) * Vec(Int, plus(x, Z))` to `(x:Nat) * Vec(Int, x)` when `plus` is known.

### 2.9 exprToTy Enhancement

**Current** (lines 276-282): Tuples become nested non-dependent Sigma:
```haskell
exprToTy (Tuple exprs) =
  tcMapM exprToTy exprs `tcBind` \tys ->
    case tys of
      []  -> tcPure (TCon "Unit")
      [t] -> tcPure t
      _   -> tcPure (Prelude.foldr1 TProd tys)
```

**Enhancement**: When tuple elements are `Typed` expressions with names, produce dependent Sigma:
```haskell
-- This will be driven by whatever surface syntax we design.
-- The key conversion for the type checker is:
-- exprToTy (SigmaType (Just "x") domExpr codExpr) =
--     exprToTy domExpr `tcBind` \domTy ->
--     exprToTy codExpr `tcBind` \codTy ->
--     tcPure (TSigma (Just "x") domTy codTy)
```

The exact form depends on the surface AST design, but the type checker just needs the conversion to produce `TSigma (Just name) domainTy codomainTy`.

### 2.10 Subtyping for Sigma

The existing `subtype` function (used for class hierarchy checks) should handle Sigma:

```haskell
subtype' (TSigma mn1 a1 b1) (TSigma mn2 a2 b2) =
    -- Sigma is covariant in both components
    -- (A more refined analysis would be: covariant in first, covariant in second
    --  after substitution — but structural covariance is sound and simpler)
    subtype a1 a2 `tcBind` \_ ->
    case (mn1, mn2) of
        (Just n1, Just n2) | n1 /= n2 ->
            tcGet `tcBind` \st ->
                let fresh = TRigid ("__sub_sigma_" ++ show (nextVar st))
                in tcModify (\s -> s { nextVar = nextVar s + 1 }) `tcBind` \_ ->
                    subtype (substTyVar n1 fresh b1) (substTyVar n2 fresh b2)
        _ -> subtype b1 b2
```

---

## Part 3: Pipeline (Pass 4) Changes

### 3.1 exprToCLM for Sigma Values

Dependent pair construction compiles to `CLMCON`:

```haskell
-- Sigma pair value → flat CLMCON
-- The exact surface AST constructor TBD, but the conversion is:
exprToCLM env (SigmaPair fstExpr sndExpr) =
    CLMCON (ConsTag "_Tup2" 0) [exprToCLM env fstExpr, exprToCLM env sndExpr]

-- Telescope value (3+ elements) → flat CLMCON
exprToCLM env (TelescopeVal exprs) =
    CLMCON (ConsTag ("_Tup" ++ show (length exprs)) 0) (map (exprToCLM env) exprs)
```

### 3.2 exprToCLM for pack/unpack

```haskell
-- pack: construct existential (witness may be erased)
exprToCLM env (Pack witnessExpr bodyExpr) =
    let clmWitness = CLMEMPTY  -- Type witness erased by default
        clmBody = exprToCLM env bodyExpr
    in CLMCON (ConsTag "_Exists" 0) [clmWitness, clmBody]

-- unpack: destructure via field access + beta reduction
exprToCLM env (Unpack scrutExpr tyVar valVar bodyExpr) =
    let clmScrut = exprToCLM env scrutExpr
        clmBody = exprToCLM env bodyExpr
        -- tyVar maps to field 0 (erased, but bind it anyway for completeness)
        -- valVar maps to field 1
    in simultBetaReduce
        (Map.fromList [ (tyVar, CLMFieldAccess ("", 0) clmScrut)
                       , (valVar, CLMFieldAccess ("", 1) clmScrut) ])
        clmBody
```

### 3.3 Sigma Type Expressions → CLMEMPTY

Sigma types in type position erase, just like Pi types:

```haskell
-- Sigma type expression (not a value) → erased
exprToCLM env (SigmaType _ _ _) = CLMEMPTY  -- types don't exist at runtime
```

### 3.4 Type Annotation Pass (maybeAddTypeHint)

`maybeAddTypeHint` in Pipeline.hs wraps `CLMIAP` with `CLMTYPED` for concrete return types. Sigma-returning functions should be annotated the same way:

```haskell
-- If a function returns (n:Nat) * Vec(Int, n), the CLMTYPED hint is:
-- CLMTYPED (CLMIAP f args) (CLMCON (ConsTag "_Tup2" 0) [CLMID "Nat", CLMAPP (CLMID "Vec") [CLMID "Int", CLMID "Nat"]])
-- This enables type-directed dispatch on sigma-returning functions
```

No special logic needed — `maybeAddTypeHint` already handles arbitrary return types via `lamToTy` → `tyToCLM`.

---

## Part 4: Implementation Order

### Phase 1: Foundational Fixes (No new features, just correctness)
1. Fix `substTyVar` shadowing for TSigma and TPi
2. Fix `freeTyVars` binding for TSigma and TPi
3. Fix `unify'` alpha-renaming for TSigma and TPi
4. Add tests for all three fixes

### Phase 2: Sigma Introduction & Elimination
5. Implement dependent pair type checking (construction)
6. Implement dependent projection typing (elimination)
7. Implement `normalizeTy` enhancement for dependent Sigma
8. Add tests for construction and projection

### Phase 3: Existentials
9. Implement `pack` type checking (existential introduction)
10. Implement `unpack` type checking (existential elimination with escape check)
11. Add `rigidVarsIn` helper
12. Add tests for existentials including escape-check rejection

### Phase 4: Pipeline Integration
13. Add CLM conversion for sigma values (CLMCON)
14. Add CLM conversion for pack/unpack
15. Add type erasure for sigma type expressions
16. Integration tests: full pipeline from surface through evaluation

### Phase 5: Subtyping & Advanced
17. Add sigma subtyping rule
18. Normalization of dependent sigma after substitution
19. Edge case tests (nested sigma, sigma + pi interaction, sigma + records)

---

## Part 5: Test Strategy

### Unit Tests (TypeCheck.hs level)

```
-- Shadowing
substTyVar "x" Int (TSigma (Just "x") Nat (TRigid "x")) == TSigma (Just "x") Nat (TRigid "x")
substTyVar "x" Int (TSigma (Just "y") Nat (TRigid "x")) == TSigma (Just "y") Nat Int

-- Free variables
freeTyVars (TSigma (Just "x") (TCon "Nat") (TRigid "x")) == []  -- x is bound
freeTyVars (TSigma (Just "x") (TCon "Nat") (TRigid "y")) == ["y"]  -- y is free

-- Unification alpha-equivalence
unify (TSigma (Just "x") Nat (TRigid "x")) (TSigma (Just "y") Nat (TRigid "y")) == OK

-- Construction checking
check (42, [1,2]) against (n:Int) * Array(Int) == OK (with appropriate representations)

-- Projection typing
infer fst(v) where v : (n:Nat) * Vec(Int,n) == Nat
infer snd(v) where v : (n:Nat) * Vec(Int,n) == Vec(Int, fst(v))

-- Existential escape check
unpack e as (a, x) in x          -- OK: result type doesn't mention a
unpack e as (a, x) in (x : a)   -- REJECT: a escapes
```

### Integration Tests (Full pipeline)

```
-- Dependent pair round-trip
function mkPair(n:Nat) : (m:Nat) * Vec(Int, m) = (n, replicate(n, 0));
function getPairSize(p : (m:Nat) * Vec(Int, m)) : Nat = fst(p);

-- Existential hide/reveal
type AnyShow = exists (a:Type). { val:a, show:a -> String };
function showIt(s:AnyShow) : String = unpack s as (a, ops) in ops.show(ops.val);

-- Telescope (3+ fields)
function mk3(x:Nat, y:Nat) : (a:Nat, b:Nat, sum:Nat) = (x, y, plus(x,y));
```

---

## Appendix: Interaction with Existing Features

### Sigma + Type-Level Normalizer
The normalizer (`normalizeTy`) already evaluates type-level function applications via `evalCLMPure`. Sigma types benefit automatically: `(n:Nat) * Vec(Int, plus(n, Z))` normalizes to `(n:Nat) * Vec(Int, n)` because `plus(n, Z)` reduces.

### Sigma + Universe Cumulativity
Sigma types at universe level 0 (value types) automatically work at level 1 (type types) via cumulativity. A function `mkPair : Nat -> (n:Nat) * T(n)` works both at value level and type level.

### Sigma + Row Polymorphism
Existentials can hide record types with row variables:
```
exists (r:Row). { val: {x:Int | r}, show: {x:Int | r} -> String }
```
This combines telescope binding (existential `r`) with row polymorphism (open record). The type checker handles both independently — row unification for the record, escape checking for the existential.

### Sigma + Effect System
Effects are orthogonal. An effectful function can return a Sigma type:
```
function readPair() : Eff {io:IO} ((n:Nat) * Vec(Int, n)) = ...
```
The effect row `{io:IO}` wraps the sigma return type. No interaction.

### Sigma + Class System
Class methods can accept/return sigma types. Class fields can be sigma-typed. No interaction with vtable dispatch — sigma values are plain `CLMCON` and class methods dispatch on object tag, not on field types.
