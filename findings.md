# Findings: raw/elaborated lambda split, `effectSeq`, and remaining unresolved dispatches

## Executive summary

We investigated the regression caused by storing elaborated lambdas back into `Environment.topLambdas` / `instanceLambdas`, implemented the raw/elaborated split, then traced the next compilation failures in the bytecode/native path.

### Main conclusions

1. **The original regression was architectural, not local to `exprToCLMTC`.**
   The environment had been made to store elaborated lambdas with dense `Typed` wrappers, while several consumers still expected raw-ish Surface syntax.

2. **The right architectural fix was to separate raw and elaborated representations.**
   Raw lambdas remain canonical in `topLambdas` / `instanceLambdas`, while elaborated lambdas are stored separately in `topLambdasElab` / `instanceLambdasElab`.

3. **The `effectSeq` unresolved error was real and caused by a monomorphization bug, not by missing handlers or missing definitions.**
   After the split, monomorphization ran on elaborated lambdas. Its dispatch-resolution logic only recognized applications of the form `App (Id funcName) args`, so calls whose callee heads were wrapped in `Typed` (e.g. `App (Typed (Id "effectSeq") ty) [...]`) bypassed handler-op resolution completely.

4. **That `effectSeq` bug is now fixed.**
   `resolveImplicitCalls` was changed to recognize callee heads through transparent `Typed` wrappers. Focused repro showed debug evidence:

   ```text
   [mono] resolve effectSeq → handler op
   [mono] resolve putStrLn → handler op
   ```

   and `effectSeq` disappeared from unresolved-call reporting.

5. **The remaining unresolved calls are a different issue.**
   The next unresolved cluster (`/`, `foldr`, `foldl`, and likely secondary fallout involving `byteToInt` / `intToByte`) points to another raw-vs-elaborated mismatch inside **type recovery** in `Monomorphize.hs`.

6. **The likely pre-existing `run_all` hang is probably separate from the `effectSeq` issue.**
   Current evidence suggests a larger monomorphization/specialization fixed-point/performance problem rather than the same specific handler-op bug.

---

## Investigation timeline and evidence

## 1. Original regression: elaborated lambdas stored into canonical env maps

### What changed
`typeCheckPass` was changed to use `checkTopLevelElab`, which elaborates lambda bodies via `inferElab` / `zonkExpr`, then writes those elaborated lambdas back into the environment.

This changed the meaning of:

- `topLambdas`
- `instanceLambdas`

from “raw-ish Surface lambdas” to “elaborated lambdas with dense `Typed` wrappers”.

### Why that broke things
Multiple consumers pattern-match on raw Surface structure, including:

- `normalizeTy`
- `exprToCLMTC`
- `lambdaToCLMLam`
- `pcToCLM`
- various downstream transforms

`exprToCLMTC` is a deliberately partial Surface→CLM bridge, not a full elaborated-term evaluator. Once every subexpression was wrapped in `Typed`, it silently stopped translating large parts of the tree and returned `CLMEMPTY` for unsupported forms.

### Symptom observed
`stack test` failed on:

```text
normalizeTy: (==) on Nats via instance dispatch — Succ(Z) == Succ(Z) reduces to True
expected: Id "True"
 but got: App (Id "==") [App (Id "Succ") [Id "Z"], App (Id "Succ") [Id "Z"]]
```

### Correct architectural diagnosis
This was not fundamentally “add more `Typed` stripping to `exprToCLMTC`”.
It was a representation-boundary bug:

- raw Surface consumers need raw Surface lambdas
- type-informed consumers prefer elaborated lambdas
- one env field cannot safely mean both

---

## 2. Implemented architectural fix: dual raw/elaborated lambda storage

### Environment redesign
Introduced separate elaborated maps:

- `topLambdasElab`
- `instanceLambdasElab`

while preserving raw canonical maps:

- `topLambdas`
- `instanceLambdas`

### Invariant
- `topLambdas` / `instanceLambdas` = canonical raw Surface representation
- `topLambdasElab` / `instanceLambdasElab` = derived elaborated representation, used by typed-aware passes

### Supporting changes
- `lookupLambdaRep`
- `lookupInstanceLambdaRep`
- `lookupInstanceLambdaByKeyRep`
- `topLambdasByRep`
- `instanceLambdasByRep`

### Writeback changes
- `TypeCheck.typeCheckPass` writes elaborated lambdas into `...Elab`
- `TypeElaborate.typeElaboratePass` writes elaborated lambdas into `...Elab`
- raw maps are no longer overwritten by elaboration

### Result
This fixed the original `normalizeTy` regression and restored architectural clarity.

---

## 3. Next problem uncovered: unresolved `effectSeq` during `:bc run`

### Repro path
Bytecode compilation path:

```text
:bc run run_all
→ Runner.runBytecode
→ compileToBytecodeWith
→ CompileDriver.buildCompilationPlan
```

### Immediate symptom
Compilation failed with unresolved implicit dispatches such as:

```text
Monomorphization failed — 6 unresolved implicit dispatch call(s):
  - effectSeq in assert_int
  - effectSeq in assert_float
  - effectSeq in run_all
  - effectSeq in assert_str
  - effectSeq in assert_char
  - effectSeq in assert_bool
```

### Key facts verified
1. `effectSeq` is introduced by action desugaring in `Pipeline.hs`:

```hs
desugarActionStmts (ActionExpr e : rest) =
    App (Id "effectSeq")
        [ e
        , Function (mkLambda "" [Var "_" UNDEFINED UNDEFINED] (desugarActionStmts rest) UNDEFINED)
        ]
```

2. `effectSeq` exists as a real algebra function in `lib/Effect.tl`:

```tl
effect Sequence = {
    function effectSeq(a:a, f:a -> b) : b;
    function effectBind(a:a, f:a -> b) : b
};
```

3. For the native target, `effectSeq` is also provided as a handler op in `lib/Backend/LLVM/Native.tl`:

```tl
handler NativeSequence : Sequence = {
    function effectSeq(a, f) = f(a);
    function effectBind(a, f) = f(a)
};
```

So the issue was **not** missing definitions or missing handler mappings.

---

## 4. CompileDriver cleanup: necessary, but not the root cause

### Observed mismatch
After the raw/elab split, `CompileDriver.buildCompilationPlan` was mixing:

- raw reachability computation
- elaborated working lambdas for monomorphization/specialization
- unresolved checking that still assumed a narrower dispatch universe

### Fixes applied
1. **Reachable working-set construction** now projects raw reachable keys into the elaborated-with-fallback representation explicitly, instead of filtering the entire elaborated map view.

2. **`collectUnresolvedCalls`** was made **target-aware** by using the same handler-op universe as monomorphization (`buildHandlerOpsMap targetName env`).

These changes made the driver more coherent and the unresolved reporting more semantically accurate.

### Important result
After those changes, `effectSeq` was still reported unresolved.

This proved the core issue was **upstream**: `effectSeq` was genuinely surviving monomorphization instead of being rewritten.

---

## 5. Root cause of `effectSeq`: typed-wrapped callee heads bypassed dispatch resolution

### The key function
`Monomorphize.resolveImplicitCalls` contained a special branch for dispatch resolution:

```hs
App (Id funcName) args -> ...
```

This works only if the application head is a bare `Id`.

### What changed after the raw/elab split
Compilation now feeds **elaborated** lambdas to monomorphization. In elaborated lambdas, callee heads are often wrapped in `Typed`, e.g.:

```hs
App (Typed (Id "effectSeq") ty) [arg1, arg2]
```

### Failure mode
Such a node does **not** match:

```hs
App (Id funcName) args
```

So it fell through to the generic recursion case:

```hs
App f args -> App (go f) (map go args)
```

which recursively traversed the tree but never invoked the dispatch-resolution logic for `effectSeq`.

### Evidence
Focused debug run on `assert_int` showed lots of dispatch logs for other functions but **no**:

```text
[mono] resolve effectSeq → handler op
```

This was the smoking gun.

### Fix applied
Added a helper to peel transparent `Typed` wrappers from callee heads, conceptually:

```hs
calleeHeadName :: Expr -> Maybe Name
calleeHeadName (Id n) = Just n
calleeHeadName (Typed e _) = calleeHeadName e
calleeHeadName _ = Nothing
```

and changed dispatch recognition from:

```hs
App (Id funcName) args -> ...
```

to a head-aware form:

```hs
App f args
  | Just funcName <- calleeHeadName f -> ...
```

Also made it consider handler ops directly:

```hs
isImplicit || hasTargetInstance || Map.member funcName handlerOps
```

### Result
Focused repro now logs:

```text
[mono] resolve effectSeq → handler op
[mono] resolve putStrLn → handler op
```

and `effectSeq` no longer appears in unresolved-call reporting.

---

## 6. Remaining unresolved cluster: `/`, `foldr`, `foldl`, and conversion fallout

After fixing `effectSeq`, compilation still fails, but now on a different set of unresolveds:

- `/` in `lib/System/Time.tl` `show(Duration)`
- `foldr` / `foldl` in generic list/foldable code
- `byteToInt` / `intToByte` in string code (likely secondary fallout)

### Definitions verified
#### `/`
Used in `lib/System/Time.tl` inside `Show(Duration)`:

```tl
show(ns / 1000000000)
```

#### `foldr` / `foldl`
Declared in `lib/Collection.tl` and concretely implemented for `List` in `lib/Instances/List.tl`.

#### `byteToInt` / `intToByte`
Defined directly in `lib/Str.tl` as explicit intrinsic functions:

```tl
function intToByte(n:Int) : Byte = intrinsic;
function byteToInt(b:Byte) : Int = intrinsic;
```

So the problem is **not missing library code**.

---

## 7. Current best diagnosis of the remaining unresolveds

### Core issue
The next systemic bug appears to be in **type recovery inside `Monomorphize.hs`**, which still contains several raw-syntax assumptions and therefore performs poorly on elaborated terms.

### Weak point 1: `exprToTypeName`
Current implementation is extremely narrow:

```hs
exprToTypeName (Id n) = ...
exprToTypeName (App (Id n) _) = ...
exprToTypeName _ = Nothing
```

This misses common elaborated shapes such as:

- `Typed e t`
- typed-wrapped type constructors
- nested elaborated type forms

So even when a usable type is present in the elaborated AST, type-name recovery often fails.

### Weak point 2: `inferExprTypeName`
Its fallback for applications also assumes a bare callee head:

```hs
inferExprTypeName env _ (App (Id n) _) = ...
```

This is the same structural bug family as the `effectSeq` issue.
If the head is `Typed (Id "/") ty` or `Typed (Id "foldr") ty`, the fallback never fires.

That means:
- return types are not recovered from env metadata
- argument types cannot be inferred robustly
- instance selection fails with unresolved dispatches

### Why `foldr` / `foldl` fit this diagnosis especially well
These are overloaded algebra methods that require the monomorphizer to infer container types well enough to select `Foldable(List)`.
If the elaborated AST carries that information but the helper functions fail to see through `Typed`, the calls remain unresolved.

### Why `/` also fits
`/` is overloaded numeric dispatch. It similarly depends on recovering concrete argument or result types from elaborated terms.

### Why `byteToInt` / `intToByte` may be secondary
These are ordinary top-level intrinsic functions, not handler ops and not class instances. Their appearance in the unresolved cluster likely indicates surrounding type recovery failure or a downstream consequence of unresolved generic code, rather than genuinely missing definitions.

---

## 8. About the `run_all` hang

### What is known
- `echo ':bc run run_all' | stack exec tulam` on full `BCTest_Core.tl` reportedly hangs / was interrupted.
- We reproduced faster compile failures on smaller slices and on the full test when unresolved dispatches are surfaced earlier.

### Current assessment
The evidence so far suggests that the hang is **probably not the same issue as `effectSeq`**.
It more likely reflects a broader monomorphization/specialization fixed-point or performance problem triggered by very large generic reachable sets.

### Why this assessment is reasonable
The debug logs show:
- many repeated resolution attempts
- many specialization candidate scans
- broad generic traversal across core library code

That pattern is more consistent with a pre-existing scaling issue than with the specific handler-op bug we fixed.

### Recommendation
Do **not** conflate the two issues:
- first fix correctness bugs in dispatch/type recovery
- then revisit `run_all` hang with targeted performance instrumentation

---

## Recommendations

## A. Keep the raw/elaborated split
This remains the correct architectural direction.

### Rationale
- Raw Surface consumers (`normalizeTy`, partial Surface→CLM bridges) should not consume elaborated trees implicitly.
- Typed-aware passes (`Monomorphize`, `Specialize`) should operate on elaborated trees, but must be structurally robust to `Typed` wrappers.

### Action
Preserve these invariants:
- `topLambdas` / `instanceLambdas` = canonical raw env
- `topLambdasElab` / `instanceLambdasElab` = derived elaborated env

---

## B. Finish making `Monomorphize` elaboration-aware
The `effectSeq` fix addressed only one instance of the broader problem.

### Next changes recommended
#### 1. Strengthen `exprToTypeName`
Make it robust to elaborated types. At minimum, support:

```hs
exprToTypeName (Typed _ t) = exprToTypeName t
```

and recursively unwrap transparent `Typed` wrappers around type constructors.

#### 2. Strengthen `inferExprTypeName`
Use the same kind of callee-head normalization as `resolveImplicitCalls`.

Conceptually:

```hs
calleeHeadName :: Expr -> Maybe Name
calleeHeadName (Id n) = Just n
calleeHeadName (Typed e _) = calleeHeadName e
calleeHeadName _ = Nothing
```

Then change application inference from bare-head matching to head-view matching.

#### 3. Audit other monomorphization helpers for raw-head assumptions
Search for patterns like:

```hs
App (Id n) ...
Id n
```

in logic that is supposed to operate on elaborated terms.

---

## C. Treat handler-op resolution and typeclass resolution as separate concerns
The current code path mixes:
- top-level implicit algebra functions
- target instances
- handler ops

This is manageable, but the code should stay explicit about which dispatch universe it is using.

### Action
Continue using:
- raw env for reachability keys
- elaborated view for mono/spec working lambdas
- target-aware unresolved checking

---

## D. Investigate remaining unresolveds in this order
### Priority 1
Fix elaboration-aware type recovery:
- `exprToTypeName`
- `inferExprTypeName`

This is likely to resolve most of:
- `/`
- `foldr`
- `foldl`

### Priority 2
Reassess `byteToInt` / `intToByte`
If they remain after type-recovery fixes, then inspect whether they are being:
- wrongly classified as dispatch heads,
- or left inside bodies whose surrounding overloaded calls never resolved.

### Priority 3
Revisit `run_all` hang separately
Once correctness failures are reduced, instrument:
- monomorphization iteration counts
- specialization growth
- repeated unresolved-call sets

to locate performance blowup.

---

## Suggested immediate patch plan

1. **Patch `exprToTypeName`** to peel `Typed` and recognize elaborated constructor heads.
2. **Patch `inferExprTypeName`** to inspect callee heads through transparent wrappers.
3. Rerun focused repros:
   - `:bc run assert_int`
   - a minimal `System.Time.show(Duration(...))` path
   - a minimal list-fold path
4. Only then reattempt `:bc run run_all`.

---

## Final assessment

### What is fixed
- The architectural raw/elab representation bug.
- The `effectSeq` unresolved handler-op bug caused by typed-wrapped callee heads.
- CompileDriver’s working-set construction and unresolved-call accounting are now cleaner and more explicit.

### What remains
- `Monomorphize` still has additional raw-syntax assumptions in type recovery helpers.
- These now appear to be the main reason overloaded calls like `/`, `foldr`, and `foldl` survive unresolved.
- The reported `run_all` hang likely reflects a separate scaling/fixed-point issue and should be investigated after the remaining correctness bugs are addressed.

---

## One-sentence recommendation

**Do not back out the raw/elaborated split. Keep it, and finish the job by making monomorphization’s type recovery fully elaboration-aware (`Typed`-transparent) before investigating remaining performance issues like the `run_all` hang.**
