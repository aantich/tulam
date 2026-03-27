# Tulam Architecture Guiding Principles

Status: draft working note  
Purpose: durable design guidance for future compiler work  
Scope: type theory, elaboration, core IR, monomorphization, specialization, and migration strategy

---

## 1. Executive summary

Tulam should move toward a **small, typed, universe-polymorphic elaborated core** where:

1. `Type`, `Kind`, `Type2`, `Type3`, ... are not fundamentally separate species of objects, but points in a single universe hierarchy.
2. Overloading, algebra/instance dispatch, morphisms, handler operations, and reflection/meta implicits are made explicit during elaboration.
3. Monomorphization and specialization operate over that explicit elaborated representation rather than reconstructing intent from surface syntax.
4. Instance keys and specialization keys remain useful as backend/codegen labels, but they must not be the primary source of semantic truth.

The most important architectural principle is:

> **Do not recover solved typing/evidence decisions from syntax after the fact. Preserve them explicitly at the elaboration boundary.**

This becomes even more important if Tulam aims to support **universe-level polymorphism** in a principled and production-grade way.

---

## 2. Guiding principles

### Principle A — One universe hierarchy, not ad-hoc strata

Tulam should converge on a theory where:

- `Type u` is the general form of universes
- `Type`, `Kind`, `Type2`, ... are logical presentations of universe levels, not fundamentally different compiler categories
- abstractions over values, types, type constructors, and higher universes are represented using the same underlying core machinery

This does **not** mean all arguments are operationally identical. It means the logical theory is uniform.

### Principle B — Operational roles must remain explicit

Even in a universe-polymorphic language, the compiler must distinguish:

- runtime arguments
- erased compile-time arguments
- evidence/dictionary arguments
- handler/effect evidence
- reflection/repr/meta arguments

These are not separated by universe level. They are separated by **operational role and relevance**.

### Principle C — Elaboration is the semantic boundary

By the end of type checking / elaboration, overloaded constructs should no longer be semantically ambiguous.

The elaborated program should explicitly record:

- chosen evidence for algebra/instance obligations
- superclass or named-instance projections
- chosen handler operations / target mappings
- explicit intrinsic/direct calls
- erased vs runtime-relevant arguments

Monomorphization must consume this representation directly.

### Principle D — Backend labels are not proof search

Things like instance keys, specialization keys, and backend names are still useful for:

- memoization
- code generation
- deduplication
- caching

But they should represent **already chosen implementations**, not serve as the mechanism for discovering semantics after type checking.

### Principle E — Preserve raw and elaborated representations separately

Raw/source-like representations and elaborated representations serve different consumers.

- raw trees are useful for source-preserving transforms, diagnostics, and partial surface-oriented pipelines
- elaborated trees are the right input to typed middle-end phases

Do not force one representation to play both roles.

### Principle F — The current raw/elaborated split is a real asset and must become an explicit invariant

Repository inspection shows Tulam already has the beginning of the right separation in `State.Environment`:

- raw maps: `topLambdas`, `instanceLambdas`
- elaborated maps: `topLambdasElab`, `instanceLambdasElab`
- representation-aware lookup through `LambdaRep`

This should become a deliberate architectural invariant:

- raw maps are the canonical source-oriented representation
- elaborated maps are the canonical typed middle-end representation
- passes that mutate raw structures must invalidate or rebuild elaborated caches
- middle-end phases should consume elaborated representations on purpose, not by convention

---

## 3. The main architectural lesson from current failures

Recent compiler failures revealed a structural issue:

- monomorphization has been trying to recover dispatch and type information from partially elaborated syntax
- helper functions infer flat type names from expression heads
- calls with typed wrappers, higher-kinded obligations, named superinstances, or non-dispatch implicits break these heuristics

This approach is inherently fragile.

It especially breaks down for:

- named superclasses (`Group(a) as Multiplicative`)
- higher-kinded algebras (`Foldable(List)`)
- morphisms and multi-parameter evidence
- reflection-bearing intrinsics that are not typeclass dispatch
- future universe-polymorphic programming where there may be no useful flat “type name” to recover

Therefore the compiler should move away from **heuristic recovery from syntax** and toward **typed explicit elaboration**.

---

## 4. Recommended core design

## 4.1 A small typed elaborated core

Tulam should grow a core IR with at least the following properties:

- universe-aware types (`Type u`)
- explicit binders
- relevance/erasure annotations
- role annotations on binders and arguments
- explicit applications
- explicit evidence terms
- explicit handler/effect operations
- explicit direct intrinsic calls

Conceptually, binders/arguments should carry metadata like:

```text
BinderInfo =
  { visibility : Explicit | Implicit
  , relevance  : Runtime | Erased
  , role       : Ordinary | Evidence | Handler | Reflection | Meta
  }
```

The parameter type itself may live in any universe.

## 4.2 Evidence should be first-class in the elaborated IR

Constraints should be treated as types of witnesses/evidence.

Examples:

- `Field(Int)`
- `Foldable(List)`
- `Convertible(a, b)`
- target/handler obligations

A solved obligation should elaborate to an explicit evidence term or evidence application, rather than being re-derived later by heuristic key lookup.

Conceptually:

```text
Evidence
  = EvVar x
  | EvInstance instanceKey
  | EvSuperclass Evidence projection
  | EvHandler handlerName opName
  | EvBuiltin builtinTag
```

The exact shape can vary, but the design goal is fixed: **preserve the solver’s choice**.

## 4.3 Separate semantic call kinds

The elaborated IR should distinguish at least:

- direct function calls
- evidence-backed overloaded calls
- handler/effect calls
- intrinsic calls
- reflection/meta-assisted calls

Conceptually:

```text
CallDirect f args
CallEvidence head evidence args
CallHandler op handlerEvidence args
CallIntrinsic prim args
```

This distinction is operational, not universe-based.

## 4.4 Superclass and named-instance projection must be explicit

Tulam’s algebra system supports things like:

- inheritance/extension
- named projections such as `Group(a) as Multiplicative`

These relationships should not be rediscovered from string keys in monomorphization.

They should elaborate into structured evidence transformations, e.g.:

```text
EvSuperclass evField MultiplicativeGroupProjection
```

That gives a sound basis for inherited methods like `/`.

## 4.5 Higher-kinded and universe-polymorphic obligations must remain structured

A call like `foldl` does not dispatch on a flat value type. It dispatches on a constructor witness such as `Foldable(List)`.

That obligation should be represented structurally in the elaborated IR, not flattened into guessed names.

This is essential for:

- higher-kinded abstractions
- generic containers
- future universe-polymorphic algebras
- principled reuse of the type checker’s results

---

## 5. What this means for universe-level polymorphism

Tulam’s long-term design should support the following viewpoint:

- universe levels are part of the logical type of terms and constraints
- evidence terms are universe-parametric
- elaboration and monomorphization should not care whether a witness originated from a “type-level” or “kind-level” abstraction
- the only distinctions that matter operationally are relevance and role

In other words:

> **Universe level answers “where does this term/type live?” while binder/argument role answers “how is it used by the compiler/runtime?”**

These concerns should remain orthogonal.

This makes explicit-evidence elaboration fully compatible with the desired universal view of `Type`, `Kind`, `Type3`, etc.

---

## 6. Proposed pipeline direction

Recommended long-term pipeline:

```text
Surface syntax
  -> scope/infix/desugaring
  -> type checking + elaboration
  -> typed elaborated core (explicit evidence, explicit roles, universe-aware)
  -> monomorphization / implementation selection
  -> specialization / partial evaluation
  -> lower core / CLM / backend IR
  -> target codegen
```

Key rule:

> The elaboration boundary is where semantic ambiguity ends.

Anything still unresolved after elaboration should be a genuine unsolved obligation, not something rediscovered by later passes.

---

## 7. Concrete redesign suggestions for the core

This is a proposed direction, not a final syntax.

## 7.1 Core term language

A suitable core probably needs constructs along these lines:

```text
Term
  = Var Name
  | Global Name
  | Lam Binder Term
  | Pi Binder Type Term
  | App Term Arg
  | Let Name Term Term
  | Type Universe
  | Ann Term Type
  | Case Term [Alt]
  | Lit Literal
  | Builtin BuiltinName
  | EvidenceTerm Evidence
  | Call CallKind Term [Arg]
```

Where:

```text
Arg =
  { argTerm : Term
  , argInfo : ArgInfo
  }

ArgInfo =
  { visibility : Explicit | Implicit
  , relevance  : Runtime | Erased
  , role       : Ordinary | Evidence | Handler | Reflection | Meta
  }
```

And:

```text
CallKind
  = DirectCall
  | EvidenceCall Evidence
  | HandlerCall Evidence
  | IntrinsicCall BuiltinName
```

The exact AST can be simplified, but these distinctions should exist somewhere in the typed IR.

## 7.2 Constraint/evidence representation

A production version should have structured obligations such as:

```text
Constraint
  = ConClass Name [Term]
  | ConMorphism Name [Term]
  | ConHandler Name [Term]
  | ConEq Term Term
  | ...
```

Evidence terms should point to chosen solutions, potentially with projections.

## 7.3 Erasure and relevance are first-class

Since the language wants powerful implicit/type-level abstractions, Tulam should make erasure explicit rather than inferred ad hoc deep in the backend.

That allows the same core to represent:

- runtime values
- erased type arguments
- erased universe arguments
- erased evidence arguments
- non-evidence meta/reflection arguments

without conflating them.

---

## 8. Migration strategy

Tulam does **not** need a total rewrite all at once.

A staged migration is more realistic.

### Repository findings informing the migration

Current inspection of the compiler modules suggests the following:

- `Surface.hs` is still the shared syntax for source expressions, type expressions, and partially elaborated forms via `Typed`; this is workable as a transition layer but not the desired final typed core
- `State.hs` already contains the key migration hinge through raw vs elaborated lambda maps and `LambdaRep`, and now also includes `clearElaboratedLambdas`, `storeElaboratedLambda`, and representation-aware accessors; however, the raw map transformer `transformLambdaMaps` still does not invalidate elaborated caches by itself
- `TypeCheck.hs` is the semantic center and already produces elaborated lambdas during `typeCheckPass`; it is the strongest current candidate for authoritative elaboration, but `typeAnnotatePass` still mutates raw lambdas after type checking in the main module pipeline, which means the current pass order still weakens the intended authority boundary
- `TypeElaborate.hs` currently acts as a synthetic/fallback elaboration layer and explicitly preserves TC output via left-biased union; that is good transitional behavior, but it remains a second elaboration producer and should still be treated as a bridge rather than the final semantic authority
- `Monomorphize.hs` already prefers `ElabLambdaRep` and writes results back into elaborated maps instead of raw ones; this is an important architectural improvement, but it still reconstructs dispatch through `hasImplicit`, expression-head inspection, and flat type-name heuristics
- `Specialize.hs` now also prefers elaborated instance lambdas through `instanceLambdasByRep ElabLambdaRep`; this is another positive move toward the A+B design, but it still inherits the same heuristic boundary from monomorphization
- `CompileDriver.hs` explicitly projects reachable names into `ElabLambdaRep` before mono/spec and already treats specialized instances separately when checking unresolved calls; this is well aligned with the planned architecture
- `Pipeline.hs` remains the main source of raw/source-level wrappers for implicit dispatch, effect ops, and certain intrinsics, and `lamToCLMPass` still lowers raw lambdas in the main module pipeline; that means backend lowering is still partly owned by raw representation even though compile-driver flows increasingly prefer elaborated reps
- The current main module pipeline order in `ModuleSystem.hs` is still `typeCheckPass` followed by `typeAnnotatePass`; this means authoritative elaboration currently happens before a later raw mutation step, which is the single biggest changed assumption affecting the immediate next steps

These findings strengthen the overall migration direction: preserve the split that already exists, make the elaborated side authoritative, then rebuild mono/spec around explicit evidence. They also refine the immediate priority: the next steps should focus less on inventing new helpers in the abstract and more on fixing the pass-boundary contract so the current elaborated path becomes truly authoritative.

### Phase A — Stabilize representation boundaries

Keep:

- `topLambdas` / `instanceLambdas` as raw canonical representation
- `topLambdasElab` / `instanceLambdasElab` as elaborated representation

This is the first architectural separation already present in the codebase.

This phase should make the following explicit:

- `State.hs` is the architecture anchor for representation ownership
- `TypeCheck.typeCheckPass` is the authoritative producer of elaborated lambdas
- raw-mutating passes must either call `clearElaboratedLambdas` or rebuild elaborated maps immediately
- `TypeElaborate` should be treated as compatibility/fallback enrichment, not the final semantic source of truth
- middle-end consumers (`Monomorphize`, `Specialize`, unresolved-call reporting) should use elaborated representations deliberately

#### Phase A findings from implementation audit

Repository inspection shows the following concrete pass-boundary behavior:

- `processBinding` in `Pipeline.hs` is the primary raw-environment constructor and writes raw lambdas, instance lambdas, constructors, and several Surface-level wrappers for implicit dispatch
- `typeCheckPass` writes TC-produced elaborated lambdas into `topLambdasElab` and `instanceLambdasElab` through `storeElaboratedLambda`; this confirms that the compiler already has a real authoritative-elaboration path in code
- `typeAnnotatePass` still mutates raw `topLambdas` and `instanceLambdas` in place after type checking in the main module pipeline; because this happens after `typeCheckPass`, the current pipeline ordering weakens the architectural claim that elaboration is authoritative
- `typeElaboratePass` synthesizes elaborated views from raw maps and unions them under the existing elaborated maps, preserving TC output when present; this is useful as a bridge but should not define semantics
- `recordDesugarPass` mutates raw lambda bodies through `transformLambdaMaps`, and `transformLambdaMaps` still does not invalidate elaborated caches by itself
- `monomorphizeForTarget` now reads from `topLambdasByRep ElabLambdaRep` / `instanceLambdasByRep ElabLambdaRep` and writes results back into elaborated maps only; this is a strong alignment with the planned architecture
- `specializeLambdas` also now prefers `instanceLambdasByRep ElabLambdaRep`, reinforcing the same trend on the specialization side
- `CompileDriver.buildCompilationPlan` explicitly projects reachable names into `ElabLambdaRep` before mono/spec, which is exactly the intended Phase A+B consumer pattern
- `lamToCLMPass` still lowers raw lambdas to CLM in the main pipeline, so elaborated representation is still not the sole owner of backend-facing semantics

The biggest changed assumption is therefore not just stale-cache risk in the abstract. It is more specific:

1. the codebase has already moved materially toward elaborated ownership in mono/spec/driver, **but**
2. the main module pipeline still runs a raw-mutating `typeAnnotatePass` after authoritative elaboration, and still performs raw CLM lowering afterwards.

So the immediate Phase A priority should be to fix the pass-boundary contract itself, not merely add generic invalidation helpers.

This phase still includes cache-discipline fixes, but they should now be understood as part of a broader pass-order/ownership correction rather than the main story.

### Phase B — Introduce an explicit elaborated core contract

Before adding new semantics, Tulam needs a clearly defined internal contract for what an elaborated lambda/expression is allowed to contain.

The goal of this phase is to stop relying on “Surface syntax plus a few `Typed` wrappers” as the long-term middle-end representation.

Key objectives:

- define the invariants of elaborated expressions and lambdas
- document which information must survive elaboration for all downstream phases
- begin separating source-oriented syntax concerns from typed/middle-end concerns
- decide whether this contract lives as a strengthened elaborated subset of `Surface.Expr` or as a new typed core module

Deliverables for this phase should include:

- a written elaborated-core contract
- clear ownership of elaboration by the type checker
- a reduced semantic role for `TypeElaborate`
- explicit expectations for downstream consumers (mono/spec/backend)

### Phase C — Add binder metadata, relevance, and implicit roles

This is the first major semantic cleanup phase.

The compiler must distinguish, at the elaborated level:

- visibility: explicit vs implicit
- relevance: runtime vs erased
- role: ordinary vs evidence vs handler vs reflection vs meta

This phase is essential for both correctness and future universe-polymorphic generality.

Why it matters:

- hidden arguments are currently conflated operationally
- unresolved-dispatch detection is too coarse because it treats many hidden params alike
- intrinsics carrying reflection-style implicits should not be treated like unresolved typeclass dispatch
- future monomorphization and specialization need to know what survives at runtime and what is compile-time only

Deliverables:

- binder/argument metadata in the elaborated representation
- replacement of coarse `hasImplicit`-style reasoning with role-aware logic
- a clean distinction between evidence-bearing calls and non-dispatch hidden arguments

### Phase D — Preserve explicit evidence from type checking

This is the real architectural turning point.

Type checking and elaboration should not merely solve constraints; they should preserve the solution as explicit internal evidence.

That includes:

- selected instance witnesses
- selected morphism witnesses
- superclass / named-instance projection paths
- handler evidence where applicable

This phase is what makes the design production-grade for:

- named superinstances such as `Group(a) as Multiplicative`
- higher-kinded obligations such as `Foldable(List)`
- future universe-polymorphic abstractions where “recover a flat type name later” ceases to be meaningful

Deliverables:

- an explicit evidence representation
- elaborated calls that preserve chosen witnesses
- structural constraints instead of relying on string-key search as the primary semantic mechanism

Latest validated finding:

- A targeted TC-side change in `TypeCheck.inferAppElab` was implemented so semantic-implicit callees emit structure constraints at application sites, rather than only when names are looked up through `infer`.
- This was directionally correct but **not sufficient** to fix `BCTest_Core`.
- After the change, the compiler still builds and tests, but `tests/run_backends.sh tests/programs/BCTest_Core.tl` still reports:
  - bytecode runtime failures (`1+2 -> <non-int>`, `10-3 -> 42`, then pattern-match failure)
  - native lowering failure due to unresolved `!=` in `==Int`
- Conclusion: the remaining bottleneck is deeper than shallow typed preservation or ad-hoc call-site constraint emission.
- Therefore the next principled step should be to preserve **explicit local obligation/evidence information** at elaborated call sites for semantic-implicit calls, especially in generic local binder contexts.

# Stage R — Evidence-preserving elaborated semantic core

## Executive summary

Stage R is the next major compiler refactor. Its purpose is to make the type-checker/elaborator produce a semantically meaningful elaborated representation that preserves overload-relevant obligations explicitly, while staying fully compatible with Tulam's universe-polymorphic design.

The central design constraint is:

> All lambdas and applications are represented **uniformly**, regardless of the universe level at which they live.

Universe stratification remains in the type language (`Type u` / current `U Level`-style machinery), while operational distinctions such as evidence, handlers, reflection, and intrinsics are modeled orthogonally via binder metadata and call semantics.

## Stage R goals

1. Preserve semantic ambiguity-breaking information at elaboration time.
2. Represent semantic/evidence-bearing calls explicitly instead of reconstructing them from `App` + heuristics.
3. Keep lambda/application representation uniform across universes.
4. Reuse the current parser/frontend and most of the existing typechecker/inference logic.
5. Rewrite the middle-end contract (especially monomorphization) around explicit obligations rather than guessed type names.

## Non-goals for Stage R

- Do not redesign the parser or surface syntax.
- Do not force a full backend rewrite at the start of the phase.
- Do not introduce separate lambda/application calculi for value/type/kind levels.
- Do not continue extending `inferExprTypeName` / string-key heuristics as primary semantics.

## Core type-theoretic principles

### 1. Uniform lambda/application calculus

There is one lambda representation and one application representation. A binder may range over terms, types, constructors, or universe-polymorphic entities, but this is expressed through its **type**, not through different AST families.

### 2. Orthogonal dimensions

Stage R tracks three orthogonal axes:

- **Universe**: logical level where a type lives.
- **Relevance / visibility / role**: operational annotations for binders and arguments.
- **Obligation/evidence semantics**: why a call is semantically valid and what obligation it carries.

These must not be conflated.

### 3. Elaboration ends semantic ambiguity

After elaboration, later phases should not need to guess which semantic obligation made a call well-typed. That information must be preserved explicitly in the elaborated representation.

## Proposed internal modules

### `src/ElabCore.hs`
New module defining the evidence-preserving elaborated representation.

### `src/ElabObligation.hs`
New module defining structured semantic obligations and, later, explicit evidence/witness forms.

### `src/ElabMetadata.hs`
Existing metadata direction retained for visibility/relevance/role.

## Proposed core data design

### Binder information

```hs
data Visibility = Explicit | Implicit

data Relevance = Runtime | Erased

data Role
  = OrdinaryRole
  | EvidenceRole
  | HandlerRole
  | ReflectionRole
  | ReprRole
  | MetaRole


data BinderInfo = BinderInfo
  { binderVisibility :: Visibility
  , binderRelevance  :: Relevance
  , binderRole       :: Role
  }
```

This stays orthogonal to universe level.

### Uniform binders and arguments

```hs
data ElabBinder = ElabBinder
  { elabBinderName :: Name
  , elabBinderType :: ElabExpr
  , elabBinderInfo :: BinderInfo
  }


data ElabArg = ElabArg
  { elabArgExpr :: ElabExpr
  , elabArgInfo :: BinderInfo
  }
```

No separate binder category for type-level vs term-level lambdas.

### Structured obligations

```hs
data ObligationOrigin
  = OriginTopLevelImplicit Name
  | OriginLocalLambda Name
  | OriginCallSite
  | OriginSuperclass ObligationOrigin Name
  deriving (Eq, Show)


data SemanticObligation
  = StructureObligation
      { oblMethod   :: Name
      , oblClass    :: Name
      , oblTag      :: Maybe Name
      , oblArgs     :: [ElabExpr]
      , oblOrigin   :: ObligationOrigin
      }
  | HandlerObligation
      { oblEffect   :: Name
      , oblOp       :: Name
      , oblArgs     :: [ElabExpr]
      , oblOrigin   :: ObligationOrigin
      }
```

This is intentionally structural, not string-key-first.

### Elaborated call kinds

```hs
data ElabCallKind
  = DirectCall
  | SemanticCall SemanticObligation
  | HandlerCall SemanticObligation
  | IntrinsicCall Name
```

These are operational distinctions, not universe-level distinctions.

### Elaborated expressions

```hs
data ElabExpr
  = EVar Name
  | EGlobal Name
  | ELit Literal
  | EType Expr                 -- keeps current universe/type encoding initially
  | EAnn ElabExpr ElabExpr
  | ELam [ElabBinder] ElabExpr ElabExpr
  | ELet [(Name, ElabExpr)] ElabExpr
  | ECase ElabExpr [ElabAlt] ElabExpr
  | EProject FieldAccess ElabExpr ElabExpr
  | ECall ElabCallKind ElabExpr [ElabArg] ElabExpr
  | ESurface Expr              -- transitional escape hatch during migration
```

Notes:
- the final `ElabExpr` may evolve, but this is the right initial shape.
- `ESurface` is explicitly transitional and should shrink over time.
- `ELam` remains uniform; universe information lives in binder/result types.

## Elaboration contract after Stage R

After `TypeCheck` / elaboration:

1. Every overload-relevant call is represented as `ECall`.
2. If the call depends on semantic implicit obligations, it must be `SemanticCall ...` or `HandlerCall ...`.
3. Direct non-semantic calls must be `DirectCall` or `IntrinsicCall ...`.
4. Lambda binders and arguments carry visibility/relevance/role information.
5. Types of semantically relevant subexpressions are structurally observable from `ElabExpr`.
6. Later phases no longer infer obligation kind from `hasImplicit` or flat argument type names.

## Stage R migration plan

### R1 — Define the new elaborated semantic layer

Deliverables:
- `ElabCore.hs`
- `ElabObligation.hs`
- conversions/helpers from current elaborated `Surface` where needed
- temporary `ESurface` fallback for incomplete migration

### R2 — Make `TypeCheck` produce `ElabExpr`

Deliverables:
- elaboration of calls into `ECall`
- direct vs semantic vs handler vs intrinsic distinction
- binder metadata attached to lambda parameters
- local generic overloaded calls preserve structured obligations

### R3 — Store elaborated semantic core in environment

Deliverables:
- environment slots for elaborated semantic lambdas
- rep-aware accessors for the new representation
- raw / old-elab / Stage-R-elab ownership made explicit

### R4 — Rewrite unresolved analysis around obligations

Deliverables:
- `CompileDriver` walks `ElabExpr`
- unresolved reporting based on unresolved obligations, not `hasImplicit`

### R5 — Rewrite monomorphization around `ECall`

Deliverables:
- `Monomorphize` consumes `SemanticCall` / `HandlerCall`
- string keys become backend labels / memo identities, not primary semantics
- intrinsic/reflection/meta calls no longer enter semantic dispatch resolution

### R6 — Retarget specialization

Deliverables:
- specialization keys and cloning driven by semantic call structure
- cleaner interaction with evidence-bearing calls

### R7 — Reduce/remove transitional `ESurface`

Deliverables:
- more of the compiler consumes the new core directly
- Stage R becomes the stable middle-end boundary

## Reuse vs rewrite under Stage R

### Reuse strongly
- parser and frontend
- much of `Surface.hs`
- much of the inference/checking engine in `TypeCheck.hs`
- environment and module loading knowledge
- metadata work from N1/N2/N3

### Refactor deeply
- `TypeCheck` elaboration output
- environment storage/access for elaborated semantic lambdas
- `CompileDriver` unresolved analysis
- `Monomorphize` semantic resolution logic
- parts of specialization assumptions

### Rewrite substantially
- core of monomorphization over semantic calls
- any logic whose semantics currently depend on type-name guessing

## Why Stage R solves the current failures

### `point_sum`
Projection results and the `+` call can be represented with explicit semantic/direct call structure instead of asking mono to guess from `RecField` terms.

### `test_flip`
The local lambda body preserves a semantic call for `-`, including the relevant obligation structure in the generic local context.

### `Str.tl`
`show`, `compare`, `!=`, etc. remain semantic calls; `byteToInt`, `intToByte`, reflection/repr plumbing can be represented as intrinsic/direct/non-semantic calls.

This removes the current category confusion.

## Approval target for implementation

Stage R should be approved as the next major refactor if we agree on these decisions:

1. **Uniform lambdas across universes** are a hard requirement.
2. Introduce a new elaborated semantic layer (`ElabCore`) rather than stretching raw `App` further.
3. Represent overload-relevant calls explicitly via structured obligations.
4. Retarget mono/driver/spec to consume the new layer, instead of continuing heuristic recovery.
5. Allow a transitional `ESurface` escape hatch during migration, but treat it as temporary technical debt.

## Recommended first implementation slice

Start with **R1 + R2**:
- define `ElabCore` / `ElabObligation`
- make `TypeCheck` produce `ECall` for a narrow but important subset first:
  - direct named calls
  - semantic-implicit named calls
  - intrinsic calls where already known
- then store Stage-R elaborated lambdas alongside the current elaborated form for migration safety.

### Phase E — Rewrite monomorphization around explicit evidence

Once elaboration preserves explicit evidence, monomorphization can be simplified and made principled.

The goal is to move from:

- reconstruct dispatch from syntax and guessed type names

To:

- consume explicit chosen evidence from elaboration

This phase should:

- replace heuristic instance recovery as the main path
- interpret explicit evidence-bearing calls directly
- keep instance keys only as backend labels / memoization identifiers
- clean up unresolved-call reporting so it reports only genuinely unsolved obligations

This is likely the largest implementation rewrite, but also the highest-value one.

### Phase F — Retarget specialization and reachability

Specialization should be rebuilt on top of the new elaborated/evidence-aware monomorphization boundary.

Key objectives:

- specialize from typed/evidence-aware call sites rather than guessed argument names
- reduce repeated rediscovery of implementation choices
- control code growth more deliberately using clearer specialization keys
- integrate cleanly with reachability planning in `CompileDriver`

This phase should come after the monomorphization rewrite, not before.

### Phase G — Finalize the typed core and backend contract

At this point Tulam should decide whether to continue using an enriched elaborated `Surface` representation or to fully introduce a distinct typed core IR.

This final architecture phase should stabilize:

- the universe-aware typed core shape
- the lowering contract from elaboration/mono/spec into CLM or backend IR
- which arguments and terms are erased vs runtime-relevant
- how handlers, evidence, and intrinsics appear in lowered code

This is where the compiler becomes internally coherent as a long-lived production architecture rather than a pipeline of partially overlapping representations.

### Recommended execution order

The correct larger roadmap is:

1. **Phase A — Stabilize representation boundaries**
2. **Phase B — Introduce an explicit elaborated core contract**
3. **Phase C — Add binder metadata, relevance, and implicit roles**
4. **Phase D — Preserve explicit evidence from type checking**
5. **Phase E — Rewrite monomorphization around explicit evidence**
6. **Phase F — Retarget specialization and reachability**
7. **Phase G — Finalize the typed core and backend contract**

The important shift is that the project should be planned in these larger semantic phases, not as isolated bug-fix patches.

## 9. Detailed design for combined Phases A + B

Phases A and B should be treated as one combined design effort with two tightly related goals:

1. **make representation ownership explicit and reliable**
2. **define a real elaborated-core contract that downstream phases can trust**

Phase A without Phase B would only add hygiene without giving the compiler a stronger semantic boundary.
Phase B without Phase A would define a better contract on top of unstable pass ownership.
They therefore need to be designed together even if implemented incrementally.

### 9.1 Goals of combined Phases A+B

The combined target is:

- raw/source-oriented representation remains available for parsing, desugaring, and source-to-source transforms
- elaborated representation becomes the authoritative input to middle-end and backend-oriented phases
- the compiler gains a clear answer to: **what semantic information must exist after elaboration, and who owns it**
- downstream passes stop depending on incidental details of `Surface.Expr` and `Typed` wrappers

This phase pair is still intentionally conservative: it should improve architecture and contracts **without yet forcing the full evidence redesign** of later phases.

### 9.2 Strategic decision: do not introduce a fully separate Core IR yet

For Phases A+B, the recommended migration path is:

- **keep using the current elaborated `Surface` representation as the implementation substrate**
- but define a much stricter elaborated subset/contract over it
- postpone a distinct typed `Core` IR until after the semantic roles/evidence work (later Phases C–G)

This is the recommended choice because:

- the repo already contains a real raw/elaborated split in `State.hs`
- `TypeCheck` already produces elaborated lambdas today
- forcing a new IR immediately would create large churn before the new semantics are stable
- the current compiler still needs several semantic clarifications before a final core is worth freezing

In other words:

- **Phase A+B should define the contract**
- **Phase G can finalize the permanent IR once the contract has proven itself**

### 9.3 Representation model after Phases A+B

After Phases A+B, Tulam should explicitly recognize two representation tiers.

#### Tier 1 — Raw representation

Purpose:

- source-faithful representation
- parser output
- desugaring input/output
- syntax-oriented rewrites
- error reporting tied closely to source structure

Canonical storage:

- `topLambdas`
- `instanceLambdas`

Expected properties:

- may omit inferred type information before annotation/checking
- may contain source sugar or wrappers introduced by desugaring
- is allowed to change during early pipeline rewriting
- is **not** the trusted source for backend/middle-end semantic decisions

#### Tier 2 — Elaborated representation

Purpose:

- typed and stabilized middle-end input
- authoritative input to monomorphization, specialization, reachability analysis, and eventually lowering
- contract boundary where semantic ambiguity should have been reduced as far as current architecture allows

Canonical storage:

- `topLambdasElab`
- `instanceLambdasElab`

Expected properties:

- parameter and return types are explicit
- relevant subexpressions are annotated enough for downstream consumers to avoid best-effort recovery where possible
- representation should be stable until invalidated by a raw rewrite or explicitly regenerated
- is the only representation that middle-end consumers should rely on semantically

### 9.4 Ownership and pass responsibilities

The compiler should adopt the following ownership model.

| Concern | Owner after Phases A+B | Notes |
|---|---|---|
| Raw lambda construction | `Pipeline.processBinding` and related raw passes | Raw/source-oriented only |
| Raw source-to-source rewrites | Early pipeline passes such as record desugaring | Must invalidate elaborated caches |
| Type-directed annotation/checking | `TypeCheck` | Primary semantic authority |
| Authoritative elaboration | `TypeCheck.typeCheckPass` | Produces canonical elaborated lambdas |
| Supplemental/fallback elaboration | `TypeElaborate.typeElaboratePass` | Compatibility layer only |
| Middle-end consumption | `Monomorphize`, `Specialize`, `CompileDriver` planning | Must use elaborated representation |
| CLM/backend lowering for compiled paths | should move toward elaborated ownership | Raw lowering is transitional only |

The key rule is:

> **Only the type checker owns the authoritative transition from raw syntax to elaborated semantics.**

### 9.5 Elaboration contract for lambdas

A lambda stored in the elaborated maps after Phases A+B should satisfy all of the following invariants.

#### Lambda-level invariants

1. **Every top-level/elaborated lambda has a complete function type**
   - all parameters have explicit types
   - the result type is explicit

2. **Every instance lambda stored in elaborated form is keyed consistently with the environment’s instance identity**
   - no downstream pass should need to rediscover whether an elaborated lambda belongs to a top-level symbol or instance key

3. **Elaborated lambdas are semantically stable snapshots**
   - if raw lambdas change, elaborated lambdas are considered invalid until rebuilt

4. **Downstream passes may assume elaborated lambdas have already passed type checking**
   - they should not re-derive basic typing information from scratch unless explicitly performing a verification/assertion step

#### Body-level invariants

1. **Critical subexpressions should preserve type information via `Typed` or equivalent explicit annotations**
   - especially where downstream phases currently depend on recovered types

2. **Application spines should be normalized enough that downstream passes can inspect heads/arguments predictably**
   - even if the representation still uses `App`, there should be a documented invariant about how nested applications are formed

3. **Elaborated bodies should distinguish semantic stabilization from mere pretty-printed source recovery**
   - `Typed` in elaborated form is semantic data, not a decoration

4. **Synthetic elaboration may enrich missing annotations, but it must never overwrite more authoritative TC-produced information**

### 9.6 Minimum contract for elaborated expressions

Phases A+B do not yet require a final typed core, but they should define what counts as an acceptable elaborated expression for downstream use.

A valid elaborated expression should, at minimum:

- preserve enough explicit type information to avoid ad-hoc inference for ordinary direct calls
- preserve stable expression-head structure for application inspection
- make universe-bearing forms (`U Level`, etc.) remain explicit rather than implicitly collapsed
- preserve distinctions already represented in syntax (e.g. `Typed`, algebra/member references, constructor references)
- avoid introducing synthetic rewrites in middle-end passes that mutate the semantic meaning of expressions after type checking

This contract is intentionally incomplete with respect to later evidence work. It is a **staging contract**, not the final core calculus.

### 9.7 Invalidation and regeneration rules

This is the heart of Phase A.

The compiler must adopt explicit cache-discipline rules:

#### Invalidation rule

Any pass that mutates any of the following in a semantically relevant way:

- `topLambdas`
- `instanceLambdas`
- top-level type signatures that affect lambda typing
- instance associations or names used to key elaborated lambdas

must immediately do one of:

- clear `topLambdasElab` and `instanceLambdasElab`, or
- rebuild them authoritatively before any later consumer runs

#### Regeneration rule

The canonical regeneration path is:

- raw env prepared
- type annotation/checking completed
- `typeCheckPass` rebuilds authoritative elaborated lambdas
- `typeElaboratePass` may only fill gaps or preserve compatibility

#### Consumer rule

Any pass making semantic decisions about implementation selection, specialization, unresolved dispatch, or backend lowering must either:

- require `ElabLambdaRep`, or
- fail fast if only raw representation is available where elaborated semantics are required

### 9.8 Detailed module-by-module design intent for Phases A+B

#### `State.hs`

This module becomes the architectural anchor.

Design intent:

- expose raw vs elaborated ownership explicitly
- provide helper APIs that make invalidation hard to forget
- centralize representation-sensitive lookup
- document invariants near `LambdaRep`, environment fields, and clearing helpers

Recommended direction:

- add cache-safe raw-transform helpers
- prefer helper-based mutation of lambda maps over open-coded map rewrites
- make it obvious from function names whether a helper touches raw or elaborated state

#### `TypeCheck.hs`

This module remains the semantic authority for Phases A+B.

Design intent:

- own the transition from checked raw terms to elaborated lambdas
- make `typeCheckPass` the single authoritative elaboration build step
- ensure elaborated output satisfies the lambda/body invariants above

Recommended direction:

- strengthen comments/contracts around `inferElab`, `checkTopLevelElab`, and elaborated lambda storage
- make it clearer which type information is guaranteed after checking
- treat elaborated output as a product, not an incidental byproduct

#### `TypeElaborate.hs`

This module becomes transitional infrastructure rather than semantic authority.

Design intent:

- remain available for compatibility and gap-filling
- never outrank TC-produced elaboration
- gradually shrink in semantic importance as the core contract strengthens

Recommended direction:

- document explicitly that it is fallback enrichment
- ensure union logic preserves TC ownership
- avoid growing this module into a second semantic source of truth

#### `Pipeline.hs`

This module should remain responsible for raw construction and early rewrites.

Design intent:

- own raw/source-facing transformations
- avoid silently becoming a second elaboration engine
- use invalidation-safe helpers whenever it rewrites raw lambdas

Recommended direction:

- separate clearly between source desugaring and semantic elaboration
- document hidden wrappers that are inserted here and which of them are still transitional

#### `Monomorphize.hs`, `Specialize.hs`, `CompileDriver.hs`

These modules are downstream consumers in Phases A+B.

Design intent:

- treat elaborated maps as authoritative input
- avoid reading semantic meaning from raw lambdas when elaborated ones exist
- prepare for later explicit-evidence redesign by making the dependency on elaborated structure explicit now

Recommended direction:

- narrow APIs toward `ElabLambdaRep`
- document any remaining places where they still reconstruct semantic meaning heuristically
- keep those heuristics visible as technical debt to be removed in later phases

### 9.9 What Phases A+B explicitly do not solve yet

To keep the scope disciplined, Phases A+B should **not** attempt to finish the following:

- explicit evidence representation
- binder role/relevance metadata
- final monomorphization rewrite
- final specialization strategy
- final typed core IR split

Those belong to later phases.

The purpose of A+B is to create a safe, explicit, and documented semantic boundary so that later semantic work lands on solid ground.

### 9.10 Approval questions for Phases A+B

The following decisions should be treated as approval checkpoints:

1. **Representation strategy**
   - Approve that Phases A+B continue using elaborated `Surface` as the implementation substrate rather than introducing a brand-new Core IR immediately?

2. **Authority of type checking**
   - Approve that `TypeCheck.typeCheckPass` is the authoritative elaboration producer, and `TypeElaborate` is explicitly demoted to compatibility/fallback status?

3. **Consumer contract**
   - Approve that middle-end semantic consumers should rely on `ElabLambdaRep` deliberately and treat raw lambdas as source-oriented only?

4. **Scope discipline**
   - Approve that Phases A+B are about representation ownership and elaboration contract only, without yet introducing explicit evidence or role/relevance metadata?

If these four decisions are approved, Phases A+B have a coherent and implementable design.

### 9.11 Approved Phase A+B migration sequence

These decisions have now been approved and should be treated as the active migration direction for the compiler.

The implementation should proceed as a sequence of landings rather than one large rewrite.

#### Landing A1 — Formalize representation ownership in `State.hs`

Goal:

- make raw vs elaborated ownership impossible to miss in code
- make invalidation/regeneration discipline explicit

Concrete changes:

- strengthen comments and invariants around `Environment`, `LambdaRep`, `topLambdas`, `instanceLambdas`, `topLambdasElab`, `instanceLambdasElab`
- add helper APIs for raw-map mutation that clearly communicate whether elaborated caches are invalidated
- make representation-sensitive lookup helpers the preferred access path in downstream modules

Why this landing comes first:

- it creates the backbone for all later A+B work without changing semantic behavior

#### Landing A2 — Make `TypeCheck.typeCheckPass` the explicit authoritative elaboration build step

Goal:

- turn current practice into an enforced architectural rule

Concrete changes:

- document `typeCheckPass` as the canonical producer of elaborated lambdas
- make elaborated-lambda storage a deliberate output of type checking rather than an incidental side effect
- verify that elaborated top-level and instance lambdas satisfy the A+B lambda invariants

Why this landing matters:

- it gives the compiler a single authoritative semantic boundary before middle-end phases run

#### Landing A3 — Demote `TypeElaborate` to compatibility/fallback enrichment

Goal:

- remove ambiguity about whether type elaboration semantics live in `TypeCheck` or `TypeElaborate`

Concrete changes:

- document `TypeElaborate` as non-authoritative
- ensure its union/merge logic preserves TC-produced elaborated output
- avoid adding new semantic obligations to this module

Why this landing matters:

- later redesign phases need one semantic source of truth, not two competing elaborators

#### Landing A4 — Enforce raw-to-elab invalidation discipline in early pipeline passes

Goal:

- eliminate accidental stale elaborated caches

Concrete changes:

- update raw-mutating passes (`recordDesugarPass`, `typeAnnotatePass`, and any helper-based raw rewrites) so they either invalidate elaborated maps immediately or run only before any elaborated maps exist
- make the intended pass discipline explicit in `Pipeline.hs`

Why this landing matters:

- a trustworthy elaborated contract requires trustworthy cache lifetime rules

#### Landing B1 — Write down the elaborated lambda/expression contract in code-facing form

Goal:

- convert the architecture note into implementation-facing invariants

Concrete changes:

- add code comments and, where feasible, lightweight assertions/checks documenting the required properties of elaborated lambdas
- record the required body-level invariants for application structure, explicit result types, and meaning of `Typed`
- identify any current violations or weak spots as tracked technical debt

Why this landing matters:

- downstream phases need a machine-near contract, not only a prose document in the project root

#### Landing B2 — Audit and tighten downstream consumers to `ElabLambdaRep`

Goal:

- make semantic dependency on elaborated representations explicit

Concrete changes:

- audit `Monomorphize`, `Specialize`, `CompileDriver`, and relevant lowering paths for use of raw lambdas where elaborated ones should be required
- narrow helper APIs so semantic consumers prefer `lookupLambdaRep`, `lookupInstanceLambdaByKeyRep`, and `topLambdasByRep` / `instanceLambdasByRep`
- document remaining raw dependencies as transitional exceptions

Why this landing matters:

- A+B only pay off if downstream consumers actually use the elaborated side intentionally

#### Landing B3 — Clarify the transitional role of raw CLM lowering

Goal:

- make it explicit that current raw-to-CLM lowering is transitional architecture, not the long-term owner of semantics

Concrete changes:

- document the current status of `lamToCLMPass`
- decide which compiled/back-end flows continue to tolerate raw lowering temporarily
- mark the future target as elaborated-driven lowering once later phases land

Why this landing matters:

- it prevents architectural confusion while avoiding premature backend churn in A+B

### 9.12 Recommended implementation order inside approved Phases A+B

The recommended order is:

1. **A1 — Formalize representation ownership in `State.hs`**
2. **A2 — Make `TypeCheck.typeCheckPass` the authoritative elaboration build step**
3. **A3 — Demote `TypeElaborate` to compatibility/fallback enrichment**
4. **A4 — Enforce invalidation discipline in raw-mutating pipeline passes**
5. **B1 — Encode the elaborated contract in code-facing invariants**
6. **B2 — Tighten downstream consumers to `ElabLambdaRep`**
7. **B3 — Clarify the transitional role of raw CLM lowering**

This sequence is intentionally designed so that:

- ownership is stabilized before downstream consumers are tightened
- the semantic source of truth is established before compatibility paths are reduced
- backend/lowering consequences are documented last, after the representation boundary is already clearer

### 9.13 Expected outcome after A+B land

If the above landings are completed successfully, Tulam should have:

- a trusted raw vs elaborated boundary
- a single authoritative elaboration producer
- explicit invalidation/regeneration discipline
- a documented elaborated-expression/lambda contract that downstream phases can rely on
- downstream consumers that intentionally depend on `ElabLambdaRep`
- a cleaner foundation for the next major semantic phases: binder-role metadata and explicit evidence

At that point, the project will be ready to begin Phase C without carrying as much ambiguity from the current mixed representation model.

## 10. Robust next stage beyond Phases A+B

After re-inspecting the current codebase, the right next stage is now clearer.

Phases A+B remain necessary, but they are no longer the whole story. The repository has already moved materially toward elaborated ownership in `TypeCheck`, `Monomorphize`, `Specialize`, and `CompileDriver`. That means the next serious stage should not be framed as generic cleanup alone. It should be framed as the first **type-theoretic middle-end redesign stage**.

The key architectural diagnosis is now:

- the compiler already has the beginnings of a raw/elaborated split
- the current elaborated representation is still `Surface` plus `Typed` wrappers rather than a principled typed core
- monomorphization and unresolved-call reporting still reconstruct semantics from `hasImplicit`, expression heads, and flat type-name recovery
- superclass inheritance, tagged parent propagation, and default-method propagation already exist in the environment layer, which means later evidence handling must be able to model projected and inherited implementations explicitly rather than by ad-hoc key guessing
- the language’s universe story already points toward a uniform hierarchy (`U Level`), so the next stage must preserve universe-parametricity rather than accidentally reintroducing type/kind-specific machinery

Taken together, these observations imply that the next robust stage should be:

> **Stage N: introduce a typed elaborated core contract with explicit operational annotations, while still reusing the existing `Surface` implementation substrate as a migration vehicle.**

This stage comes conceptually after A+B but should be designed early so A+B are implemented in a direction that supports it.

### 10.1 Purpose of Stage N

The purpose of the next stage is to create an internal representation boundary that is strong enough to support:

- universe-level polymorphism
- higher-kinded structures and instances
- explicit future evidence passing
- principled monomorphization
- a later clean split into a permanent typed core IR

This stage should still avoid a giant rewrite. It should instead introduce the semantic ingredients that later phases will need.

### 10.2 Core design principle

The compiler should distinguish three orthogonal concerns:

1. **universe level** — where a term/type lives (`U Level`)
2. **relevance/erasure** — whether an argument matters at runtime
3. **operational role** — what a binder/argument means operationally

The third axis is the missing one today.

Current code often treats “has an implicit parameter” as if that were enough to classify a call semantically. That is not robust. In a production universe-polymorphic compiler, a hidden argument might be:

- typeclass/algebra evidence
- morphism evidence
- handler evidence
- reflection support
- representation/repr support
- meta/compiler-only plumbing

These must not be conflated.

### 10.3 Recommended semantic additions in the next stage

The next stage should introduce, conceptually first and then in code, binder and argument metadata of the following form:

```text
BinderInfo =
  { visibility : Explicit | Implicit
  , relevance  : Runtime | Erased
  , role       : Ordinary | Evidence | Handler | Reflection | Repr | Meta
  }
```

This should be understood as an architectural target, not necessarily as the exact final Haskell datatype.

The key idea is:

- **visibility** is not enough
- **universe** is not enough
- **role** must be tracked explicitly

This is fully compatible with the language’s desired universe-polymorphic design because the role is operational, not universe-based.

### 10.4 Why this is the correct next stage

This stage addresses the real semantic failures that remain in the codebase:

#### Unresolved dispatch collection is still too coarse

`CompileDriver.collectUnresolvedCalls` still uses:

- `lookupLambda`
- `hasImplicit`
- handler-op membership

That means unresolved reporting still confuses “has hidden args” with “requires unresolved semantic dispatch”.

#### Monomorphization is still reconstructive rather than consumptive

`Monomorphize.resolveImplicitCalls` still relies on:

- `hasImplicit`
- `exprToTypeName`
- `inferExprTypeName`
- `resolveTargetInstanceMulti`

So even though it consumes `ElabLambdaRep`, it still tries to recover semantic choices instead of reading them directly.

#### Universe-polymorphic future demands richer internal distinctions

Once the language supports more uniform programming across universe levels, any design that depends on flattening type structure into a small set of names becomes increasingly fragile.

The next stage therefore needs to prepare the compiler to represent semantic intent more directly.

### 10.5 Proposed representation strategy for the next stage

The recommended strategy is:

- **do not create the final permanent Core IR yet**
- **do introduce a typed-elaborated contract that carries explicit metadata beyond bare `Typed` wrappers**
- **encode it incrementally inside the current representation boundary if needed**

Concretely, this means the next stage should probably introduce one of:

1. enriched binder metadata attached to `Var` / lambda params and possibly application args, or
2. a small sidecar elaborated descriptor layer used by TC and consumed by later passes, or
3. a minimal new internal module for elaborated metadata that still reuses `Surface.Expr` as the expression tree

The exact encoding may evolve, but the semantic goal should remain fixed.

### 10.6 Recommended sub-phases of the next stage

The next robust stage should itself be split into the following sub-phases.

#### N1 — Make pass-boundary ownership truly correct

This finishes the unfinished business of A+B before semantic enrichment begins.

Concrete goals:

- move or redesign `typeAnnotatePass` so raw mutation does not occur after authoritative elaboration
- ensure raw-mutating transforms invalidate elaborated caches automatically
- document that raw CLM lowering is transitional architecture

This is still prerequisite work, but it should now be done specifically in service of the richer next stage.

#### N2 — Introduce operational-role metadata

Concrete goals:

- classify implicit binders/arguments by role
- stop using `hasImplicit` as the main semantic detector
- make it possible to distinguish evidence from reflection/repr/meta plumbing

This is the single most important semantic cleanup before explicit evidence itself lands.

#### N3 — Tighten the elaborated expression contract around typed applications

Concrete goals:

- specify how application spines are represented after elaboration
- preserve enough information to identify argument visibility/relevance/role in elaborated calls
- ensure `Typed` wrappers are semantic and stable, not best-effort annotations

This sub-phase should prepare the path toward explicit evidence applications.

#### N4 — Redefine unresolved semantic obligations

Concrete goals:

- change unresolved-call collection so it tracks real unsolved semantic obligations rather than merely heads with hidden parameters
- separate unresolved evidence obligations from handler obligations and from harmless reflection/meta args

This will likely improve diagnostics immediately even before full evidence representation exists.

#### N5 — Prepare monomorphization for evidence consumption

Concrete goals:

- narrow the places where monomorphization is allowed to guess from names/types
- concentrate heuristic recovery behind explicit compatibility functions
- make the future replacement path obvious

This is the bridge from the current implementation to the later explicit-evidence rewrite.

### 10.7 Reuse vs rewrite judgment after latest inspection

After the latest code review, the reuse judgment is now more precise.

#### Strong reuse

- `Surface.hs` universe hierarchy and core expression family as a migration substrate
- `State.hs` representation split and environment machinery
- large parts of `TypeCheck.hs` inference/checking machinery
- `CompileDriver.hs` planning structure
- parent/tag/default propagation logic in `Pipeline.hs` as environment-building knowledge

#### Reuse with strong refactoring

- `TypeElaborate.hs` as temporary compatibility machinery
- `ModuleSystem.hs` pipeline staging
- unresolved-call reporting in `CompileDriver.hs`

#### Major redesign / eventual rewrite

- the semantic core of `Monomorphize.hs`
- the specialization boundary in `Specialize.hs` once monomorphization changes
- any path that treats raw CLM lowering as the semantically authoritative compiled form

So the compiler still does **not** need a total rewrite. But it **does** need a real middle-end semantic redesign.

### 10.8 Production-quality judgment

If the goal is a great production compiler, the right guiding judgment is:

- keep the frontend and most of the type checker
- strengthen the elaborated boundary
- add operational-role metadata before full explicit evidence
- then rewrite monomorphization around preserved semantics instead of reconstructive heuristics

This is the smallest path that is still compatible with:

- universe-level polymorphism
- higher-kinded structures
- named/tagged parent instances
- robust specialization and backend lowering

### 10.9 Recommended immediate next actions

The next actions should be:

1. **review and fix pipeline/pass ownership**
   - especially `typeCheckPass` / `typeAnnotatePass` ordering and raw mutation after elaboration

2. **design the operational-role metadata layer**
   - before introducing full evidence terms

3. **change unresolved-call analysis to operate on roles, not `hasImplicit`**

4. **plan the explicit-evidence stage only after role metadata and pass ownership are stable**

This is the most robust next stage available from the current codebase.

## 11. Concrete next-stage design: operational-role metadata and elaborated-call contract

This section refines the “robust next stage” into a more concrete design that can be implemented incrementally.

The purpose of this stage is **not yet** to introduce the final permanent typed core or full explicit evidence terms. Instead, it introduces the missing semantic structure that later evidence-aware monomorphization will rely on.

The guiding rule is:

> **Keep the universe story logically uniform, but make runtime/elaboration roles operationally explicit.**

That means Tulam should avoid separate “type-level vs kind-level” compiler paths while still distinguishing, for compilation purposes, whether a hidden argument is ordinary, evidence, handler-related, reflection-related, repr-related, or purely meta.

### 11.1 Core semantic axes

The next-stage elaborated contract should track three orthogonal axes.

#### Axis A — Universe

Tulam should continue treating the universe hierarchy uniformly through existing `Level` / `U Level` machinery. Nothing in this stage should introduce special-case compiler logic that treats kind-level code as fundamentally different from type-level code.

#### Axis B — Relevance

Arguments and binders should be classified by whether they matter at runtime.

Recommended conceptual categories:

- `Runtime`
- `Erased`

This stage does not need a very rich relevance lattice yet. A simple runtime-vs-erased split is enough.

#### Axis C — Operational role

This is the new semantic axis that should be introduced explicitly.

Recommended conceptual categories:

- `OrdinaryRole` — ordinary user-level parameters
- `EvidenceRole` — algebra/class/morphism-style semantic evidence
- `HandlerRole` — effect handler or operation-routing evidence
- `ReflectionRole` — implicit arguments used for type/reflection support
- `ReprRole` — representation witnesses / backend-facing repr support
- `MetaRole` — compiler-only or internal elaboration plumbing

The exact constructor names can change, but the semantics should remain stable.

### 11.2 Recommended binder metadata model

The compiler should move toward annotating binders/parameters with metadata conceptually equivalent to:

```text
Visibility = Explicit | Implicit
Relevance  = Runtime | Erased
Role       = OrdinaryRole | EvidenceRole | HandlerRole | ReflectionRole | ReprRole | MetaRole

BinderInfo =
  { binderVisibility : Visibility
  , binderRelevance  : Relevance
  , binderRole       : Role
  }
```

This is intentionally orthogonal to the universe story:

- a binder can live in any universe level
- and independently be explicit/implicit, runtime/erased, ordinary/evidence/etc.

That preserves the desired universe-polymorphic language model.

### 11.3 Recommended argument metadata model

A matching argument-side view should also exist conceptually.

```text
ArgInfo =
  { argVisibility : Visibility
  , argRelevance  : Relevance
  , argRole       : Role
  }
```

Later, this may either:

- be stored explicitly in application nodes, or
- be recovered from elaborated callee type plus binder metadata

For the next stage, either encoding is acceptable as long as the middle-end can reliably distinguish real evidence/handler applications from reflection/meta plumbing.

### 11.4 Recommended elaborated-call contract

The current compiler still represents most calls as `App` over `Surface.Expr`, often enriched only with `Typed` wrappers.

The next stage should retain this implementation substrate if necessary, but define a stronger contract for elaborated call sites.

At elaborated call sites, the compiler should be able to determine:

1. the callee head
2. the full application spine
3. which arguments are explicit vs implicit
4. which arguments are runtime relevant vs erased
5. which hidden arguments are semantic evidence/handler arguments versus reflection/repr/meta support

This does **not** yet require a separate `CallEvidence` node. But it does require enough metadata to classify applications reliably.

### 11.5 Transitional representation options

The most practical options for this stage are:

#### Option 1 — enrich existing binder data in `Surface`

Advantages:

- minimal IR churn
- easiest migration path

Disadvantages:

- keeps semantic metadata mixed with transition-era syntax

#### Option 2 — add a sidecar elaboration metadata layer

For example, TC could produce elaborated lambdas plus binder/call metadata accessible to middle-end passes.

Advantages:

- reduces immediate AST churn
- lets semantics evolve without rewriting all expression constructors

Disadvantages:

- requires careful synchronization between expression tree and side metadata

#### Option 3 — introduce a minimal elaborated-core module while still reusing `Surface.Expr`

For example, a new module could define:

- binder info structures
- elaborated lambda wrappers
- maybe application-spine descriptors

without yet defining an entirely separate term language.

Advantages:

- cleaner long-term migration path
- avoids overloading raw `Surface` too much further

Disadvantages:

- more up-front work than the sidecar approach

### Recommendation

For Tulam’s current state, the best next-stage choice is likely **Option 3 in a minimal form**:

- keep `Surface.Expr` as the underlying tree for now
- introduce a new small elaboration-facing module for metadata and contracts
- avoid a full new core term language until explicit evidence lands

This gives the compiler a place to express role/relevance semantics without prematurely rewriting the whole IR.

### 11.6 How unresolved semantic obligations should change

Current unresolved-call logic is too coarse because it treats hidden-argument presence as a proxy for semantic dispatch.

The next-stage rule should be:

- only **EvidenceRole** and possibly **HandlerRole** contribute to unresolved semantic obligations
- **ReflectionRole**, **ReprRole**, and **MetaRole** must not by themselves cause a call to be reported as unresolved dispatch

This means unresolved-call analysis should evolve from:

- “callee has implicit params”

to:

- “callee or elaborated call site still requires unconsumed semantic evidence/handler resolution”

That is a much more principled basis for diagnostics and planning.

### 11.7 How monomorphization should use this stage

Before explicit evidence terms exist, monomorphization should use the new metadata layer to narrow and isolate heuristics.

The intended rule is:

- only calls classified as containing unresolved `EvidenceRole` / `HandlerRole` arguments are candidates for semantic resolution
- hidden arguments classified as reflection/repr/meta should be ignored by dispatch resolution
- any remaining type-name recovery should live behind compatibility helpers and be clearly transitional

This is the bridge from the current reconstructive monomorphizer to a future evidence-consuming monomorphizer.

### 11.8 Module mapping for the next stage

The next-stage work maps naturally onto the current repository modules.

#### `Surface.hs`

Role in this stage:

- remain the migration substrate for `Expr`, `Lambda`, `Var`, `Level`, `Typed`
- possibly accept small metadata additions only if needed

Recommended approach:

- avoid large direct expansion of `Surface.Expr` right away unless a minimal metadata hook is clearly beneficial

#### New module(s) recommended

A small new elaboration-focused module should likely be introduced. Candidate names:

- `ElabContract.hs`
- `ElabMetadata.hs`
- `CoreMetadata.hs`

Its responsibilities should include:

- `Visibility`, `Relevance`, `Role`
- binder/argument metadata structures
- helper functions for classifying semantic vs non-semantic hidden args
- possibly wrappers for elaborated lambdas/call spines

This is the cleanest place to put the new stage’s semantics.

#### `TypeCheck.hs`

Role in this stage:

- become the producer of binder-role/relevance metadata in addition to elaborated lambdas
- remain the authoritative source of semantic classification

Expected changes:

- classify hidden binders it introduces or propagates
- preserve enough call information for downstream semantic analysis
- avoid leaving role classification to monomorphization

#### `TypeElaborate.hs`

Role in this stage:

- transitional compatibility only

Expected changes:

- if it synthesizes elaborated information, it must also preserve or defer to metadata produced by TC
- it must not invent conflicting role semantics

#### `State.hs`

Role in this stage:

- store and retrieve the new elaborated metadata alongside elaborated lambdas, or provide the access hooks needed by middle-end consumers

Expected changes:

- possibly extend environment storage with metadata keyed similarly to elaborated lambdas
- preserve representation-aware ownership boundaries

#### `CompileDriver.hs`

Role in this stage:

- shift unresolved-call collection from `hasImplicit`-style detection toward role-aware obligation detection

Expected changes:

- use the new metadata layer to decide what counts as a real unresolved semantic obligation

#### `Monomorphize.hs`

Role in this stage:

- become role-aware before becoming fully evidence-aware

Expected changes:

- restrict dispatch resolution to calls that actually carry semantic evidence/handler roles
- ignore reflection/repr/meta hidden args for dispatch purposes
- concentrate remaining heuristic recovery behind compatibility boundaries

#### `Specialize.hs`

Role in this stage:

- mostly follow monomorphization’s new boundary

Expected changes:

- little direct redesign at first, but ensure it consumes the same role-aware elaborated data

#### `Pipeline.hs` and `ModuleSystem.hs`

Role in this stage:

- ensure pass ownership is compatible with metadata production and preservation

Expected changes:

- fix raw-vs-elaborated pass ordering first
- avoid introducing hidden args in ways that lose semantic role classification

### 11.9 Proposed implementation landings for the next stage

#### Landing N1 — Correct pass ownership before metadata propagation

- fix `typeCheckPass` / `typeAnnotatePass` boundary
- ensure raw mutation after elaboration does not silently occur
- ensure metadata would not be invalidated immediately after creation

#### Landing N2 — Introduce the metadata module and core datatypes

- define `Visibility`, `Relevance`, `Role`
- define binder/argument metadata helpers
- define the notion of “semantic hidden argument” versus “non-semantic hidden argument”

#### Landing N3 — Thread role metadata out of `TypeCheck`

- annotate elaborated lambdas/binders with role-aware metadata
- make TC the owner of this classification

#### Landing N4 — Make unresolved-call analysis role-aware

- revise `CompileDriver.collectUnresolvedCalls`
- stop treating all implicit-bearing calls as unresolved semantic dispatch

#### Landing N5 — Make monomorphization role-aware

- revise `resolveImplicitCalls` and related helpers
- ignore non-semantic hidden args
- isolate remaining compatibility heuristics

#### Landing N6 — Evaluate whether the metadata layer is sufficient or whether the project is ready for explicit evidence terms

- after N2–N5, reassess whether the remaining ambiguity is small enough to proceed directly to explicit evidence
- or whether a minimal dedicated elaborated-core term layer is needed first

### 11.10 Approval criteria for this next stage

This stage should be considered successful if it yields all of the following:

- the compiler can distinguish semantic evidence/handler hidden args from reflection/repr/meta hidden args
- unresolved-call reporting no longer flags direct intrinsics merely because they carry hidden plumbing
- monomorphization only attempts semantic resolution on appropriately classified calls
- the design remains fully compatible with a uniform universe hierarchy
- later explicit evidence introduction has a clear landing zone

## 12. Implementation-ready plan for N1 + N2

This section turns the next-stage architecture into an approval-ready implementation plan for the first two concrete landings:

- **N1** — correct pass ownership and raw/elaborated lifecycle
- **N2** — introduce the metadata module and foundational role/relevance datatypes

These two landings should be implemented together in sequence because N2 must not be built on top of a pass pipeline that still mutates raw lambdas after authoritative elaboration.

---

### 12.1 N1 — Pass ownership correction

#### Goal

Make the raw → elaborated boundary trustworthy before metadata is threaded through it.

#### Current architectural problem

The latest code review shows that the main module pipeline still runs:

- `typeCheckPass`
- then `typeAnnotatePass`

But `typeAnnotatePass` mutates raw `topLambdas` and `instanceLambdas` in place. That means authoritative elaboration happens before a later raw mutation step.

This is the largest remaining mismatch with Phases A+B.

#### N1 design decision

Adopt the following ownership rule:

> **All raw-shape or raw-type enrichment passes must run before authoritative elaboration.**

That means `typeAnnotatePass` should no longer run after `typeCheckPass` in the main module-processing pipeline.

#### Recommended implementation strategy for N1

##### N1.1 Reorder the main pipeline

Update `ModuleSystem.processModule` so that all raw preparation steps happen before authoritative elaboration.

Target direction:

1. raw/source desugaring and transformation passes
2. raw type-annotation enrichment (`typeAnnotatePass`) if still needed
3. authoritative type checking / elaboration (`typeCheckPass`)
4. compatibility/fallback elaboration (`typeElaboratePass`) if still needed
5. later middle-end / lowering consumers

The exact local ordering among early raw passes can remain conservative as long as no raw mutation occurs after step 3.

##### N1.2 Define invalidation discipline in `State.hs`

Even after pipeline reorder, raw-mutating helpers should remain safe by construction.

Required change:

- introduce a helper or helper pair in `State.hs` so that raw lambda map transforms either:
  - always invalidate elaborated caches, or
  - are explicitly split into safe raw-only and raw+invalidate variants

Recommended interface direction:

- keep a low-level transformation helper for tightly controlled use if necessary
- add a preferred helper used by pipeline passes that transforms raw lambda maps **and clears elaborated maps**

The point is to make the safe path the default path.

##### N1.3 Update raw-mutating passes to use the safe path

At minimum, audit and update:

- `recordDesugarPass`
- `typeAnnotatePass`
- any other pass that rewrites `topLambdas` / `instanceLambdas`

If a pass may run after elaborated maps exist, it must invalidate them automatically.

##### N1.4 Clarify `typeElaboratePass` pipeline position

`typeElaboratePass` should remain after `typeCheckPass`, but only as compatibility/fallback enrichment.

Required rule:

- it may merge or backfill elaborated information
- it must never outrank or silently replace TC-produced elaboration

The current left-biased merge behavior is aligned with this and should be preserved.

##### N1.5 Document raw CLM lowering as transitional

`lamToCLMPass` currently lowers raw lambdas.

N1 should not rewrite backend lowering yet, but it should make the architecture explicit:

- raw lowering remains transitional
- elaborated ownership is the intended future direction
- raw lowering must not be used as an argument against making elaborated semantics authoritative for middle-end reasoning

#### N1 files expected to change

- `src/ModuleSystem.hs`
- `src/State.hs`
- `src/Pipeline.hs`
- possibly `src/TypeCheck.hs` / `src/TypeElaborate.hs` comments and invariants

#### N1 success criteria

N1 is successful if:

- no raw lambda mutation occurs after `typeCheckPass` in the main pipeline, or such mutation automatically invalidates elaborated caches
- authoritative elaboration is no longer weakened by later raw mutation
- `typeElaboratePass` remains fallback-only
- the codebase has a defensible raw→elab ownership story

---

### 12.2 N2 — Metadata module and foundational data model

#### Goal

Introduce the minimal semantic metadata needed to distinguish different classes of hidden binders/arguments without yet introducing full explicit evidence terms.

#### N2 design decision

Introduce a new small module dedicated to elaboration metadata.

Recommended file:

- `src/ElabMetadata.hs`

This module should define the foundational datatypes and helper predicates that later passes will consume.

#### Recommended N2 data model

##### Core enums

```text
Visibility = Explicit | Implicit
Relevance  = Runtime | Erased
Role       = OrdinaryRole | EvidenceRole | HandlerRole | ReflectionRole | ReprRole | MetaRole
```

##### Binder metadata

```text
BinderInfo =
  { binderVisibility :: Visibility
  , binderRelevance  :: Relevance
  , binderRole       :: Role
  }
```

##### Argument metadata

```text
ArgInfo =
  { argVisibility :: Visibility
  , argRelevance  :: Relevance
  , argRole       :: Role
  }
```

##### Helper predicates

At minimum:

- `isSemanticRole :: Role -> Bool`
  - true for `EvidenceRole` and `HandlerRole`
- `isSemanticHiddenArg :: ArgInfo -> Bool`
- `isNonSemanticHiddenArg :: ArgInfo -> Bool`
- `defaultExplicitBinderInfo :: BinderInfo`
- `defaultImplicitBinderInfo :: BinderInfo`

The purpose of these helpers is to centralize semantics instead of scattering ad-hoc `hasImplicit` logic.

#### N2 storage strategy

For the first landing, avoid a giant AST rewrite.

Recommended storage approach:

- start by attaching metadata at the lambda/binder level, not the full expression-tree level
- make `TypeCheck` the authoritative producer of binder metadata for elaborated lambdas
- if full argument metadata at every call site is not yet practical, allow an initial phase where `CompileDriver` / `Monomorphize` recover call-role meaning by consulting binder metadata of the elaborated callee

This gives a realistic migration path:

1. classify binders first
2. classify elaborated call sites better later
3. introduce explicit evidence terms after the role layer stabilizes

#### N2 environment/storage recommendation

To avoid overloading `Surface` too early, extend elaborated-side storage rather than rewriting the full AST immediately.

Recommended direction:

- augment environment storage with metadata keyed by elaborated lambda name / instance key
- or add metadata fields to an elaborated-lambda wrapper structure if the current code already has a natural wrapper point

The exact encoding should be chosen for minimal churn, but the ownership rule should be:

> **metadata lives with authoritative elaborated information and is produced by `TypeCheck`.**

#### N2 producer/consumer responsibilities

##### `TypeCheck.hs`

- authoritative producer of binder-role metadata
- classifies the binders it introduces or normalizes
- should assign at least a conservative first-pass role classification

##### `TypeElaborate.hs`

- must preserve metadata when present
- may synthesize only conservative defaults when TC metadata is absent
- must not invent conflicting semantics

##### `CompileDriver.hs`

- next landing after N2 should consume binder metadata to refine unresolved-call analysis

##### `Monomorphize.hs`

- next landing after N2 should use metadata to decide which hidden args can trigger semantic resolution

#### Initial conservative classification policy

For the first metadata landing, the classification policy should be conservative and explicit.

Recommended initial defaults:

- ordinary explicit user parameters → `OrdinaryRole`
- ordinary implicit parameters with no known semantic meaning → `OrdinaryRole`
- known effect-handler plumbing → `HandlerRole`
- known reflection-type support (`__refl`-style machinery and equivalents) → `ReflectionRole`
- known repr-specific plumbing → `ReprRole`
- internal-only elaboration artifacts → `MetaRole`
- algebra/class/morphism-style implicit obligations where the compiler already knows they are semantic dispatch inputs → `EvidenceRole`

It is acceptable if the first landing classifies some cases conservatively as `OrdinaryRole` or `MetaRole` until the evidence layer is stronger. The key requirement is to stop conflating reflection/repr/meta with semantic dispatch.

#### N2 files expected to change

- new: `src/ElabMetadata.hs`
- `src/TypeCheck.hs`
- `src/TypeElaborate.hs`
- `src/State.hs`
- possibly light touchpoints in `src/CompileDriver.hs` / `src/Monomorphize.hs` only if needed for plumbing, but those behavioral changes are better treated as the next landing after N2

#### N2 success criteria

N2 is successful if:

- the repository has a single shared definition of visibility/relevance/role metadata
- authoritative elaborated lambdas can carry binder-level role metadata
- metadata ownership clearly belongs to the elaborated side
- later role-aware unresolved analysis and monomorphization become straightforward follow-up landings

---

### 12.3 Recommended implementation order for N1 + N2

1. **N1.1** reorder pipeline ownership
2. **N1.2 / N1.3** make raw mutation invalidation-safe
3. **N1.4 / N1.5** preserve fallback-elaboration and document transitional lowering
4. **N2** add `ElabMetadata.hs`
5. thread binder metadata out of `TypeCheck`
6. store metadata with authoritative elaborated information

This order keeps the metadata layer from being built on a stale or ambiguous representation lifecycle.

---

### 12.4 Explicit non-goals for N1 + N2

To keep scope disciplined, N1 + N2 should **not** yet attempt to:

- introduce explicit evidence terms
- rewrite monomorphization semantics
- redesign specialization
- introduce the final permanent typed core IR
- solve every hidden-arg classification perfectly on the first pass

Those belong to later landings.

---

### 12.5 Approval checkpoint for implementation

Implementation of N1 + N2 should proceed under the following approved assumptions:

1. `typeCheckPass` remains the authoritative elaboration producer
2. `Surface.Expr` remains the short-term migration substrate
3. a new metadata module is the preferred place for role/relevance semantics
4. binder-level metadata is the right first landing before full call-site evidence
5. backend/lowering redesign is intentionally postponed

### 12.6 Implementation outcome (completed landing)

N1 + N2 have now been implemented in the codebase in an intentionally conservative form.

#### Implemented N1 outcomes

- `runPhase2Passes` in `ModuleSystem.hs` was reordered so that:
  - `typeAnnotatePass` runs before `typeCheckPass`
  - `typeCheckPass` remains the authoritative elaboration producer
  - `typeElaboratePass` now runs explicitly afterward as compatibility/fallback enrichment
- `recordDesugarPass` now uses a raw-transform helper that invalidates elaborated caches automatically
- `clearElaboratedLambdas` now clears both elaborated lambda maps and binder-metadata maps
- the raw/elaborated lifecycle is therefore more coherent than before: raw enrichment occurs before authoritative elaboration, and later raw rewrites invalidate elaborated snapshots

#### Implemented N2 outcomes

- a new module `src/ElabMetadata.hs` was introduced
- foundational metadata types were added:
  - `Visibility`
  - `Relevance`
  - `Role`
  - `BinderInfo`
  - `ArgInfo`
- helper predicates were added, including semantic-role classification helpers
- `State.Environment` now stores binder metadata alongside elaborated lambdas via:
  - `topLambdaBinderInfo`
  - `instanceLambdaBinderInfo`
- `TypeCheck.typeCheckPass` now stores binder-level metadata for authoritative elaborated lambdas

#### Important design note about the initial classification

The current N2 classification is deliberately conservative and heuristic. It is meant to establish the metadata pathway and ownership model, not to claim that the compiler already has perfect semantic-role classification.

That is acceptable at this stage because the real architectural win is:

- metadata now has a stable home
- TC owns its production
- later passes can begin consuming it instead of relying only on `hasImplicit`

#### Validation outcome

After the landing:

- `stack build` succeeds
- `stack test` succeeds (`532 examples, 0 failures`)
- one-by-one loading across the curated files under `tests/` completed successfully within timeout bounds
- no hangs were observed in that load-only sweep

#### What this means architecturally

This landing does **not** yet solve monomorphization semantically. But it does complete the first robust staging step toward that goal:

- pass ownership is substantially improved
- elaborated snapshots are less stale-prone
- binder-level role metadata exists and is preserved on the elaborated side

The next stage should therefore focus on **consuming** this metadata:

1. make unresolved semantic-obligation collection role-aware
2. make monomorphization role-aware before full explicit evidence terms
3. only then proceed toward explicit evidence representation

### Phase 2 — enrich elaborated representation with explicit roles/evidence

Before replacing everything, augment the elaborated side with:

- explicit binder metadata
- explicit callee/call classification
- explicit evidence representation where instance resolution succeeds

This can initially coexist with legacy expression forms.

### Phase 3 — migrate monomorphization to consume explicit evidence

This is the highest-value change.

Once monomorphization stops guessing from syntax, many current correctness issues should disappear.

### Phase 4 — tighten unresolved-obligation reporting

At this stage unresolved-call reporting should only track genuinely unsolved evidence/handler obligations, not arbitrary hidden parameters.

### Phase 5 — move specialization onto the new core

After monomorphization is evidence-aware and structurally correct, specialization can be retargeted to the same elaborated core with better control over code growth.

---

## 9. Reuse vs rewrite assessment

## 9.1 Type checker (TC)

### Reuse level: substantial reuse likely

The current type checker likely remains the **best source of semantic truth** and should not be discarded casually.

What can likely be reused:

- parsing and front-end desugaring structure
- constraint generation ideas
- substantial inference/checking machinery
- environment/model of known algebras, instances, handlers, and top-level symbols
- zonking/substitution/unification infrastructure (assuming it is reasonably modular)

What likely needs redesign inside TC/elaboration:

- the output contract of elaboration
- how solved obligations are represented and returned
- how implicit arguments are classified by role
- how superclass/named-instance projections are recorded
- how universe information is preserved explicitly in the elaborated result

Conclusion:

> **Do not rewrite the whole TC first. Refactor it so its output becomes a typed elaborated core with explicit evidence.**

## 9.2 Monomorphization (mono)

### Reuse level: concepts reusable, implementation likely needs major rewrite

The *purpose* of monomorphization remains valid:

- choose concrete implementations
- instantiate reachable polymorphic code
- remove unresolved overloaded dispatch before backend lowering

However, the current implementation appears too dependent on:

- raw/elaborated surface syntax shapes
- type-name recovery heuristics
- string-key search as semantic reconstruction
- coarse implicit/detect-dispatch logic

These are exactly the wrong foundations for the long-term design.

Conclusion:

> **Monomorphization likely needs a major rewrite around explicit evidence, even if some supporting utilities and environment plumbing can be reused.**

Reuse candidates:

- reachable-set plumbing
- backend target integration
- caching/memoization concepts
- instance-key generation as labels for already chosen implementations

Likely rewrite:

- call resolution logic
- unresolved-obligation tracking
- type recovery assumptions
- instance selection entry points

## 9.3 Specialization (spec)

### Reuse level: partial reuse, but only after mono is fixed

Specialization sits downstream of correct implementation selection.
If the input IR is semantically ambiguous, specialization will either explode or preserve wrong structure.

What can likely be reused:

- high-level role of specialization
- some cost heuristics / worklist ideas
- some substitution/clone utilities

What likely needs redesign:

- specialization keys should be derived from typed/evidence-aware core entities
- growth control should understand erased vs runtime arguments
- repeated scans/fixed-point behavior should be instrumented and probably simplified

Conclusion:

> **Do not try to perfect specialization on top of the current ambiguous mono boundary. First make mono principled; then retarget specialization.**

## 9.4 Overall judgment

A full ground-up rewrite of the entire compiler is probably unnecessary and risky.

A more realistic assessment is:

- **Frontend / parser / desugaring**: mostly reusable
- **Type checking core algorithms**: reusable in large part, but elaboration output must be redesigned
- **Monomorphization**: concept reusable, current implementation likely needs major rewrite
- **Specialization**: keep the phase, but likely redesign substantial parts after the new mono boundary exists
- **Backend lowering / CLM bridge**: reusable where it consumes concrete code, but may need adaptation to a cleaner typed core

One additional practical conclusion from repository inspection is that the first safe landing should not be “invent a whole new core in one jump.” It should be:

1. formalize and enforce the current raw/elaborated split,
2. make elaborated representations authoritative for middle-end phases,
3. classify implicit/binder roles explicitly,
4. then introduce explicit evidence and retarget monomorphization.

In short:

> **Reuse the front-end and much of the type-checking engine. Rewrite the semantic boundary between elaboration and monomorphization. Then rebuild mono/spec around that boundary.**

---

## 10. Anti-goals

Tulam should avoid the following long-term strategies:

- relying on flat type-name recovery as the primary dispatch mechanism
- encoding semantic decisions only in environment string keys
- treating all hidden implicits as if they meant typeclass-style dispatch
- collapsing raw and elaborated representations back together
- hard-coding separate compiler mechanisms for “type-level” vs “kind-level” constructs
- optimizing specialization before establishing a sound typed middle-end

---

## 11. One-paragraph north star

Tulam’s north-star architecture should be a **universe-polymorphic, typed elaboration pipeline** in which the compiler preserves explicit evidence, argument roles, relevance, and universe information across the boundary from type checking into middle-end transformations. Monomorphization and specialization should become consumers of that explicit semantic representation rather than heuristic reconstructors of intent from syntax. This is the most promising route to a robust production compiler that supports higher-kinded abstractions, named superinstances, effect handlers, and future universe-level polymorphism without accumulating fragile special cases.

## 12. Working agreement for this document

This file is the shared architecture reference for ongoing Tulam compiler redesign work.

Process agreement:

- update this note whenever important repository findings change the design picture
- prefer revising this document over scattering architectural decisions across transient chat-only reasoning
- keep `findings.md` as an investigation/debug log and keep this file as the durable design north star
- when implementation experience contradicts the current design, update this note rather than silently drifting away from it
