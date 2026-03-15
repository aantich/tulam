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

### Phase 1 — preserve current raw/elaborated split

Keep:

- `topLambdas` / `instanceLambdas` as raw canonical representation
- `topLambdasElab` / `instanceLambdasElab` as elaborated representation

This is already the correct first separation.

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
