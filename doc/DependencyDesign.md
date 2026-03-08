# Content-Addressed Dependency Management for tulam

## 1. Problem Statement: Why Dependency Hell Happens

Dependency hell is the single most complained-about problem in software ecosystem management. It manifests in several concrete forms:

**Diamond dependency.** Library A depends on Library C v1.x. Library B depends on Library C v2.x. Your project depends on both A and B. The package manager must pick one version of C, but A's code breaks with v2 and B's code breaks with v1. This is unsolvable in any system where exactly one version of a library can exist at a given name in a given scope.

**Semantic versioning lies.** Semver promises that 1.2.3 to 1.3.0 is backward-compatible. In practice, it is not. Any change to observable behavior -- including performance characteristics, error messages, exception types, or the order of items in a collection -- can break downstream consumers. Semver is a social contract, not a mechanical guarantee.

**Version resolution is NP-hard.** When every package pins version ranges on its dependencies, and those ranges intersect imperfectly, the resolver must solve a SAT problem. Cargo, pip, npm, and Maven all have resolvers that can take minutes or fail entirely on large dependency graphs.

**Phantom dependencies.** Package A depends on package B which depends on package C. Your code accidentally uses something from C (because it happens to be in scope). When B upgrades and drops C, your code breaks -- even though you never explicitly depended on C.

**Instance / typeclass coherence.** In Haskell, orphan instances are a particular form of dependency hell: two packages define `instance Show MyType` differently, and linking both packages into one program is a compile error. This is unsolvable with version numbers.

These problems share a root cause: **names and version numbers are the unit of identity**, and they are unreliable identifiers for code.

---

## 2. Core Design: Content-Addressed Definitions

### 2.1 The Central Idea

Every definition in tulam -- function, type, algebra, morphism, instance -- is identified by a cryptographic hash of its **normalized abstract syntax tree**. Names are metadata that point to hashes. Two definitions are "the same" if and only if they have the same hash.

This is inspired by Unison, but adapted to tulam's specific features: its algebra/morphism system, its constructor-tag-based sum types, its multi-backend compilation, and its implicit-parameter-based instance dispatch.

### 2.2 What Gets Hashed

A definition's hash is computed from its **normalized AST** after desugaring (Pass 0) but before environment building (Pass 1). Specifically:

**Included in the hash:**
- The structure of the AST (node types, tree shape)
- All type annotations (parameter types, return types)
- Literal values
- Constructor tags (the integer tags that tulam uses for sum type discrimination)
- References to other definitions, **by their hashes** (not by name)
- Universe levels (`U 0`, `U 1`, etc.)
- Structure kind (`SAlgebra`, `SMorphism`, `SGeneral`)
- Law bodies (propositional equalities, implications)

**Excluded from the hash:**
- The definition's own name (names are metadata, not identity)
- Comments and whitespace
- Source locations (`SourceInfo`)
- Module declarations (`ModuleDecl`)
- Import statements (imports are resolved to hashes before hashing)
- Parameter names in lambda bindings (alpha-equivalence: `function f(x:Int):Int = x + 1` and `function f(y:Int):Int = y + 1` produce the same hash)

**Alpha-normalization.** Before hashing, all bound variable names are replaced with de Bruijn indices. This ensures that `function f(x:Int):Int = x + 1` and `function g(n:Int):Int = n + 1` have the same hash.

**Dependency hashes.** When a definition references another definition, the reference is replaced by the referenced definition's hash. This makes the hash **transitively content-addressed**: if any transitive dependency changes, the hash changes.

### 2.3 Hash Computation in tulam Terms

Given tulam's existing AST from `Surface.hs`, the hash is computed roughly as:

```
hash(Function lam) =
  H("Function"
    || hash(normalize_params(params lam))
    || hash(normalize_expr(body lam))
    || hash(lamType lam))

hash(SumType lam) =
  H("SumType"
    || hash(normalize_params(params lam))
    || hash_constructors(body lam))

hash(Structure lam si) =
  H("Structure"
    || hash(structKind si)
    || hash(normalize_params(params lam))
    || hash(normalize_expr(body lam))
    || hash_extends(structExtends si)
    || hash_requires(structRequires si))

hash(Instance name typeArgs impls reqs) =
  H("Instance"
    || hash(resolve(name))          -- hash of the structure being instanced
    || map hash(resolve(typeArgs))  -- hashes of the type arguments
    || map hash(normalize_expr(impls))
    || map hash(resolve(reqs)))
```

Where `resolve(name)` looks up the content hash of the definition that `name` currently points to, and `H` is a cryptographic hash function (SHA-256 or BLAKE3).

### 2.4 The Content Store

The content store is a persistent, content-addressed database mapping hashes to definitions:

```
~/.tulam/store/
  index.db            -- hash -> metadata (name hints, type signature, source location, backends)
  objects/
    ab/cd1234...      -- serialized normalized AST for this hash
  names/
    project.json      -- name -> hash mappings for the current project
    published/
      MyLib.json      -- name -> hash mappings for published library "MyLib"
```

The store is append-only for objects (you never delete a hash). Name mappings are mutable (you can point a name at a different hash).

### 2.5 How Diamond Dependencies Disappear

Consider the classic diamond:

```
Your project
  depends on LibA  (which uses  sort : List(a) -> List(a)  with hash #abc123)
  depends on LibB  (which uses  sort : List(a) -> List(a)  with hash #abc123)
```

If both libraries use the same implementation of `sort`, they have the same hash. There is no conflict. There is no diamond. There is only one `sort`, and it lives at `#abc123`.

If LibA uses a different `sort` than LibB, they have different hashes. Both can coexist in the content store. Each library's code references its own hash. No conflict.

The only case that causes trouble is when your project tries to pass the output of LibA's sort to LibB's code that expects a different sort. But this is a **type error**, not a dependency error -- the type checker catches it, and the error message tells you exactly what happened.

---

## 3. Module & Import System Integration

### 3.1 Layering on the Existing Module System

tulam already has a module system with `ModuleDecl`, `Import`, `Open`, `Export`, `PrivateDecl`, and `OpaqueTy` AST nodes. The `ModuleSystem.hs` module handles path resolution, dependency graph construction, cycle detection, and topological sort. The content-addressing layer sits **underneath** this existing system, not beside it.

The key change: when the module system resolves an import, it resolves **names to hashes**. The existing `loadModuleTree` in `Main.hs` would gain a step between parsing and pipeline execution: resolve all name references to their content hashes in the store.

### 3.2 Import by Name (the common case)

```tulam
module MyProject;
import Algebra.Semigroup;           -- resolved to hashes from project's name mapping
import Core.List (map, filter);     -- specific names resolved to specific hashes
import Morphism.Convertible as C;   -- qualified: C.convert resolves to a hash
```

When you write `import Algebra.Semigroup`, the system:
1. Looks up "Algebra.Semigroup" in your project's name mapping file
2. Finds that it points to a set of name-to-hash bindings (e.g., `combine -> #aabb11`, `Semigroup -> #ccdd22`, `Monoid -> #eeff33`, ...)
3. Makes those names available in the current scope, each backed by their hash

This is exactly how the current module system works, except each name is backed by a hash rather than a mutable definition in the global `Environment`.

### 3.3 Import by Hash (for pinning)

For absolute precision, you can import by hash directly:

```tulam
import #aabb11cc as combine;     -- pin to exact implementation
import #ccdd2233 as Semigroup;   -- pin to exact algebra definition
```

This is unusual in practice but useful for:
- Reproducible builds (lock files are just lists of name-to-hash bindings)
- Debugging (which exact implementation am I using?)
- Forensics (when did this definition change?)

### 3.4 How `open` and `export` Work

`open Algebra.Semigroup` works the same as today -- it brings all names into scope -- but each name resolves to a hash.

`export Algebra.Semigroup` re-exports the name-to-hash bindings, meaning downstream consumers get the same hashes you used.

### 3.5 Interaction with `PrivateDecl` and `OpaqueTy`

`private` definitions still get hashed and stored. They are simply not included in the module's public name-to-hash mapping. The hash exists in the store; it is just unreachable by name from outside the module.

`opaque` types export the type's hash but not its constructors' hashes. Consumers can reference the type by hash but cannot pattern-match on it or construct values of it. This is enforced by the name mapping, not by the hash itself.

### 3.6 Namespace Separation

Content addressing naturally gives namespace separation. Today, all symbols pour into one flat `Environment`. With content addressing, each module's scope is a `HashMap Name Hash`, and name resolution walks the import chain. Two modules can define a function named `combine` -- they will have different hashes because they have different implementations (or the same hash if the implementations happen to be identical, which is fine).

---

## 4. Structure Constraints as Dependencies

### 4.1 The Insight

tulam's algebra and morphism system already provides structural contracts. An `algebra Ord(a:Type)` says: "any type `a` that has `compare`, `<`, `>`, `<=`, `>=` functions with specific signatures." This is a **semantic interface**, not a version number.

In a content-addressed world, we can go further: instead of saying "I depend on library X version 2.3 which provides Ord(Int)", you say "I depend on something that provides Ord(Int)." The type checker resolves this against whatever instances are available in scope.

### 4.2 Example: Library Author

```tulam
module Collections.SortedSet;
import Algebra.Ord;

type SortedSet(a:Type) = Leaf | Node(left:SortedSet(a), value:a, right:SortedSet(a));

function insert [a:Type] (x:a, s:SortedSet(a)) : SortedSet(a) = match
    | x, Leaf -> Node(Leaf, x, Leaf)
    | x, Node(l, v, r) -> match compare(x, v)
        | LessThan    -> Node(insert(x, l), v, r)
        | Equal       -> Node(l, v, r)
        | GreaterThan -> Node(l, v, insert(x, r));
```

This module's hash includes:
- The hash of the `Ord` algebra definition (the structural contract)
- The hash of the `SortedSet` type
- The hash of the `insert` function (which references `compare` by the hash of `Ord`'s `compare` declaration)

It does **not** include any specific `Ord(Int)` or `Ord(String)` instance. Those are provided by the consumer.

### 4.3 Example: Library Consumer

```tulam
module MyApp;
import Collections.SortedSet;
import Algebra.Ord;
import Numeric.Int;              -- provides instance Ord(Int) = intrinsic

function main() : SortedSet(Int) = insert(3, insert(1, insert(2, Leaf)));
```

The type checker sees that `insert` requires `Ord(a)`, we're calling with `a = Int`, and `Ord(Int)` is in scope from `Numeric.Int`. Resolved.

### 4.4 How This Replaces Version Pinning

Traditional: "I depend on `collections >= 2.0, < 3.0` because I need `SortedSet` which was added in 2.0 and the API changed in 3.0."

Content-addressed: "I depend on `insert : (a, SortedSet(a)) -> SortedSet(a)` at hash `#1234abcd`, which itself depends on `Ord(a)` at hash `#5678efgh`." If the author changes `insert`'s implementation without changing its type signature or behavior, the hash changes but the type signature hash stays the same -- and the type checker can verify compatibility.

### 4.5 Structure Compatibility Checking

The type checker already has `Constraint` types including `CStructure` for structure constraints. The content-addressed system extends this: when checking whether two algebras are "compatible," we compare their **structural hashes** (the hash of the algebra definition minus instance bodies):

```
structural_hash(algebra) = H(structKind || params || function_signatures)
  -- excludes: law bodies, default implementations, names
```

Two versions of `Ord` are structurally compatible if they have the same function signatures even if the law bodies differ.

---

## 5. The Package/Library Layer

### 5.1 What is a "Package"?

A package is a **curated set of name-to-hash bindings** plus metadata. It is not a unit of code; it is a unit of naming.

```json
{
  "name": "tulam-collections";
  "author": "someone";
  "description": "Sorted sets, balanced trees, priority queues";
  "bindings": {
    "Collections.SortedSet.SortedSet": "#aabb1122...";
    "Collections.SortedSet.insert": "#ccdd3344...";
    "Collections.SortedSet.lookup": "#eeff5566...";
    "Collections.PriorityQueue.PQueue": "#ccdd9900...";
    "Collections.PriorityQueue.push": "#eeff1122..."
  };
  "structure-deps": {
    "Algebra.Ord": "#5678efgh..."
  }
}
```

The `bindings` section maps qualified names to content hashes. The `structure-deps` section declares which algebra/morphism definitions this package assumes.

### 5.2 Publishing

Publishing a package means:
1. Upload all referenced content hashes to a shared content store (or verify they already exist)
2. Upload the package manifest (the name-to-hash mapping)
3. Optionally, upload source text for each hash (for documentation browsing)

There is no version number. If the author publishes new bindings, they publish a new manifest. Consumers choose which manifest to use.

### 5.3 Multiple Versions Coexisting

Since definitions are identified by hash, "v1" and "v2" of a function simply have different hashes. Both exist in the content store. Both can be used in the same project if needed:

```tulam
import #aabb1122 as sortV1;    -- old sort implementation
import #ccdd3344 as sortV2;    -- new sort implementation

function migrate(xs:List(Int)) : List(Int) = sortV2(xs);
```

### 5.4 Lock Files

A lock file is a complete, flattened name-to-hash mapping for every transitive dependency:

```json
{
  "resolved": {
    "Algebra.Semigroup.combine": "#aabb11...";
    "Algebra.Semigroup.Semigroup": "#ccdd22...";
    "Collections.SortedSet.insert": "#ccdd3344...";
    ...
  }
}
```

Because the lock file contains only hashes, and hashes are deterministic, the build is perfectly reproducible regardless of what the content store or package registry looks like at build time (as long as the hashes are available).

---

## 6. Upgrade & Migration

### 6.1 What "Upgrading" Means

Upgrading a dependency means updating name-to-hash bindings in your project's manifest. There is no version resolution algorithm. You are simply saying: "the name `sort` now points at `#newHash` instead of `#oldHash`."

### 6.2 Automatic Compatibility Checking

When you update a binding, the system checks:

1. **Type signature compatibility.** Does the new hash have the same type signature as the old one?

2. **Structure constraint compatibility.** Does the new hash require the same (or fewer) structure constraints? If old `sort` required `Ord(a)` and new `sort` additionally requires `Eq(a)`, that is a breaking change.

3. **Transitive impact.** The system walks all definitions that reference the old hash and checks whether they would still type-check with the new hash.

### 6.3 Migration Tooling

```bash
tulam upgrade tulam-collections    # check for new manifest, show diff
tulam upgrade --check              # dry-run: show what would change, flag incompatibilities
tulam upgrade --auto               # auto-upgrade all compatible changes
tulam refs #oldHash                # show all definitions that transitively depend on this hash
tulam diff #hash1 #hash2           # show structural diff between two definitions
```

### 6.4 Gradual Migration

Because old and new hashes coexist, you can migrate gradually:
1. Add the new manifest alongside the old one
2. Update references one module at a time
3. Run the type checker after each change to verify consistency
4. Remove the old manifest when no references remain

---

## 7. Capability-Based Security

### 7.1 Effect Tracking

tulam's algebra system provides a natural hook for capability tracking. An IO-performing function can be modeled as requiring a specific algebra:

```tulam
algebra IO(a:Type) = {
    function perform(action:IOAction(a)) : a
};

function readConfig [cap:FileIO] (path:String) : Config = ...;
```

When you import a definition by hash, the system can inspect its transitive structure constraints and report which capabilities it requires:

```
$ tulam inspect #aabb1122
  readConfig : String -> Config
  requires: FileIO, NetworkIO
```

### 7.2 Sandboxing

```tulam
import UntrustedLib with { deny NetworkIO };
```

The system checks that no definition reachable from `UntrustedLib`'s bindings transitively requires `NetworkIO`. If any does, the import fails at compile time.

This works because capabilities are expressed as structure constraints, and structure constraints are part of the hash. The check is purely static.

### 7.3 Open Question: Effect Granularity

How fine-grained should capabilities be? Recommendation: start coarse (FileIO, NetworkIO, SystemIO, PureComputation) and let the ecosystem evolve finer-grained capabilities as algebras.

---

## 8. Instance Coherence

### 8.1 The Problem

Instance coherence is the hardest open problem in this design. tulam's instance dispatch (via `CLMIAP`) uses `mkInstanceKey` to look up instance implementations keyed by `"funcName\0typeName"`. In a content-addressed world, what happens when two different libraries define `instance Ord(MyType)` with different implementations?

The two instances have different hashes, so they coexist in the store. But at a call site that requires `Ord(MyType)`, which one do we use?

### 8.2 Recommended Approach: Instance Sets

An **instance set** is a named, coherent collection of instance bindings. Every project has a default instance set, and instances are resolved from the instance set in scope.

```json
{
  "instance-set": {
    "Ord\0MyType": "#instance-hash-1";
    "Eq\0MyType":  "#instance-hash-2"
  }
}
```

When your project compiles, the instance set determines which instance is used for each `CLMIAP` dispatch. If two imported libraries each bring their own `Ord(MyType)` instance, the compiler emits an error:

```
Error: Conflicting instances for Ord(MyType):
  #abc123 (from tulam-collections)
  #def456 (from tulam-sorting)
Resolution: add an explicit instance binding to your project's instance-set.
```

This makes instance coherence **explicit** and **local** to each project.

### 8.3 Instance Scoping Rules

1. **Defining module wins.** If `MyType` is defined in module M and M also defines `instance Ord(MyType)`, that instance is automatically in scope wherever `MyType` is.
2. **Explicit import wins.** If you explicitly import an instance, it takes precedence over any other.
3. **Conflict is an error.** If two instances for the same key are in scope and neither is from the defining module, the compiler requires you to choose.

This is stricter than Haskell (which has global coherence but allows orphans with warnings) and more flexible than Rust (which forbids orphans entirely).

### 8.4 Interaction with `intrinsic` Instances

Intrinsic instances (like `instance Ord(Int) = intrinsic`) are implemented by the compiler itself (in `Intrinsics.hs`). Their hash is computed from the algebra definition + type argument + the sentinel `intrinsic`. Two compilers that implement the same intrinsic produce the same hash. If a compiler version changes an intrinsic implementation, the hash changes, and all downstream hashes change too.

---

## 9. Interaction with Target Backends

### 9.1 The Challenge

tulam targets JavaScript, .NET, and native. Content hashes are computed from the Surface AST, which is backend-independent. But `target` blocks contain backend-specific code, `extern` declarations reference platform-specific symbols, and intrinsic implementations differ per backend.

### 9.2 Approach: Backend-Neutral Hashing with Backend Annotations

The content hash is computed from the **backend-neutral** portion of the definition. Backend-specific code is stored as annotations on the hash:

```
store/objects/ab/cd1234... = {
  "hash": "#abcd1234...";
  "ast": <normalized surface AST>;
  "backends": {
    "dotnet": <.NET-specific CLM or codegen output>;
    "js":     <JS-specific codegen output>;
    "native": <native-specific codegen output>
  }
}
```

### 9.3 Target-Specific Imports

`import System.Windows.Forms target dotnet` brings in extern types. These are hashed differently:

```
hash(extern type) = H("extern" || target || fully_qualified_name || type_signature)
```

Two projects importing the same .NET type get the same hash, ensuring interoperability.

### 9.4 TargetSwitch

`target | dotnet -> expr1 | js -> expr2` is hashed as the full switch, including all branches. A definition with a target switch has a single hash covering all backends. Backend-specific compilation selects the relevant branch.

---

## 10. Comparison with Existing Systems

### 10.1 Unison

**What we take:** Content addressing of definitions, names as metadata, append-only codebase.

**Where we differ:**
- tulam uses standard file-based tooling; Unison requires its custom codebase manager (UCM)
- tulam must handle .NET, JS, and native targets; Unison has no multi-backend story
- tulam uses algebra constraints as capabilities; Unison has first-class "abilities" (effect types)
- tulam allows distributed content stores (Git-like); Unison's codebase is a single shared database

### 10.2 Nix

**What we take:** Content addressing for reproducibility, hash-based identity, no mutable global state.

**Where we differ:**
- Nix operates at the package/derivation level; tulam at the definition level (much more granular)
- Nix hashes include the entire build environment; tulam hashes only semantic content
- Nix has no type checking for compatibility; tulam's type checker provides automatic verification

### 10.3 Go Modules

**What we take:** Stable identifiers beyond version numbers (Go uses module path + version in go.sum as hashes).

**Where we differ:**
- Go still uses semantic versioning as primary identity; tulam uses content hashes
- Go's "minimum version selection" still requires version numbers; tulam has none
- Go has no typeclass/algebra system for structural compatibility checking

### 10.4 Rust Crates (Cargo)

**What we take:** The lock file concept, reproducible builds.

**Where we differ:**
- Cargo uses semver with a SAT solver; tulam uses content hashes with no solver
- Cargo does not support multiple versions of the same crate in one binary (by default); tulam does naturally
- Rust enforces strict orphan rules; tulam uses instance sets for more flexibility with explicit conflict resolution

---

## 11. Trade-offs and Risks

### 11.1 Content Store Infrastructure

**Cost:** Every tulam installation needs a local content store. Shared content stores need hosting. The store grows monotonically.

**Mitigation:** The store is just a directory of files (like `.git/objects`). Garbage collection prunes unreachable hashes. Shared stores use CDN-style distribution (hash-based URLs are perfectly cacheable).

### 11.2 Developer Experience

**Risk:** Debugging by hash is painful. "Your function `#ab12cd34` failed" is not helpful.

**Mitigation:** Hashes are never shown to the user unless they ask. The tooling always resolves hashes back to names. Error messages say "your function `sort` (at Collections.SortedSet, line 42) failed." The hash is available via `--verbose` or `:inspect` in the REPL.

**Risk:** Renaming a function does not change its hash.

**Mitigation:** This is a feature. Renaming is a metadata operation, not a semantic change.

### 11.3 Instance Coherence Complexity

**Risk:** The instance set mechanism adds complexity.

**Mitigation:** The default behavior (defining module's instance wins, conflicts are errors) handles 95% of cases with zero configuration.

### 11.4 Compile-Time Cost

**Risk:** Computing hashes for every definition adds overhead.

**Mitigation:** Hash computation is fast (BLAKE3 >1 GB/s). The content store lookup is an in-memory HashMap during compilation. Cost is dominated by parsing and type checking, not hashing.

### 11.5 Interaction with `intrinsic` and Platform-Specific Code

**Risk:** Two different compiler versions may implement the same intrinsic differently, causing hash divergence.

**Mitigation:** Intrinsic hashes include a compiler-version component. The system warns when intrinsic hashes differ between compiler versions.

---

## 12. Open Questions

These are decisions to make during implementation, not upfront:

1. **Hash algorithm.** SHA-256 (universal, well-understood) vs BLAKE3 (faster, shorter). Recommendation: start with SHA-256, switch to BLAKE3 if needed.

2. **Store format.** Flat files (like Git objects) vs SQLite vs custom binary. Recommendation: start with flat files for simplicity.

3. **Remote store protocol.** HTTP (like Unison Share) vs Git-based vs IPFS. Recommendation: start with HTTP.

4. **Instance set granularity.** Per-project (recommended) vs per-module vs per-expression.

5. **Hashing granularity for types.** Should a sum type and its constructors share one hash? Recommendation: yes, constructors are not independently meaningful.

6. **Law equivalence.** Should two algebras with same functions but different laws have the same structural compatibility hash? Recommendation: yes, because laws are specifications, not implementations.

7. **REPL integration.** Should REPL definitions be hashed? Recommendation: no, REPL definitions are ephemeral.

8. **Backward compatibility.** File-based loading should coexist with content addressing during transition. Recommendation: compute hashes lazily on load.

---

## 13. Concrete Syntax Summary

### For Library Authors

```tulam
module Collections.SortedSet;
import Algebra.Ord;

type SortedSet(a:Type) = Leaf | Node(left:SortedSet(a), value:a, right:SortedSet(a));

function insert [a:Type] (x:a, s:SortedSet(a)) : SortedSet(a) = match
    | x, Leaf -> Node(Leaf, x, Leaf)
    | x, Node(l, v, r) -> match compare(x, v)
        | LessThan    -> Node(insert(x, l), v, r)
        | Equal       -> Node(l, v, r)
        | GreaterThan -> Node(l, v, insert(x, r));

function member [a:Type] (x:a, s:SortedSet(a)) : Bool = match
    | x, Leaf -> False
    | x, Node(l, v, r) -> match compare(x, v)
        | LessThan    -> member(x, l)
        | Equal       -> True
        | GreaterThan -> member(x, r);
```

No changes to the syntax. The tooling handles hashing transparently.

### For Library Consumers

```tulam
module MyApp;
import Collections.SortedSet;
import Algebra.Ord;
import Numeric.Int;

function buildSet() : SortedSet(Int) =
    insert(5, insert(3, insert(7, insert(1, Leaf))));

function hasThree() : Bool = member(3, buildSet());
```

No changes to the syntax. The dependency system is invisible in normal code.

### Project Manifest

```json
{
  "name": "my-app";
  "deps": {
    "tulam-collections": "#manifest:e4a7b2c1d3f5..."
  };
  "instance-set": {}
}
```

### Lock File

```json
{
  "resolved": {
    "Collections.SortedSet.SortedSet": "#def:a1b2c3d4...";
    "Collections.SortedSet.insert":    "#def:e5f6a7b8...";
    "Collections.SortedSet.member":    "#def:c9d0e1f2...";
    "Algebra.Ord.Ord":                 "#def:11223344...";
    "Algebra.Ord.compare":             "#def:55667788...";
    "Algebra.Eq.Eq":                   "#def:99aabbcc..."
  }
}
```

### CLI Commands

```bash
tulam build                     # build, computing and caching hashes
tulam lock                      # generate lock file from current deps
tulam upgrade tulam-collections # check for new manifest
tulam upgrade --check           # dry-run upgrade
tulam inspect #hash             # show definition at hash
tulam refs #hash                # show what depends on this hash
tulam diff #hash1 #hash2        # structural diff between definitions
tulam store gc                  # garbage-collect unreachable hashes
tulam publish                   # upload hashes + manifest to shared store
```

---

## 14. Implementation Sketch

### 14.1 Hash Computation Module (`src/ContentHash.hs`)

- Alpha-normalization: replace bound variable names with de Bruijn indices
- AST serialization: deterministic binary encoding of normalized `Expr`
- Dependency resolution: replace `Id name` references with `Id "#hash"` before hashing
- Hash function: SHA-256 or BLAKE3
- Exports: `computeHash :: Expr -> Environment -> Hash`, `normalizeExpr :: Expr -> Expr`

### 14.2 Content Store Module (`src/ContentStore.hs`)

- On-disk storage: read/write normalized ASTs keyed by hash
- In-memory cache: HashMap for current compilation session
- Name mapping: read/write JSON manifests
- Garbage collection: mark-and-sweep from project roots
- Exports: `storeDefinition`, `lookupDefinition`, `loadManifest`, `saveManifest`

### 14.3 Module System Extension (`src/ModuleSystem.hs`)

- Extend `ModuleInfo` with hash-to-name bindings
- Extend `resolveModulePath` to resolve through content store
- Add instance set resolution
- Extend `extractDependencies` to work with hashes

### 14.4 Environment Extension (`src/State.hs`)

- Add `HashStore` to `InterpreterState` (in-memory hash cache)
- Add instance set to `InterpreterState`
- Extend `Environment` maps to optionally store hashes alongside definitions

### 14.5 Pipeline Integration

- After Pass 0 (desugar): compute hashes for all top-level definitions
- Pass 1 (env build): populate environment using hash-backed definitions
- Pass 3 (type check): use structural compatibility hashes for constraint resolution
- Pass 4 (CLM convert): hash-based instance lookup in `CLMIAP` dispatch
