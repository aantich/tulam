# Algebra-Based Collection System Design

## 1. Design Philosophy

tulam's collection system is built on a single principle: **collections are defined by their capabilities, not their implementation**.

A `List` is not special. An `Array` is not special. They are containers that happen to satisfy different sets of algebraic contracts. Code that folds should work on anything foldable. Code that indexes should only compile against containers that support efficient indexing. Code that builds a new container should be explicit about what it is building.

Four rules follow from this:

1. **List gets zero special treatment.** It is one container among many — a singly-linked recursive structure with O(1) cons and O(n) everything else. It should not be the universal interchange format.

2. **The fold is the universal interface.** Any container that can be consumed element-by-element implements `Foldable`. No `toList` escape hatch is needed because `fromFoldable` converts between any pair of containers directly.

3. **Separate orthogonal capabilities into separate algebras.** "Can I fold it?", "Does it have a size?", "Can I index into it?", "Can I build one?" — these are independent questions with independent answers. A single monolithic typeclass that bundles them all forces containers to either implement operations that make no sense for them or be excluded entirely.

4. **Algebraic laws license optimizations.** When `Bulk` promises index-map and generate-size laws, the compiler can lower operations to SIMD or GPU intrinsics without the programmer changing a line of code.


## 2. Critique of Haskell's Approach

Haskell's `Foldable`/`Traversable` hierarchy was a step forward from standalone functions, but it has ten structural problems that tulam's design avoids.

### 2.1. Foldable is too weak

`Foldable` provides `toList :: Foldable t => t a -> [a]`, and in practice most "generic" code immediately calls it. The abstraction leaks: you write `Foldable t =>` in the signature but `toList` in the body, which means you are actually operating on `[a]`. The container polymorphism is an illusion — the real work happens on lists.

### 2.2. Traversable requires Applicative

`traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)` bakes the `Applicative` constraint into the class definition itself. If you have a plain function `a -> Maybe b` and want to map it over a container, collecting failures, you must know that `Maybe` is `Applicative`. The constraint belongs on the *call site*, not on the algebra.

### 2.3. No indexed access

`Foldable` and `Traversable` assume sequential left-to-right or right-to-left access. There is no standard way to express "this container supports O(1) random access by position." `Vector` provides `(!)` but it is not part of any typeclass — you must import `Data.Vector` specifically and abandon polymorphism.

### 2.4. No size information

`Foldable` does not know the size of a container. `length` is defined as `foldl' (\c _ -> c + 1) 0`, which is always O(n). An `Array` that stores its length in a machine word still pays O(n) to answer "how big are you?" through `Foldable`.

### 2.5. No construction

`Foldable` and `Traversable` are consume-only. There is no standard algebra for "build an empty container and add elements." The result is `fromList` scattered everywhere — and `fromList` once again privileges `List` as the interchange format.

### 2.6. filter is orphaned

`filter` is not in `Foldable`, not in `Traversable`, not in any standard typeclass. It is a standalone function on lists, a different standalone function on vectors, a different standalone function on maps. Writing container-polymorphic filtering code is impossible with Haskell's standard library.

### 2.7. sort, zip, take/drop are orphaned

The same problem. `sort` lives in `Data.List`, `zip` lives in `Prelude` (for lists only), `take`/`drop` are list-specific. None of these fundamental operations have algebraic homes.

### 2.8. MonoFoldable hack

The `mono-traversable` package was invented because `ByteString` and `Text` are not `Foldable` (they are monomorphic containers — `ByteString` contains only `Word8`, not `Word8` parameterized by a type variable). The fix is a parallel hierarchy (`MonoFoldable`, `MonoTraversable`) that duplicates every function. This should not be necessary. In tulam, `Array(Byte)` is just `Array` applied to `Byte` — it implements all the same algebras as `Array(Int)` with no special treatment.

### 2.9. List as privileged interchange

`toList`/`fromList` make `List` the universal interchange format. But `List` is the worst container to privilege: O(n) random access, O(n) length, high per-element heap overhead (each cons cell is a separate allocation). Privileging it means that "generic" code silently pays list tax on every conversion.

### 2.10. No GPU/SIMD path

Haskell's `Foldable`/`Traversable` abstractions carry no information about memory layout or access patterns. There is no way to express "this fold can be parallelized" or "this map can be vectorized" within the typeclass system. Libraries like `accelerate` and `massiv` build entirely separate ecosystems.


## 3. tulam's Solution: Orthogonal Capability Algebras

tulam replaces the monolithic `Foldable`/`Traversable` pair with a 4-tier hierarchy of 10 focused algebras. Each algebra captures exactly one capability. Containers implement only the algebras that make semantic and performance sense for them.

### Tier 1 — Consumption (read-only)

Tier 1 algebras consume containers without producing new ones.

#### Foldable

The universal eliminator. Any container that can yield its elements one at a time is `Foldable`. There is no `toList` — that is `fromFoldable`'s job (see Section 4).

```
algebra Foldable(c:Type1) = {
    function foldr(f: a -> b -> b, z:b, xs:c(a)) : b;
    function foldl(f: b -> a -> b, z:b, xs:c(a)) : b;

    /// Fold using a Monoid's combine/empty.
    function fold(xs:c(a)) : a
        requires Monoid(a) =
        foldl(combine, empty, xs);

    /// Map each element to a monoid value and fold.
    function foldMap(f: a -> m, xs:c(a)) : m
        requires Monoid(m) =
        foldl(\acc, x -> combine(acc, f(x)), empty, xs);

    law foldr_foldl(f: a -> a -> a, z:a, xs:c(a)) =
        foldr(f, z, xs) === foldl(\b, a -> f(a, b), z, xs)
};
```

#### Sized

Separate from `Foldable` because infinite streams are foldable but have no finite size.

```
algebra Sized(c:Type1) extends Foldable(c) = {
    function size(xs:c(a)) : Int;
    function isEmpty(xs:c(a)) : Bool = size(xs) == 0;

    law size_nonneg(xs:c(a)) = (size(xs) >= 0) === True;
    law isEmpty_size(xs:c(a)) = isEmpty(xs) === (size(xs) == 0)
};
```

#### Searchable

Separate from `Foldable` because search semantics differ across containers. A sorted array uses binary search. A hash set checks membership in O(1). A list scans linearly. The algebra is the same; the instance decides the strategy.

```
algebra Searchable(c:Type1) extends Foldable(c) = {
    function find(pred: a -> Bool, xs:c(a)) : Maybe(a);
    function any(pred: a -> Bool, xs:c(a)) : Bool;
    function all(pred: a -> Bool, xs:c(a)) : Bool;

    function elem(x:a, xs:c(a)) : Bool
        requires Eq(a) =
        any(\y -> y == x, xs);

    function count(pred: a -> Bool, xs:c(a)) : Int =
        foldl(\n, x -> if pred(x) then n + 1 else n, 0, xs);

    law find_any(pred: a -> Bool, xs:c(a)) =
        any(pred, xs) === match find(pred, xs)
            | Just(_) -> True
            | Nothing -> False
};
```

#### Filterable

Separate from `Foldable` because not all foldables support rebuilding a container of the same shape. A `Tree` can be folded, but filtering its leaves requires rebuilding the tree — which is a `Buildable` concern, not a `Foldable` one. `Filterable` is for containers where removing elements produces a valid container of the same type.

```
algebra Filterable(c:Type1) extends Foldable(c) = {
    function filter(pred: a -> Bool, xs:c(a)) : c(a);
    function partition(pred: a -> Bool, xs:c(a)) : Pair(c(a), c(a)) =
        Pair(filter(pred, xs), filter(\x -> not(pred(x)), xs));
    function filterMap(f: a -> Maybe(b), xs:c(a)) : c(b);

    law filter_id(xs:c(a)) = filter(\_ -> True, xs) === xs;
    law filter_empty(xs:c(a)) =
        isEmpty(filter(\_ -> False, xs)) === True
        requires Sized(c)
};
```

### Tier 2 — Construction

Tier 2 algebras build and modify containers.

#### Buildable

The dual of `Foldable`. Combined with `Foldable`, it enables the universal `fromFoldable` converter (Section 4).

```
algebra Buildable(c:Type1) extends Sized(c) = {
    /// The empty container.
    function bempty() : c(a);

    /// Add an element to the front.
    function cons(x:a, xs:c(a)) : c(a);

    /// Add an element to the back.
    function snoc(xs:c(a), x:a) : c(a);

    /// A container with exactly one element.
    function singleton(x:a) : c(a) = cons(x, bempty());

    /// A container of n copies of the same element.
    function replicate(n:Int, x:a) : c(a) =
        foldl(\acc, _ -> snoc(acc, x), bempty(), range(0, n));

    law empty_size() = size(bempty() : c(a)) === 0
        requires Eq(a);
    law singleton_size(x:a) = size(singleton(x) : c(a)) === 1;
    law cons_size(x:a, xs:c(a)) = size(cons(x, xs)) === size(xs) + 1;
    law snoc_size(xs:c(a), x:a) = size(snoc(xs, x)) === size(xs) + 1
};
```

#### Indexable

Separate from `Sized` because not all sized containers have efficient random access. `List` is `Sized` but not `Indexable` — O(n) indexing should not masquerade as O(1) random access.

```
algebra Indexable(c:Type1) extends Sized(c) = {
    /// Access element at position i. Partial — may fail on out-of-bounds.
    function index(xs:c(a), i:Int) : a;

    /// Safe access — returns Nothing on out-of-bounds.
    function indexMaybe(xs:c(a), i:Int) : Maybe(a) =
        if i >= 0 then
            if i < size(xs) then Just(index(xs, i))
            else Nothing
        else Nothing;

    /// Extract a sub-container [start, end).
    function slice(xs:c(a), start:Int, end:Int) : c(a);

    /// Return a new container with element at position i replaced.
    function update(xs:c(a), i:Int, val:a) : c(a);

    law index_bounds(xs:c(a), i:Int) =
        (i >= 0) === True ==> (i < size(xs)) === True ==>
        indexMaybe(xs, i) === Just(index(xs, i));
    law update_index(xs:c(a), i:Int, v:a) =
        (i >= 0) === True ==> (i < size(xs)) === True ==>
        index(update(xs, i, v), i) === v
};
```

#### Bulk

The gateway to GPU/SIMD acceleration. Any `Bulk` container can participate in parallel computation because its laws guarantee that element access and generation are well-defined. The `Math/LinAlg.tl` and `Math/Stats.tl` morphisms require `Bulk` for vectorized operations.

```
algebra Bulk(c:Type1) extends Indexable(c), Functor(c) = {
    /// Build a container of n elements from an index function.
    function generate(n:Int, f: Int -> a) : c(a);

    /// Convert to a flat Array (for FFI / SIMD boundary).
    function toArray(xs:c(a)) : Array(a);

    /// Build from a flat Array.
    function fromArray(arr:Array(a)) : c(a);

    law indexMap(xs:c(a), f: a -> b, i:Int) =
        (i >= 0) === True ==> (i < size(xs)) === True ==>
        index(fmap(f, xs), i) === f(index(xs, i));
    law generateSize(n:Int, f: Int -> a) =
        (n >= 0) === True ==>
        size(generate(n, f)) === n;
    law roundtrip(xs:c(a)) =
        toArray(fromArray(toArray(xs))) === toArray(xs)
};
```

### Tier 3 — Traversal

#### Traversable

The key difference from Haskell: the constraint on the effect context (`Applicative`) is on the *call*, not on the algebra definition. `Traversable` requires only `Functor` and `Foldable` from the container. The caller supplies the `Applicative` (or `Monad`) when invoking `traverse`.

```
algebra Traversable(c:Type1) extends Functor(c), Foldable(c) = {
    /// Apply an effectful function to each element, collecting results.
    function traverse(f: a -> g(b), xs:c(a)) : g(c(b))
        requires Applicative(g:Type1);

    /// Sequence a container of effects into an effect of a container.
    function sequence(xs:c(g(a))) : g(c(a))
        requires Applicative(g:Type1) =
        traverse(id, xs);

    law identity(xs:c(a)) =
        traverse(Just, xs) === Just(xs);
    law naturality(t: g(a) -> h(a), f: a -> g(b), xs:c(a)) =
        t(traverse(f, xs)) === traverse(\x -> t(f(x)), xs)
        requires Applicative(g:Type1), Applicative(h:Type1)
};
```

### Tier 4 — Structural operations

Tier 4 algebras express operations that manipulate the shape of a container.

#### Sortable

```
algebra Sortable(c:Type1) extends Buildable(c) = {
    function sort(xs:c(a)) : c(a)
        requires Ord(a);
    function sortBy(cmp: a -> a -> Ordering, xs:c(a)) : c(a);
    function sortOn(f: a -> k, xs:c(a)) : c(a)
        requires Ord(k) =
        sortBy(\x, y -> compare(f(x), f(y)), xs);

    law sort_idempotent(xs:c(a)) =
        sort(sort(xs)) === sort(xs)
        requires Ord(a), Eq(a)
};
```

#### Zippable

```
algebra Zippable(c:Type1) extends Sized(c) = {
    function zipWith(f: a -> b -> d, xs:c(a), ys:c(b)) : c(d);
    function zip(xs:c(a), ys:c(b)) : c(Pair(a, b)) =
        zipWith(Pair, xs, ys);
    function unzip(xs:c(Pair(a, b))) : Pair(c(a), c(b));

    law zip_unzip(xs:c(a), ys:c(b)) =
        unzip(zip(xs, ys)) === Pair(xs, ys)
};
```

#### Sliceable

```
algebra Sliceable(c:Type1) extends Sized(c) = {
    function take(n:Int, xs:c(a)) : c(a);
    function drop(n:Int, xs:c(a)) : c(a);
    function splitAt(n:Int, xs:c(a)) : Pair(c(a), c(a)) =
        Pair(take(n, xs), drop(n, xs));
    function takeWhile(pred: a -> Bool, xs:c(a)) : c(a);
    function dropWhile(pred: a -> Bool, xs:c(a)) : c(a);
    function scanl(f: b -> a -> b, z:b, xs:c(a)) : c(b);

    law splitAt_take_drop(n:Int, xs:c(a)) =
        splitAt(n, xs) === Pair(take(n, xs), drop(n, xs));
    law take_drop_identity(n:Int, xs:c(a)) =
        size(take(n, xs)) + size(drop(n, xs)) === size(xs)
};
```


### Standalone generic function: fromFoldable

```
function fromFoldable(xs:c1(a)) : c2(a)
    requires Foldable(c1:Type1), Buildable(c2:Type1) =
    foldl(\acc, x -> snoc(acc, x), bempty(), xs);
```

This is not in any algebra. It is a free function that exists at the intersection of two algebras. Any `Foldable` can convert to any `Buildable`. No privileged container needed.


## 4. The Key Innovation: fromFoldable

In Haskell, converting between containers goes through `List`:

```haskell
-- Haskell: Vector -> Set requires two steps through List
Set.fromList (Vector.toList myVector)
```

In tulam, `fromFoldable` converts directly between any pair:

```
// List -> Array
let arr : Array(Int) = fromFoldable(myList);

// Array -> List
let lst : List(Int) = fromFoldable(myArray);

// Tree -> Array
let flat : Array(Int) = fromFoldable(myTree);

// Array -> Set (future)
let s : Set(Int) = fromFoldable(myArray);

// Any Foldable -> Any Buildable
let result : c2(a) = fromFoldable(input);
```

The return type annotation drives `Buildable` instance selection. The argument type drives `Foldable` instance selection. No intermediate container is allocated.

**Compiler optimization opportunities:**

- `fromFoldable` where both source and target are `Array` compiles to identity (or `memcpy`).
- `fromFoldable` from `List` to `Array` compiles to a single-pass allocation: fold to compute length, allocate, fold to fill.
- `fromFoldable` from any `Sized` source to `Array` can pre-allocate: `size(xs)` gives the exact length upfront.

The compiler can recognize specific `(Foldable, Buildable)` pairs and emit specialized code via intrinsics, just as it does for arithmetic today.


## 5. Instance Matrix

| Type | Foldable | Sized | Searchable | Filterable | Buildable | Indexable | Bulk | Traversable | Sortable | Zippable | Sliceable |
|------|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| **Array** | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **List** | ✓ | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ | ✓ | ✓ | ✓ | ✓ |
| **NonEmpty** | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ | ✗ | ✓ | ✓ | ✓ | ✗ |
| **Maybe** | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ |
| **Tree** | ✓ | ✓ | ✓ | ✗ | ✗ | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ |
| **Set** (future) | ✓ | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ |
| **Map** (future) | ✓ | ✓ | ✓ | ✓ | ✓ | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ |
| **Stream** (future) | ✓ | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ | ✗ | ✗ | ✓ | ✓ |
| **DenseVector** (future) | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| **Matrix** (future) | ✓ | ✓ | ✓ | ✗ | ✗ | ✓ | ✓ | ✗ | ✗ | ✗ | ✗ |

### Why specific cells are ✗

**List is NOT Indexable.** `List` has O(n) access by position. Providing an `index` method would compile and run, but it would mislead programmers into writing O(n^2) algorithms thinking they are O(n). If you need indexed access, convert to `Array` explicitly: `let arr = fromFoldable(myList) : Array(Int)`. The conversion is O(n) once; every subsequent access is O(1).

**NonEmpty is NOT Buildable.** `Buildable` requires `bempty()`, which returns an empty container. A `NonEmpty` container with zero elements is a contradiction — it violates the type's fundamental invariant. `NonEmpty` can be constructed from a `Buildable` result via a smart constructor, but it cannot *be* a `Buildable`.

**NonEmpty is NOT Sliceable.** `take(0, xs)` and `drop(size(xs), xs)` would produce empty results, violating the non-empty invariant. `Sliceable` requires that `take` and `drop` produce valid containers of the same type.

**Tree is NOT Filterable.** Filtering leaves from a binary tree requires deciding what to do with branches that lose both children, or branches that lose one child. This is a policy decision (collapse? replace with a default?) that the `Filterable` algebra cannot express generically. A `Tree`-specific `prune` function is the right abstraction.

**Stream is NOT Sized.** An infinite stream has no finite size. `size` would diverge. `Sized` is kept separate from `Foldable` precisely for this case.

**Stream is NOT Searchable.** `find` on an infinite stream that does not contain the target diverges. `any` and `all` have the same problem. Streams support `takeWhile`/`dropWhile` (via `Sliceable`) as the safe alternative to unbounded search.

**Set is NOT Traversable.** `traverse` requires preserving structure: applying `f` to each element and rebuilding the container with the results. But if `f` maps two distinct elements to the same value, the rebuilt `Set` would have fewer elements than the original. The structure is not preserved. Use `fmap` on a set (via `Functor`, which sets do implement) when you want to map, accepting that the result may shrink.

**Matrix is NOT Filterable.** Removing individual elements from a matrix does not produce a valid matrix. An m-by-n matrix with some cells removed is a sparse structure, not a matrix. Matrix filtering is a domain-specific operation (e.g., zeroing out elements below a threshold) that belongs in `Math/LinAlg.tl`, not in a generic collection algebra.

**Matrix is NOT Traversable.** Traversing a matrix with an effectful function could produce elements that violate dimensional constraints. Matrix operations are inherently structured — `Bulk` provides the right level of abstraction (index-based access and generation).


## 6. Comparison with Haskell

| Problem in Haskell | tulam Solution |
|---|---|
| `Foldable.length` is always O(n) | `Sized` is separate — `Array` O(1), `List` O(n), each with the right implementation |
| `filter` not in any typeclass | `Filterable` algebra |
| `Traversable` rigidly requires `Applicative` | Constraint is on the call (`requires Applicative(g:Type1)`), not the algebra |
| No construction typeclass | `Buildable` — `bempty`, `cons`, `snoc`, `singleton`, `replicate` |
| No indexed access typeclass | `Indexable` — and `List` does not implement it |
| `sort` is standalone | `Sortable` algebra |
| `zip` is standalone | `Zippable` algebra |
| `take`/`drop` are standalone | `Sliceable` algebra |
| `MonoFoldable` hack for ByteString | Not needed — `Array(Byte)` implements all algebras naturally |
| `toList` is universal escape hatch | `fromFoldable` works between ANY container pair |
| No GPU/SIMD integration | `Bulk` extends into the Math library's acceleration path |


## 7. Integration with Math Library

The `Bulk` algebra is the bridge between the collection system and tulam's numerical computing libraries.

### Math morphisms accept any Bulk container

```
/// Descriptive statistics — works on any Bulk container.
morphism Descriptive(c:Type1, s:Type) requires Bulk(c), Field(s) = {
    function mean(xs:c(s)) : s;
    function variance(xs:c(s)) : s;
    function stddev(xs:c(s)) : s = sqrt(variance(xs));
};
```

This means `mean(myArray)`, `mean(myDenseVector)`, and `mean(fromFoldable(myList) : Array(Float64))` all work. The container does not matter — only the `Bulk` contract matters.

### GPU acceleration through Bulk.toArray/fromArray

The LLVM backend (see `doc/backends/LLVMBackendDesign.md`) uses `Bulk` laws to lower operations to vectorized code:

1. The `indexMap` law guarantees that `fmap(f, xs)` can be computed element-wise in any order — licensing SIMD and GPU parallelism.
2. The `generateSize` law guarantees that `generate(n, f)` produces exactly `n` elements — licensing pre-allocation.
3. The `roundtrip` law guarantees that `toArray`/`fromArray` is faithful — licensing flat-memory interchange with C/LLVM intrinsics.

When targeting LLVM, a chain like `fmap(f, fmap(g, xs))` on a `Bulk` container fuses into `generate(size(xs), \i -> f(g(index(xs, i))))` — a single pass with no intermediate allocation.

### Target-qualified intrinsic instances

```
// GPU-accelerated Bulk for DenseVector on CUDA targets
instance Bulk(DenseVector)
    target "cuda" = intrinsic;

// SIMD-accelerated Bulk for Array on x86
instance Bulk(Array)
    target "x86" = intrinsic;
```

The `Bulk` algebra's `toArray`/`fromArray` methods serve as the FFI boundary. Data crosses into the accelerated domain as a flat array, computation happens in parallel, and results come back as a flat array.

### Concrete example: linear algebra pipeline

```
// Works on ANY Bulk container
function normalize(xs:c(Float64)) : c(Float64)
    requires Bulk(c:Type1) =
    let m = mean(xs);
    let s = stddev(xs);
    fmap(\x -> (x - m) / s, xs);

// Usage:
let data : Array(Float64) = fromFoldable(rawList);
let normed = normalize(data);         // Array path — may use SIMD
let vec : DenseVector(Float64) = fromArray(data);
let normed2 = normalize(vec);         // DenseVector path — may use GPU
```


## 8. Integration with Effect System

All collection algebras are **pure**. They take containers in and return containers out, with no side effects. This is a deliberate design decision: purity enables equational reasoning, fusion, and parallelization.

Effectful collection operations go through `Traversable.traverse`:

```
// Read lines from files listed in an array
function readAll(paths:Array(String)) : IO(Array(String)) =
    traverse(readFile, paths);

// Parse each element, collecting failures
function parseAll(xs:Array(String)) : Either(String, Array(Int)) =
    traverse(parseInt, xs);
```

The `traverse` function takes an effectful function `a -> g(b)` and threads the effect context `g` through the container. The container algebra itself remains pure; the effect comes from the function argument and the `Applicative`/`Monad` instance of `g`.

### Interaction with the Random effect

The `Random` effect layer (future `Math/Random.tl`) uses pure collection algebras internally for bulk sampling:

```
// Generate n random samples — effect is in the outer layer
function sample(n:Int) : Random(Array(Float64)) =
    traverse(\_ -> randomFloat(), range(0, n));

// Shuffle uses Buildable + Indexable internally, effect externally
function shuffle(xs:Array(a)) : Random(Array(a)) =
    // Fisher-Yates via fold with Random effect threading
    foldl(\acc, i -> bind(randomInt(0, i), \j -> pure(swap(acc, i, j))),
          pure(xs),
          range(0, size(xs)));
```

The principle: collection algebras describe *what* operations are available on containers. The effect system describes *how* those operations interact with the outside world. The two compose orthogonally.


## 9. File Layout

```
lib/
  Categorical.tl    — Category, Arrow, Functor, Applicative, Monad, Kleisli
  Collection.tl     — Foldable, Sized, Searchable, Filterable, Buildable,
                       Indexable, Bulk, Traversable, Sortable, Zippable,
                       Sliceable, fromFoldable
  Instances/
    Array.tl        — Array instances for all 11 collection algebras
    List.tl         — List instances (Foldable, Sized, Searchable, Filterable,
                       Buildable, Traversable, Sortable, Zippable, Sliceable)
    Maybe.tl        — Maybe instances (Foldable, Sized, Searchable, Filterable,
                       Traversable)
    NonEmpty.tl     — NonEmpty instances (Foldable, Sized, Searchable, Filterable,
                       Traversable, Sortable, Zippable)
    Tree.tl         — Tree instances (Foldable, Sized, Searchable, Traversable)
    Numeric.tl      — Numeric type instances (unchanged)
    ...
```

The `Collection.tl` file contains only algebra *definitions* and the `fromFoldable` function. All *instances* live in the `Instances/` folder, one file per container type. This keeps the algebra definitions clean and allows adding new container types (Set, Map, Stream, DenseVector, Matrix) by adding new instance files without modifying any existing code.

`Categorical.tl` takes over `Functor`, `Applicative`, and `Monad` from the current `HKT.tl`, since these are categorical structures rather than collection-specific ones. `Collection.tl` imports `Categorical.tl` for the `Functor` dependency in `Bulk` and `Traversable`.
