/// Collection algebras: capability-based container abstractions.
///
/// Unlike Haskell's approach where List is the privileged interchange format,
/// tulam defines collections purely by their capabilities as orthogonal algebras.
/// List is just one container among many — no special treatment.
///
/// Tier 1 — Consumption:
///   Foldable(c: Type -> Type)     — universal eliminator (foldr, foldl, fold, foldMap)
///   Sized(c: Type -> Type)        — finite containers with known size
///   Searchable(c: Type -> Type)   — find, any, all, elem, count
///   Filterable(c: Type -> Type)   — filter, partition, filterMap
///
/// Tier 2 — Construction:
///   Buildable(c: Type -> Type)    — bempty, cons, snoc, singleton, replicate
///   Indexable(c: Type -> Type)    — random access: index, slice, update
///   Bulk(c: Type -> Type)         — GPU/SIMD gateway: generate, toArray, fromArray
///
/// Tier 3 — Traversal:
///   Traversable(c: Type -> Type)  — structure-preserving effectful map
///
/// Tier 4 — Structural:
///   Sortable(c: Type -> Type)     — sort, sortBy, sortOn
///   Zippable(c: Type -> Type)     — zipWith, zip, unzip
///   Sliceable(c: Type -> Type)    — take, drop, splitAt, takeWhile, dropWhile, scanl
///
/// Standalone:
///   fromFoldable                  — convert ANY Foldable to ANY Buildable
///
/// See doc/CollectionDesign.md for the full design rationale and Haskell comparison.
module Collection;
import Categorical;
import Algebra;

// ============================================================================
// Tier 1: Consumption
// ============================================================================

// ----------------------------------------------------------------------------
// Foldable
// ----------------------------------------------------------------------------

/// The fundamental consumption algebra. Every container can be folded.
/// This is THE primitive — all other consumption algebras build on it.
///
/// Unlike Haskell's Foldable, there is no `toList`. The fold itself IS
/// the universal interface. To materialize into a specific container,
/// use `fromFoldable` with the target container's Buildable instance.
///
/// @param c A container type constructor (Type -> Type)
algebra Foldable(c: Type -> Type) = {
    /// Right fold: the universal eliminator for containers.
    /// Replaces each element with `f(element, accumulator)` from right to left.
    function foldr(f: a -> b -> b, acc:b, xs:c(a)) : b;

    /// Left fold: strict accumulation from left to right.
    /// More efficient than foldr for most operations (no stack buildup).
    function foldl(f: b -> a -> b, acc:b, xs:c(a)) : b;

    /// Monoidal fold: combine all elements using their Monoid instance.
    function fold(xs:c(a)) : a requires Monoid(a);

    /// Map each element to a monoid value, then combine.
    function foldMap(f: a -> b, xs:c(a)) : b requires Monoid(b);

    // Defaults
    function fold(xs:c(a)) : a = foldl(combine, empty, xs);
    function foldMap(f: a -> b, xs:c(a)) : b =
        foldl(fn(acc, x) = combine(acc, f(x)), empty, xs);
    function foldl(f: b -> a -> b, acc:b, xs:c(a)) : b =
        let g = foldr(fn(x, k) = fn(a) = k(f(a, x)), id, xs) in g(acc)
};

// ----------------------------------------------------------------------------
// Sized
// ----------------------------------------------------------------------------

/// Containers with a known finite size.
/// Separate from Foldable because some containers (infinite streams, iterators)
/// are foldable but don't have a finite size.
///
/// @param c A container type constructor (Type -> Type)
algebra Sized(c: Type -> Type) extends Foldable(c) = {
    /// Number of elements. O(1) for Array, O(n) for List.
    function size(xs:c(a)) : Int;
    /// Is the container empty?
    function isEmpty(xs:c(a)) : Bool;

    // Default
    function isEmpty(xs:c(a)) : Bool = size(xs) == 0;

    law sizeNonNeg(xs:c(a)) = size(xs) >= 0;
    law emptyIffZero(xs:c(a)) = isEmpty(xs) === (size(xs) == 0)
};

// ----------------------------------------------------------------------------
// Searchable
// ----------------------------------------------------------------------------

/// Search operations on containers.
/// Separate algebra because search semantics differ across container types:
/// - Sets search by value (O(log n))
/// - Maps search by key
/// - Sorted containers use binary search
/// - Lists/Arrays use linear scan
///
/// @param c A container type constructor (Type -> Type)
algebra Searchable(c: Type -> Type) extends Foldable(c) = {
    /// Find the first element satisfying a predicate.
    function find(p: a -> Bool, xs:c(a)) : Maybe(a);
    /// Does any element satisfy the predicate?
    function any(p: a -> Bool, xs:c(a)) : Bool;
    /// Do all elements satisfy the predicate?
    function all(p: a -> Bool, xs:c(a)) : Bool;
    /// Is the element present? Requires Eq on the element type.
    function elem(x:a, xs:c(a)) : Bool requires Eq(a);
    /// Count elements satisfying a predicate.
    function count(p: a -> Bool, xs:c(a)) : Int;

    // Defaults via Foldable
    function find(p: a -> Bool, xs:c(a)) : Maybe(a) =
        foldr(fn(x, acc) = if p(x) then Just(x) else acc, Nothing, xs);
    function any(p: a -> Bool, xs:c(a)) : Bool =
        foldl(fn(acc, x) = if acc then True else p(x), False, xs);
    function all(p: a -> Bool, xs:c(a)) : Bool =
        foldl(fn(acc, x) = if acc then p(x) else False, True, xs);
    function elem(x:a, xs:c(a)) : Bool = any(fn(y) = y == x, xs);
    function count(p: a -> Bool, xs:c(a)) : Int =
        foldl(fn(acc, x) = if p(x) then acc + 1 else acc, 0, xs);

    law findAny(p: a -> Bool, xs:c(a)) =
        any(p, xs) === not(find(p, xs) == Nothing)
};

// ----------------------------------------------------------------------------
// Filterable
// ----------------------------------------------------------------------------

/// Containers that support element-wise filtering.
/// Separate from Foldable because not all foldable containers support
/// filtering — e.g., filtering a Tree requires rebuilding, which needs
/// knowledge of tree structure beyond what this algebra provides.
///
/// @param c A container type constructor (Type -> Type)
algebra Filterable(c: Type -> Type) extends Foldable(c) = {
    /// Keep only elements satisfying the predicate.
    function filter(p: a -> Bool, xs:c(a)) : c(a);
    /// Split into (satisfying, not satisfying).
    function partition(p: a -> Bool, xs:c(a)) : Pair(c(a), c(a));
    /// Filter and map simultaneously — keep Just results, discard Nothing.
    function filterMap(f: a -> Maybe(b), xs:c(a)) : c(b);

    law filterAll(xs:c(a)) = filter(fn(x) = True, xs) === xs;
    law filterNone(xs:c(a)) = isEmpty(filter(fn(x) = False, xs)) === True
};

// ============================================================================
// Tier 2: Construction
// ============================================================================

// ----------------------------------------------------------------------------
// Buildable
// ----------------------------------------------------------------------------

/// Container construction. The dual of Foldable.
///
/// Combined with Foldable, Buildable enables generic container-to-container
/// conversion via `fromFoldable` — the universal interchange function.
/// No privileged container type needed.
///
/// @param c A container type constructor (Type -> Type)
algebra Buildable(c: Type -> Type) extends Sized(c) = {
    /// The empty container.
    function bempty() : c(a);
    /// Prepend an element. O(1) for List, O(n) for Array.
    function cons(x:a, xs:c(a)) : c(a);
    /// Append an element. O(n) for List, O(1) amortized for Array.
    function snoc(xs:c(a), x:a) : c(a);
    /// Single-element container.
    function singleton(x:a) : c(a);
    /// Repeat an element n times.
    function replicate(n:Int, x:a) : c(a);

    // Defaults
    function singleton(x:a) : c(a) = cons(x, bempty());
    function replicate(n:Int, x:a) : c(a) =
        if n <= 0 then bempty() else cons(x, replicate(n - 1, x));

    law singletonSize(x:a) = size(singleton(x)) === 1;
    law consSize(x:a, xs:c(a)) = size(cons(x, xs)) === size(xs) + 1;
    law snocSize(xs:c(a), x:a) = size(snoc(xs, x)) === size(xs) + 1
};

// ----------------------------------------------------------------------------
// Indexable
// ----------------------------------------------------------------------------

/// Indexed random access. The key algebra for performance-critical code.
///
/// Crucially, List does NOT implement this — O(n) indexing should not
/// masquerade as O(1) random access. If you need indexing, use Array
/// or another container with genuine O(1) access.
///
/// @param c A container type constructor (Type -> Type)
algebra Indexable(c: Type -> Type) extends Sized(c) = {
    /// Element at position i (0-based). Undefined for out-of-bounds.
    function index(xs:c(a), i:Int) : a;
    /// Safe indexing — returns Nothing for out-of-bounds.
    function indexMaybe(xs:c(a), i:Int) : Maybe(a);
    /// Extract a contiguous sub-container [start, end).
    function slice(xs:c(a), start:Int, end:Int) : c(a);
    /// Functional update: return a new container with element at i replaced.
    function update(xs:c(a), i:Int, val:a) : c(a);

    // Default
    function indexMaybe(xs:c(a), i:Int) : Maybe(a) =
        if i < 0 then Nothing
        else if i >= size(xs) then Nothing
        else Just(index(xs, i));

    law indexInBounds(xs:c(a), i:Int) =
        (i >= 0) ==> (i < size(xs)) ==> (indexMaybe(xs, i) === Just(index(xs, i)));
    law updateIndex(xs:c(a), i:Int, v:a) =
        index(update(xs, i, v), i) === v
};

// ----------------------------------------------------------------------------
// Bulk
// ----------------------------------------------------------------------------

/// Bulk operations: the GPU/SIMD acceleration gateway.
///
/// Any container implementing Bulk can participate in parallel computation.
/// The `generate`, `toArray`, and `fromArray` operations provide the bridge
/// between abstract containers and the machine-level Array primitive.
///
/// Laws (indexMap, generateSize, roundtrip) license the compiler to
/// parallelize and vectorize operations on Bulk containers.
///
/// Extends both Indexable (random access) and Functor (mapping) because
/// bulk operations need both capabilities.
///
/// @param c A container type constructor (Type -> Type)
algebra Bulk(c: Type -> Type) extends Indexable(c), Functor(c) = {
    /// Generate a container of size n from an index function.
    function generate(n:Int, f: Int -> a) : c(a);
    /// Convert to Array (the universal machine-level representation).
    function toArray(xs:c(a)) : Array(a);
    /// Convert from Array.
    function fromArray(xs:Array(a)) : c(a);

    law indexGenerate(n:Int, f: Int -> a, i:Int) =
        index(generate(n, f), i) === f(i);
    law generateSize(n:Int, f: Int -> a) =
        size(generate(n, f)) === n;
    law roundtrip(xs:c(a)) =
        fromArray(toArray(xs)) === xs;
    /// Functor law + Indexable: mapping and indexing commute.
    law indexMap(f: a -> b, xs:c(a), i:Int) =
        index(fmap(f, xs), i) === f(index(xs, i))
};

// ============================================================================
// Tier 3: Traversal
// ============================================================================

// ----------------------------------------------------------------------------
// Traversable
// ----------------------------------------------------------------------------

/// Structure-preserving effectful map.
///
/// `traverse` maps an effectful function over a container, sequencing the
/// effects while preserving the container structure (shape).
///
/// Unlike Haskell, the Applicative constraint is on the CALL SITE
/// (via `requires`), not baked into the algebra definition. This means
/// the algebra itself is lighter and the constraint is only checked
/// where traverse is actually used with a specific Applicative.
///
/// @param c A container type constructor (Type -> Type)
algebra Traversable(c: Type -> Type) extends Functor(c), Foldable(c) = {
    /// Map an effectful function over the container, sequencing effects.
    function traverse(f: a -> m(b), xs:c(a)) : m(c(b))
        requires Applicative(m);

    /// Sequence a container of effects into an effect of container.
    function sequence(xs:c(m(a))) : m(c(a))
        requires Applicative(m);

    // Default
    function sequence(xs:c(m(a))) : m(c(a)) = traverse(id, xs);

    law traverseIdentity(xs:c(a)) = traverse(Just, xs) === Just(xs)
};

// ============================================================================
// Tier 4: Structural operations
// ============================================================================

// ----------------------------------------------------------------------------
// Sortable
// ----------------------------------------------------------------------------

/// Containers whose elements can be sorted.
/// Not all containers have a meaningful sort — sets are already sorted,
/// maps sort by key not value, trees have their own ordering invariant.
///
/// @param c A container type constructor (Type -> Type)
algebra Sortable(c: Type -> Type) extends Buildable(c) = {
    /// Sort in ascending order using the element's Ord instance.
    function sort(xs:c(a)) : c(a) requires Ord(a);
    /// Sort using a custom comparison function.
    function sortBy(cmp: a -> a -> Ordering, xs:c(a)) : c(a);
    /// Sort by a key-extraction function (Schwartzian transform).
    function sortOn(key: a -> k, xs:c(a)) : c(a) requires Ord(k);

    // Defaults
    function sort(xs:c(a)) : c(a) = sortBy(compare, xs);
    function sortOn(key: a -> k, xs:c(a)) : c(a) =
        sortBy(fn(x, y) = compare(key(x), key(y)), xs);

    law sortIdempotent(xs:c(a)) = sort(sort(xs)) === sort(xs);
    law sortPreservesSize(xs:c(a)) = size(sort(xs)) === size(xs)
};

// ----------------------------------------------------------------------------
// Zippable
// ----------------------------------------------------------------------------

/// Element-wise combination of containers.
/// Result length is the minimum of the two inputs.
///
/// @param c A container type constructor (Type -> Type)
algebra Zippable(c: Type -> Type) extends Sized(c) = {
    /// Combine two containers element-wise using a function.
    function zipWith(f: a -> b -> d, xs:c(a), ys:c(b)) : c(d);
    /// Pair elements from two containers.
    function zip(xs:c(a), ys:c(b)) : c(Pair(a, b));
    /// Split a container of pairs into a pair of containers.
    function unzip(xs:c(Pair(a, b))) : Pair(c(a), c(b));

    // Default
    function zip(xs:c(a), ys:c(b)) : c(Pair(a, b)) = zipWith(Pair, xs, ys);

    law zipWithSize(f: a -> b -> d, xs:c(a), ys:c(b)) =
        size(zipWith(f, xs, ys)) <= size(xs);
    law zipUnzip(xs:c(a), ys:c(b)) =
        unzip(zip(xs, ys)) === Pair(xs, ys)
};

// ----------------------------------------------------------------------------
// Sliceable
// ----------------------------------------------------------------------------

/// Positional slicing and prefix/suffix operations.
///
/// @param c A container type constructor (Type -> Type)
algebra Sliceable(c: Type -> Type) extends Sized(c) = {
    /// First n elements.
    function take(n:Int, xs:c(a)) : c(a);
    /// Drop first n elements.
    function drop(n:Int, xs:c(a)) : c(a);
    /// Split at position n: equivalent to (take n xs, drop n xs).
    function splitAt(n:Int, xs:c(a)) : Pair(c(a), c(a));
    /// Take elements from the front while predicate holds.
    function takeWhile(p: a -> Bool, xs:c(a)) : c(a);
    /// Drop elements from the front while predicate holds.
    function dropWhile(p: a -> Bool, xs:c(a)) : c(a);
    /// Left scan: like foldl but keeps all intermediate accumulator values.
    function scanl(f: b -> a -> b, acc:b, xs:c(a)) : c(b);

    // Default
    function splitAt(n:Int, xs:c(a)) : Pair(c(a), c(a)) = Pair(take(n, xs), drop(n, xs));

    law takeDropReconstitute(n:Int, xs:c(a)) =
        size(take(n, xs)) + size(drop(n, xs)) === size(xs);
    law takeSize(n:Int, xs:c(a)) =
        size(take(n, xs)) <= n
};

// ============================================================================
// Universal container conversion
// ============================================================================

/// Convert any Foldable container to any Buildable container.
///
/// This is the universal interchange function, replacing Haskell's
/// privileged toList/fromList pattern. No container gets special treatment:
///
///     fromFoldable(myList)  : Array(Int)   // List → Array
///     fromFoldable(myArray) : List(Int)    // Array → List
///     fromFoldable(myTree)  : Array(Int)   // Tree → Array
///
/// The compiler can optimize specific pairs (e.g., Array→Array is identity).
function fromFoldable(xs:c1(a)) : c2(a)
    requires Foldable(c1), Buildable(c2) =
    foldl(fn(acc, x) = snoc(acc, x), bempty(), xs);

// ============================================================================
// Array instances (intrinsic-backed)
// ============================================================================

/// Semigroup instance for Array — concatenation.
instance Semigroup(Array(a)) = intrinsic;

/// Monoid instance for Array — empty array identity.
instance Monoid(Array(a)) = {
    function combine(xs:Array(a), ys:Array(a)) : Array(a) = intrinsic;
    value empty : Array(a) = []
};

/// Functor instance for Array (backed by dispatchArrayHOF).
instance Functor(Array) = intrinsic;

/// Foldable instance for Array.
instance Foldable(Array) = intrinsic;

/// Sized instance for Array. O(1) size via intrinsic length.
instance Sized(Array) = intrinsic;

/// Searchable instance for Array.
instance Searchable(Array) = intrinsic;

/// Filterable instance for Array.
instance Filterable(Array) = intrinsic;

/// Buildable instance for Array.
instance Buildable(Array) = intrinsic;

/// Indexable instance for Array. O(1) random access.
instance Indexable(Array) = intrinsic;

/// Bulk instance for Array. The primary GPU/SIMD acceleration target.
instance Bulk(Array) = intrinsic;

/// Sortable instance for Array.
instance Sortable(Array) = intrinsic;

/// Zippable instance for Array.
instance Zippable(Array) = intrinsic;

/// Sliceable instance for Array.
instance Sliceable(Array) = intrinsic;

// ============================================================================
// Backward-compatible Array operations
// ============================================================================

/// Element access by index (0-based). Delegates to Indexable.
// function index(arr:Array(a), i:Int) : a = intrinsic;

/// Extract a subarray by range [start, end). Delegates to Indexable.
// function slice(arr:Array(a), start:Int, end:Int) : Array(a) = intrinsic;

/// Reverse an array.
function reverse(arr:Array(a)) : Array(a) = intrinsic;

/// Generate a range of integers [start, end).
function range(start:Int, end:Int) : Array(Int) = intrinsic;
