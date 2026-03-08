/// Core data types and combinators.
///
/// Pure algebraic data types with no algebra imports.
/// Bool, Unit, Void, Ordering, Nat, Maybe, Either, List, Pair, NonEmpty, Tree,
/// and standard combinators (id, const, compose, flip, apply).
module Core;

// ============================================================================
// Bool
// ============================================================================

/// The boolean type with exactly two values.
/// @example True
/// @example False
type Bool = True | False;

/// Logical negation.
/// @param b The boolean to negate
/// @returns The opposite boolean value
/// @example not(True) => False
/// @example not(False) => True
function not (b:Bool) : Bool = match
    | True  -> False
    | False -> True;

// ============================================================================
// Unit
// ============================================================================

/// The unit type with a single constructor.
type Unit = Unit;

/// Discards any value and returns Unit.
/// @param x Any value
/// @returns Unit
function unit(x:a) : Unit = Unit;

// ============================================================================
// Ordering
// ============================================================================

/// Result of comparing two values.
/// @example compare(1, 2) => LessThan
/// @example compare(2, 2) => Equal
/// @example compare(3, 2) => GreaterThan
type Ordering = LessThan | Equal | GreaterThan;

// ============================================================================
// Nat
// ============================================================================

/// Natural numbers in Peano representation.
/// @example Z
/// @example Succ(Z)
/// @example Succ(Succ(Z))
type Nat = Z | Succ(n:Nat);

/// Structural equality on natural numbers.
function eq(x:Nat,y:Nat):Bool = match
  | Z, Z -> True
  | Z, n -> False
  | n, Z -> False
  | Succ(m), Succ(n) -> eq(m,n);

/// Addition of natural numbers.
function plus (x:Nat,y:Nat):Nat = match
  | Z, n -> n
  | Succ(n), m -> Succ (plus(n, m));

/// Multiplication of natural numbers.
function mult (x:Nat,y:Nat):Nat = match
  | Z, n -> Z
  | Succ(n), m -> plus(mult(n, m), m);

// ============================================================================
// Maybe
// ============================================================================

/// Optional value — either Nothing or Just(value).
/// @param a The type of the contained value
type Maybe(a:Type) = Nothing | Just(val:a);

/// Check if a Maybe is Nothing.
function isNothing(m:Maybe(a)) : Bool = match
    | Nothing -> True
    | Just(x) -> False;

/// Extract the value or use a default.
function fromMaybe(def:a, m:Maybe(a)) : a = match
    | d, Nothing -> d
    | d, Just(x) -> x;

// ============================================================================
// Either
// ============================================================================

/// A value of type a (Left) or type b (Right).
/// By convention, Left = error, Right = success.
type Either(a:Type, b:Type) = Left(val:a) | Right(val:b);

// ============================================================================
// List
// ============================================================================

/// Singly-linked list — either empty (Nil) or head + tail (Cons).
/// @param a Element type
type List(a:Type) = Nil | Cons(hd:a, tl:List(a));

/// Safe head — returns the first element wrapped in Maybe.
function head(xs:List(a)) : Maybe(a) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(h);

/// Safe tail — returns the rest of the list wrapped in Maybe.
function tail(xs:List(a)) : Maybe(List(a)) = match
    | Nil -> Nothing
    | Cons(h, t) -> Just(t);

/// Check if a list is empty.
function isEmpty(xs:List(a)) : Bool = match
    | Nil -> True
    | Cons(h, t) -> False;

// ============================================================================
// Pair
// ============================================================================

/// A pair of two values.
type Pair(a:Type, b:Type) = Pair(fst:a, snd:b);

/// First projection.
function fst(p:Pair(a, b)) : a = match
    | Pair(x, y) -> x;

/// Second projection.
function snd(p:Pair(a, b)) : b = match
    | Pair(x, y) -> y;

/// Exchanges the two components.
function swap(p:Pair(a, b)) : Pair(b, a) = match
    | Pair(x, y) -> Pair(y, x);

/// Applies a function to the first component.
function mapFst(f:a, p:Pair(a, b)) : Pair(c, b) = match
    | f, Pair(x, y) -> Pair(f(x), y);

/// Applies a function to the second component.
function mapSnd(f:b, p:Pair(a, b)) : Pair(a, c) = match
    | f, Pair(x, y) -> Pair(x, f(y));

/// Applies the same function to both components of a homogeneous pair.
function both(f:a, p:Pair(a, a)) : Pair(b, b) = match
    | f, Pair(x, y) -> Pair(f(x), f(y));

// ============================================================================
// NonEmpty
// ============================================================================

/// Non-empty list — always has at least one element.
/// @param a Element type
type NonEmpty(a:Type) = NonEmpty(hd:a, tl:List(a));

/// Returns the first element (always safe).
function neHead(xs:NonEmpty(a)) : a = match
    | NonEmpty(h, t) -> h;

/// Converts to a regular List.
function toList(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> Cons(h, t);

/// Creates a non-empty list with exactly one element.
function singleton(x:a) : NonEmpty(a) = NonEmpty(x, Nil);

/// Prepends an element to a non-empty list.
function neCons(x:a, xs:NonEmpty(a)) : NonEmpty(a) = match
    | x, NonEmpty(h, t) -> NonEmpty(x, Cons(h, t));

/// Returns the tail as a regular List.
function neTail(xs:NonEmpty(a)) : List(a) = match
    | NonEmpty(h, t) -> t;

// ============================================================================
// Tree
// ============================================================================

/// Binary tree — Leaf(value) or Branch(left, right).
/// @param a The type of values stored at leaves
type Tree(a:Type) = Leaf(val:a) | Branch(left:Tree(a), right:Tree(a));

/// Counts the number of leaves in the tree.
function treeSize(t:Tree(a)) : Nat = match
    | Leaf(x) -> Succ(Z)
    | Branch(l, r) -> plus(treeSize(l), treeSize(r));

/// Computes the depth (height) of the tree.
function treeDepth(t:Tree(a)) : Nat = match
    | Leaf(x) -> Z
    | Branch(l, r) -> Succ(natMax(treeDepth(l), treeDepth(r)));

/// Returns the larger of two natural numbers.
function natMax(x:Nat, y:Nat) : Nat = match
    | Z, n -> n
    | n, Z -> n
    | Succ(m), Succ(n) -> Succ(natMax(m, n));

/// Applies a function to every leaf value, preserving structure.
function treeMap(f:a, t:Tree(a)) : Tree(b) = match
    | f, Leaf(x) -> Leaf(f(x))
    | f, Branch(l, r) -> Branch(treeMap(f, l), treeMap(f, r));

/// Catamorphism — fold a tree by replacing Leaf and Branch with functions.
function treeFold(f:a, g:b, t:Tree(a)) : b = match
    | f, g, Leaf(x) -> f(x)
    | f, g, Branch(l, r) -> g(treeFold(f, g, l), treeFold(f, g, r));

// ============================================================================
// Combinators
// ============================================================================

/// Identity function — returns its argument unchanged.
function id(x:a) : a = x;

/// Constant function — ignores its second argument.
function const(x:a, y:b) : a = x;

/// Function application.
function apply(f:a, x:b) : c = f(x);

/// Function composition — (f . g)(x) = f(g(x)).
function compose(f:b, g:a, x:a) : c = f(g(x));

/// Flips the first two arguments of a function.
function flip(f:a, x:b, y:c) = f(y, x);
