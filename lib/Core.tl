/// Core data types and combinators.
///
/// The foundational types of the tulam type system, ordered by mathematical
/// structure: initial/terminal objects, then finite types, then the identity
/// type, then basic type constructors, then recursive data structures.
///
/// All operations on these types are defined as algebra instances in the
/// Instances/ folder. This file contains only type definitions and
/// universal combinators.
module Core;

// ============================================================================
// Void (⊥) — The empty type / initial object
// ============================================================================

/// The uninhabited type — has no constructors and no values.
/// Corresponds to ⊥ (bottom) in logic, the initial object in category theory.
/// A function `Void -> a` is vacuously true for any `a`.
type Void;

// ============================================================================
// Unit (⊤) — The unit type / terminal object
// ============================================================================

/// The unit type with exactly one value.
/// Corresponds to ⊤ (top) in logic, the terminal object in category theory.
type Unit = Unit;

/// Discards any value and returns Unit.
/// @param x Any value
/// @returns Unit
function unit(x:a) : Unit = Unit;

// ============================================================================
// Bool (𝟐) — The boolean type
// ============================================================================

/// The boolean type with exactly two values.
/// @example True
/// @example False
type Bool = True + False;

/// Logical negation.
/// Defined in Core because it's used by Algebra default methods (Eq, Ord).
/// @param b The boolean to negate
/// @returns The opposite boolean value
function not (b:Bool) : Bool = match
    | True  -> False
    | False -> True;

// ============================================================================
// PropEq — Propositional equality (identity type)
// ============================================================================

/// Propositional equality: a proof that x and y are equal at type a.
/// This is the fundamental type for expressing and proving equalities.
/// Refl is the sole constructor, and can only be constructed when both
/// indices are definitionally equal: Refl : PropEq(a, x, x).
///
/// @param a The type of the values being compared
/// @param x The left-hand value
/// @param y The right-hand value
///
/// Used by the `law` keyword: `law name(params) = lhs === rhs` desugars to
/// a function returning `PropEq(_, lhs, rhs) = Refl`.
type PropEq(a:Type, x:a, y:a) = Refl : PropEq(a, x, x);

// ============================================================================
// Ordering — Three-valued comparison result
// ============================================================================

/// Result of comparing two values.
/// @example compare(1, 2) => LessThan
/// @example compare(2, 2) => Equal
/// @example compare(3, 2) => GreaterThan
type Ordering = LessThan + Equal + GreaterThan;

// ============================================================================
// Nat (ℕ) — Natural numbers (Peano representation)
// ============================================================================

/// Natural numbers in Peano representation.
/// Z is zero, Succ(n) is the successor of n.
/// @example Z
/// @example Succ(Z)
/// @example Succ(Succ(Z))
type Nat = Z + Succ * n:Nat;

// ============================================================================
// Maybe (1 + a) — Optional values
// ============================================================================

/// Optional value — either Nothing or Just(value).
/// @param a The type of the contained value
type Maybe(a:Type) = Nothing + Just * val:a;

// ============================================================================
// Either (a + b) — Coproduct / sum type
// ============================================================================

/// A value of type a (Left) or type b (Right).
/// By convention, Left = error, Right = success.
type Either(a:Type, b:Type) = Left * val:a + Right * val:b;

// ============================================================================
// Pair (a × b) — Product type
// ============================================================================

/// A pair of two values (product type).
type Pair(a:Type, b:Type) = Pair * fst:a * snd:b;

// ============================================================================
// List (μX. 1 + a × X) — Singly-linked list
// ============================================================================

/// Singly-linked list — either empty (Nil) or head + tail (Cons).
/// @param a Element type
type List(a:Type) = Nil + Cons * hd:a * tl:List(a);

// ============================================================================
// NonEmpty — Non-empty list
// ============================================================================

/// Non-empty list — always has at least one element.
/// @param a Element type
type NonEmpty(a:Type) = NonEmpty * hd:a * tl:List(a);

// ============================================================================
// Tree — Binary tree
// ============================================================================

/// Binary tree — Leaf(value) or Branch(left, right).
/// @param a The type of values stored at leaves
type Tree(a:Type) = Leaf * val:a + Branch * left:Tree(a) * right:Tree(a);

// ============================================================================
// Combinators
// ============================================================================

/// Identity function — returns its argument unchanged.
function id(x:a) : a = x;

/// Constant function — ignores its second argument.
function const(x:a, y:b) : a = x;

/// Function application.
function apply(f:a -> b, x:a) : b = f(x);

/// Function composition — (f . g)(x) = f(g(x)).
function compose(f:b -> c, g:a -> b, x:a) : c = f(g(x));

/// Flips the first two arguments of a function.
function flip(f:a -> b -> c, x:b, y:a) : c = f(y, x);

/// Terminate with an error message. Type is forall a. String -> a (bottom/absurd).
function error(msg:String) : a = intrinsic;
