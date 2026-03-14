/// Core algebraic structures.
///
/// Single abstract hierarchy: Semigroup → Monoid → Group → AbelianGroup.
/// Named instances distinguish additive vs multiplicative structure:
///   instance Semigroup(Int) as Additive = { ... };
///   instance Semigroup(Int) as Multiplicative = { ... };
/// Ring hierarchy: Semiring, Ring, CommutativeRing, EuclideanDomain, Field, Floating.
/// Utility: Absolute, Bits, Eq, Ord, Show, Bounded, Enum, Hashable.
/// Lattice: JoinSemiLattice, MeetSemiLattice, Lattice, BoundedLattice.
module Algebra;
import Core;
import Reflection;

// ============================================================================
// Semigroup / Monoid / Group — the ONE hierarchy
// ============================================================================

infixr 5 (<>), (⊕);

/// Associative binary operation.
algebra Semigroup(a:Type) = {
    /// Combine two values associatively.
    function combine(x:a, y:a) : a;
    /// ASCII operator alias for combine.
    function (<>)(x:a, y:a) : a = combine(x, y);
    /// Unicode operator alias for combine.
    function (⊕)(x:a, y:a) : a = combine(x, y);

    law associativity(x:a, y:a, z:a) = combine(combine(x, y), z) === combine(x, combine(y, z))
};

/// Semigroup with an identity element.
algebra Monoid(a:Type) extends Semigroup(a) = {
    /// The identity element for combine.
    value empty : a;

    law left_identity(x:a) = combine(empty, x) === x;
    law right_identity(x:a) = combine(x, empty) === x
};

/// Monoid with an inverse operation.
algebra Group(a:Type) extends Monoid(a) = {
    /// Compute the inverse of a value.
    function inverse(x:a) : a;

    law left_inverse(x:a) = combine(inverse(x), x) === empty;
    law right_inverse(x:a) = combine(x, inverse(x)) === empty
};

/// Group with a commutative combine operation.
algebra AbelianGroup(a:Type) extends Group(a) = {
    law commutativity(x:a, y:a) = combine(x, y) === combine(y, x)
};

// ============================================================================
// Ring hierarchy (operators live inside their algebras)
// ============================================================================

infixl 6 (+), (-);
infixl 7 (*), (/);

/// Addition and multiplication with distributivity (no negation required).
algebra Semiring(a:Type) extends Monoid(a) as Additive, Monoid(a) as Multiplicative = {
    /// Addition: additive combine.
    function (+)(x:a, y:a) : a requires Monoid(a) as Additive = combine(x, y);
    /// Multiplication: multiplicative combine.
    function (*)(x:a, y:a) : a requires Monoid(a) as Multiplicative = combine(x, y);
    /// Additive identity.
    value zero : a requires Monoid(a) as Additive = empty;
    /// Multiplicative identity.
    value one : a requires Monoid(a) as Multiplicative = empty;

    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z;
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z;
    law annihilation(x:a) = zero * x === zero;
    law additive_commutativity(x:a, y:a) = x + y === y + x
};

/// Full ring: semiring with additive negation.
algebra Ring(a:Type) extends Semiring(a), Group(a) as Additive = {
    /// Additive negation.
    function negate(x:a) : a requires Group(a) as Additive = inverse(x);
    /// Subtraction.
    function (-)(x:a, y:a) : a = x + negate(y);

    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z;
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z;
    law annihilation(x:a) = zero * x === zero
};

/// Ring with commutative multiplication.
algebra CommutativeRing(a:Type) extends Ring(a) = {
    law multiplicative_commutativity(x:a, y:a) = x * y === y * x
};

/// Commutative ring with integer division and modulo.
algebra EuclideanDomain(a:Type) extends CommutativeRing(a) = {
    function div(x:a, y:a) : a;
    function mod(x:a, y:a) : a;

    law division(x:a, y:a) = div(x, y) * y + mod(x, y) === x
};

/// Commutative ring where every non-zero has a multiplicative inverse.
algebra Field(a:Type) extends CommutativeRing(a), Group(a) as Multiplicative = {
    /// Multiplicative reciprocal.
    function recip(x:a) : a requires Group(a) as Multiplicative = inverse(x);
    /// Division.
    function (/)(x:a, y:a) : a = x * recip(y);

    law multiplicative_inverse(x:a) = x * recip(x) === one
};

// ============================================================================
// Floating
// ============================================================================

/// Transcendental floating-point operations.
algebra Floating(a:Type) extends Field(a) = {
    function sqrt(x:a) : a;
    function exp(x:a) : a;
    function log(x:a) : a;
    function sin(x:a) : a;
    function cos(x:a) : a;
    function tan(x:a) : a;
    function asin(x:a) : a;
    function acos(x:a) : a;
    function atan(x:a) : a;
    function pow(x:a, y:a) : a;
    value pi : a;

    law sqrt_square(x:a) = sqrt(x) * sqrt(x) === x;
    law exp_log(x:a) = exp(log(x)) === x;
    law pythagorean(x:a) = sin(x) * sin(x) + cos(x) * cos(x) === one
};

// ============================================================================
// Absolute
// ============================================================================

/// Absolute value and signum (sign) operations.
algebra Absolute(a:Type) = {
    function abs(x:a) : a;
    function signum(x:a) : a;

    law idempotence(x:a) = abs(abs(x)) === abs(x)
};

// ============================================================================
// Bits
// ============================================================================

infixr 3 (.&.);
infixr 2 (.|.);

/// Bitwise operations algebra.
algebra Bits(a:Type) = {
    function (.&.)(x:a, y:a) : a;
    function (.|.)(x:a, y:a) : a;
    function xor(x:a, y:a) : a;
    function complement(x:a) : a;
    function shiftL(x:a, y:a) : a;
    function shiftR(x:a, y:a) : a;
    value bitSize : a;

    law double_complement(x:a) = complement(complement(x)) === x;
    law and_idempotent(x:a)   = x .&. x === x;
    law or_idempotent(x:a)    = x .|. x === x;
    law xor_self(x:a, y:a)    = xor(x, x) === xor(y, y);
    law and_commutativity(x:a, y:a) = x .&. y === y .&. x;
    law or_commutativity(x:a, y:a)  = x .|. y === y .|. x;
    law xor_commutativity(x:a, y:a) = xor(x, y) === xor(y, x)
};

// ============================================================================
// Eq
// ============================================================================

infix 4 (==), (!=), (≠);

/// Equality algebra with reflexivity, symmetry, and transitivity laws.
algebra Eq(a:Type) = {
    function (==)(x:a, y:a) : Bool = not(x ≠ y);
    function (!=)(x:a, y:a) : Bool = not(x == y);
    /// Unicode alias for !=.
    function (≠)(x:a, y:a) : Bool = not(x == y);

    law reflexivity(x:a) = (x == x) === True;
    law symmetry(x:a, y:a) = (x == y) === (y == x);
    law transitivity(x:a, y:a, z:a) = ((x == y) === True) ==> ((y == z) === True) ==> ((x == z) === True);

    derive {
        function (==)(x:a, y:a) : Bool = structuralEq(x, y);
        function (!=)(x:a, y:a) : Bool = not(structuralEq(x, y));
        function (≠)(x:a, y:a) : Bool = not(structuralEq(x, y))
    }
};

// ============================================================================
// Ord
// ============================================================================

infix 4 (<), (>), (<=), (>=), (≤), (≥);

/// Total ordering with comparison and relational operators.
algebra Ord(a:Type) extends Eq(a) = {
    function compare(x:a, y:a) : Ordering =
        if x == y then Equal
        else if x < y then LessThan
        else GreaterThan;

    function (<)(x:a, y:a) : Bool = compare(x, y) == LessThan;
    function (>)(x:a, y:a) : Bool = compare(x, y) == GreaterThan;
    function (<=)(x:a, y:a) : Bool = not(compare(x, y) == GreaterThan);
    function (>=)(x:a, y:a) : Bool = not(compare(x, y) == LessThan);
    /// Unicode alias for <=.
    function (≤)(x:a, y:a) : Bool = x <= y;
    /// Unicode alias for >=.
    function (≥)(x:a, y:a) : Bool = x >= y;

    law trichotomy(x:a, y:a) = (x < y) === not(x ≥ y);
    law antisymmetry(x:a, y:a) = ((x ≤ y) === True) ==> ((y ≤ x) === (x == y));
    law transitivity(x:a, y:a, z:a) = ((x ≤ y) === True) ==> ((y ≤ z) === True) ==> ((x ≤ z) === True);

    derive {
        function compare(x:a, y:a) : Ordering = structuralCompare(x, y);
        function (<)(x:a, y:a) : Bool = match structuralCompare(x, y) | LessThan -> True | _ -> False;
        function (>)(x:a, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> True | _ -> False;
        function (<=)(x:a, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> False | _ -> True;
        function (>=)(x:a, y:a) : Bool = match structuralCompare(x, y) | LessThan -> False | _ -> True;
        function (≤)(x:a, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> False | _ -> True;
        function (≥)(x:a, y:a) : Bool = match structuralCompare(x, y) | LessThan -> False | _ -> True
    }
};

// ============================================================================
// Show
// ============================================================================

/// String conversion algebra.
algebra Show(a:Type) = {
    function show(x:a) : String;

    derive {
        function show(x:a) : String = structuralShow(x)
    }
};

// ============================================================================
// Bounded
// ============================================================================

/// Bounded range with minimum and maximum values.
algebra Bounded(a:Type) = {
    value minBound : a;
    value maxBound : a
};

// ============================================================================
// Enum
// ============================================================================

/// Sequential enumeration algebra.
algebra Enum(a:Type) = {
    function succ(x:a) : a;
    function pred(x:a) : a;
    function toEnum(i:Int) : a;
    function fromEnum(x:a) : Int;

    law succ_enum(x:a) = fromEnum(succ(x)) === fromEnum(x) + 1;
    law roundtrip(x:a) = toEnum(fromEnum(x)) === x
};

// ============================================================================
// Hashable
// ============================================================================

/// Hashing algebra for hash-based data structures.
algebra Hashable(a:Type) = {
    function hash(x:a) : Int;

    derive {
        function hash(x:a) : Int = structuralHash(x)
    }
};

// ============================================================================
// Lattice
// ============================================================================

infixr 3 (/\), (∧);
infixr 2 (\/), (∨);

/// Join (least upper bound) semi-lattice.
algebra JoinSemiLattice(a:Type) = {
    function (\/)(x:a, y:a) : a;
    /// Unicode alias for \/.
    function (∨)(x:a, y:a) : a = x \/ y;

    law join_associativity(x:a, y:a, z:a) = (x ∨ y) ∨ z === x ∨ (y ∨ z);
    law join_commutativity(x:a, y:a) = (x ∨ y) === (y ∨ x);
    law join_idempotence(x:a) = (x ∨ x) === x
};

/// Meet (greatest lower bound) semi-lattice.
algebra MeetSemiLattice(a:Type) = {
    function (/\)(x:a, y:a) : a;
    /// Unicode alias for /\.
    function (∧)(x:a, y:a) : a = x /\ y;

    law meet_associativity(x:a, y:a, z:a) = (x ∧ y) ∧ z === x ∧ (y ∧ z);
    law meet_commutativity(x:a, y:a) = (x ∧ y) === (y ∧ x);
    law meet_idempotence(x:a) = (x ∧ x) === x
};

/// Full lattice with both join and meet.
algebra Lattice(a:Type) extends JoinSemiLattice(a), MeetSemiLattice(a) = {
    law join_absorption(x:a, y:a) = x ∨ (x ∧ y) === x;
    law meet_absorption(x:a, y:a) = x ∧ (x ∨ y) === x
};

/// Lattice with top and bottom elements.
algebra BoundedLattice(a:Type) extends Lattice(a) = {
    value bottom : a;
    value top : a;

    law bottom_identity(x:a) = (bottom ∨ x) === x;
    law top_identity(x:a) = (top ∧ x) === x
};
