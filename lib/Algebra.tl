/// All algebraic structures (algebras/typeclasses).
///
/// Semigroup, Monoid, Group, Additive, Multiplicative, Ring, Field, Floating,
/// Absolute, Bits, Eq, Ord, Show, Bounded, Enum, Hashable, Lattice,
/// StringOps, StringExt, StringLike.
module Algebra;
import Core;

// ============================================================================
// Semigroup / Monoid / Group
// ============================================================================

/// Associative binary operation.
algebra Semigroup(a:Type) = {
    /// Combine two values associatively.
    function combine(x:a, y:a) : a;

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
// Additive hierarchy
// ============================================================================

infixl 6 (+), (-);

/// Associative addition.
algebra AdditiveSemigroup(a:Type) = {
    function (+)(x:a, y:a) : a;

    law associativity(x:a, y:a, z:a) = (x + y) + z === x + (y + z)
};

/// Addition with a zero element.
algebra AdditiveMonoid(a:Type) extends AdditiveSemigroup(a) = {
    value zero : a;

    law left_identity(x:a)  = zero + x === x;
    law right_identity(x:a) = x + zero === x
};

/// Addition with negation and subtraction.
algebra AdditiveGroup(a:Type) extends AdditiveMonoid(a) = {
    function negate(x:a) : a;
    function (-)(x:a, y:a) : a = x + negate(y);

    law left_inverse(x:a)  = negate(x) + x === zero;
    law right_inverse(x:a) = x + negate(x) === zero;
    law subtraction(x:a, y:a) = x - y === x + negate(y)
};

/// Commutative addition.
algebra AdditiveAbelianGroup(a:Type) extends AdditiveGroup(a) = {
    law additive_commutativity(x:a, y:a) = x + y === y + x
};

// ============================================================================
// Multiplicative hierarchy
// ============================================================================

infixl 7 (*), (/);

/// Associative multiplication.
algebra MultiplicativeSemigroup(a:Type) = {
    function (*)(x:a, y:a) : a;

    law associativity(x:a, y:a, z:a) = (x * y) * z === x * (y * z)
};

/// Multiplication with a one element.
algebra MultiplicativeMonoid(a:Type) extends MultiplicativeSemigroup(a) = {
    value one : a;

    law left_identity(x:a)  = one * x === x;
    law right_identity(x:a) = x * one === x
};

/// Multiplication with reciprocal and division.
algebra MultiplicativeGroup(a:Type) extends MultiplicativeMonoid(a) = {
    function recip(x:a) : a;
    function (/)(x:a, y:a) : a = x * recip(y);

    law left_inverse(x:a)  = recip(x) * x === one;
    law right_inverse(x:a) = x * recip(x) === one;
    law division(x:a, y:a) = x / y === x * recip(y)
};

// ============================================================================
// Ring hierarchy
// ============================================================================

/// Addition and multiplication with distributivity (no negation required).
algebra Semiring(a:Type) extends AdditiveMonoid(a), MultiplicativeMonoid(a) = {
    law left_distributivity(x:a, y:a, z:a)  = x * (y + z) === x * y + x * z;
    law right_distributivity(x:a, y:a, z:a) = (x + y) * z === x * z + y * z;
    law annihilation(x:a) = zero * x === zero;
    law additive_commutativity(x:a, y:a) = x + y === y + x
};

/// Full ring: addition (with negation) and multiplication with distributivity.
algebra Ring(a:Type) extends AdditiveAbelianGroup(a), MultiplicativeMonoid(a) = {
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

/// Commutative ring where every non-zero element has a multiplicative inverse.
algebra Field(a:Type) extends CommutativeRing(a), MultiplicativeGroup(a) = {
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

    law decomposition(x:a) = abs(x) * signum(x) === x;
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
// StringOps
// ============================================================================

/// String concatenation and length algebra.
algebra StringOps(a:Type) = {
    function concat(x:a, y:a) : a;
    function length(x:a) : Int;

    law concat_associativity(x:a, y:a, z:a) = concat(concat(x, y), z) === concat(x, concat(y, z))
};

// ============================================================================
// Eq
// ============================================================================

infix 4 (==), (!=);

/// Equality algebra with reflexivity, symmetry, and transitivity laws.
algebra Eq (a:Type) = {
    function (==) (x,y:a) : Bool = not (x != y);
    function (!=) (x,y:a) : Bool = not (x == y);

    law reflexivity(x:a) = (x == x) === True;
    law symmetry(x:a, y:a) = (x == y) === (y == x);
    law transitivity(x:a, y:a, z:a) = ((x == y) === True) ==> ((y == z) === True) ==> ((x == z) === True);

    derive {
        function (==)(x, y:a) : Bool = structuralEq(x, y);
        function (!=)(x, y:a) : Bool = not(structuralEq(x, y))
    }
};

// ============================================================================
// Ord
// ============================================================================

infix 4 (<), (>), (<=), (>=);

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

    law trichotomy(x:a, y:a) = (x < y) === not(x >= y);
    law antisymmetry(x:a, y:a) = ((x <= y) === True) ==> ((y <= x) === (x == y));
    law transitivity(x:a, y:a, z:a) = ((x <= y) === True) ==> ((y <= z) === True) ==> ((x <= z) === True);

    derive {
        function compare(x, y:a) : Ordering = structuralCompare(x, y);
        function (<)(x, y:a) : Bool = match structuralCompare(x, y) | LessThan -> True | _ -> False;
        function (>)(x, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> True | _ -> False;
        function (<=)(x, y:a) : Bool = match structuralCompare(x, y) | GreaterThan -> False | _ -> True;
        function (>=)(x, y:a) : Bool = match structuralCompare(x, y) | LessThan -> False | _ -> True
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
    value maxBound : a;

    law bounds_order(x:a) = (minBound <= maxBound) === True
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

    law consistency(x:a, y:a) = ((x == y) === True) ==> (hash(x) === hash(y));

    derive {
        function hash(x:a) : Int = structuralHash(x)
    }
};

// ============================================================================
// Lattice
// ============================================================================

infixr 3 (/\);
infixr 2 (\/);

/// Join (least upper bound) semi-lattice.
algebra JoinSemiLattice(a:Type) = {
    function (\/)(x:a, y:a) : a;

    law join_associativity(x:a, y:a, z:a) = (x \/ y) \/ z === x \/ (y \/ z);
    law join_commutativity(x:a, y:a) = (x \/ y) === (y \/ x);
    law join_idempotence(x:a) = (x \/ x) === x
};

/// Meet (greatest lower bound) semi-lattice.
algebra MeetSemiLattice(a:Type) = {
    function (/\)(x:a, y:a) : a;

    law meet_associativity(x:a, y:a, z:a) = (x /\ y) /\ z === x /\ (y /\ z);
    law meet_commutativity(x:a, y:a) = (x /\ y) === (y /\ x);
    law meet_idempotence(x:a) = (x /\ x) === x
};

/// Full lattice with both join and meet.
algebra Lattice(a:Type) extends JoinSemiLattice(a), MeetSemiLattice(a) = {
    law join_absorption(x:a, y:a) = x \/ (x /\ y) === x;
    law meet_absorption(x:a, y:a) = x /\ (x \/ y) === x
};

/// Lattice with top and bottom elements.
algebra BoundedLattice(a:Type) extends Lattice(a) = {
    value bottom : a;
    value top : a;

    law bottom_identity(x:a) = (bottom \/ x) === x;
    law top_identity(x:a) = (top /\ x) === x
};

// ============================================================================
// StringExt
// ============================================================================

/// Extended string manipulation algebra.
algebra StringExt(a:Type) = {
    function charAt(s:a, i:Int) : Char;
    function substring(s:a, start:Int, end:Int) : a;
    function indexOf(s:a, needle:a) : Int;
    function trim(s:a) : a;
    function toUpper(s:a) : a;
    function toLower(s:a) : a;
    function startsWith(s:a, prefix:a) : Bool;
    function endsWith(s:a, suffix:a) : Bool;
    function replace(s:a, old:a, new:a) : a;
    function parseInt(s:a) : Maybe(Int);
    function parseFloat(s:a) : Maybe(Float64)
};

// ============================================================================
// StringLike
// ============================================================================

/// Check if a code point is ASCII whitespace.
function isWhitespace(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp == 32 then True
    else if cp == 9 then True
    else if cp == 10 then True
    else if cp == 13 then True
    else False;

/// Check if a char is an ASCII uppercase letter.
function isUpperCase(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp >= 65 then cp <= 90 else False;

/// Check if a char is an ASCII lowercase letter.
function isLowerCase(c:Char) : Bool =
    let cp = fromEnum(c) in
    if cp >= 97 then cp <= 122 else False;

/// Convert a single char to uppercase (ASCII only).
function charToUpper(c:Char) : Char =
    if isLowerCase(c) then toEnum(fromEnum(c) - 32)
    else c;

/// Convert a single char to lowercase (ASCII only).
function charToLower(c:Char) : Char =
    if isUpperCase(c) then toEnum(fromEnum(c) + 32)
    else c;

/// String-specific operations algebra.
/// Types implementing StringLike should also implement Eq, Ord, and Monoid.
algebra StringLike(s:Type) = {
    // === Core methods (required, no body) ===

    function byteLength(str:s) : Int;
    function strSlice(str:s, start:Int, end:Int) : s;
    function strDecodeAt(str:s, byteOffset:Int) : Maybe(Pair(Char, Int));
    function strIndexOf(haystack:s, needle:s) : Int;
    function strFromChar(c:Char) : s;

    // === High-level operations (defaulted) ===

    function charCount(str:s) : Int = charCountFrom(str, 0, 0);

    function charCountFrom(str:s, byteOff:Int, count:Int) : Int =
        charCountStep(strDecodeAt(str, byteOff), count, str);

    function charCountStep(r:Maybe(Pair(Char, Int)), count:Int, str:s) : Int = match
        | Nothing, count, str -> count
        | Just(Pair(ch, nextOff)), count, str -> charCountFrom(str, nextOff, count + 1);

    function nthChar(str:s, n:Int) : Maybe(Char) = nthCharHelper(str, 0, n);

    function nthCharHelper(str:s, byteOff:Int, remaining:Int) : Maybe(Char) =
        nthCharStep(strDecodeAt(str, byteOff), remaining, str);

    function nthCharStep(r:Maybe(Pair(Char, Int)), remaining:Int, str:s) : Maybe(Char) = match
        | Nothing, remaining, str -> Nothing
        | Just(Pair(ch, nextOff)), remaining, str ->
            if remaining == 0 then Just(ch)
            else nthCharHelper(str, nextOff, remaining - 1);

    function toChars(str:s) : List(Char) = toCharsFrom(str, 0);

    function toCharsFrom(str:s, byteOff:Int) : List(Char) =
        toCharsStep(strDecodeAt(str, byteOff), str);

    function toCharsStep(r:Maybe(Pair(Char, Int)), str:s) : List(Char) = match
        | Nothing, str -> Nil
        | Just(Pair(ch, nextOff)), str -> Cons(ch, toCharsFrom(str, nextOff));

    function strStartsWith(str:s, prefix:s) : Bool =
        let pLen = byteLength(prefix) in
        if pLen > byteLength(str) then False
        else strSlice(str, 0, pLen) == prefix;

    function strEndsWith(str:s, suffix:s) : Bool =
        let sLen = byteLength(suffix) in
        let tLen = byteLength(str) in
        if sLen > tLen then False
        else strSlice(str, tLen - sLen, tLen) == suffix;

    function strTrim(str:s) : s =
        let startOff = trimLeading(str, 0) in
        let endOff = trimTrailing(str, byteLength(str)) in
        if startOff >= endOff then strSlice(str, 0, 0)
        else strSlice(str, startOff, endOff);

    function trimLeading(str:s, byteOff:Int) : Int =
        trimLeadingStep(strDecodeAt(str, byteOff), byteOff, str);

    function trimLeadingStep(r:Maybe(Pair(Char, Int)), byteOff:Int, str:s) : Int = match
        | Nothing, byteOff, str -> byteOff
        | Just(Pair(ch, nextOff)), byteOff, str ->
            if isWhitespace(ch) then trimLeading(str, nextOff)
            else byteOff;

    function trimTrailing(str:s, endOff:Int) : Int =
        if endOff <= 0 then 0
        else
            let lastCharStart = endOff - 1 in
            trimTrailingStep(strDecodeAt(str, lastCharStart), lastCharStart, endOff, str);

    function trimTrailingStep(r:Maybe(Pair(Char, Int)), lastCharStart:Int, endOff:Int, str:s) : Int = match
        | Nothing, lastCharStart, endOff, str -> endOff
        | Just(Pair(ch, _)), lastCharStart, endOff, str ->
            if isWhitespace(ch) then trimTrailing(str, lastCharStart)
            else endOff;

    function strToUpper(str:s) : s = mapChars(str, charToUpper, 0);
    function strToLower(str:s) : s = mapChars(str, charToLower, 0);

    function mapChars(str:s, f:Char -> Char, byteOff:Int) : s =
        mapCharsStep(strDecodeAt(str, byteOff), str, f, byteOff);

    function mapCharsStep(r:Maybe(Pair(Char, Int)), str:s, f:Char -> Char, byteOff:Int) : s = match
        | Nothing, str, f, byteOff -> strSlice(str, 0, 0)
        | Just(Pair(ch, nextOff)), str, f, byteOff ->
            let mapped = strFromChar(f(ch)) in
            let rest = mapChars(str, f, nextOff) in
            combine(mapped, rest);

    function strReplace(str:s, old:s, new:s) : s =
        let oldLen = byteLength(old) in
        if oldLen == 0 then str
        else strReplaceFrom(str, old, new, oldLen, 0);

    function strReplaceFrom(str:s, old:s, new:s, oldLen:Int, start:Int) : s =
        let remaining = strSlice(str, start, byteLength(str)) in
        let pos = strIndexOf(remaining, old) in
        if pos < 0 then remaining
        else
            let before = strSlice(remaining, 0, pos) in
            let after = strReplaceFrom(str, old, new, oldLen, start + pos + oldLen) in
            combine(combine(before, new), after);

    function strSplit(str:s, delim:s) : List(s) =
        let dLen = byteLength(delim) in
        if dLen == 0 then Cons(str, Nil)
        else strSplitFrom(str, delim, dLen, 0);

    function strSplitFrom(str:s, delim:s, dLen:Int, start:Int) : List(s) =
        let remaining = strSlice(str, start, byteLength(str)) in
        let pos = strIndexOf(remaining, delim) in
        if pos < 0 then Cons(remaining, Nil)
        else
            let before = strSlice(remaining, 0, pos) in
            let rest = strSplitFrom(str, delim, dLen, start + pos + dLen) in
            Cons(before, rest);

    function strJoin(parts:List(s), sep:s) : s = match
        | Nil, sep -> strSlice(sep, 0, 0)
        | Cons(h, Nil), sep -> h
        | Cons(h, t), sep -> combine(combine(h, sep), strJoin(t, sep))
};
