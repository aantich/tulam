/// All instance declarations: reflection, core types, numerics, conversions.
///
/// This is the bridge module that connects Core types with Algebra definitions.
/// It also contains all numeric intrinsic instances and type conversions.
module Instances;
import Core;
import Algebra;
import Morphism;

// ============================================================================
// Reflection intrinsics
// ============================================================================

/// Constructor tag index (0-based). Primitives return 0.
function tag(x:a) : Int = intrinsic;

/// Constructor name as a string. Primitives return their type name.
function tagName(x:a) : String = intrinsic;

/// Number of fields in the constructor application. Primitives return 0.
function arity(x:a) : Int = intrinsic;

/// Get the i-th field of a constructor value (0-indexed).
function field(x:a, i:Int) : b = intrinsic;

/// Number of constructors in a sum type.
function numConstructors(typeName:String) : Int = intrinsic;

/// Create a nullary constructor value by type name and tag index.
function constructorByIndex(typeName:String, i:Int) : a = intrinsic;

// --- Structural helpers (used by derive blocks) ---

/// Structural equality: compare constructor tags, then all fields recursively.
function structuralEq(x:a, y:a) : Bool =
    if tag(x) != tag(y) then False
    else eqFields(x, y, 0, arity(x));

function eqFields(x:a, y:a, i:Int, n:Int) : Bool =
    if i >= n then True
    else if field(x, i) == field(y, i) then eqFields(x, y, i + 1, n)
    else False;

/// Structural ordering: compare constructor tags first, then fields lexicographically.
function structuralCompare(x:a, y:a) : Ordering =
    if tag(x) < tag(y) then LessThan
    else if tag(x) > tag(y) then GreaterThan
    else compareFields(x, y, 0, arity(x));

function compareFields(x:a, y:a, i:Int, n:Int) : Ordering =
    if i >= n then Equal
    else match compare(field(x, i), field(y, i))
         | Equal -> compareFields(x, y, i + 1, n)
         | result -> result;

/// Structural show: constructor name followed by fields in parentheses.
function structuralShow(x:a) : String =
    if arity(x) == 0 then tagName(x)
    else concat(tagName(x), concat("(", concat(showFields(x, 0, arity(x)), ")")));

function showFields(x:a, i:Int, n:Int) : String =
    if i >= n - 1 then show(field(x, i))
    else concat(show(field(x, i)), concat(", ", showFields(x, i + 1, n)));

/// Structural hash: combine constructor tag with field hashes.
function structuralHash(x:a) : Int =
    hashFields(x, tag(x) * 31, 0, arity(x));

function hashFields(x:a, acc:Int, i:Int, n:Int) : Int =
    if i >= n then acc
    else hashFields(x, acc * 31 + hash(field(x, i)), i + 1, n);

// ============================================================================
// Core type instances
// ============================================================================

// --- Nat ---

instance Eq(Nat) = {
    function (==) (x:Nat,y:Nat) : Bool = eq(x,y)
};

instance Ord(Nat) = {
    function compare(x:Nat, y:Nat) : Ordering = match
        | Z, Z -> Equal
        | Z, n -> LessThan
        | n, Z -> GreaterThan
        | Succ(m), Succ(n) -> compare(m, n);
    function (<)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> True
        | n, Z -> False
        | Succ(m), Succ(n) -> m < n;
    function (>)(x:Nat, y:Nat) : Bool = match
        | Z, Z -> False
        | Z, n -> False
        | n, Z -> True
        | Succ(m), Succ(n) -> m > n;
    function (<=)(x:Nat, y:Nat) : Bool = not(x > y);
    function (>=)(x:Nat, y:Nat) : Bool = not(x < y)
};

instance Monoid(Nat) = {
    function combine(x:Nat, y:Nat) : Nat = plus(x, y);
    value empty = Z
};

instance Semiring(Nat) = {
    function (+)(x:Nat, y:Nat) : Nat = plus(x, y);
    function (*)(x:Nat, y:Nat) : Nat = mult(x, y);
    value zero = Z;
    value one = Succ(Z)
};

// --- Bool ---

instance Eq(Bool) = {
    function (==) (x:Bool,y:Bool) : Bool = match
        | True, True -> True
        | False, False -> True
        | True, False -> False
        | False, True -> False
};

// --- List ---

instance Semigroup(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys))
};

instance Monoid(List(a)) = {
    function combine(xs:List(a), ys:List(a)) : List(a) = match
        | Nil, ys -> ys
        | Cons(h, t), ys -> Cons(h, combine(t, ys));
    value empty = Nil
};

// --- Ordering ---

instance Eq(Ordering) = {
    function (==)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, LessThan -> True
        | Equal, Equal -> True
        | GreaterThan, GreaterThan -> True
        | x, y -> False
};

instance Ord(Ordering) = {
    function compare(x:Ordering, y:Ordering) : Ordering = match
        | LessThan, LessThan -> Equal
        | LessThan, n -> LessThan
        | Equal, LessThan -> GreaterThan
        | Equal, Equal -> Equal
        | Equal, GreaterThan -> LessThan
        | GreaterThan, GreaterThan -> Equal
        | GreaterThan, n -> GreaterThan;
    function (<)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, Equal -> True
        | LessThan, GreaterThan -> True
        | Equal, GreaterThan -> True
        | x, y -> False;
    function (>)(x:Ordering, y:Ordering) : Bool = match
        | Equal, LessThan -> True
        | GreaterThan, LessThan -> True
        | GreaterThan, Equal -> True
        | x, y -> False;
    function (<=)(x:Ordering, y:Ordering) : Bool = not(x > y);
    function (>=)(x:Ordering, y:Ordering) : Bool = not(x < y)
};

// --- Convertible instances for core types ---

instance Convertible(Nat, Bool) = {
    function convert(x:Nat) : Bool = match
        | Z -> False
        | n -> True
};

instance Convertible(Bool, Nat) = {
    function convert(x:Bool) : Nat = match
        | True -> Succ(Z)
        | False -> Z
};

instance Convertible(Nat, Int) = {
    function convert(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + convert(m)
};

instance Convertible(Int, Nat) = {
    function convert(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(convert(n - 1))
};

// --- Utility functions ---

function describeInt(x:Int) : Nat = match
    | 0 -> Z
    | 1 -> Succ(Z)
    | n -> Succ(Succ(Z));

function doubleNat(x:Nat) : Nat = plus(twice, twice) where {
    twice = plus(x, x)
};

// ============================================================================
// Numeric instances — Int
// ============================================================================

instance EuclideanDomain(Int) = intrinsic;
instance Absolute(Int) = intrinsic;
instance Bits(Int) = intrinsic;
instance Eq(Int) = intrinsic;
instance Ord(Int) = intrinsic;

instance Group(Int) = {
    function combine(x:Int, y:Int) : Int = x + y;
    value empty = 0;
    function inverse(x:Int) : Int = negate(x)
};

// ============================================================================
// Numeric instances — Float64 / Float32
// ============================================================================

instance Floating(Float64) = intrinsic;
instance Absolute(Float64) = intrinsic;
instance Eq(Float64) = intrinsic;
instance Ord(Float64) = intrinsic;

instance AbelianGroup(Float64) = {
    function combine(x:Float64, y:Float64) : Float64 = x + y;
    value empty = 0.0;
    function inverse(x:Float64) : Float64 = negate(x)
};

instance Floating(Float32) = intrinsic;
instance Absolute(Float32) = intrinsic;
instance Eq(Float32) = intrinsic;
instance Ord(Float32) = intrinsic;

// ============================================================================
// Numeric instances — UInt, UInt8-64, Byte
// ============================================================================

instance EuclideanDomain(UInt) = intrinsic;
instance Absolute(UInt) = intrinsic;
instance Bits(UInt) = intrinsic;
instance Eq(UInt) = intrinsic;
instance Ord(UInt) = intrinsic;

instance EuclideanDomain(UInt8) = intrinsic;
instance Bits(UInt8) = intrinsic;
instance Eq(UInt8) = intrinsic;
instance Ord(UInt8) = intrinsic;

instance EuclideanDomain(UInt16) = intrinsic;
instance Bits(UInt16) = intrinsic;
instance Eq(UInt16) = intrinsic;
instance Ord(UInt16) = intrinsic;

instance EuclideanDomain(UInt32) = intrinsic;
instance Bits(UInt32) = intrinsic;
instance Eq(UInt32) = intrinsic;
instance Ord(UInt32) = intrinsic;

instance EuclideanDomain(UInt64) = intrinsic;
instance Bits(UInt64) = intrinsic;
instance Eq(UInt64) = intrinsic;
instance Ord(UInt64) = intrinsic;

instance EuclideanDomain(Byte) = intrinsic;
instance Bits(Byte) = intrinsic;
instance Eq(Byte) = intrinsic;
instance Ord(Byte) = intrinsic;

instance Convertible(Int, Byte) = intrinsic;
instance Convertible(Byte, Int) = intrinsic;

// ============================================================================
// Numeric instances — Int8, Int16, Int32, Int64
// ============================================================================

instance EuclideanDomain(Int8) = intrinsic;
instance Absolute(Int8) = intrinsic;
instance Bits(Int8) = intrinsic;
instance Eq(Int8) = intrinsic;
instance Ord(Int8) = intrinsic;

instance EuclideanDomain(Int16) = intrinsic;
instance Absolute(Int16) = intrinsic;
instance Bits(Int16) = intrinsic;
instance Eq(Int16) = intrinsic;
instance Ord(Int16) = intrinsic;

instance EuclideanDomain(Int32) = intrinsic;
instance Absolute(Int32) = intrinsic;
instance Bits(Int32) = intrinsic;
instance Eq(Int32) = intrinsic;
instance Ord(Int32) = intrinsic;

instance EuclideanDomain(Int64) = intrinsic;
instance Absolute(Int64) = intrinsic;
instance Bits(Int64) = intrinsic;
instance Eq(Int64) = intrinsic;
instance Ord(Int64) = intrinsic;

// ============================================================================
// Numeric instances — Char, String
// ============================================================================

instance Eq(Char) = intrinsic;
instance Ord(Char) = intrinsic;

instance StringOps(String) = intrinsic;
instance StringExt(String) = intrinsic;
instance Eq(String) = intrinsic;
instance Ord(String) = intrinsic;

// ============================================================================
// Numeric conversions
// ============================================================================

// Widening (safe)
instance Convertible(Int8, Int16) = intrinsic;
instance Convertible(Int16, Int32) = intrinsic;
instance Convertible(Int32, Int64) = intrinsic;
instance Convertible(Int32, Int) = intrinsic;
instance Convertible(UInt8, UInt16) = intrinsic;
instance Convertible(UInt16, UInt32) = intrinsic;
instance Convertible(UInt32, UInt64) = intrinsic;
instance Convertible(Float32, Float64) = intrinsic;
instance Convertible(Int, Float64) = intrinsic;

// Narrowing (may truncate)
instance Convertible(Int, Int32) = intrinsic;
instance Convertible(Int32, Int16) = intrinsic;
instance Convertible(Int16, Int8) = intrinsic;
instance Convertible(Float64, Float32) = intrinsic;
instance Convertible(Int, UInt) = intrinsic;
instance Convertible(UInt, Int) = intrinsic;

// ============================================================================
// Show instances
// ============================================================================

// Primitive types
instance Show(Int) = intrinsic;
instance Show(Float64) = intrinsic;
instance Show(String) = intrinsic;
instance Show(Char) = intrinsic;
instance Show(Int8) = intrinsic;
instance Show(Int16) = intrinsic;
instance Show(Int32) = intrinsic;
instance Show(Int64) = intrinsic;
instance Show(UInt8) = intrinsic;
instance Show(UInt16) = intrinsic;
instance Show(UInt32) = intrinsic;
instance Show(UInt64) = intrinsic;
instance Show(Float32) = intrinsic;
instance Show(Byte) = intrinsic;

// Algebraic types
instance Show(Bool) = {
    function show(x:Bool) : String = match
        | True -> "True"
        | False -> "False"
};

instance Show(Ordering) = {
    function show(x:Ordering) : String = match
        | LessThan -> "LessThan"
        | Equal -> "Equal"
        | GreaterThan -> "GreaterThan"
};

instance Show(Unit) = {
    function show(x:Unit) : String = "Unit"
};

instance Show(Nat) = {
    function show(x:Nat) : String = match
        | Z -> "Z"
        | Succ(n) -> concat("Succ(", concat(show(n), ")"))
};

instance Show(Maybe(a)) = {
    function show(x:Maybe(a)) : String = match
        | Nothing -> "Nothing"
        | Just(v) -> concat("Just(", concat(show(v), ")"))
};

instance Show(Either(a, b)) = {
    function show(x:Either(a, b)) : String = match
        | Left(v) -> concat("Left(", concat(show(v), ")"))
        | Right(v) -> concat("Right(", concat(show(v), ")"))
};

instance Show(List(a)) = {
    function show(x:List(a)) : String = match
        | Nil -> "Nil"
        | Cons(h, t) -> concat("Cons(", concat(show(h), concat(", ", concat(show(t), ")"))))
};

instance Show(Pair(a, b)) = {
    function show(x:Pair(a, b)) : String = match
        | Pair(l, r) -> concat("Pair(", concat(show(l), concat(", ", concat(show(r), ")"))))
};

instance Show(NonEmpty(a)) = {
    function show(x:NonEmpty(a)) : String = match
        | NonEmpty(h, t) -> concat("NonEmpty(", concat(show(h), concat(", ", concat(show(t), ")"))))
};

instance Show(Tree(a)) = {
    function show(x:Tree(a)) : String = match
        | Leaf(v) -> concat("Leaf(", concat(show(v), ")"))
        | Branch(l, r) -> concat("Branch(", concat(show(l), concat(", ", concat(show(r), ")"))))
};

// ============================================================================
// Bounded instances
// ============================================================================

instance Bounded(Int8) = intrinsic;
instance Bounded(Int16) = intrinsic;
instance Bounded(Int32) = intrinsic;
instance Bounded(Int64) = intrinsic;
instance Bounded(UInt8) = intrinsic;
instance Bounded(UInt16) = intrinsic;
instance Bounded(UInt32) = intrinsic;
instance Bounded(UInt64) = intrinsic;
instance Bounded(Char) = intrinsic;
instance Bounded(Byte) = intrinsic;

instance Bounded(Bool) = {
    value minBound = False;
    value maxBound = True
};

instance Bounded(Ordering) = {
    value minBound = LessThan;
    value maxBound = GreaterThan
};

instance Bounded(Unit) = {
    value minBound = Unit;
    value maxBound = Unit
};

// ============================================================================
// Enum instances
// ============================================================================

instance Enum(Int) = intrinsic;
instance Enum(Char) = intrinsic;

instance Enum(Bool) = {
    function succ(x:Bool) : Bool = match
        | False -> True
        | True -> True;
    function pred(x:Bool) : Bool = match
        | True -> False
        | False -> False;
    function toEnum(i:Int) : Bool = match
        | 0 -> False
        | n -> True;
    function fromEnum(x:Bool) : Int = match
        | False -> 0
        | True -> 1
};

instance Enum(Ordering) = {
    function succ(x:Ordering) : Ordering = match
        | LessThan -> Equal
        | Equal -> GreaterThan
        | GreaterThan -> GreaterThan;
    function pred(x:Ordering) : Ordering = match
        | GreaterThan -> Equal
        | Equal -> LessThan
        | LessThan -> LessThan;
    function toEnum(i:Int) : Ordering = match
        | 0 -> LessThan
        | 1 -> Equal
        | n -> GreaterThan;
    function fromEnum(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};

instance Enum(Nat) = {
    function succ(x:Nat) : Nat = Succ(x);
    function pred(x:Nat) : Nat = match
        | Z -> Z
        | Succ(n) -> n;
    function toEnum(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(toEnum(n - 1));
    function fromEnum(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + fromEnum(n)
};

// ============================================================================
// Hashable instances
// ============================================================================

instance Hashable(Int) = intrinsic;
instance Hashable(String) = intrinsic;
instance Hashable(Char) = intrinsic;

instance Hashable(Bool) = {
    function hash(x:Bool) : Int = match
        | True -> 1
        | False -> 0
};

instance Hashable(Unit) = {
    function hash(x:Unit) : Int = 0
};

instance Hashable(Ordering) = {
    function hash(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};

instance Hashable(Nat) = {
    function hash(x:Nat) : Int = match
        | Z -> 0
        | Succ(n) -> 1 + hash(n)
};

// ============================================================================
// Lattice instances (Bool)
// ============================================================================

instance JoinSemiLattice(Bool) = {
    function (\/)(x:Bool, y:Bool) : Bool = match
        | False, False -> False
        | x, y -> True
};

instance MeetSemiLattice(Bool) = {
    function (/\)(x:Bool, y:Bool) : Bool = match
        | True, True -> True
        | x, y -> False
};

instance Lattice(Bool) = {};

instance BoundedLattice(Bool) = {
    value bottom = False;
    value top = True
};

// ============================================================================
// Eq/Ord — Unit, Pair
// ============================================================================

instance Eq(Unit) = {
    function (==)(x:Unit, y:Unit) : Bool = True;
    function (!=)(x:Unit, y:Unit) : Bool = False
};

instance Ord(Unit) = {
    function compare(x:Unit, y:Unit) : Ordering = Equal;
    function (<)(x:Unit, y:Unit) : Bool = False;
    function (>)(x:Unit, y:Unit) : Bool = False;
    function (<=)(x:Unit, y:Unit) : Bool = True;
    function (>=)(x:Unit, y:Unit) : Bool = True
};

instance Eq(Pair(a, b)) = {
    function (==)(x:Pair(a, b), y:Pair(a, b)) : Bool = match
        | Pair(x1, x2), Pair(y1, y2) ->
            if x1 == y1 then x2 == y2 else False
};

// ============================================================================
// Semigroup/Monoid — NonEmpty, Unit
// ============================================================================

instance Semigroup(NonEmpty(a)) = {
    function combine(xs:NonEmpty(a), ys:NonEmpty(a)) : NonEmpty(a) = match
        | NonEmpty(h, Nil), ys -> NonEmpty(h, toList(ys))
        | NonEmpty(h, Cons(t, rest)), ys -> NonEmpty(h, Cons(t, combine(rest, toList(ys))))
};

instance Semigroup(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit
};

instance Monoid(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit;
    value empty = Unit
};
