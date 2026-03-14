/// Numeric type instances: Int, Float64, Float32, UInt, Int8-64, UInt8-64, Byte.
///
/// Named instances (as Additive / as Multiplicative) distinguish the two
/// monoidal structures on numeric types.
module Instances.Numeric;
import Core;
import Algebra;

// ============================================================================
// Int
// ============================================================================

instance Semigroup(Int) as Additive = intrinsic;
instance Monoid(Int) as Additive = intrinsic;
instance Group(Int) as Additive = intrinsic;
instance AbelianGroup(Int) as Additive = intrinsic;
instance Semigroup(Int) as Multiplicative = intrinsic;
instance Monoid(Int) as Multiplicative = intrinsic;
instance EuclideanDomain(Int) = intrinsic;
instance Absolute(Int) = intrinsic;
instance Bits(Int) = intrinsic;
instance Eq(Int) = intrinsic;
instance Ord(Int) = intrinsic;
instance Show(Int) = intrinsic;
instance Enum(Int) = intrinsic;
instance Hashable(Int) = intrinsic;

// ============================================================================
// Float64
// ============================================================================

instance Semigroup(Float64) as Additive = intrinsic;
instance Monoid(Float64) as Additive = intrinsic;
instance Group(Float64) as Additive = intrinsic;
instance AbelianGroup(Float64) as Additive = intrinsic;
instance Semigroup(Float64) as Multiplicative = intrinsic;
instance Monoid(Float64) as Multiplicative = intrinsic;
instance Group(Float64) as Multiplicative = intrinsic;
instance Floating(Float64) = intrinsic;
instance Absolute(Float64) = intrinsic;
instance Eq(Float64) = intrinsic;
instance Ord(Float64) = intrinsic;
instance Show(Float64) = intrinsic;

// ============================================================================
// Float32
// ============================================================================

instance Semigroup(Float32) as Additive = intrinsic;
instance Monoid(Float32) as Additive = intrinsic;
instance Group(Float32) as Additive = intrinsic;
instance AbelianGroup(Float32) as Additive = intrinsic;
instance Semigroup(Float32) as Multiplicative = intrinsic;
instance Monoid(Float32) as Multiplicative = intrinsic;
instance Group(Float32) as Multiplicative = intrinsic;
instance Floating(Float32) = intrinsic;
instance Absolute(Float32) = intrinsic;
instance Eq(Float32) = intrinsic;
instance Ord(Float32) = intrinsic;
instance Show(Float32) = intrinsic;

// ============================================================================
// UInt
// ============================================================================

instance Semigroup(UInt) as Additive = intrinsic;
instance Monoid(UInt) as Additive = intrinsic;
instance Semigroup(UInt) as Multiplicative = intrinsic;
instance Monoid(UInt) as Multiplicative = intrinsic;
instance EuclideanDomain(UInt) = intrinsic;
instance Absolute(UInt) = intrinsic;
instance Bits(UInt) = intrinsic;
instance Eq(UInt) = intrinsic;
instance Ord(UInt) = intrinsic;

// ============================================================================
// UInt8-64
// ============================================================================

instance Semigroup(UInt8) as Additive = intrinsic;
instance Monoid(UInt8) as Additive = intrinsic;
instance Semigroup(UInt8) as Multiplicative = intrinsic;
instance Monoid(UInt8) as Multiplicative = intrinsic;
instance EuclideanDomain(UInt8) = intrinsic;
instance Bits(UInt8) = intrinsic;
instance Eq(UInt8) = intrinsic;
instance Ord(UInt8) = intrinsic;

instance Semigroup(UInt16) as Additive = intrinsic;
instance Monoid(UInt16) as Additive = intrinsic;
instance Semigroup(UInt16) as Multiplicative = intrinsic;
instance Monoid(UInt16) as Multiplicative = intrinsic;
instance EuclideanDomain(UInt16) = intrinsic;
instance Bits(UInt16) = intrinsic;
instance Eq(UInt16) = intrinsic;
instance Ord(UInt16) = intrinsic;

instance Semigroup(UInt32) as Additive = intrinsic;
instance Monoid(UInt32) as Additive = intrinsic;
instance Semigroup(UInt32) as Multiplicative = intrinsic;
instance Monoid(UInt32) as Multiplicative = intrinsic;
instance EuclideanDomain(UInt32) = intrinsic;
instance Bits(UInt32) = intrinsic;
instance Eq(UInt32) = intrinsic;
instance Ord(UInt32) = intrinsic;

instance Semigroup(UInt64) as Additive = intrinsic;
instance Monoid(UInt64) as Additive = intrinsic;
instance Semigroup(UInt64) as Multiplicative = intrinsic;
instance Monoid(UInt64) as Multiplicative = intrinsic;
instance EuclideanDomain(UInt64) = intrinsic;
instance Bits(UInt64) = intrinsic;
instance Eq(UInt64) = intrinsic;
instance Ord(UInt64) = intrinsic;

// ============================================================================
// Byte
// ============================================================================

instance Semigroup(Byte) as Additive = intrinsic;
instance Monoid(Byte) as Additive = intrinsic;
instance Semigroup(Byte) as Multiplicative = intrinsic;
instance Monoid(Byte) as Multiplicative = intrinsic;
instance EuclideanDomain(Byte) = intrinsic;
instance Bits(Byte) = intrinsic;
instance Eq(Byte) = intrinsic;
instance Ord(Byte) = intrinsic;
instance Show(Byte) = intrinsic;

// ============================================================================
// Int8-64
// ============================================================================

instance Semigroup(Int8) as Additive = intrinsic;
instance Monoid(Int8) as Additive = intrinsic;
instance Group(Int8) as Additive = intrinsic;
instance Semigroup(Int8) as Multiplicative = intrinsic;
instance Monoid(Int8) as Multiplicative = intrinsic;
instance EuclideanDomain(Int8) = intrinsic;
instance Absolute(Int8) = intrinsic;
instance Bits(Int8) = intrinsic;
instance Eq(Int8) = intrinsic;
instance Ord(Int8) = intrinsic;
instance Show(Int8) = intrinsic;

instance Semigroup(Int16) as Additive = intrinsic;
instance Monoid(Int16) as Additive = intrinsic;
instance Group(Int16) as Additive = intrinsic;
instance Semigroup(Int16) as Multiplicative = intrinsic;
instance Monoid(Int16) as Multiplicative = intrinsic;
instance EuclideanDomain(Int16) = intrinsic;
instance Absolute(Int16) = intrinsic;
instance Bits(Int16) = intrinsic;
instance Eq(Int16) = intrinsic;
instance Ord(Int16) = intrinsic;
instance Show(Int16) = intrinsic;

instance Semigroup(Int32) as Additive = intrinsic;
instance Monoid(Int32) as Additive = intrinsic;
instance Group(Int32) as Additive = intrinsic;
instance Semigroup(Int32) as Multiplicative = intrinsic;
instance Monoid(Int32) as Multiplicative = intrinsic;
instance EuclideanDomain(Int32) = intrinsic;
instance Absolute(Int32) = intrinsic;
instance Bits(Int32) = intrinsic;
instance Eq(Int32) = intrinsic;
instance Ord(Int32) = intrinsic;
instance Show(Int32) = intrinsic;

instance Semigroup(Int64) as Additive = intrinsic;
instance Monoid(Int64) as Additive = intrinsic;
instance Group(Int64) as Additive = intrinsic;
instance Semigroup(Int64) as Multiplicative = intrinsic;
instance Monoid(Int64) as Multiplicative = intrinsic;
instance EuclideanDomain(Int64) = intrinsic;
instance Absolute(Int64) = intrinsic;
instance Bits(Int64) = intrinsic;
instance Eq(Int64) = intrinsic;
instance Ord(Int64) = intrinsic;
instance Show(Int64) = intrinsic;

// ============================================================================
// Bounded
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

// ============================================================================
// Show — remaining numeric types
// ============================================================================

instance Show(UInt8) = intrinsic;
instance Show(UInt16) = intrinsic;
instance Show(UInt32) = intrinsic;
instance Show(UInt64) = intrinsic;

// ============================================================================
// Char
// ============================================================================

instance Eq(Char) = intrinsic;
instance Ord(Char) = intrinsic;
instance Show(Char) = intrinsic;
instance Enum(Char) = intrinsic;
instance Hashable(Char) = intrinsic;

// ============================================================================
// String
// ============================================================================

instance StringOps(String) = intrinsic;
instance StringExt(String) = intrinsic;
instance Eq(String) = intrinsic;
instance Ord(String) = intrinsic;
instance Show(String) = intrinsic;
instance Hashable(String) = intrinsic;

/// String forms a semigroup under concatenation (untagged — only one structure).
instance Semigroup(String) = {
    function combine(x:String, y:String) : String = concat(x, y)
};

/// String forms a monoid with empty string as identity.
instance Monoid(String) = {
    function combine(x:String, y:String) : String = concat(x, y);
    value empty : String = ""
};

/// StringLike instance for the primitive String type.
instance StringLike(String) = {
    function byteLength(s:String) : Int = length(s);
    function strSlice(s:String, start:Int, end:Int) : String = substring(s, start, end);
    function strDecodeAt(s:String, byteOffset:Int) : Maybe(Pair(Char, Int)) =
        if byteOffset >= length(s) then Nothing
        else Just(Pair(charAt(s, byteOffset), byteOffset + 1));
    function strIndexOf(haystack:String, needle:String) : Int = indexOf(haystack, needle);
    function strFromChar(c:Char) : String = fromChar(c);
    function strToUpper(s:String) : String = toUpper(s);
    function strToLower(s:String) : String = toLower(s);
    function strReplace(s:String, old:String, new:String) : String = replace(s, old, new)
};
