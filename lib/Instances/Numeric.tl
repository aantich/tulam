/// Numeric type instances: Int, Float64, Float32, UInt, Int8-64, UInt8-64, Byte.
///
/// All primitive numeric types get their algebra instances here.
/// Most are intrinsic (compiler-provided). Some have hand-written
/// implementations (e.g., Group(Int), AbelianGroup(Float64)).
module Instances.Numeric;
import Core;
import Algebra;

// ============================================================================
// Int
// ============================================================================

instance EuclideanDomain(Int) = intrinsic;
instance Absolute(Int) = intrinsic;
instance Bits(Int) = intrinsic;
instance Eq(Int) = intrinsic;
instance Ord(Int) = intrinsic;
instance Show(Int) = intrinsic;
instance Enum(Int) = intrinsic;
instance Hashable(Int) = intrinsic;

instance Group(Int) = {
    function combine(x:Int, y:Int) : Int = x + y;
    value empty = 0;
    function inverse(x:Int) : Int = negate(x)
};

// ============================================================================
// Float64
// ============================================================================

instance Floating(Float64) = intrinsic;
instance Absolute(Float64) = intrinsic;
instance Eq(Float64) = intrinsic;
instance Ord(Float64) = intrinsic;
instance Show(Float64) = intrinsic;

instance AbelianGroup(Float64) = {
    function combine(x:Float64, y:Float64) : Float64 = x + y;
    value empty = 0.0;
    function inverse(x:Float64) : Float64 = negate(x)
};

// ============================================================================
// Float32
// ============================================================================

instance Floating(Float32) = intrinsic;
instance Absolute(Float32) = intrinsic;
instance Eq(Float32) = intrinsic;
instance Ord(Float32) = intrinsic;
instance Show(Float32) = intrinsic;

// ============================================================================
// UInt
// ============================================================================

instance EuclideanDomain(UInt) = intrinsic;
instance Absolute(UInt) = intrinsic;
instance Bits(UInt) = intrinsic;
instance Eq(UInt) = intrinsic;
instance Ord(UInt) = intrinsic;

// ============================================================================
// UInt8-64
// ============================================================================

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

// ============================================================================
// Byte
// ============================================================================

instance EuclideanDomain(Byte) = intrinsic;
instance Bits(Byte) = intrinsic;
instance Eq(Byte) = intrinsic;
instance Ord(Byte) = intrinsic;
instance Show(Byte) = intrinsic;

// ============================================================================
// Int8-64
// ============================================================================

instance EuclideanDomain(Int8) = intrinsic;
instance Absolute(Int8) = intrinsic;
instance Bits(Int8) = intrinsic;
instance Eq(Int8) = intrinsic;
instance Ord(Int8) = intrinsic;
instance Show(Int8) = intrinsic;

instance EuclideanDomain(Int16) = intrinsic;
instance Absolute(Int16) = intrinsic;
instance Bits(Int16) = intrinsic;
instance Eq(Int16) = intrinsic;
instance Ord(Int16) = intrinsic;
instance Show(Int16) = intrinsic;

instance EuclideanDomain(Int32) = intrinsic;
instance Absolute(Int32) = intrinsic;
instance Bits(Int32) = intrinsic;
instance Eq(Int32) = intrinsic;
instance Ord(Int32) = intrinsic;
instance Show(Int32) = intrinsic;

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

/// String forms a semigroup under concatenation.
instance Semigroup(String) = {
    function combine(x:String, y:String) : String = concat(x, y)
};

/// String forms a monoid with empty string as identity.
instance Monoid(String) = {
    function combine(x:String, y:String) : String = concat(x, y);
    value empty : String = ""
};

/// StringLike instance for the primitive String type.
/// Treats String as character-indexed (each char = 1 unit).
instance StringLike(String) = {
    function byteLength(s:String) : Int = length(s);
    function strSlice(s:String, start:Int, end:Int) : String = substring(s, start, end);
    function strDecodeAt(s:String, byteOffset:Int) : Maybe(Pair(Char, Int)) =
        if byteOffset >= length(s) then Nothing
        else Just(Pair(charAt(s, byteOffset), byteOffset + 1));
    function strIndexOf(haystack:String, needle:String) : Int = indexOf(haystack, needle);
    function strFromChar(c:Char) : String = fromChar(c);
    /// Override defaults that use strFromChar internally — use StringExt intrinsics directly.
    function strToUpper(s:String) : String = toUpper(s);
    function strToLower(s:String) : String = toLower(s);
    function strReplace(s:String, old:String, new:String) : String = replace(s, old, new)
};
