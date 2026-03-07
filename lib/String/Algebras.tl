/// Standard algebra instances for the Str type.
///
/// Provides Eq, Ord, Semigroup, Monoid, and Show for Str using
/// byte-level array comparison and concatenation.
///
/// @see String.Str, Algebra.Eq, Algebra.Ord, Algebra.Semigroup
module String.Algebras;
import String.Str;
import String.Utf8;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Semigroup;
import Algebra.Show;
import Core.Bool;
import Core.Pair;
import Core.Ordering;
import Morphism.Convertible;

/// Helper: compare byte arrays element-by-element for equality.
function eqBytes(a:Array(Byte), b:Array(Byte), i:Int, len:Int) : Bool =
    if i >= len then True
    else if index(a, i) != index(b, i) then False
    else eqBytes(a, b, i + 1, len);

/// Helper: lexicographic byte array comparison.
function compareBytes(a:Array(Byte), b:Array(Byte), i:Int, minLen:Int) : Ordering =
    if i >= minLen then compare(length(a), length(b))
    else
        let ba = byteToInt(index(a, i)) in
        let bb = byteToInt(index(b, i)) in
        if ba < bb then LessThan
        else if ba > bb then GreaterThan
        else compareBytes(a, b, i + 1, minLen);

/// Helper: minimum of two Ints.
function minInt(a:Int, b:Int) : Int =
    if a <= b then a else b;

/// Str equality: byte-by-byte comparison.
instance Eq(Str) = {
    function (==)(a:Str, b:Str) : Bool =
        if a.byteLen != b.byteLen then False
        else eqBytes(a.bytes, b.bytes, 0, a.byteLen),
    function (!=)(a:Str, b:Str) : Bool =
        if a.byteLen != b.byteLen then True
        else not(eqBytes(a.bytes, b.bytes, 0, a.byteLen))
};

/// Str ordering: lexicographic byte comparison.
instance Ord(Str) = {
    function compare(a:Str, b:Str) : Ordering =
        compareBytes(a.bytes, b.bytes, 0, minInt(a.byteLen, b.byteLen)),
    function (<)(a:Str, b:Str) : Bool = match compare(a, b)
        | LessThan -> True
        | _ -> False,
    function (>)(a:Str, b:Str) : Bool = match compare(a, b)
        | GreaterThan -> True
        | _ -> False,
    function (<=)(a:Str, b:Str) : Bool = match compare(a, b)
        | GreaterThan -> False
        | _ -> True,
    function (>=)(a:Str, b:Str) : Bool = match compare(a, b)
        | LessThan -> False
        | _ -> True
};

/// Str semigroup: byte array concatenation.
instance Semigroup(Str) = {
    function combine(a:Str, b:Str) : Str =
        Str(arrayConcat(a.bytes, b.bytes), a.byteLen + b.byteLen)
};

/// Str monoid: empty string identity.
instance Monoid(Str) = {
    function combine(a:Str, b:Str) : Str =
        Str(arrayConcat(a.bytes, b.bytes), a.byteLen + b.byteLen),
    value empty : Str = Str([], 0)
};

/// Helper: decode all bytes to build a display string.
function strToDisplayString(bytes:Array(Byte), i:Int, acc:String) : String =
    if i >= length(bytes) then acc
    else strToDisplayStep(decodeAt(bytes, i), bytes, i, acc);

/// Helper step: process one decoded code point.
function strToDisplayStep(result:Maybe(Pair(Int, Int)), bytes:Array(Byte), i:Int, acc:String) : String = match
    | Nothing, bytes, i, acc -> concat(acc, "?")
    | Just(Pair(cp, next)), bytes, i, acc ->
        let ch = toEnum(cp) in
        strToDisplayString(bytes, next, concat(acc, show(ch)));

/// Str show: decode bytes and display.
instance Show(Str) = {
    function show(s:Str) : String = strToDisplayString(s.bytes, 0, "")
};
