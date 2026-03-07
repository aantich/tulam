/// StringLike instance for Str.
///
/// Pure tulam implementation of all StringLike operations for the Str type.
///
/// @see Algebra.StringLike, String.Str, String.Utf8
module String.Instances;
import String.Str;
import String.Utf8;
import Algebra.StringLike;
import Core.Maybe;
import Core.Pair;
import Algebra.Enum;

/// Helper: search for needle bytes in haystack starting at byte offset.
function findBytes(haystack:Array(Byte), hLen:Int, needle:Array(Byte), nLen:Int, i:Int) : Int =
    if i + nLen > hLen then 0 - 1
    else if matchBytesAt(haystack, needle, i, 0, nLen) then i
    else findBytes(haystack, hLen, needle, nLen, i + 1);

/// Helper: check if needle matches at position pos in haystack.
function matchBytesAt(haystack:Array(Byte), needle:Array(Byte), pos:Int, j:Int, nLen:Int) : Bool =
    if j >= nLen then True
    else if index(haystack, pos + j) != index(needle, j) then False
    else matchBytesAt(haystack, needle, pos, j + 1, nLen);

/// Helper: convert a decoded Int pair to a Char pair.
function decodeIntToChar(result:Maybe(Pair(Int, Int))) : Maybe(Pair(Char, Int)) = match
    | Nothing -> Nothing
    | Just(Pair(cp, next)) -> Just(Pair(toEnum(cp), next));

/// StringLike instance for Str — pure tulam byte operations.
instance StringLike(Str) = {
    function byteLength(s:Str) : Int = s.byteLen,

    function strSlice(s:Str, start:Int, end:Int) : Str =
        let sliced = slice(s.bytes, start, end) in
        Str(sliced, end - start),

    function strDecodeAt(s:Str, byteOffset:Int) : Maybe(Pair(Char, Int)) =
        decodeIntToChar(decodeAt(s.bytes, byteOffset)),

    function strIndexOf(haystack:Str, needle:Str) : Int =
        if needle.byteLen == 0 then 0
        else findBytes(haystack.bytes, haystack.byteLen, needle.bytes, needle.byteLen, 0),

    function strFromChar(c:Char) : Str =
        let bs = encodeCodePoint(fromEnum(c)) in
        Str(bs, length(bs))
};
