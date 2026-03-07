/// Pure tulam UTF-8 encoding and decoding.
///
/// All functions use bitwise operations on Int and Byte values.
/// No magic — just math on bytes following the UTF-8 specification.
///
/// UTF-8 encoding table:
///   U+0000..U+007F    → 1 byte:  0xxxxxxx
///   U+0080..U+07FF    → 2 bytes: 110xxxxx 10xxxxxx
///   U+0800..U+FFFF    → 3 bytes: 1110xxxx 10xxxxxx 10xxxxxx
///   U+10000..U+10FFFF → 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
///
/// @see String.Str, Algebra.StringLike
module String.Utf8;
import Core.Bool;
import Core.Maybe;
import Core.Pair;
import Algebra.Bits;

/// Convert an Int to a Byte (truncating to low 8 bits).
/// @param n The integer value (0-255 expected)
/// @returns The byte value
function intToByte(n:Int) : Byte = intrinsic;

/// Convert a Byte to an Int (widening).
/// @param b The byte value
/// @returns The integer value (0-255)
function byteToInt(b:Byte) : Int = intrinsic;

/// Encode a Unicode code point (as Int) to 1-4 UTF-8 bytes.
/// @param cp The Unicode code point (0 to 0x10FFFF)
/// @returns Array of 1-4 bytes in UTF-8 encoding
function encodeCodePoint(cp:Int) : Array(Byte) =
    if cp < 0x80 then
        [intToByte(cp)]
    else if cp < 0x800 then
        [intToByte(0xC0 .|. shiftR(cp, 6)),
         intToByte(0x80 .|. (cp .&. 0x3F))]
    else if cp < 0x10000 then
        [intToByte(0xE0 .|. shiftR(cp, 12)),
         intToByte(0x80 .|. (shiftR(cp, 6) .&. 0x3F)),
         intToByte(0x80 .|. (cp .&. 0x3F))]
    else
        [intToByte(0xF0 .|. shiftR(cp, 18)),
         intToByte(0x80 .|. (shiftR(cp, 12) .&. 0x3F)),
         intToByte(0x80 .|. (shiftR(cp, 6) .&. 0x3F)),
         intToByte(0x80 .|. (cp .&. 0x3F))];

/// Determine the byte length of a UTF-8 sequence from its lead byte.
/// @param leadByte The first byte of a UTF-8 sequence
/// @returns Number of bytes in the sequence (1-4), or 0 if invalid
function utf8SeqLen(leadByte:Byte) : Int =
    let b = byteToInt(leadByte) in
    if b < 0x80 then 1
    else if b < 0xC0 then 0
    else if b < 0xE0 then 2
    else if b < 0xF0 then 3
    else if b < 0xF8 then 4
    else 0;

/// Check if a byte is a UTF-8 continuation byte (10xxxxxx).
/// @param b The byte to check
/// @returns True if it is a continuation byte
function isContinuation(b:Byte) : Bool =
    let i = byteToInt(b) in
    (i .&. 0xC0) == 0x80;

/// Decode one code point starting at byte offset i.
/// @param bytes The UTF-8 byte array
/// @param i The byte offset to start decoding at
/// @returns Just(Pair(codePoint, nextByteOffset)) or Nothing if invalid
function decodeAt(bytes:Array(Byte), i:Int) : Maybe(Pair(Int, Int)) =
    if i >= length(bytes) then Nothing
    else
        let b0 = byteToInt(index(bytes, i)) in
        let seqLen = utf8SeqLen(intToByte(b0)) in
        if seqLen == 0 then Nothing
        else if seqLen == 1 then Just(Pair(b0, i + 1))
        else if i + seqLen > length(bytes) then Nothing
        else if seqLen == 2 then
            let b1 = byteToInt(index(bytes, i + 1)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x1F, 6) .|. (b1 .&. 0x3F) in
                Just(Pair(cp, i + 2))
        else if seqLen == 3 then
            let b1 = byteToInt(index(bytes, i + 1)) in
            let b2 = byteToInt(index(bytes, i + 2)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else if (b2 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x0F, 12) .|. shiftL(b1 .&. 0x3F, 6) .|. (b2 .&. 0x3F) in
                Just(Pair(cp, i + 3))
        else
            let b1 = byteToInt(index(bytes, i + 1)) in
            let b2 = byteToInt(index(bytes, i + 2)) in
            let b3 = byteToInt(index(bytes, i + 3)) in
            if (b1 .&. 0xC0) != 0x80 then Nothing
            else if (b2 .&. 0xC0) != 0x80 then Nothing
            else if (b3 .&. 0xC0) != 0x80 then Nothing
            else
                let cp = shiftL(b0 .&. 0x07, 18) .|. shiftL(b1 .&. 0x3F, 12) .|. shiftL(b2 .&. 0x3F, 6) .|. (b3 .&. 0x3F) in
                Just(Pair(cp, i + 4));

/// Count the number of code points in a UTF-8 byte array.
/// @param bytes The UTF-8 encoded bytes
/// @returns Number of Unicode code points
function countCodePoints(bytes:Array(Byte)) : Int =
    countCodePointsFrom(bytes, 0, 0);

/// Helper: count code points starting from byte offset.
function countCodePointsFrom(bytes:Array(Byte), i:Int, count:Int) : Int =
    if i >= length(bytes) then count
    else
        let seqLen = utf8SeqLen(index(bytes, i)) in
        if seqLen == 0 then count
        else countCodePointsFrom(bytes, i + seqLen, count + 1);

/// Get the byte offset of the n-th code point (0-indexed).
/// @param bytes The UTF-8 encoded bytes
/// @param n The code point index (0-based)
/// @returns Just(byteOffset) or Nothing if n is out of range
function nthByteOffset(bytes:Array(Byte), n:Int) : Maybe(Int) =
    nthByteOffsetFrom(bytes, 0, n);

/// Helper: find byte offset of the n-th code point.
function nthByteOffsetFrom(bytes:Array(Byte), i:Int, n:Int) : Maybe(Int) =
    if n == 0 then
        if i <= length(bytes) then Just(i) else Nothing
    else if i >= length(bytes) then Nothing
    else
        let seqLen = utf8SeqLen(index(bytes, i)) in
        if seqLen == 0 then Nothing
        else nthByteOffsetFrom(bytes, i + seqLen, n - 1);

/// Validate that a byte array is well-formed UTF-8.
/// @param bytes The byte array to validate
/// @returns True if the bytes form valid UTF-8
function isValidUtf8(bytes:Array(Byte)) : Bool =
    isValidUtf8From(bytes, 0);

/// Helper: validate UTF-8 from a given byte offset.
function isValidUtf8From(bytes:Array(Byte), i:Int) : Bool =
    if i >= length(bytes) then True
    else isValidUtf8Check(decodeAt(bytes, i), bytes);

/// Helper: check decode result for validity.
function isValidUtf8Check(result:Maybe(Pair(Int, Int)), bytes:Array(Byte)) : Bool = match
    | Nothing, bytes -> False
    | Just(Pair(cp, next)), bytes -> isValidUtf8From(bytes, next);
