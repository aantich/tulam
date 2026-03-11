// P25: Pure Tulam String Library Tests

// === Helper functions for pattern matching ===

// Extract code point from decode result (2-step pattern match)
function extractCpInner(p:Pair(Int, Int)) : Int = match
    | Pair(cp, next) -> cp;
function extractCp(r:Maybe(Pair(Int, Int))) : Int = match
    | Just(p) -> extractCpInner(p)
    | Nothing -> 0 - 1;

// Extract char from decode result (2-step pattern match)
function extractCharInner(p:Pair(Char, Int)) : Int = match
    | Pair(ch, next) -> fromEnum(ch);
function extractChar(r:Maybe(Pair(Char, Int))) : Int = match
    | Just(p) -> extractCharInner(p)
    | Nothing -> 0 - 1;

// Extract char from Maybe
function extractMaybeChar(r:Maybe(Char)) : Int = match
    | Just(ch) -> fromEnum(ch)
    | Nothing -> 0 - 1;

// === UTF-8 Codec Tests ===

// T1: Encode ASCII code point (1 byte)
function t1() : Int = size(encodeCodePoint(65));

// T2: Encode 2-byte code point (e.g., U+00E9 = e-acute)
function t2() : Int = size(encodeCodePoint(0xE9));

// T3: Encode 3-byte code point (e.g., U+4E16 = CJK char)
function t3() : Int = size(encodeCodePoint(0x4E16));

// T4: Encode 4-byte code point (e.g., U+1F600 = grinning face emoji)
function t4() : Int = size(encodeCodePoint(0x1F600));

// T5: Decode ASCII byte — should return 65
function t5() : Int = extractCp(decodeAt([intToByte(0x41)], 0));

// T6: Count code points in pure ASCII
function t6() : Int =
    countCodePoints([intToByte(0x48), intToByte(0x69)]);

// T7: Validate valid UTF-8 (ASCII)
function t7() : Bool =
    isValidUtf8([intToByte(0x48), intToByte(0x65), intToByte(0x6C), intToByte(0x6C), intToByte(0x6F)]);

// T8: Validate invalid UTF-8 (lone continuation byte)
function t8() : Bool = isValidUtf8([intToByte(0x80)]);

// === Str Type Tests ===

// T9: Create Str and check byte length
function t9() : Int =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C), intToByte(0x6C), intToByte(0x6F)], 5) in
    s.byteLen;

// T10: Empty string check
function t10() : Bool = strIsEmpty(strEmpty());

// T11: Non-empty string check
function t11() : Bool =
    strIsEmpty(Str([intToByte(0x41)], 1));

// === Str Algebra Tests ===

// T12: Str equality (same strings)
function t12() : Bool =
    let s1 = Str([intToByte(0x48), intToByte(0x69)], 2) in
    let s2 = Str([intToByte(0x48), intToByte(0x69)], 2) in
    s1 == s2;

// T13: Str inequality (different strings)
function t13() : Bool =
    let s1 = Str([intToByte(0x48), intToByte(0x69)], 2) in
    let s2 = Str([intToByte(0x48), intToByte(0x6F)], 2) in
    s1 != s2;

// T14: Str ordering (less than)
function t14() : Ordering =
    let s1 = Str([intToByte(0x41)], 1) in
    let s2 = Str([intToByte(0x42)], 1) in
    compare(s1, s2);

// T15: Str concatenation via combine (Semigroup)
function t15() : Int =
    let s1 = Str([intToByte(0x48), intToByte(0x69)], 2) in
    let s2 = Str([intToByte(0x21)], 1) in
    let s3 = combine(s1, s2) in
    s3.byteLen;

// === StringLike Tests ===

// T16: byteLength
function t16() : Int =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C), intToByte(0x6C), intToByte(0x6F)], 5) in
    byteLength(s);

// T17: strSlice
function t17() : Bool =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C), intToByte(0x6C), intToByte(0x6F)], 5) in
    let sliced = strSlice(s, 0, 2) in
    sliced == Str([intToByte(0x48), intToByte(0x65)], 2);

// T18: strDecodeAt returns correct char (A = 65)
function t18() : Int =
    let s = Str([intToByte(0x41)], 1) in
    extractChar(strDecodeAt(s, 0));

// T19: strIndexOf found
function t19() : Int =
    let hay = Str([intToByte(0x41), intToByte(0x42), intToByte(0x43)], 3) in
    let needle = Str([intToByte(0x42), intToByte(0x43)], 2) in
    strIndexOf(hay, needle);

// T20: strIndexOf not found
function t20() : Int =
    let hay = Str([intToByte(0x41), intToByte(0x42)], 2) in
    let needle = Str([intToByte(0x44)], 1) in
    strIndexOf(hay, needle);

// === High-Level String Operations ===

// T21: charCount on ASCII string
function t21() : Int =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C), intToByte(0x6C), intToByte(0x6F)], 5) in
    charCount(s);

// T22: nthChar gets second char (index 1, should be 'B' = 66)
function t22() : Int =
    let s = Str([intToByte(0x41), intToByte(0x42), intToByte(0x43)], 3) in
    extractMaybeChar(nthChar(s, 1));

// T23: strStartsWith true
function t23() : Bool =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C)], 3) in
    let p = Str([intToByte(0x48), intToByte(0x65)], 2) in
    strStartsWith(s, p);

// T24: strStartsWith false
function t24() : Bool =
    let s = Str([intToByte(0x48), intToByte(0x65)], 2) in
    let p = Str([intToByte(0x65), intToByte(0x48)], 2) in
    strStartsWith(s, p);

// T25: strEndsWith true
function t25() : Bool =
    let s = Str([intToByte(0x48), intToByte(0x65), intToByte(0x6C)], 3) in
    let suf = Str([intToByte(0x65), intToByte(0x6C)], 2) in
    strEndsWith(s, suf);

// T26: strSplit count — "A,B,C" split by "," gives 3 parts
function t26len(parts:List(Str)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> 1 + t26len(t);
function t26() : Int =
    let s = Str([intToByte(0x41), intToByte(0x2C), intToByte(0x42), intToByte(0x2C), intToByte(0x43)], 5) in
    let delim = Str([intToByte(0x2C)], 1) in
    t26len(strSplit(s, delim));

// T27: strJoin "A" and "B" with "-"
function t27() : Bool =
    let a = Str([intToByte(0x41)], 1) in
    let b = Str([intToByte(0x42)], 1) in
    let sep = Str([intToByte(0x2D)], 1) in
    let joined = strJoin(Cons(a, Cons(b, Nil)), sep) in
    joined == Str([intToByte(0x41), intToByte(0x2D), intToByte(0x42)], 3);

// T28: strReplace
function t28() : Bool =
    let s = Str([intToByte(0x41), intToByte(0x42), intToByte(0x43)], 3) in
    let old = Str([intToByte(0x42)], 1) in
    let new = Str([intToByte(0x58)], 1) in
    strReplace(s, old, new) == Str([intToByte(0x41), intToByte(0x58), intToByte(0x43)], 3);
