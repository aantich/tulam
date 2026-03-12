// P26: String Literal Desugaring Tests
// These tests require :s newstrings on to work.
// String literals "..." are desugared to fromStringLiteral([byte array]).

// T1: Basic string literal creates a Str with correct byte length
function t1() : Int =
    let s = "Hello" in
    s.byteLen;

// T2: Empty string literal
function t2() : Bool = strIsEmpty("");

// T3: String equality via literal
function t3() : Bool =
    let { s1 = "Hi"; s2 = "Hi" } in
    s1 == s2;

// T4: String inequality via literal
function t4() : Bool =
    let { s1 = "Hi"; s2 = "Ho" } in
    s1 != s2;

// T5: String concatenation via combine (Semigroup)
function t5() : Int =
    let s = combine("AB", "CD") in
    s.byteLen;

// T6: charCount on string literal
function t6() : Int = charCount("Hello");

// T7: strStartsWith with literals
function t7() : Bool = strStartsWith("Hello", "He");

// T8: strEndsWith with literals
function t8() : Bool = strEndsWith("Hello", "lo");

// T9: strIndexOf with literals
function t9() : Int = strIndexOf("ABCDE", "CD");

// T10: Str compare ordering
function t10() : Ordering = compare("A", "B");
