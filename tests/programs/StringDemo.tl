// Simple string manipulation demo
// Requires :s newstrings on

// T1: Create a string and get its byte length
function t1() : Int =
    let s = "Hello" in
    s.byteLen;

// T2: Concatenate two strings
function t2() : Int =
    let s = combine("Hello", " World") in
    s.byteLen;

// T3: Check string equality
function t3() : Bool = "abc" == "abc";

// T4: Check string inequality
function t4() : Bool = "abc" != "xyz";

// T5: Count characters
function t5() : Int = charCount("Hello");

// T6: startsWith
function t6() : Bool = strStartsWith("Hello World", "Hello");

// T7: endsWith
function t7() : Bool = strEndsWith("Hello World", "World");

// T8: indexOf
function t8() : Int = strIndexOf("Hello World", "World");

// T9: Slice a string and check byte length
function t9() : Int =
    let s = strSlice("Hello World", 0, 5) in
    s.byteLen;

// T10: Empty string check
function t10() : Bool = strIsEmpty("");
