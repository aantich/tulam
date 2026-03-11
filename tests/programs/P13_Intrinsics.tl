// P13: Intrinsic Completeness Tests

// === Show tests ===
// Test 1: show Int
function showInt() : String = show(42);

// Test 2: show Float64
function showFloat() : String = show(3.14);

// Test 3: show String (with quotes)
function showString() : String = show("hi");

// Test 4: show Char
function showChar() : String = show('A');

// === Char tests ===
// Test 5: Char equality
function charEq() : Bool = 'A' == 'A';

// Test 6: Char inequality
function charNeq() : Bool = 'A' == 'B';

// Test 7: Char less-than
function charLt() : Bool = 'A' < 'B';

// Test 8: Char compare
function charCmp() : Ordering = compare('A', 'B');

// === Enum tests ===
// Test 9: succ Int
function succInt() : Int = succ(42);

// Test 10: pred Int
function predInt() : Int = pred(42);

// Test 11: succ Char
function succChar() : Char = succ('A');

// Test 12: pred Char
function predChar() : Char = pred('B');

// Test 13: fromEnum Char
function fromEnumChar() : Int = fromEnum('A');

// Test 14: toEnum Char
function toEnumChar() : Char = toEnum(65);

// === Hashable tests ===
// Test 15: hash Int (identity)
function hashInt() : Int = hash(42);

// Test 16: hash Char
function hashChar() : Int = hash('A');

// === String extended ops ===
// Test 17: charAt
function testCharAt() : Char = charAt("hello", 0);

// Test 18: substring
function testSubstring() : String = substring("hello world", 0, 5);

// Test 19: indexOf (found)
function testIndexOf() : Int = indexOf("hello world", "world");

// Test 20: indexOf (not found)
function testIndexOfNot() : Int = indexOf("hello", "xyz");

// Test 21: trim
function testTrim() : String = trim("  hello  ");

// Test 22: toUpper
function testToUpper() : String = toUpper("hello");

// Test 23: toLower
function testToLower() : String = toLower("HELLO");

// Test 24: startsWith
function testStartsWith() : Bool = startsWith("hello world", "hello");

// Test 25: endsWith
function testEndsWith() : Bool = endsWith("hello world", "world");

// Test 26: replace
function testReplace() : String = replace("hello world", "world", "tulam");

// Test 27: parseInt valid
function testParseInt() : Maybe(Int) = parseInt("42");

// Test 28: parseInt invalid
function testParseIntBad() : Maybe(Int) = parseInt("abc");

// === Array ops ===
// Test 29: array push
function testArrayPush() : Int = size(snoc([1, 2, 3], 4));

// Test 30: array reverse (check size preserved)
function testArrayReverse() : Int = size(reverse([1, 2, 3]));

// Test 31: array range
function testRange() : Int = size(range(0, 5));

// Test 32: error function
function testError() : Int = error("boom");
