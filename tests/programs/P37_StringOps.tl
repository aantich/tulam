// P37: String algebra operations — split, join, fromChar, Semigroup/Monoid, StringLike

// t1: split basic
function t1() : List(String) = split("hello world foo", " ");
// expected: Cons("hello", Cons("world", Cons("foo", Nil)))

// t2: join basic
function t2() : String = join(Cons("a", Cons("b", Cons("c", Nil))), "-");
// expected: "a-b-c"

// t3: fromChar
function t3() : String = fromChar('A');
// expected: "A"

// t4: split with empty results
function t4() : List(String) = split("a,,b", ",");
// expected: Cons("a", Cons("", Cons("b", Nil)))

// t5: join empty list
function t5() : String = join(Nil, ",");
// expected: ""

// t6: join singleton
function t6() : String = join(Cons("only", Nil), ",");
// expected: "only"

// t7: Semigroup combine
function t7() : String = combine("hello ", "world");
// expected: "hello world"

// t8: Monoid empty
function t8() : String = empty;
// expected: ""

// t9: split then join roundtrip
function t9() : String = join(split("a-b-c", "-"), "-");
// expected: "a-b-c"

// t10: split on no match
function t10() : List(String) = split("hello", ",");
// expected: Cons("hello", Nil)

// t11: fromChar special char
function t11() : String = fromChar('\n');
// expected: "\n" (newline char as string)
