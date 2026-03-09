// P30: Derive for Parameterized Types
// Tests that derive works correctly on types with type parameters,
// using reflection intrinsics that recurse into fields via CLMIAP dispatch.

// --- Eq for Maybe ---
instance Eq(Maybe(a)) = derive;

function t1() : Bool = Just(1) == Just(1);         // True
function t2() : Bool = Just(1) == Just(2);         // False
function t3() : Bool = Nothing == Nothing;          // True (Maybe(a))
function t4() : Bool = Just(1) == Nothing;          // False

// --- Show for Maybe (derive) ---
// Note: lib already has Show(Maybe(a)) manually defined.
// We test with a custom type instead.

type Box = MkBox * val:a;
instance Eq(Box(a)) = derive;
instance Show(Box(a)) = derive;

function t5() : Bool = MkBox(42) == MkBox(42);     // True
function t6() : Bool = MkBox(42) == MkBox(99);     // False
function t7() : String = show(MkBox(42));           // "MkBox(42)"

// --- Eq for List ---
instance Eq(List(a)) = derive;

function t8() : Bool = Cons(1, Cons(2, Nil)) == Cons(1, Cons(2, Nil));  // True
function t9() : Bool = Cons(1, Nil) == Cons(2, Nil);                     // False
function t10() : Bool = Nil == Nil;                                       // True (List)
function t11() : Bool = Cons(1, Nil) == Nil;                              // False

// --- Nested parameterized types ---
function t12() : Bool = Just(Just(1)) == Just(Just(1));   // True
function t13() : Bool = Just(Just(1)) == Just(Just(2));   // False
function t14() : Bool = Just(Nil) == Just(Nil);           // True (Maybe(List(a)))

// --- Derive with multi-field constructors ---
type Pair = MkPair * fst:a * snd:b;
instance Eq(Pair(a, b)) = derive;
instance Show(Pair(a, b)) = derive;

function t15() : Bool = MkPair(1, 2) == MkPair(1, 2);    // True
function t16() : Bool = MkPair(1, 2) == MkPair(1, 3);    // False
function t17() : String = show(MkPair(1, True));           // "MkPair(1, True)"

// --- Deriving syntax on parameterized types ---
type Triple = MkTriple * fst:a * snd:b * thd:c deriving Eq, Show;

function t18() : Bool = MkTriple(1, 2, 3) == MkTriple(1, 2, 3);    // True
function t19() : Bool = MkTriple(1, 2, 3) == MkTriple(1, 2, 4);    // False
function t20() : String = show(MkTriple(1, True, "hi"));             // "MkTriple(1, True, hi)"
