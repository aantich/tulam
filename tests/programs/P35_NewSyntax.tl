// =====================================================
// P35: New Surface Syntax Tests (+/* for type definitions)
// All tests exercise the new unified syntax.
// =====================================================

// ===========================================
// A: Enum types with + separator
// ===========================================

// A1: Simple two-variant enum
type TestBool = TestTrue + TestFalse;

// A2: Three-variant enum
type TestColor = Red2 + Green2 + Blue2;

// A3: Single nullary constructor (trivial)
type TestUnit2 = TestUnit2Val;

// ===========================================
// B: Single-variant types (implicit constructor)
// ===========================================

// B1: Two-field record with implicit constructor name
type TestPoint = x:Float64 * y:Float64;

// B2: Three-field record
type TestPerson = name:String * age:Int * active:Bool;

// B3: Single field
type TestWrapper = val:Int;

// ===========================================
// C: Sum types with fields using + and *
// ===========================================

// C1: Maybe-like with one nullary and one unary variant
type TestMaybe(a:Type) = TestNothing + TestJust * val:a;

// C2: Either-like with two unary variants
type TestEither(a:Type, b:Type) = TestLeft * val:a + TestRight * val:b;

// C3: Multi-field constructors in a sum
type TestShape = TestCircle * cx:Float64 * cy:Float64 * r:Float64
               + TestRect * x1:Float64 * y1:Float64 * x2:Float64 * y2:Float64;

// C4: Recursive sum type
type TestList(a:Type) = TestNil + TestCons * head:a * tail:TestList(a);

// C5: Three-way sum with mixed arity
type TestExpr = TestLitI * v:Int
              + TestAdd * l:TestExpr * r:TestExpr
              + TestNeg * e:TestExpr;

// ===========================================
// D: GADTs with new syntax
// ===========================================

// D1: GADT with nullary constructor specifying return type
type TestVec(a:Type, n:Nat) =
    TestVNil : TestVec(a, Z)
  + TestVCons * head:a * tail:TestVec(a, n) : TestVec(a, Succ(n));

// D2: GADT - typed expressions
type TestTExpr(a:Type) =
    TestTLitI * n:Int : TestTExpr(Int)
  + TestTLitB * b:Bool : TestTExpr(Bool)
  + TestTAdd * x:TestTExpr(Int) * y:TestTExpr(Int) : TestTExpr(Int);

// D3: Propositional equality (nullary GADT)
type TestPropEq(a:Type, b:Type) = TestRefl : TestPropEq(a, a);

// D4: SafeOption with Bool index
type TestSafeOption(a:Type, present:Bool) =
    TestSomeVal * x:a : TestSafeOption(a, True)
  + TestNoVal : TestSafeOption(a, False);

// ===========================================
// E: Dependent telescopes
// ===========================================

// E1: Simple dependent record
type TestSizedVec(a:Type) = n:Nat * elems:TestVec(a, n);

// E2: Nested dependency
type TestMatrix(a:Type) = rows:Nat * cols:Nat * data:TestVec(TestVec(a, cols), rows);

// ===========================================
// F: Function types in fields
// ===========================================

// F1: Arrow type in a field (no parens needed)
type TestHandler = hname:String * f:Int -> Bool;

// F2: Multiple arrows in a field
type TestMapper = transform:Int -> Int -> Int * label:String;

// ===========================================
// G: Functions using the new types
// ===========================================

// G1: Function that constructs a TestPoint
function mkPoint(x:Float64, y:Float64) : TestPoint = TestPoint(x, y);

// G2: Pattern match on new-syntax sum type
function isTestJust(m:TestMaybe(Int)) : Bool =
    match m | TestJust(v) -> True | TestNothing -> False;

// G3: Pattern match on enum
function colorToInt(c:TestColor) : Int =
    match c | Red2 -> 0 | Green2 -> 1 | Blue2 -> 2;

// G4: Test GADT constructor
function testVNil() : TestVec(Int, Z) = TestVNil;

// ===========================================
// H: Runtime tests
// ===========================================

// H1: Enum creation and matching
function t1() : Int = colorToInt(Green2);

// H2: Record creation and field access
function t2() : Float64 = let p = mkPoint(3.0, 4.0) in p.x;

// H3: Maybe-like sum
function t3() : Bool = isTestJust(TestJust(42));

// H4: Maybe Nothing
function t4() : Bool = isTestJust(TestNothing);

// H5: Either left
function t5() : Int =
    let e : TestEither(Int, String) = TestLeft(10) in
    match e | TestLeft(v) -> v | TestRight(s) -> 0;

// H6: Nested sum type
function t6() : Int =
    let e = TestAdd(TestLitI(1), TestLitI(2)) in
    match e | TestLitI(v) -> v | TestAdd(l, r) -> 99 | TestNeg(x) -> neg(99);
