// P35: GADTs — Generalized Algebraic Data Types

// --- GADT 1: Length-indexed vectors ---
type Vec(a:Type, n:Nat) = VNil : Vec(a, Z) + VCons * head:a * tail:Vec(a, n) : Vec(a, Succ(n));

// Construct some vectors
function emptyVec() : Vec(Int, Z) = VNil;
function singleVec() : Vec(Int, Succ(Z)) = VCons(42, VNil);
function doubleVec() : Vec(Int, Succ(Succ(Z))) = VCons(1, VCons(2, VNil));

// Pattern match on GADT constructors
function vecHead(v:Vec(Int, Succ(n))) : Int = match | VCons(x, _) -> x;
function t1() : Int = vecHead(singleVec());
function t2() : Int = vecHead(doubleVec());

// --- GADT 2: Well-typed expressions ---
type TExpr(a:Type) =
    TLitI * n:Int : TExpr(Int)
  + TLitB * b:Bool : TExpr(Bool)
  + TAdd * x:TExpr(Int) * y:TExpr(Int) : TExpr(Int);

// Construct GADT expressions
function mkLitI(n:Int) : TExpr(Int) = TLitI(n);
function mkAdd() : TExpr(Int) = TAdd(TLitI(1), TLitI(2));
function mkLitB() : TExpr(Bool) = TLitB(True);

// --- GADT 3: Propositional equality ---
type PropEq(a:Type, b:Type) = Refl : PropEq(a, a);

function mkRefl() : PropEq(Int, Int) = Refl;

// --- GADT 4: Simple option-like GADT ---
type SafeOption(a:Type, present:Bool) =
    SomeVal * x:a : SafeOption(a, True)
  + NoVal : SafeOption(a, False);

function mkSome(x:Int) : SafeOption(Int, True) = SomeVal(x);
function mkNone() : SafeOption(Int, False) = NoVal;
function t3() : SafeOption(Int, True) = mkSome(99);

// --- Runtime tests ---
function extractI(e:TExpr(Int)) : Int = match | TLitI(n) -> n | TAdd(_, _) -> 0;
function t4() : Int = extractI(mkLitI(5));
function extractB(e:TExpr(Bool)) : Bool = match | TLitB(b) -> b;
function t5() : Bool = extractB(mkLitB());

// More runtime: vector construction and head extraction
function t6() : Int = vecHead(VCons(100, VNil));
function t7() : SafeOption(Int, True) = SomeVal(7);
