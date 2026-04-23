/// Negative test: matching VCons against Vec(a, Z) is absurd — VCons's
/// return type Vec(a, Succ n) cannot unify with Z. T1.6: the checker rejects
/// the program (either via GADT refinement failure or regular unification).

type Vec(a:Type, n:Nat) = VNil : Vec(a, Z) + VCons * head:a * tail:Vec(a, n) : Vec(a, Succ(n));

function impossible(v:Vec(Int, Z)) : Int = match v | VCons(x, _) -> x;
