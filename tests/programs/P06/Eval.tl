module P06.Eval;
import P06.Expr;

function eval(e:Expr) : Int = match
    | Num(n)    -> n
    | Add(l, r) -> eval(l) + eval(r)
    | Mul(l, r) -> eval(l) * eval(r)
    | Neg(e)    -> 0 - eval(e);

function countOps(e:Expr) : Nat = match
    | Num(n)    -> Z
    | Add(l, r) -> Succ(plus(countOps(l), countOps(r)))
    | Mul(l, r) -> Succ(plus(countOps(l), countOps(r)))
    | Neg(e)    -> Succ(countOps(e));
