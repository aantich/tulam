module P06.Main;
import P06.Expr;
import P06.Eval;

function testExpr1() : Expr = Mul(Add(Num(3), Num(4)), Num(2));
function testExpr2() : Expr = Neg(Add(Num(5), Num(3)));
function testExpr3() : Expr = Add(Num(1), Add(Num(2), Num(3)));

function isPositiveResult(e:Expr) : Bool = eval(e) > 0;
