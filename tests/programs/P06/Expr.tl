module P06.Expr;
type Expr = Num * val:Int + Add * left:Expr * right:Expr + Mul * left:Expr * right:Expr + Neg * inner:Expr;
