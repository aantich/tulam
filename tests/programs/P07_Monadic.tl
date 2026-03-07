function safeDivide(x:Int, y:Int) : Maybe(Int) =
    if y == 0 then Nothing else Just(div(x, y));

function doubleAll(xs:List(Int)) : List(Int) =
    fmap(\x -> x + x, xs);

function addOneAll(xs:List(Int)) : List(Int) =
    fmap(\x -> x + 1, xs);

function flatMapDouble(xs:List(Int)) : List(Int) =
    bind(xs, \x -> Cons(x, Cons(x, Nil)));

function filterPositive(xs:List(Int)) : List(Int) =
    bind(xs, \x -> if x > 0 then Cons(x, Nil) else Nil);

function chainDivide(x:Int, y:Int, z:Int) : Maybe(Int) =
    bind(safeDivide(x, y), \r -> safeDivide(r, z));

function liftMaybe2(f:a, mx:Maybe(a), my:Maybe(b)) : Maybe(c) =
    bind(mx, \x -> bind(my, \y -> Just(f(x, y))));
