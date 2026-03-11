function safeDivide(x:Int, y:Int) : Maybe(Int) =
    if y == 0 then Nothing else Just(div(x, y));

function doubleAll(xs:List(Int)) : List(Int) =
    fmap(fn(x) = x + x, xs);

function addOneAll(xs:List(Int)) : List(Int) =
    fmap(fn(x) = x + 1, xs);

function flatMapDouble(xs:List(Int)) : List(Int) =
    bind(xs, fn(x) = Cons(x, Cons(x, Nil)));

function filterPositive(xs:List(Int)) : List(Int) =
    bind(xs, fn(x) = if x > 0 then Cons(x, Nil) else Nil);

function chainDivide(x:Int, y:Int, z:Int) : Maybe(Int) =
    bind(safeDivide(x, y), fn(r) = safeDivide(r, z));

function liftMaybe2(f:a, mx:Maybe(a), my:Maybe(b)) : Maybe(c) =
    bind(mx, fn(x) = bind(my, fn(y) = Just(f(x, y))));
