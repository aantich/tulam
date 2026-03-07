function len(xs:List(a)) : Nat = match
    | Nil -> Z
    | Cons(h, t) -> Succ(len(t));

function append(xs:List(a), ys:List(a)) : List(a) = match
    | Nil, ys -> ys
    | Cons(h, t), ys -> Cons(h, append(t, ys));

function nth(xs:List(a), n:Nat) : Maybe(a) = match
    | Nil, n -> Nothing
    | Cons(h, t), Z -> Just(h)
    | Cons(h, t), Succ(n) -> nth(t, n);

function take(n:Nat, xs:List(a)) : List(a) = match
    | Z, xs -> Nil
    | n, Nil -> Nil
    | Succ(n), Cons(h, t) -> Cons(h, take(n, t));

function drop(n:Nat, xs:List(a)) : List(a) = match
    | Z, xs -> xs
    | n, Nil -> Nil
    | Succ(n), Cons(h, t) -> drop(n, t);

function mapList(f:a, xs:List(b)) : List(c) = match
    | f, Nil -> Nil
    | f, Cons(h, t) -> Cons(f(h), mapList(f, t));
