module P05.Main;
import P05.Stack;

function pushMany(xs:List(a), s:Stack(a)) : Stack(a) = match
    | Nil, s -> s
    | Cons(h, t), s -> pushMany(t, push(h, s));

function stackToList(s:Stack(a)) : List(a) = match
    | EmptyStack -> Nil
    | Push(t, r) -> Cons(t, stackToList(r));

function reverseViaStack(xs:List(a)) : List(a) =
    stackToList(pushMany(xs, EmptyStack));
