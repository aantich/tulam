module P05.Stack;

type Stack(a:Type) = EmptyStack | Push(top:a, rest:Stack(a));

function push(x:a, s:Stack(a)) : Stack(a) = Push(x, s);

function pop(s:Stack(a)) : Maybe(Stack(a)) = match
    | EmptyStack -> Nothing
    | Push(t, r) -> Just(r);

function peek(s:Stack(a)) : Maybe(a) = match
    | EmptyStack -> Nothing
    | Push(t, r) -> Just(t);

function stackSize(s:Stack(a)) : Nat = match
    | EmptyStack -> Z
    | Push(t, r) -> Succ(stackSize(r));

function isStackEmpty(s:Stack(a)) : Bool = match
    | EmptyStack -> True
    | Push(t, r) -> False;
