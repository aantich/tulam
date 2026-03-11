type Priority = Low + Medium + High + Critical;
type Status = Pending + InProgress + Done;
type TaskItem = Task * pri:Priority * status:Status * effort:Nat;

algebra Scorable(a:Type) = {
    function score(x:a) : Int
};

instance Scorable(Priority) = {
    function score(x:Priority) : Int = match
        | Low      -> 1
        | Medium   -> 2
        | High     -> 3
        | Critical -> 4
};

instance Scorable(Status) = {
    function score(x:Status) : Int = match
        | Pending    -> 0
        | InProgress -> 1
        | Done       -> 2
};

function taskScore(t:TaskItem) : Int = match
    | Task(p, s, e) -> score(p) * 10 + score(s) * 5 + toRepr(e);

function isHighPriority(t:TaskItem) : Bool = match
    | Task(p, s, e) -> score(p) >= 3;

function isDone(t:TaskItem) : Bool = match
    | Task(p, Done, e)  -> True
    | Task(p, s, e)     -> False;

instance Convertible(Priority, Nat) = {
    function convert(p:Priority) : Nat = match
        | Low      -> Z
        | Medium   -> Succ(Z)
        | High     -> Succ(Succ(Z))
        | Critical -> Succ(Succ(Succ(Z)))
};

function priorities(tasks:List(TaskItem)) : List(Int) =
    fmap(fn(t) = match t | Task(p, s, e) -> score(p), tasks);

function filterDone(tasks:List(TaskItem)) : List(TaskItem) =
    bind(tasks, fn(t) = if isDone(t) then Cons(t, Nil) else Nil);

function totalEffort(tasks:List(TaskItem)) : Nat = match
    | Nil -> Z
    | Cons(Task(p, s, e), rest) -> plus(e, totalEffort(rest));

function maxScore(tasks:List(TaskItem)) : Int = match
    | Nil -> 0
    | Cons(Task(p, s, e), Nil) -> score(p)
    | Cons(Task(p, s, e), rest) ->
        let pr = score(p) in
        let mr = maxScore(rest) in
        if pr > mr then pr else mr;

function projectHealth(tasks:List(TaskItem)) : Maybe(Int) =
    let total = maxScore(tasks) in
    if total > 0 then Just(total) else Nothing;
