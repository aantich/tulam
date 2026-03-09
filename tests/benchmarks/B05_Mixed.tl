// B05: Mixed Workload — exercises all optimization paths together
// Fibonacci, sorting, tree operations

function fib(n:Int) : Int =
    if n <= 1 then n else fib(n - 1) + fib(n - 2);

function benchFib() : Int = fib(20);

// Insertion sort on lists
function insert(x:Int, xs:List(Int)) : List(Int) = match
    | x, Nil -> Cons(x, Nil)
    | x, Cons(h, t) -> if x <= h then Cons(x, Cons(h, t)) else Cons(h, insert(x, t));

function isort(xs:List(Int)) : List(Int) = match
    | Nil -> Nil
    | Cons(h, t) -> insert(h, isort(t));

function mkDescList(n:Int) : List(Int) =
    if n <= 0 then Nil else Cons(n, mkDescList(n - 1));

function headOrZero(xs:List(Int)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> h;

function benchSort() : Int =
    let xs = mkDescList(50) in
    let sorted = isort(xs) in
    headOrZero(sorted);

// Binary tree operations
type BTree = BLeaf + BNode * val:Int * left:BTree * right:BTree;

function treeInsert(x:Int, t:BTree) : BTree = match
    | x, BLeaf -> BNode(x, BLeaf, BLeaf)
    | x, BNode(v, l, r) ->
        if x < v then BNode(v, treeInsert(x, l), r)
        else if x > v then BNode(v, l, treeInsert(x, r))
        else BNode(v, l, r);

function treeSize(t:BTree) : Int = match
    | BLeaf -> 0
    | BNode(v, l, r) -> 1 + treeSize(l) + treeSize(r);

function buildTree(xs:List(Int)) : BTree = match
    | Nil -> BLeaf
    | Cons(h, t) -> treeInsert(h, buildTree(t));

function benchTree() : Int =
    let xs = Cons(5, Cons(3, Cons(8, Cons(1, Cons(9, Cons(2, Cons(7, Cons(4, Cons(6, Cons(10, Nil)))))))))) in
    treeSize(buildTree(xs));
