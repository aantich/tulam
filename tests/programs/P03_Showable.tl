algebra Rankable(a:Type) = {
    function rank(x:a) : Int
};

type Color = Red + Green + Blue;
type Shape = Circle * r:Nat + Square * s:Nat + Triangle * a:Nat * b:Nat * c:Nat;

instance Rankable(Color) = {
    function rank(x:Color) : Int = match
        | Red   -> 1
        | Green -> 2
        | Blue  -> 3
};

instance Rankable(Shape) = {
    function rank(x:Shape) : Int = match
        | Circle(r)      -> 10
        | Square(s)       -> 20
        | Triangle(a,b,c) -> 30
};

function doubleRank(x:a) : Int = rank(x) + rank(x);
function isHighRank(x:a) : Bool = rank(x) > 10;
