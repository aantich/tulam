// Record system completion tests

type Point = x:Int * y:Int;
type Color = r:Int * g:Int * b:Int;

// Functions using dot-access
function getX(p:Point) : Int = p.x;
function getY(p:Point) : Int = p.y;

// Function using named construction
function mkPoint(a:Int, b:Int) : Point = Point { x = a, y = b };

// Function using named construction with reordered fields
function mkPointRev(a:Int, b:Int) : Point = Point { y = b, x = a };

// Function using record update
function setX(p:Point, newX:Int) : Point = p { x = newX };
function setY(p:Point, newY:Int) : Point = p { y = newY };

// Function using dot-access on function result
function doubleX(p:Point) : Int = mkPoint(p.x + p.x, p.y).x;

// Named field pattern matching
function isOrigin(p:Point) : Bool = match
    | Point { x = 0, y = 0 } -> True
    | p -> False;

// Color using named construction
function red() : Color = Color { r = 255, g = 0, b = 0 };
function getR(c:Color) : Int = c.r;

// Sum-of-records: type alias unions that inherit record fields
type Rect = w:Int * h:Int;
type Circle = radius:Int;
type Shape = Rect + Circle;

function area(s:Shape) : Int = match
    | Rect(w, h) -> w * h
    | Circle(r) -> r * r * 3;

function shapeArea1() : Int = area(Rect(10, 20));
function shapeArea2() : Int = area(Circle(5));

// Structural subtyping: nominal record passed to structural-typed function
function getXStructural(p: {x:Int, y:Int}) : Int = p.x;
function structTest() : Int = getXStructural(Point(3, 4));
