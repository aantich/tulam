// Record system completion tests

record Point = { x:Int, y:Int };
record Color = { r:Int, g:Int, b:Int };

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
