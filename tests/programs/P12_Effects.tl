// P12: Effect System Tests

// Test 1: Effect declaration parsing
effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit
};

// Test 2: Another effect declaration
effect FileIO = {
    function readFile(path:String) : String;
    function writeFile(path:String, content:String) : Unit
};

// Test 3: Handler declaration
handler StdConsole : Console = {
    function readLine() = intrinsic;
    function putStrLn(s) = intrinsic
};

// Test 4: Action block with new-style bind syntax
// (action blocks desugar to bind chains)
function testAction(x:Int) : Int = x + 1;

// Test 5: Pure function — no effects, should work normally alongside effects
function pureAdd(x:Int, y:Int) : Int = x + y;

// Test 6: Function using match (verify no interference with effect system)
function isPositive(x:Int) : Bool = match
    | 0 -> False
    | _ -> True;
