// Test the derive system with reflection

type Color = Red | Green | Blue;

// All instances auto-derived from algebra derive blocks
instance Eq(Color) = derive;
instance Ord(Color) = derive;
instance Show(Color) = derive;
instance Hashable(Color) = derive;

// Test functions
function testColorEq1() : Bool = Red == Red;
function testColorEq2() : Bool = Red == Blue;
function testColorOrd() : Ordering = compare(Red, Blue);
function testColorShow() : String = show(Green);
function testColorHash() : Bool = hash(Red) != hash(Blue);

// Test deriving syntax on type declarations
type Direction = North | South | East | West deriving Eq, Show;

function testDirEq() : Bool = North == North;
function testDirNeq() : Bool = North != South;
function testDirShow() : String = show(East);
