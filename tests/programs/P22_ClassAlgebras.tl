// P22: Class Algebra (implements) Tests

// Class that implements Show with explicit method body
class Point(px:Int, py:Int) implements Show = {
    function show(self:Point) : String = "Point"
};

// Class that implements Eq with derive fallback (no explicit == method)
class Color(cr:Int, cg:Int, cb:Int) implements Eq = {
    function display(self:Color) : String = "Color"
};

// Class that implements Show with derive fallback
class Tag(tagLabel:String) implements Show = {
    function info(self:Tag) : String = self.tagLabel
};

// --- Test values ---

// T1: show(Point) via algebra dispatch
function t1() : String = show(Point.new(1, 2));

// T2: Point.show() direct method call
function t2() : String = Point.new(3, 4).show();

// T3: Color equality via derive fallback (same values)
function t3() : Bool = Color.new(1, 2, 3) == Color.new(1, 2, 3);

// T4: Color equality via derive fallback (different values)
function t4() : Bool = Color.new(1, 2, 3) == Color.new(4, 5, 6);

// T5: Color.display() own method still works
function t5() : String = Color.new(0, 0, 0).display();
