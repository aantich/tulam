// P31: Requires Constraint Enforcement Tests

// A structure that requires Eq
algebra Printable(a:Type) requires Eq(a) = {
    function display(x:a) : String
};

// This should work fine: Int has Eq
instance Printable(Int) = {
    function display(x:Int) : String = show(x)
};

// Test that Printable(Int) works
function t1() : String = display(42);

// Test that Eq is available alongside Printable
function t2() : Bool = 42 == 42;

// A morphism with requires
morphism Mappable(a:Type, b:Type) requires Eq(a), Eq(b) = {
    function mapTo(x:a) : b
};

// Instance with satisfied requires
instance Mappable(Int, Bool) = {
    function mapTo(x:Int) : Bool = if x == 0 then False else True
};

function t3() : Bool = mapTo(1);
function t4() : Bool = mapTo(0);

// Structure with requires on a param used in derive context
// (When derive generates structural helpers, the inner Eq is needed)
type Wrapper = MkWrapper * val:a;
instance Eq(Wrapper(a)) = derive;

// Eq(Wrapper) works because structuralEq recurses into fields
function t5() : Bool = MkWrapper(10) == MkWrapper(10);
function t6() : Bool = MkWrapper(10) == MkWrapper(20);
