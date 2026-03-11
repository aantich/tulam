/// Test program for strict type checking validation.
/// Contains intentional type errors to verify strict mode catches them.

/// Good function — should pass type checking
function double(x:Int) : Int = x + x;

/// Function with wrong return type annotation (body returns Int, annotation says Bool)
function wrongReturn(x:Int) : Bool = x + 1;

/// Function with correct polymorphic type
function identity(x:a) : a = x;
