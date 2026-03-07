// Test type-directed dispatch for return-type-dependent functions

// toEnum dispatches based on return type, not argument type
function testToEnumBool1() : Bool = toEnum(0);
function testToEnumBool2() : Bool = toEnum(1);
function testToEnumOrd() : Ordering = toEnum(2);

// Functions where context drives dispatch
function testFromEnumBool() : Int = fromEnum(True);
function testFromEnumOrd() : Int = fromEnum(GreaterThan);
