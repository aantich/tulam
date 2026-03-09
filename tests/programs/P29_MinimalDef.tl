/// Test program for automatic minimal definition checking.

/// A simple type to test with.
type Color3 = Red3 + Green3 + Blue3;

/// Provide only (==) — (!=) should be auto-derived from the default.
instance Eq(Color3) = {
    function (==)(x:Color3, y:Color3) : Bool = match
        | Red3, Red3 -> True
        | Green3, Green3 -> True
        | Blue3, Blue3 -> True
        | _, _ -> False
};

/// Test: (==) works directly
function t1() : Bool = Red3 == Red3;

/// Test: (!=) works via default (not (x == y))
function t2() : Bool = Red3 != Green3;

/// Test: (!=) false case
function t3() : Bool = Blue3 != Blue3;

/// A type where we provide only compare — all operators should derive from it.
type Priority = Low + Medium + High;

instance Eq(Priority) = derive;

instance Ord(Priority) = {
    function compare(x:Priority, y:Priority) : Ordering = match
        | Low, Low -> Equal
        | Low, _ -> LessThan
        | Medium, Low -> GreaterThan
        | Medium, Medium -> Equal
        | Medium, High -> LessThan
        | High, High -> Equal
        | High, _ -> GreaterThan
};

/// Test: compare works directly
function t4() : Ordering = compare(Low, High);

/// Test: (<) derived from compare default
function t5() : Bool = Low < High;

/// Test: (>=) derived from compare default
function t6() : Bool = High >= Medium;

/// Test: (<=) derived from compare default
function t7() : Bool = Medium <= Medium;

/// Test: (>) derived from compare default
function t8() : Bool = High > Low;
