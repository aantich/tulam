/// Negative test: non-exhaustive pattern match. `isRed` only handles one
/// constructor of `Color`, missing `Green` and `Blue`. T1.2: coverage
/// check must flag this and halt by default.

type Color = Red + Green + Blue;

function isRed(c:Color) : Bool = match
    | Red -> True;
