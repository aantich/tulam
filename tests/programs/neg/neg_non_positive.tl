/// Negative test: non-positive recursive type. `Bad` appears in negative
/// position (the domain of a function type) which would make the type
/// non-well-founded. T1.2: positivity check must flag this and halt by
/// default.

type Bad = MkBad * f:(Bad -> Int);

function useBad(b:Bad) : Int = match b | MkBad(f) -> f(b);
