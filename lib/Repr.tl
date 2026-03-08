/// Representation mappings: Nat as Int, Char as Int.
module Repr;
import Core;
import Algebra;

/// Map Nat to Int (default representation).
repr Nat as Int default where {
    function toRepr(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + toRepr(m);
    function fromRepr(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(fromRepr(n - 1))
};

// Repr.Char disabled: fromRepr keyed by ["Int"] collides with Repr.Nat.
// Char<->Int conversion available via fromEnum/toEnum.
// TODO: fix repr dispatch to support multiple fromRepr instances (type-directed)
// repr Char as Int where {
//     function toRepr(c:Char) : Int = fromEnum(c);
//     function fromRepr(i:Int) : Char = toEnum(i)
// };
