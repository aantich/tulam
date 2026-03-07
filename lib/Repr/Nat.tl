/// Representation mapping: Nat as Int.
///
/// Allows efficient integer arithmetic on natural numbers by mapping
/// the Peano representation to machine integers.
///
/// @see Core.Nat
module Repr.Nat;
import Core.Nat;

/// Map Nat to Int (default representation).
/// Z <-> 0, Succ(Z) <-> 1, Succ(Succ(Z)) <-> 2, etc.
repr Nat as Int default where {
    function toRepr(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + toRepr(m),
    function fromRepr(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(fromRepr(n - 1))
};
