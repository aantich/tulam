/// Utility functions that depend on instances.
module Instances.Utility;
import Core;
import Instances.Nat;

/// Describe an Int as a Nat (demo function).
function describeInt(x:Int) : Nat = match
    | 0 -> Z
    | 1 -> Succ(Z)
    | n -> Succ(Succ(Z));

/// Double a Nat using a where clause.
function doubleNat(x:Nat) : Nat = plus(x, x);
