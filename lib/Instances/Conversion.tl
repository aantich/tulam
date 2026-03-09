/// Conversion instances: Convertible morphism instances for all type pairs.
///
/// Includes both intrinsic numeric conversions and hand-written Core type
/// conversions (Nat↔Bool, Nat↔Int, Bool↔Nat).
module Instances.Conversion;
import Core;
import Morphism;
import Instances.Bool;
import Instances.Nat;

// ============================================================================
// Core type conversions
// ============================================================================

instance Convertible(Nat, Bool) = {
    function convert(x:Nat) : Bool = match
        | Z -> False
        | n -> True
};

instance Convertible(Bool, Nat) = {
    function convert(x:Bool) : Nat = match
        | True -> Succ(Z)
        | False -> Z
};

instance Convertible(Nat, Int) = {
    function convert(n:Nat) : Int = match
        | Z -> 0
        | Succ(m) -> 1 + convert(m)
};

instance Convertible(Int, Nat) = {
    function convert(i:Int) : Nat = match
        | 0 -> Z
        | n -> Succ(convert(n - 1))
};

// ============================================================================
// Byte conversions
// ============================================================================

instance Convertible(Int, Byte) = intrinsic;
instance Convertible(Byte, Int) = intrinsic;

// ============================================================================
// Numeric widening (safe)
// ============================================================================

instance Convertible(Int8, Int16) = intrinsic;
instance Convertible(Int16, Int32) = intrinsic;
instance Convertible(Int32, Int64) = intrinsic;
instance Convertible(Int32, Int) = intrinsic;
instance Convertible(UInt8, UInt16) = intrinsic;
instance Convertible(UInt16, UInt32) = intrinsic;
instance Convertible(UInt32, UInt64) = intrinsic;
instance Convertible(Float32, Float64) = intrinsic;
instance Convertible(Int, Float64) = intrinsic;

// ============================================================================
// Numeric narrowing (may truncate)
// ============================================================================

instance Convertible(Int, Int32) = intrinsic;
instance Convertible(Int32, Int16) = intrinsic;
instance Convertible(Int16, Int8) = intrinsic;
instance Convertible(Float64, Float32) = intrinsic;
instance Convertible(Int, UInt) = intrinsic;
instance Convertible(UInt, Int) = intrinsic;
