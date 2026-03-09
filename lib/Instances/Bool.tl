/// Bool instances: Eq, Ord, Show, Bounded, Enum, Hashable, Lattice.
///
/// `not` is defined in Core (needed by Algebra default methods).
module Instances.Bool;
import Core;
import Algebra;

// ============================================================================
// Algebra instances
// ============================================================================

instance Eq(Bool) = {
    function (==) (x:Bool,y:Bool) : Bool = match
        | True, True -> True
        | False, False -> True
        | True, False -> False
        | False, True -> False
};

instance Show(Bool) = {
    function show(x:Bool) : String = match
        | True -> "True"
        | False -> "False"
};

instance Bounded(Bool) = {
    value minBound = False;
    value maxBound = True
};

instance Enum(Bool) = {
    function succ(x:Bool) : Bool = match
        | False -> True
        | True -> True;
    function pred(x:Bool) : Bool = match
        | True -> False
        | False -> False;
    function toEnum(i:Int) : Bool = match
        | 0 -> False
        | n -> True;
    function fromEnum(x:Bool) : Int = match
        | False -> 0
        | True -> 1
};

instance Hashable(Bool) = {
    function hash(x:Bool) : Int = match
        | True -> 1
        | False -> 0
};

instance JoinSemiLattice(Bool) = {
    function (\/)(x:Bool, y:Bool) : Bool = match
        | False, False -> False
        | x, y -> True
};

instance MeetSemiLattice(Bool) = {
    function (/\)(x:Bool, y:Bool) : Bool = match
        | True, True -> True
        | x, y -> False
};

instance Lattice(Bool) = {};

instance BoundedLattice(Bool) = {
    value bottom = False;
    value top = True
};
