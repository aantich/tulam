/// Unit instances: Eq, Ord, Show, Bounded, Hashable, Semigroup, Monoid.
module Instances.Unit;
import Core;
import Algebra;

instance Eq(Unit) = {
    function (==)(x:Unit, y:Unit) : Bool = True;
    function (!=)(x:Unit, y:Unit) : Bool = False
};

instance Ord(Unit) = {
    function compare(x:Unit, y:Unit) : Ordering = Equal;
    function (<)(x:Unit, y:Unit) : Bool = False;
    function (>)(x:Unit, y:Unit) : Bool = False;
    function (<=)(x:Unit, y:Unit) : Bool = True;
    function (>=)(x:Unit, y:Unit) : Bool = True
};

instance Show(Unit) = {
    function show(x:Unit) : String = "Unit"
};

instance Bounded(Unit) = {
    value minBound = Unit;
    value maxBound = Unit
};

instance Hashable(Unit) = {
    function hash(x:Unit) : Int = 0
};

instance Semigroup(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit
};

instance Monoid(Unit) = {
    function combine(x:Unit, y:Unit) : Unit = Unit;
    value empty = Unit
};
