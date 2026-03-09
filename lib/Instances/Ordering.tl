/// Ordering instances: Eq, Ord, Show, Bounded, Enum, Hashable.
module Instances.Ordering;
import Core;
import Algebra;
import Instances.Bool;

instance Eq(Ordering) = {
    function (==)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, LessThan -> True
        | Equal, Equal -> True
        | GreaterThan, GreaterThan -> True
        | x, y -> False
};

instance Ord(Ordering) = {
    function compare(x:Ordering, y:Ordering) : Ordering = match
        | LessThan, LessThan -> Equal
        | LessThan, n -> LessThan
        | Equal, LessThan -> GreaterThan
        | Equal, Equal -> Equal
        | Equal, GreaterThan -> LessThan
        | GreaterThan, GreaterThan -> Equal
        | GreaterThan, n -> GreaterThan;
    function (<)(x:Ordering, y:Ordering) : Bool = match
        | LessThan, Equal -> True
        | LessThan, GreaterThan -> True
        | Equal, GreaterThan -> True
        | x, y -> False;
    function (>)(x:Ordering, y:Ordering) : Bool = match
        | Equal, LessThan -> True
        | GreaterThan, LessThan -> True
        | GreaterThan, Equal -> True
        | x, y -> False;
    function (<=)(x:Ordering, y:Ordering) : Bool = not(x > y);
    function (>=)(x:Ordering, y:Ordering) : Bool = not(x < y)
};

instance Show(Ordering) = {
    function show(x:Ordering) : String = match
        | LessThan -> "LessThan"
        | Equal -> "Equal"
        | GreaterThan -> "GreaterThan"
};

instance Bounded(Ordering) = {
    value minBound = LessThan;
    value maxBound = GreaterThan
};

instance Enum(Ordering) = {
    function succ(x:Ordering) : Ordering = match
        | LessThan -> Equal
        | Equal -> GreaterThan
        | GreaterThan -> GreaterThan;
    function pred(x:Ordering) : Ordering = match
        | GreaterThan -> Equal
        | Equal -> LessThan
        | LessThan -> LessThan;
    function toEnum(i:Int) : Ordering = match
        | 0 -> LessThan
        | 1 -> Equal
        | n -> GreaterThan;
    function fromEnum(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};

instance Hashable(Ordering) = {
    function hash(x:Ordering) : Int = match
        | LessThan -> 0
        | Equal -> 1
        | GreaterThan -> 2
};
