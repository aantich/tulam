/// Either instances: Show.
module Instances.Either;
import Core;
import Algebra;

instance Show(Either(a, b)) = {
    function show(x:Either(a, b)) : String = match
        | Left(v) -> concat("Left(", concat(show(v), ")"))
        | Right(v) -> concat("Right(", concat(show(v), ")"))
};
