/// Either instances: Show.
module Instances.Either;
import Core;
import Algebra;

instance Show(Either(a, b)) = {
    function show(x:Either(a, b)) : String = match
        | Left(v) -> "Left(" ⊕ show(v) ⊕ ")"
        | Right(v) -> "Right(" ⊕ show(v) ⊕ ")"
};
