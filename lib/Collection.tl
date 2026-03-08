/// Array operations: functor, element access, and higher-order functions.
///
/// Array is declared as a primitive in Prelude. Operations are backed
/// by intrinsic implementations in the interpreter.
module Collection;
import HKT;
import Algebra;

/// Functor instance for Array (backed by dispatchArrayHOF).
instance Functor(Array) = intrinsic;

/// Mutable array operations.
algebra ArrayOps(a:Type) = {
    function set(arr:a, i:Int, val:Int) : a;
    function push(arr:a, val:Int) : a;
    function arrayConcat(arr1:a, arr2:a) : a;
    function reverse(arr:a) : a
};

instance ArrayOps(Array) = intrinsic;

/// Higher-order array operations.
algebra ArrayHOF(a:Type) = {
    function filter(f: Int -> Bool, arr:a) : a;
    function foldl(f: Int -> Int -> Int, acc:Int, arr:a) : Int;
    function foldr(f: Int -> Int -> Int, acc:Int, arr:a) : Int;
    function generate(n:Int, f: Int -> Int) : a
};

instance ArrayHOF(Array) = intrinsic;

/// Element access by index (0-based).
function index(arr:Array(a), i:Int) : a = intrinsic;

/// Extract a subarray by range [start, end).
function slice(arr:Array(a), start:Int, end:Int) : Array(a) = intrinsic;

/// Generate a range of integers [start, end).
function range(start:Int, end:Int) : Array(Int) = intrinsic;
