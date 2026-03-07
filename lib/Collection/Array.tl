/// Array operations: functor, element access, and higher-order functions.
///
/// Array is declared as a primitive in Prelude. Operations are backed
/// by intrinsic implementations in the interpreter.
///
/// @see HKT.Functor, Algebra.StringOps
module Collection.Array;
import HKT.Functor;
import Algebra.StringOps;

/// Functor instance for Array (backed by dispatchArrayHOF).
instance Functor(Array) = intrinsic;

/// Mutable array operations.
algebra ArrayOps(a:Type) = {
    /// Set element at index.
    /// @param arr The array
    /// @param i Index
    /// @param val New value
    /// @returns New array with element replaced
    function set(arr:a, i:Int, val:Int) : a,

    /// Append an element.
    /// @param arr The array
    /// @param val Value to append
    /// @returns New array with element appended
    function push(arr:a, val:Int) : a,

    /// Concatenate two arrays.
    /// @param arr1 First array
    /// @param arr2 Second array
    /// @returns Combined array
    function arrayConcat(arr1:a, arr2:a) : a,

    /// Reverse an array.
    /// @param arr The array
    /// @returns Reversed array
    function reverse(arr:a) : a
};

/// ArrayOps backed by intrinsics.
instance ArrayOps(Array) = intrinsic;

/// Higher-order array operations.
algebra ArrayHOF(a:Type) = {
    /// Keep elements satisfying a predicate.
    /// @param f Predicate function
    /// @param arr The array
    /// @returns Filtered array
    function filter(f: Int -> Bool, arr:a) : a,

    /// Left fold over array elements.
    /// @param f Accumulation function
    /// @param acc Initial accumulator
    /// @param arr The array
    /// @returns Final accumulated value
    function foldl(f: Int -> Int -> Int, acc:Int, arr:a) : Int,

    /// Right fold over array elements.
    /// @param f Accumulation function
    /// @param acc Initial accumulator
    /// @param arr The array
    /// @returns Final accumulated value
    function foldr(f: Int -> Int -> Int, acc:Int, arr:a) : Int,

    /// Generate an array of n elements using a function.
    /// @param n Number of elements
    /// @param f Generator function (index -> value)
    /// @returns Generated array
    function generate(n:Int, f: Int -> Int) : a
};

/// ArrayHOF backed by intrinsics.
instance ArrayHOF(Array) = intrinsic;
