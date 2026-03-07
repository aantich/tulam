/// Mutable arrays backed by IORef.
///
/// MutArray is an opaque handle to a mutable array. Operations are
/// dispatched as intrinsics in the interpreter. Use freeze/thaw to
/// convert between immutable Array and MutArray.
///
/// @see Mutable.Ref, Collection.Array
module Mutable.MutArray;

/// Opaque mutable array type.
/// @param a The type of the contained elements
primitive MutArray(a:Type);

/// Create a new mutable array of n elements, all initialized to val.
/// @param n Number of elements
/// @param val Initial value for each element
/// @returns A fresh MutArray handle
function newMutArray(n:Int, val:a) : MutArray(a) = intrinsic;

/// Read the element at the given index (0-based, bounds-checked).
/// @param arr The mutable array
/// @param i Index to read
/// @returns The element at index i
function mutRead(arr:MutArray(a), i:Int) : a = intrinsic;

/// Write a value at the given index (0-based, bounds-checked).
/// @param arr The mutable array
/// @param i Index to write
/// @param val The new value
/// @returns Unit
function mutWrite(arr:MutArray(a), i:Int, val:a) : Unit = intrinsic;

/// Get the length of a mutable array.
/// @param arr The mutable array
/// @returns The number of elements
function mutLength(arr:MutArray(a)) : Int = intrinsic;

/// Convert a mutable array to an immutable Array (snapshot).
/// @param arr The mutable array
/// @returns An immutable Array with the same elements
function freeze(arr:MutArray(a)) : Array(a) = intrinsic;

/// Convert an immutable Array to a new mutable array.
/// @param arr The immutable array
/// @returns A fresh MutArray with the same elements
function thaw(arr:Array(a)) : MutArray(a) = intrinsic;

/// Append an element to the end of a mutable array.
/// @param arr The mutable array
/// @param val The value to append
/// @returns Unit
function mutPush(arr:MutArray(a), val:a) : Unit = intrinsic;
