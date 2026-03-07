/// Mutable references backed by IORef.
///
/// Ref is an opaque handle to a mutable cell. Operations are
/// dispatched as intrinsics in the interpreter.
///
/// @see Mutable.MutArray
module Mutable.Ref;

/// Opaque mutable reference type.
/// @param a The type of the contained value
primitive Ref(a:Type);

/// Create a new mutable reference with the given initial value.
/// @param val Initial value
/// @returns A fresh Ref handle
function newRef(val:a) : Ref(a) = intrinsic;

/// Read the current value from a reference.
/// @param r The reference to read
/// @returns The current value
function readRef(r:Ref(a)) : a = intrinsic;

/// Write a new value to a reference.
/// @param r The reference to write
/// @param val The new value
/// @returns Unit
function writeRef(r:Ref(a), val:a) : Unit = intrinsic;

/// Modify the value in a reference by applying a function.
/// @param r The reference to modify
/// @param f The transformation function
/// @returns Unit
function modifyRef(r:Ref(a), f: a -> a) : Unit = intrinsic;
