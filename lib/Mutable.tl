/// Mutable state primitives: Ref and MutArray.
module Mutable;

// ============================================================================
// Ref
// ============================================================================

/// Opaque mutable reference type.
primitive Ref(a:Type);

function newRef(val:a) : Ref(a) = intrinsic;
function readRef(r:Ref(a)) : a = intrinsic;
function writeRef(r:Ref(a), val:a) : Unit = intrinsic;
function modifyRef(r:Ref(a), f: a -> a) : Unit = intrinsic;

// ============================================================================
// MutArray
// ============================================================================

/// Opaque mutable array type.
primitive MutArray(a:Type);

function newMutArray(n:Int, val:a) : MutArray(a) = intrinsic;
function mutRead(arr:MutArray(a), i:Int) : a = intrinsic;
function mutWrite(arr:MutArray(a), i:Int, val:a) : Unit = intrinsic;
function mutLength(arr:MutArray(a)) : Int = intrinsic;
function freeze(arr:MutArray(a)) : Array(a) = intrinsic;
function thaw(arr:Array(a)) : MutArray(a) = intrinsic;
function mutPush(arr:MutArray(a), val:a) : Unit = intrinsic;
