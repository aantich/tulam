/// Mutable state primitives: Ref and MutArray.
///
/// These are declared as effects so that backends can provide target-specific
/// handlers (e.g., Native.tl maps them to __newref/__readref/etc. primitives).
/// The interpreter uses the default intrinsic handlers below.
module Mutable;

// ============================================================================
// Ref
// ============================================================================

/// Opaque mutable reference type.
primitive Ref(a:Type);

/// Effect for mutable reference operations.
effect MutableRef = {
    function newRef(val:a) : Ref(a);
    function readRef(r:Ref(a)) : a;
    function writeRef(r:Ref(a), val:a) : Unit;
    function modifyRef(r:Ref(a), f: a -> a) : Unit
};

/// Default interpreter handler — dispatches to intrinsics.
handler IntrinsicRef : MutableRef = default {
    function newRef(val) = intrinsic;
    function readRef(r) = intrinsic;
    function writeRef(r, val) = intrinsic;
    function modifyRef(r, f) = intrinsic
};

// ============================================================================
// MutArray
// ============================================================================

/// Opaque mutable array type.
primitive MutArray(a:Type);

/// Effect for mutable array operations.
effect MutableArray = {
    function newMutArray(n:Int, val:a) : MutArray(a);
    function mutRead(arr:MutArray(a), i:Int) : a;
    function mutWrite(arr:MutArray(a), i:Int, val:a) : Unit;
    function mutLength(arr:MutArray(a)) : Int;
    function freeze(arr:MutArray(a)) : Array(a);
    function thaw(arr:Array(a)) : MutArray(a);
    function mutPush(arr:MutArray(a), val:a) : Unit
};

/// Default interpreter handler — dispatches to intrinsics.
handler IntrinsicMutArray : MutableArray = default {
    function newMutArray(n, val) = intrinsic;
    function mutRead(arr, i) = intrinsic;
    function mutWrite(arr, i, val) = intrinsic;
    function mutLength(arr) = intrinsic;
    function freeze(arr) = intrinsic;
    function thaw(arr) = intrinsic;
    function mutPush(arr, val) = intrinsic
};
