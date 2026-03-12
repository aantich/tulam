// P18: Managed Mutability Tests
// Tests for Ref and MutArray intrinsic operations

// Ref tests

// basic newRef + readRef
function testRefRead() : Int =
    let r = newRef(42) in readRef(r);

// writeRef overwrites value
function testRefWrite() : Int =
    let { r = newRef(10); u = writeRef(r, 99) } in
    readRef(r);

// modifyRef applies function
function testRefModify() : Int =
    let { r = newRef(5); u = modifyRef(r, fn(x) = x + 10) } in
    readRef(r);

// two refs are independent
function testRefIndependent() : Int =
    let { r1 = newRef(1); r2 = newRef(2); u = writeRef(r1, 100) } in
    readRef(r2);

// MutArray tests

// newMutArray + mutRead
function testMutArrayRead() : Int =
    let a = newMutArray(3, 0) in mutRead(a, 1);

// mutWrite + mutRead
function testMutArrayWrite() : Int =
    let { a = newMutArray(3, 0); u = mutWrite(a, 2, 42) } in
    mutRead(a, 2);

// mutLength
function testMutArrayLength() : Int =
    let a = newMutArray(5, 0) in mutLength(a);

// freeze produces immutable array
function testFreeze() : Array(Int) =
    let { a = newMutArray(3, 0); u = mutWrite(a, 0, 1); u2 = mutWrite(a, 1, 2); u3 = mutWrite(a, 2, 3) } in
    freeze(a);

// thaw + mutWrite
function testThaw() : Int =
    let { a = thaw([10, 20, 30]); u = mutWrite(a, 1, 99) } in
    mutRead(a, 1);

// mutPush extends array
function testMutPush() : Int =
    let { a = newMutArray(2, 0); u = mutPush(a, 42) } in
    mutLength(a);

// ref in closure
function testRefClosure() : Int =
    let { r = newRef(0); inc = fn(x) = modifyRef(r, fn(v) = v + x); u1 = inc(3); u2 = inc(7) } in
    readRef(r);
