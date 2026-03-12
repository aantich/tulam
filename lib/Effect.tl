/// Effect declarations and handlers: Sequence, Console, FileIO, Exception, State.
module Effect;

// ============================================================================
// Sequence — core effect sequencing
// ============================================================================

/// Sequential execution of effectful computations.
/// In strict evaluation, seq(a, f) = f(a) — evaluate a, pass result to f.
/// bind is identical to seq; both exist for clarity in desugared action blocks.
/// Lazy(a) values are handled naturally: delay() creates a thunk value in strict
/// eval, and force() is called explicitly where needed — seq itself never forces.
effect Sequence = {
    function effectSeq(a:a, f:a -> b) : b;
    function effectBind(a:a, f:a -> b) : b
};

/// Default strict sequencing handler.
/// Used by the interpreter when no other handler is active.
/// StrictSequence is NOT marked as default because effectBind/effectSeq
/// dispatch is handled specially by the interpreter (supports Monad instances).
/// Only use as explicit handler when overriding sequencing behavior.
handler StrictSequence : Sequence = {
    function effectSeq(a, f) = f(a);
    function effectBind(a, f) = f(a)
};

// ============================================================================
// Console
// ============================================================================

effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit;
    function putStr(s:String) : Unit
};

handler StdConsole : Console = default {
    function readLine() = intrinsic;
    function putStrLn(s) = intrinsic;
    function putStr(s) = intrinsic
};

handler SilentConsole : Console = {
    function readLine() = "";
    function putStrLn(s) = Unit;
    function putStr(s) = Unit
};

// ============================================================================
// FileIO
// ============================================================================

effect FileIO = {
    function readFile(path:String) : String;
    function writeFile(path:String, content:String) : Unit;
    function appendFile(path:String, content:String) : Unit;
    function fileExists(path:String) : Bool
};

handler StdFileIO : FileIO = default {
    function readFile(path) = intrinsic;
    function writeFile(path, content) = intrinsic;
    function appendFile(path, content) = intrinsic;
    function fileExists(path) = intrinsic
};

// ============================================================================
// Exception
// ============================================================================

effect Exception = {
    function throw(err:a) : b;
    function catch(m:b, h: a -> b) : b
};

handler DefaultException : Exception = default {
    function throw(err) = error(show(err));
    function catch(m, h) = m
};

// ============================================================================
// State
// ============================================================================

effect State = {
    function get() : a;
    function put(x:a) : Unit;
    function modify(f: a -> a) : Unit
};

handler RefState(init:a) : State = {
    let state = newRef(init);
    function get() = readRef(state);
    function put(x) = writeRef(state, x);
    function modify(f) = modifyRef(state, f)
};
