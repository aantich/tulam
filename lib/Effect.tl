/// Effect declarations and handlers: Console, FileIO, Exception, State.
module Effect;

// ============================================================================
// Console
// ============================================================================

effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit;
    function putStr(s:String) : Unit
};

handler StdConsole : Console = {
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

handler StdFileIO : FileIO = {
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

handler DefaultException : Exception = {
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
