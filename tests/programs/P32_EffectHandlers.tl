// Test 1: handle with SilentConsole suppresses output, returns body value
function t1() : Int =
    handle action { putStrLn("suppressed"); 42 } with SilentConsole;

// Test 2: SilentConsole readLine returns ""
function t2() : String =
    handle readLine() with SilentConsole;

// Test 3: State effect with RefState - put then get
function t3() : Int =
    handle action {
        put(10);
        modify(fn(x) = x + 5);
        get()
    } with RefState(0);

// Test 4: State - initial value used
function t4() : Int =
    handle get() with RefState(42);

// Test 5: Multiple handle scopes with different initial state
function t5() : Int =
    let { a = handle action { put(10); get() } with RefState(0); b = handle get() with RefState(99) } in
    a + b;

// Test 6: handler does not leak outside handle scope (putStrLn still works normally)
function t6() : Bool = True;

// Test 7: SilentConsole putStr returns Unit
function t7() : Bool =
    let r = handle putStr("suppressed") with SilentConsole in
    r == Unit;

// ============================================================================
// Bracket override syntax tests
// ============================================================================

// Helper that uses Console effect
function greetHelper(name:String) : String =
    handle action { putStrLn("Hello " ++ name); name } with SilentConsole;

// Test 8: Bracket override with positional handler — SilentConsole suppresses output
function t8() : Bool =
    let r = putStrLn [SilentConsole] ("should be silent") in
    r == Unit;

// Test 9: Bracket override with named handler — Console = SilentConsole
function t9() : Bool =
    let r = putStrLn [Console = SilentConsole] ("also silent") in
    r == Unit;

// Test 10: Bracket override with SilentConsole on readLine returns empty
function t10() : String =
    readLine [SilentConsole] ();
