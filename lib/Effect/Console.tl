/// Console I/O effect: reading input and printing output.
///
/// @see Effect.FileIO, Effect.State
module Effect.Console;

/// Standard console I/O operations.
effect Console = {
    /// Read a line from standard input.
    /// @returns The line read (without trailing newline)
    function readLine() : String,

    /// Print a string followed by a newline to standard output.
    /// @param s The string to print
    function putStrLn(s:String) : Unit,

    /// Print a string to standard output (no trailing newline).
    /// @param s The string to print
    function putStr(s:String) : Unit
};

/// Default handler mapping Console operations to intrinsic I/O.
handler StdConsole : Console = {
    function readLine() = intrinsic,
    function putStrLn(s) = intrinsic,
    function putStr(s) = intrinsic
};
