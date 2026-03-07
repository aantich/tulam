/// File I/O effect: reading and writing files.
///
/// @see Effect.Console
module Effect.FileIO;

/// File system operations.
effect FileIO = {
    /// Read the entire contents of a file.
    /// @param path File path
    /// @returns File contents as a String
    function readFile(path:String) : String,

    /// Write a string to a file (overwriting existing content).
    /// @param path File path
    /// @param content Content to write
    function writeFile(path:String, content:String) : Unit,

    /// Append a string to the end of a file.
    /// @param path File path
    /// @param content Content to append
    function appendFile(path:String, content:String) : Unit,

    /// Check if a file exists.
    /// @param path File path
    /// @returns True if the file exists
    function fileExists(path:String) : Bool
};

/// Default FileIO handler mapping to intrinsic file operations.
handler StdFileIO : FileIO = {
    function readFile(path) = intrinsic,
    function writeFile(path, content) = intrinsic,
    function appendFile(path, content) = intrinsic,
    function fileExists(path) = intrinsic
};
