/// Converting values to human-readable string representations.
///
/// The Show algebra provides the ability to convert any value of a type
/// into a String representation. Almost every type should have a Show instance.
///
/// Convention: output should be valid tulam syntax when possible.
///
/// @see Instances.Show
module Algebra.Show;

/// String conversion algebra.
algebra Show(a:Type) = {
    /// Convert a value to its string representation.
    /// @param x The value to show
    /// @returns String representation
    /// @example show(42) => "42"
    /// @example show(True) => "True"
    function show(x:a) : String,

    /// Automatic structural show derivation using reflection.
    /// Usage: instance Show(MyType) = derive;
    derive {
        function show(x:a) : String = structuralShow(x)
    }
};
