/// Mutable state effect.
///
/// @see Effect.Console, Effect.Exception
module Effect.State;

/// Mutable state operations.
effect State = {
    /// Get the current state value.
    /// @returns The current state
    function get() : a,

    /// Replace the state with a new value.
    /// @param x The new state
    function put(x:a) : Unit,

    /// Modify the state by applying a function.
    /// @param f Transformation function
    function modify(f: a -> a) : Unit
};
