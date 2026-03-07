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

/// Reference-based State handler: uses a mutable ref initialized with the given value.
/// Usage: handle action { put(10), get() } with RefState(0)
handler RefState(init:a) : State = {
    let state = newRef(init),
    function get() = readRef(state),
    function put(x) = writeRef(state, x),
    function modify(f) = modifyRef(state, f)
};
