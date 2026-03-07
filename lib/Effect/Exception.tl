/// Exception effect: throw and catch.
///
/// @see Effect.State, Effect.Console
module Effect.Exception;

/// Exception handling operations.
effect Exception = {
    /// Throw an exception, aborting the current computation.
    /// @param err The error value
    /// @returns Never returns (type b is the continuation type)
    function throw(err:a) : b,

    /// Execute a computation with an exception handler.
    /// @param m The computation to run
    /// @param handler Function to handle thrown exceptions
    /// @returns Either the computation result or the handler result
    function catch(m:b, handler: a -> b) : b
};
