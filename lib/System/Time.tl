/// System.Time — monotonic clock and duration measurement.
///
/// Provides high-resolution timing via the Clock effect.
/// Each backend provides its own Clock handler:
///   - Interpreter: Haskell System.Clock
///   - Bytecode VM: OP_CLOCK opcode
///   - Native LLVM: __clock_nanos() C runtime call
module System.Time;

// ============================================================================
// Duration type
// ============================================================================

/// A duration of time, stored as nanoseconds.
/// Use the convenience functions to convert to other units.
type Duration = nanos:Int;

/// Extract the raw nanosecond count from a Duration.
function toNanos(d:Duration) : Int = match d | Duration(n) -> n;

/// Create a Duration from a nanosecond count.
function fromNanos(n:Int) : Duration = Duration(n);

// ============================================================================
// Clock effect
// ============================================================================

/// Monotonic clock effect.
/// Returns the current time as nanoseconds from an arbitrary epoch.
/// Monotonic: the value never decreases, even across NTP adjustments.
effect Clock = {
    function clockNanos() : Int
};

/// Default clock handler using the interpreter's built-in clock.
handler SystemClock : Clock = default {
    function clockNanos() = intrinsic
};

// ============================================================================
// Duration convenience API
// ============================================================================

/// Convert a Duration to microseconds.
function durationMicros(d:Duration) : Int = toNanos(d) / 1000;

/// Convert a Duration to milliseconds.
function durationMillis(d:Duration) : Int = toNanos(d) / 1000000;

/// Convert a Duration to seconds as a floating-point value.
function durationSeconds(d:Duration) : Float64 = toFloat(toNanos(d)) / 1000000000.0;

/// Measure the wall-clock duration of executing an action.
/// Returns a pair of (result, duration).
function timeAction(f: Unit -> a) : Pair(a, Duration) =
    let { start = clockNanos(); result = f(Unit); end = clockNanos() } in
    Pair(result, Duration(end - start));

/// Measure just the duration of an action, discarding the result.
function timeit(f: Unit -> a) : Duration =
    let { start = clockNanos(); u = f(Unit); end = clockNanos() } in
    Duration(end - start);

/// Show a Duration in human-readable form.
instance Show(Duration) = {
    function show(d:Duration) : String =
        let ns = toNanos(d) in
        if ns >= 1000000000 then show(ns / 1000000000) ++ "." ++ show(mod(ns / 100000000, 10)) ++ "s"
        else if ns >= 1000000 then show(ns / 1000000) ++ "." ++ show(mod(ns / 100000, 10)) ++ "ms"
        else if ns >= 1000 then show(ns / 1000) ++ "us"
        else show(ns) ++ "ns"
};
