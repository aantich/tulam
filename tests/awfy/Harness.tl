module Harness;

/// Helper: show an Int as a String (avoids implicit-param dispatch issues).
function showInt(n:Int) : String = show(n);

/// Run a single iteration of a benchmark, printing the result in AWFY format.
/// Returns elapsed time in microseconds.
function benchOnce(name:String, result:a, elapsed:Int, iter:Int) : Int =
    let {
        u = putStrLn(name ++ ": iterations=1 runtime: " ++ showInt(elapsed) ++ "us")
    } in elapsed;

/// Print the summary line after all iterations.
function benchSummary(name:String, totalUs:Int, numIters:Int) : Unit =
    let {
        avg = if numIters > 0 then div(totalUs, numIters) else 0;
        u1 = putStrLn(name ++ ": iterations=" ++ showInt(numIters) ++ " average: " ++ showInt(avg) ++ "us total: " ++ showInt(totalUs) ++ "us");
        u2 = putStrLn("");
        u3 = putStrLn("Total Runtime: " ++ showInt(totalUs) ++ "us")
    } in Unit;
