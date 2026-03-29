#!/bin/bash
# Benchmark: Native (LLVM) vs Bytecode VM
#
# Runs each benchmark N iterations and reports average wall-clock times.
# Usage: ./tests/bench_backends.sh [iterations]

set -euo pipefail

ITERS="${1:-3}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

TULAM="stack exec tulam --"
NATIVE_BIN="/tmp/tulam_bench_native"

BOLD='\033[1m'
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
YELLOW='\033[0;33m'
NC='\033[0m'

# --- Benchmarks: each is (name, file, entry_function) ---
# We create a dedicated benchmark .tl file that wraps expensive computations
# in loops to get measurable times.

BENCH_FILE="/tmp/tulam_perf_bench.tl"
cat > "$BENCH_FILE" << 'TULAM_EOF'
/// Performance benchmarks for native vs bytecode comparison.
/// Each benchmark runs a tight loop to get measurable time.

/// --- Arithmetic: sum 1..N ---
function bench_sum(n:Int, acc:Int) : Int =
    if n <= 0 then acc else bench_sum(n - 1, acc + n);

function bench_arith_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_arith_loop(i - 1, acc + bench_sum(1000, 0));

function bench_arithmetic() : Int = bench_arith_loop(2000, 0);

/// --- Fibonacci (exponential, stresses recursion + branching) ---
function bench_fib(n:Int) : Int =
    if n <= 1 then n else bench_fib(n - 1) + bench_fib(n - 2);

function bench_fibonacci() : Int = bench_fib(32);

/// --- List operations (stresses allocation + pattern matching) ---
function bench_make_list(n:Int) : List(Int) =
    if n <= 0 then Nil else Cons(n, bench_make_list(n - 1));

function bench_list_sum(xs:List(Int)) : Int = match xs
    | Nil -> 0
    | Cons(h, t) -> h + bench_list_sum(t);

function bench_list_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_list_loop(i - 1, acc + bench_list_sum(bench_make_list(500)));

function bench_lists() : Int = bench_list_loop(1000, 0);

/// --- Higher-order functions (stresses closures) ---
function bench_apply_n(f:Int -> Int, n:Int, x:Int) : Int =
    if n <= 0 then x else bench_apply_n(f, n - 1, f(x));

function bench_closure_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_closure_loop(i - 1, acc + bench_apply_n(fn(x) = x + i, 500, 0));

function bench_closures() : Int = bench_closure_loop(1000, 0);

/// --- Pattern matching (stresses tag dispatch) ---
type BenchTree = BLeaf * val:Int + BBranch * left:BenchTree * right:BenchTree;

function bench_build_tree(depth:Int) : BenchTree =
    if depth <= 0 then BLeaf(1)
    else BBranch(bench_build_tree(depth - 1), bench_build_tree(depth - 1));

function bench_tree_sum(t:BenchTree) : Int = match t
    | BLeaf(v) -> v
    | BBranch(l, r) -> bench_tree_sum(l) + bench_tree_sum(r);

function bench_tree_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_tree_loop(i - 1, acc + bench_tree_sum(bench_build_tree(15)));

function bench_trees() : Int = bench_tree_loop(100, 0);

/// --- Nat recursion (stresses structural recursion + null/non-null dispatch) ---
function bench_nat_add(a:Nat, b:Nat) : Nat = match a
    | Z -> b
    | Succ(n) -> Succ(bench_nat_add(n, b));

function bench_nat_to_int(n:Nat) : Int = match n
    | Z -> 0
    | Succ(m) -> 1 + bench_nat_to_int(m);

function bench_make_nat(n:Int) : Nat =
    if n <= 0 then Z else Succ(bench_make_nat(n - 1));

function bench_nat_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_nat_loop(i - 1, acc + bench_nat_to_int(bench_nat_add(bench_make_nat(200), bench_make_nat(200))));

function bench_nats() : Int = bench_nat_loop(200, 0);

/// --- String operations ---
function bench_string_build(n:Int, acc:String) : String =
    if n <= 0 then acc
    else bench_string_build(n - 1, acc ++ "x");

function bench_string_loop(i:Int, acc:Int) : Int =
    if i <= 0 then acc
    else bench_string_loop(i - 1, acc + length(bench_string_build(500, "")));

function bench_strings() : Int = bench_string_loop(200, 0);

/// --- Individual entry points for shell-level timing ---
function run_bench_arithmetic() : Unit = action { let { r = bench_arithmetic() } in putStrLn(show(r)); };
function run_bench_fibonacci()  : Unit = action { let { r = bench_fibonacci() } in putStrLn(show(r)); };
function run_bench_lists()      : Unit = action { let { r = bench_lists() } in putStrLn(show(r)); };
function run_bench_closures()   : Unit = action { let { r = bench_closures() } in putStrLn(show(r)); };
function run_bench_trees()      : Unit = action { let { r = bench_trees() } in putStrLn(show(r)); };
function run_bench_nats()       : Unit = action { let { r = bench_nats() } in putStrLn(show(r)); };
function run_bench_strings()    : Unit = action { let { r = bench_strings() } in putStrLn(show(r)); };

/// Dummy entry for initial compile
function run_bench() : Unit = action { putStrLn("bench"); };
TULAM_EOF

echo -e "${BOLD}=== tulam Backend Performance Benchmark ===${NC}"
echo -e "Iterations: ${CYAN}$ITERS${NC}"
echo ""

# Timing helper (returns milliseconds)
time_ms() {
    local start end
    start=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
    eval "$@" > /dev/null 2>&1
    end=$(python3 -c 'import time; print(int(time.monotonic_ns()))')
    echo $(( (end - start) / 1000000 ))
}

# --- Measure bytecode startup baseline (load + quit, no computation) ---
echo -ne "Measuring bytecode startup baseline... "
BC_BASE_TOTAL=0
for i in $(seq 1 3); do
    ms=$(time_ms "printf ':load %s\n:quit\n' '$BENCH_FILE' | $TULAM")
    BC_BASE_TOTAL=$((BC_BASE_TOTAL + ms))
done
BC_BASELINE=$((BC_BASE_TOTAL / 3))
echo -e "${CYAN}${BC_BASELINE} ms${NC}"

# --- Measure native startup baseline ---
# Compile a no-op entry for baseline
printf ':load %s\n:compile native run_bench\n:quit\n' "$BENCH_FILE" | $TULAM > /dev/null 2>&1
echo -ne "Measuring native startup baseline... "
NAT_BASE_TOTAL=0
for i in $(seq 1 3); do
    ms=$(time_ms "$NATIVE_BIN")
    NAT_BASE_TOTAL=$((NAT_BASE_TOTAL + ms))
done
NAT_BASELINE=$((NAT_BASE_TOTAL / 3))
echo -e "${CYAN}${NAT_BASELINE} ms${NC}"
echo ""

BENCHMARKS="arithmetic fibonacci lists closures trees nats strings"

printf "${BOLD}%-16s %14s %14s %10s${NC}\n" "Benchmark" "Bytecode VM" "Native LLVM" "Speedup"
printf "%-16s %14s %14s %10s\n" "────────────────" "──────────────" "──────────────" "──────────"

for bench in $BENCHMARKS; do
    entry="run_bench_${bench}"

    # Compile native for this entry point
    printf ':load %s\n:compile native %s\n:quit\n' "$BENCH_FILE" "$entry" | $TULAM > /dev/null 2>&1

    # Bytecode timing (subtract baseline)
    bc_total=0
    for i in $(seq 1 "$ITERS"); do
        ms=$(time_ms "printf ':load %s\n:bc run %s\n:quit\n' '$BENCH_FILE' '$entry' | $TULAM")
        adjusted=$((ms - BC_BASELINE))
        [ "$adjusted" -lt 1 ] && adjusted=1
        bc_total=$((bc_total + adjusted))
    done
    bc_avg=$((bc_total / ITERS))

    # Native timing (subtract baseline)
    nat_total=0
    for i in $(seq 1 "$ITERS"); do
        ms=$(time_ms "$NATIVE_BIN")
        adjusted=$((ms - NAT_BASELINE))
        [ "$adjusted" -lt 1 ] && adjusted=1
        nat_total=$((nat_total + adjusted))
    done
    nat_avg=$((nat_total / ITERS))

    # Format times
    format_time() {
        local ms=$1
        if [ "$ms" -ge 1000 ]; then
            awk "BEGIN { printf \"%.2f s\", $ms / 1000 }"
        else
            echo "${ms} ms"
        fi
    }
    bc_str=$(format_time "$bc_avg")
    nat_str=$(format_time "$nat_avg")

    # Speedup
    if [ "$nat_avg" -gt 0 ]; then
        speedup=$(awk "BEGIN { printf \"%.1fx\", $bc_avg / $nat_avg }")
    else
        speedup="--"
    fi

    if [ "$nat_avg" -lt "$bc_avg" ]; then
        color="$GREEN"
    else
        color="$RED"
    fi

    printf "%-16s %14s %14s ${color}%10s${NC}\n" "$bench" "$bc_str" "$nat_str" "$speedup"
done

echo ""
echo -e "Baselines subtracted: bytecode=${BC_BASELINE}ms (startup+load), native=${NAT_BASELINE}ms (process start)"

# Cleanup
rm -f "$NATIVE_BIN" "$BENCH_FILE" 2>/dev/null
