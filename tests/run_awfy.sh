#!/bin/bash
# AWFY Benchmark Suite: Bytecode VM vs Native LLVM
#
# Runs all Are-We-Fast-Yet benchmarks on both backends and produces
# a comparison table. Each benchmark has its own self-timing via clockNanos.
#
# Usage: ./tests/run_awfy.sh

set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

TULAM="stack exec tulam --"
NATIVE_BIN="/tmp/tulam_native"

BOLD='\033[1m'
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'

# AWFY benchmarks
BENCHMARKS="Sieve Queens Bounce Permute Storage Towers List Mandelbrot NBody"

echo -e "${BOLD}=== AWFY Benchmark Suite: Bytecode VM vs Native LLVM ===${NC}"
echo ""

# --- Header ---
printf "${BOLD}%-14s %14s %14s %10s${NC}\n" "Benchmark" "Bytecode VM" "Native LLVM" "Speedup"
printf "%-14s %14s %14s %10s\n" "──────────────" "──────────────" "──────────────" "──────────"

for bench in $BENCHMARKS; do
    file="tests/awfy/AWFY_${bench}.tl"

    # --- Bytecode ---
    BC_OUT=$(printf ':load %s\n:bc run main\n:quit\n' "$file" | $TULAM 2>&1)
    bc_avg=$(echo "$BC_OUT" | grep 'average:' | head -1 | sed 's/.*average: \([0-9]*\)us.*/\1/' || echo "0")
    [ -z "$bc_avg" ] && bc_avg=0

    # --- Native ---
    NAT_COMPILE=$(printf ':load %s\n:compile native main\n:quit\n' "$file" | $TULAM 2>&1)
    if echo "$NAT_COMPILE" | grep -q 'Compiled:'; then
        NAT_OUT=$($NATIVE_BIN 2>&1)
        nat_avg=$(echo "$NAT_OUT" | grep 'average:' | head -1 | sed 's/.*average: \([0-9]*\)us.*/\1/' || echo "0")
        [ -z "$nat_avg" ] && nat_avg=0
    else
        nat_avg=0
    fi

    # --- Format times ---
    format_us() {
        local us=$1
        if [ "$us" -ge 1000000 ]; then
            awk "BEGIN { printf \"%.2f s\", $us / 1000000 }"
        elif [ "$us" -ge 1000 ]; then
            awk "BEGIN { printf \"%.1f ms\", $us / 1000 }"
        else
            echo "${us} us"
        fi
    }

    bc_str=$(format_us "$bc_avg")
    if [ "$nat_avg" -gt 0 ]; then
        nat_str=$(format_us "$nat_avg")
    else
        nat_str="FAILED"
    fi

    # --- Speedup ---
    if [ "$nat_avg" -gt 0 ]; then
        speedup=$(awk "BEGIN { printf \"%.0fx\", $bc_avg / $nat_avg }")
        color="$GREEN"
    else
        speedup="--"
        color="$RED"
    fi

    printf "%-14s %14s %14s ${color}%10s${NC}\n" "$bench" "$bc_str" "$nat_str" "$speedup"
done

echo ""
echo -e "${BOLD}Times are per-iteration averages${NC}, self-measured via clockNanos()."
echo "Both backends exclude startup/loading overhead."

# Cleanup
rm -f "$NATIVE_BIN" 2>/dev/null
