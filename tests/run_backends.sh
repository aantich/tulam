#!/bin/bash
# Multi-backend test runner for tulam
# Compiles ONCE per backend, runs all tests in a single execution.
#
# Usage: ./tests/run_backends.sh [test_file.tl]

set -euo pipefail

RUN_TIMEOUT="${RUN_TIMEOUT:-30}"
COMPILE_TIMEOUT="${COMPILE_TIMEOUT:-60}"

# cd to project root regardless of where script is invoked from
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

TULAM="stack exec tulam --"
TEST_FILE="${1:-tests/programs/BCTest_Core.tl}"
NATIVE_BIN="/tmp/tulam_native"

BOLD='\033[1m'
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

run_with_timeout() {
    local secs="$1"
    shift
    if command -v timeout >/dev/null 2>&1; then
        timeout "${secs}s" "$@"
    elif command -v gtimeout >/dev/null 2>&1; then
        gtimeout "${secs}s" "$@"
    else
        "$@"
    fi
}

print_failure_excerpt() {
    local label="$1"
    local text="$2"
    echo -e "${RED}${label}${NC}"
    echo "$text" | grep -E 'Compile error:|Runtime error:|error:|Error:|Monomorphization failed|unresolved implicit dispatch|Type error:|command not found' | head -20 || true
}

echo -e "${BOLD}=== Multi-Backend Test Runner ===${NC}"
echo -e "Test file: $TEST_FILE"
echo -e "Timeouts: compile ${COMPILE_TIMEOUT}s, run ${RUN_TIMEOUT}s"
echo ""

# ─── Bytecode Backend ───
echo -e "${BOLD}--- Bytecode VM ---${NC}"
echo "Running :bc run run_all ..."
set +e
BC_OUT=$(printf ':load %s\n:bc run run_all\n:quit\n' "$TEST_FILE" | run_with_timeout "$RUN_TIMEOUT" stack exec tulam -- 2>&1)
BC_STATUS=$?
set -e
BC_CLEAN=$(echo "$BC_OUT" | sed $'s/\x1b\[[0-9;]*m//g')

if [ "$BC_STATUS" -eq 124 ]; then
    echo -e "${RED}TIMEOUT after ${RUN_TIMEOUT}s during bytecode run${NC}"
    print_failure_excerpt "Last bytecode diagnostics:" "$BC_CLEAN"
    BC_PASS=0
    BC_FAIL=0
else
    if echo "$BC_CLEAN" | grep -q 'Runtime error:'; then
        BC_ERR=$(echo "$BC_CLEAN" | grep 'Runtime error:' | head -1)
        echo -e "${RED}CRASHED: $BC_ERR${NC}"
    fi
    if echo "$BC_CLEAN" | grep -q 'Compile error:'; then
        print_failure_excerpt "Bytecode compile failure:" "$BC_CLEAN"
    fi

    BC_RESULTS=$(echo "$BC_CLEAN" | grep -E '^\s+(PASS|FAIL):' || true)
    echo "$BC_RESULTS"
    BC_PASS=$(echo "$BC_RESULTS" | grep -c 'PASS:' || true)
    BC_FAIL=$(echo "$BC_RESULTS" | grep -c 'FAIL:' || true)
fi
echo ""
echo -e "Bytecode: ${GREEN}$BC_PASS PASS${NC}, ${RED}$BC_FAIL FAIL${NC}"
echo ""

# ─── Native Backend ───
echo -e "${BOLD}--- Native (LLVM) ---${NC}"
echo "Running :compile native run_all ..."
set +e
NAT_OUT=$(printf ':load %s\n:compile native run_all\n:quit\n' "$TEST_FILE" | run_with_timeout "$COMPILE_TIMEOUT" stack exec tulam -- 2>&1)
NAT_STATUS=$?
set -e
NAT_CLEAN=$(echo "$NAT_OUT" | sed $'s/\x1b\[[0-9;]*m//g')

if [ "$NAT_STATUS" -eq 124 ]; then
    echo -e "${RED}TIMEOUT after ${COMPILE_TIMEOUT}s during native compilation${NC}"
    print_failure_excerpt "Last native diagnostics:" "$NAT_CLEAN"
    NAT_PASS=0
    NAT_FAIL=0
elif echo "$NAT_CLEAN" | grep -q 'Compiled:'; then
    set +e
    NAT_RUN=$(run_with_timeout "$RUN_TIMEOUT" "$NATIVE_BIN" 2>&1)
    NAT_RUN_STATUS=$?
    set -e
    if [ "$NAT_RUN_STATUS" -eq 124 ]; then
        echo -e "${RED}TIMEOUT after ${RUN_TIMEOUT}s while running native binary${NC}"
        print_failure_excerpt "Last native runtime diagnostics:" "$NAT_RUN"
        NAT_PASS=0
        NAT_FAIL=0
    else
        NAT_RESULTS=$(echo "$NAT_RUN" | grep -E '^\s+(PASS|FAIL):' || true)
        echo "$NAT_RESULTS"
        NAT_PASS=$(echo "$NAT_RESULTS" | grep -c 'PASS:' || true)
        NAT_FAIL=$(echo "$NAT_RESULTS" | grep -c 'FAIL:' || true)
        echo ""
        echo -e "Native:   ${GREEN}$NAT_PASS PASS${NC}, ${RED}$NAT_FAIL FAIL${NC}"
    fi
else
    print_failure_excerpt "Native compilation failed:" "$NAT_CLEAN"
    NAT_PASS=0
    NAT_FAIL=0
fi

# ─── Summary Table ───
echo ""
echo -e "${BOLD}=== Summary ===${NC}"
printf "%-15s %10s %10s\n" "Backend" "PASS" "FAIL"
printf "%-15s %10s %10s\n" "───────────" "──────" "──────"
printf "%-15s ${GREEN}%10s${NC} ${RED}%10s${NC}\n" "Bytecode" "$BC_PASS" "$BC_FAIL"
printf "%-15s ${GREEN}%10s${NC} ${RED}%10s${NC}\n" "Native" "$NAT_PASS" "$NAT_FAIL"

# Cleanup
rm -f $NATIVE_BIN 2>/dev/null
