#!/bin/bash
# Multi-backend test runner for tulam
# Compiles ONCE per backend, runs all tests in a single execution.
#
# Usage: ./tests/run_backends.sh [test_file.tl]

set -uo pipefail

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

echo -e "${BOLD}=== Multi-Backend Test Runner ===${NC}"
echo -e "Test file: $TEST_FILE"
echo ""

# ─── Bytecode Backend ───
echo -e "${BOLD}--- Bytecode VM ---${NC}"
BC_OUT=$(echo ":load $TEST_FILE
:bc run run_all
:quit" | $TULAM 2>&1 || true)
BC_CLEAN=$(echo "$BC_OUT" | sed $'s/\x1b\[[0-9;]*m//g')

# Check for crash
if echo "$BC_CLEAN" | grep -q 'Runtime error:'; then
    BC_ERR=$(echo "$BC_CLEAN" | grep 'Runtime error:' | head -1)
    echo -e "${RED}CRASHED: $BC_ERR${NC}"
fi

# Extract PASS/FAIL lines
BC_RESULTS=$(echo "$BC_CLEAN" | grep -E '^\s+(PASS|FAIL):' || true)
echo "$BC_RESULTS"
BC_PASS=$(echo "$BC_RESULTS" | grep -c 'PASS:' || true)
BC_FAIL=$(echo "$BC_RESULTS" | grep -c 'FAIL:' || true)
echo ""
echo -e "Bytecode: ${GREEN}$BC_PASS PASS${NC}, ${RED}$BC_FAIL FAIL${NC}"
echo ""

# ─── Native Backend ───
echo -e "${BOLD}--- Native (LLVM) ---${NC}"
NAT_OUT=$(echo ":load $TEST_FILE
:compile native run_all
:quit" | $TULAM 2>&1 || true)
NAT_CLEAN=$(echo "$NAT_OUT" | sed $'s/\x1b\[[0-9;]*m//g')

if echo "$NAT_CLEAN" | grep -q 'Compiled:'; then
    # Run the compiled binary
    NAT_RUN=$($NATIVE_BIN 2>&1 || true)
    NAT_RESULTS=$(echo "$NAT_RUN" | grep -E '^\s+(PASS|FAIL):' || true)
    echo "$NAT_RESULTS"
    NAT_PASS=$(echo "$NAT_RESULTS" | grep -c 'PASS:' || true)
    NAT_FAIL=$(echo "$NAT_RESULTS" | grep -c 'FAIL:' || true)
    echo ""
    echo -e "Native:   ${GREEN}$NAT_PASS PASS${NC}, ${RED}$NAT_FAIL FAIL${NC}"
else
    NAT_ERR=$(echo "$NAT_CLEAN" | grep -E '(Error:|error:)' | head -3)
    echo -e "${RED}Compilation failed:${NC}"
    echo "$NAT_ERR"
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
