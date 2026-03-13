#!/bin/bash
# =============================================================================
# AWFY Benchmark Runner for tulam
# =============================================================================
# Runs Are We Fast Yet benchmarks across multiple backends and presents
# results in a formatted comparison table.
#
# Usage:
#   ./benchmarks/awfy/run_benchmarks.sh [--backends=cpp,hs,js,ghci,bc,native] [--iters=N]
#
# Backends:
#   cpp     — C++ reference implementation
#   hs      — Haskell (GHC -O2 compiled) reference implementation
#   js      — JavaScript (Node.js) reference implementation
#   ghci    — Haskell (runghc interpreted) reference implementation
#   bc      — tulam bytecode VM
#   native  — tulam LLVM native compilation
#
# Adding new backends:
#   1. Add a run_<backend>() function
#   2. It must output "Total Runtime: NNNus" on stdout for the harness to parse
#   3. Add the backend name to DEFAULT_BACKENDS
# =============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
AWFY_DIR="$PROJECT_ROOT/tests/awfy"
REF_JS="$SCRIPT_DIR/reference/js"
REF_CPP="$SCRIPT_DIR/reference/cpp"
REF_HS="$SCRIPT_DIR/reference/haskell"
ITERS=10

BENCHMARKS=(List Queens Permute Sieve Towers NBody Bounce Storage Mandelbrot)
DEFAULT_BACKENDS="cpp,hs,js,ghci,bc,native"

# Inner iterations for JS/C++ reference implementations.
# For most benchmarks: 1 (match tulam's single-call-per-measurement).
# For NBody and Mandelbrot: these are PROBLEM SIZE parameters, not repeat counts.
# The tulam versions have these baked in (NBody=250000 steps, Mandelbrot=500 grid).
get_inner_iters() {
  case $1 in
    NBody)      echo 250000 ;;
    Mandelbrot) echo 500 ;;
    *)          echo 1 ;;
  esac
}

# Per-benchmark iteration count.
# NBody and Mandelbrot are heavy (250000 sim steps / 500x500 grid),
# so use fewer iterations. All backends use the same count for fairness.
get_iters() {
  local bench=$1
  case $bench in
    NBody)      echo 3 ;;
    Mandelbrot) echo 3 ;;
    *)          echo $ITERS ;;
  esac
}

# Parse arguments
BACKENDS="$DEFAULT_BACKENDS"
for arg in "$@"; do
  case $arg in
    --backends=*) BACKENDS="${arg#*=}" ;;
    --iters=*)    ITERS="${arg#*=}" ;;
    --help)
      echo "Usage: $0 [--backends=cpp,hs,js,ghci,bc,native] [--iters=N]"
      exit 0 ;;
  esac
done

IFS=',' read -ra BACKEND_LIST <<< "$BACKENDS"

# =============================================================================
# Backend runners
# Each returns the average runtime in microseconds via stdout
# =============================================================================

run_cpp() {
  local bench=$1
  local iters=$2
  local inner=$(get_inner_iters "$bench")
  local harness="$REF_CPP/harness"
  if [ ! -x "$harness" ]; then
    echo "SKIP"
    return
  fi
  local output
  output=$("$harness" "$bench" "$iters" "$inner" 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

run_js() {
  local bench=$1
  local iters=$2
  if ! command -v node &> /dev/null; then
    echo "SKIP"
    return
  fi
  local output
  local inner=$(get_inner_iters "$bench")
  output=$(cd "$REF_JS" && node harness.js "$bench" "$iters" "$inner" 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

run_hs() {
  local bench=$1
  local iters=$2
  local inner=$(get_inner_iters "$bench")
  local binary="/tmp/awfy_hs_${bench}"
  local src="$REF_HS/${bench}.hs"
  if [ ! -f "$src" ]; then
    echo "SKIP"
    return
  fi
  if ! command -v ghc &> /dev/null; then
    echo "SKIP"
    return
  fi
  # Compile if needed (recompile if source is newer)
  if [ ! -x "$binary" ] || [ "$src" -nt "$binary" ]; then
    ghc -O2 -no-keep-hi-files -no-keep-o-files -o "$binary" "$src" > /dev/null 2>&1
    if [ $? -ne 0 ]; then
      echo "ERR"
      return
    fi
  fi
  local output
  output=$("$binary" "$iters" "$inner" 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

run_ghci() {
  local bench=$1
  local iters=$2
  local inner=$(get_inner_iters "$bench")
  local src="$REF_HS/${bench}.hs"
  if [ ! -f "$src" ]; then
    echo "SKIP"
    return
  fi
  if ! command -v runghc &> /dev/null; then
    echo "SKIP"
    return
  fi
  local output
  output=$(runghc "$src" "$iters" "$inner" 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

run_bc() {
  local bench=$1
  local iters=$2
  local tlfile="$AWFY_DIR/AWFY_${bench}.tl"
  if [ ! -f "$tlfile" ]; then
    echo "SKIP"
    return
  fi
  local output
  output=$(echo -e ":load lib/Backend/LLVM/Native.tl\n:load $tlfile\n:bc run main\n:quit" | \
    (cd "$PROJECT_ROOT" && stack exec tulam) 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

run_native() {
  local bench=$1
  local iters=$2
  local tlfile="$AWFY_DIR/AWFY_${bench}.tl"
  local binary="/tmp/tulam_awfy_${bench}"
  if [ ! -f "$tlfile" ]; then
    echo "SKIP"
    return
  fi
  # Compile
  echo -e ":load lib/Backend/LLVM/Native.tl\n:load $tlfile\n:compile native main\n:quit" | \
    (cd "$PROJECT_ROOT" && stack exec tulam) > /dev/null 2>&1
  if [ ! -x /tmp/tulam_native ]; then
    echo "ERR"
    return
  fi
  cp /tmp/tulam_native "$binary"
  local output
  output=$("$binary" 2>&1)
  local total
  total=$(echo "$output" | grep "^Total Runtime:" | head -1 | sed 's/[^0-9]//g')
  if [ -n "$total" ] && [ "$total" -gt 0 ] 2>/dev/null; then
    echo $(( total / iters ))
  else
    echo "ERR"
  fi
}

# =============================================================================
# Table formatting
# =============================================================================

# Column widths
COL_BENCH=14
COL_RESULT=12

pad_right() {
  printf "%-${1}s" "$2"
}

pad_left() {
  printf "%${1}s" "$2"
}

format_us() {
  local us=$1
  if [ "$us" = "SKIP" ] || [ "$us" = "ERR" ]; then
    echo "$us"
    return
  fi
  if [ "$us" -ge 1000000 ]; then
    echo "$(( us / 1000000 )).$(( (us / 100000) % 10 ))s"
  elif [ "$us" -ge 1000 ]; then
    echo "$(( us / 1000 )).$(( (us / 100) % 10 ))ms"
  else
    echo "${us}us"
  fi
}

# =============================================================================
# Main
# =============================================================================

echo ""
echo "===== AWFY Benchmark Suite ====="
echo "Iterations: $ITERS per benchmark"
echo "Backends:   ${BACKEND_LIST[*]}"
echo ""

# Build C++ harness if needed
for b in "${BACKEND_LIST[@]}"; do
  if [ "$b" = "cpp" ] && [ ! -x "$REF_CPP/harness" ]; then
    echo "Building C++ harness..."
    (cd "$REF_CPP" && clang++ -std=c++17 -O3 -flto -march=native -ffp-contract=off \
      src/harness.cpp src/deltablue.cpp src/memory/object_tracker.cpp src/richards.cpp \
      -o harness 2>&1) || echo "C++ build failed"
  fi
done

# Results file for ratio computation
RESULTS_FILE=$(mktemp /tmp/awfy_results.XXXXXX)
trap "rm -f $RESULTS_FILE /tmp/tulam_awfy_*" EXIT

# Comparison ratio columns: tulam backend / reference backend
# Only shown when both sides are present in BACKEND_LIST
RATIO_PAIRS=()
RATIO_LABELS=()
contains_backend() {
  local needle=$1
  for b in "${BACKEND_LIST[@]}"; do [ "$b" = "$needle" ] && return 0; done
  return 1
}
if contains_backend "native" && contains_backend "cpp"; then
  RATIO_PAIRS+=("native:cpp"); RATIO_LABELS+=("nat/cpp")
fi
if contains_backend "native" && contains_backend "hs"; then
  RATIO_PAIRS+=("native:hs"); RATIO_LABELS+=("nat/hs")
fi
if contains_backend "native" && contains_backend "js"; then
  RATIO_PAIRS+=("native:js"); RATIO_LABELS+=("nat/js")
fi
if contains_backend "bc" && contains_backend "ghci"; then
  RATIO_PAIRS+=("bc:ghci"); RATIO_LABELS+=("bc/ghci")
fi

COL_RATIO=10

# Helper: compute ratio string "N.Mx" from two microsecond values
format_ratio() {
  local num=$1
  local den=$2
  if [ "$num" = "SKIP" ] || [ "$num" = "ERR" ] || \
     [ "$den" = "SKIP" ] || [ "$den" = "ERR" ]; then
    echo "-"
    return
  fi
  if ! [ "$den" -gt 0 ] 2>/dev/null; then
    echo "-"
    return
  fi
  local ratio100=$(( num * 100 / den ))
  local whole=$(( ratio100 / 100 ))
  local frac=$(( (ratio100 / 10) % 10 ))
  echo "${whole}.${frac}x"
}

# Print header
printf "$(pad_right $COL_BENCH "Benchmark")"
for b in "${BACKEND_LIST[@]}"; do
  printf "  $(pad_left $COL_RESULT "$b")"
done
if [ ${#RATIO_PAIRS[@]} -gt 0 ]; then
  printf "  │"
  for label in "${RATIO_LABELS[@]}"; do
    printf "  $(pad_left $COL_RATIO "$label")"
  done
fi
echo ""
printf "%s" "$(pad_right $COL_BENCH "--------------")"
for b in "${BACKEND_LIST[@]}"; do
  printf "  %s" "$(pad_left $COL_RESULT "------------")"
done
if [ ${#RATIO_PAIRS[@]} -gt 0 ]; then
  printf "  │"
  for _ in "${RATIO_LABELS[@]}"; do
    printf "  %s" "$(pad_left $COL_RATIO "----------")"
  done
fi
echo ""

# Run benchmarks
for bench in "${BENCHMARKS[@]}"; do
  printf "$(pad_right $COL_BENCH "$bench")"
  for b in "${BACKEND_LIST[@]}"; do
    bench_iters=$(get_iters "$bench")
    us=$(run_$b "$bench" "$bench_iters" 2>/dev/null || echo "ERR")
    echo "${bench} ${b} ${us}" >> "$RESULTS_FILE"
    formatted=$(format_us "$us")
    printf "  $(pad_left $COL_RESULT "$formatted")"
  done
  # Ratio columns
  if [ ${#RATIO_PAIRS[@]} -gt 0 ]; then
    printf "  │"
    for pair in "${RATIO_PAIRS[@]}"; do
      local_num="${pair%%:*}"
      local_den="${pair##*:}"
      num_us=$(grep "^${bench} ${local_num} " "$RESULTS_FILE" | awk '{print $3}')
      den_us=$(grep "^${bench} ${local_den} " "$RESULTS_FILE" | awk '{print $3}')
      ratio=$(format_ratio "$num_us" "$den_us")
      printf "  $(pad_left $COL_RATIO "$ratio")"
    done
  fi
  echo ""
done

# Separator
printf "%s" "$(pad_right $COL_BENCH "--------------")"
for b in "${BACKEND_LIST[@]}"; do
  printf "  %s" "$(pad_left $COL_RESULT "------------")"
done
if [ ${#RATIO_PAIRS[@]} -gt 0 ]; then
  printf "  │"
  for _ in "${RATIO_LABELS[@]}"; do
    printf "  %s" "$(pad_left $COL_RATIO "----------")"
  done
fi
echo ""

# Print geomean ratio row (relative to first backend)
ref_backend="${BACKEND_LIST[0]}"
printf "$(pad_right $COL_BENCH "vs $ref_backend")"
for b in "${BACKEND_LIST[@]}"; do
  if [ "$b" = "$ref_backend" ]; then
    printf "  $(pad_left $COL_RESULT "1.0x")"
  else
    sum_ratio=0
    count=0
    for bench in "${BENCHMARKS[@]}"; do
      ref_us=$(grep "^${bench} ${ref_backend} " "$RESULTS_FILE" | awk '{print $3}')
      cur_us=$(grep "^${bench} ${b} " "$RESULTS_FILE" | awk '{print $3}')
      if [ -n "$ref_us" ] && [ -n "$cur_us" ] && \
         [ "$ref_us" != "SKIP" ] && [ "$ref_us" != "ERR" ] && \
         [ "$cur_us" != "SKIP" ] && [ "$cur_us" != "ERR" ] && \
         [ "$ref_us" -gt 0 ] 2>/dev/null && [ "$cur_us" -gt 0 ] 2>/dev/null; then
        ratio10=$(( cur_us * 10 / ref_us ))
        sum_ratio=$(( sum_ratio + ratio10 ))
        count=$(( count + 1 ))
      fi
    done
    if [ "$count" -gt 0 ]; then
      avg10=$(( sum_ratio / count ))
      whole=$(( avg10 / 10 ))
      frac=$(( avg10 % 10 ))
      printf "  $(pad_left $COL_RESULT "${whole}.${frac}x")"
    else
      printf "  $(pad_left $COL_RESULT "N/A")"
    fi
  fi
done
# Average ratio columns
if [ ${#RATIO_PAIRS[@]} -gt 0 ]; then
  printf "  │"
  for pair in "${RATIO_PAIRS[@]}"; do
    local_num="${pair%%:*}"
    local_den="${pair##*:}"
    sum_ratio=0
    count=0
    for bench in "${BENCHMARKS[@]}"; do
      num_us=$(grep "^${bench} ${local_num} " "$RESULTS_FILE" | awk '{print $3}')
      den_us=$(grep "^${bench} ${local_den} " "$RESULTS_FILE" | awk '{print $3}')
      if [ -n "$num_us" ] && [ -n "$den_us" ] && \
         [ "$num_us" != "SKIP" ] && [ "$num_us" != "ERR" ] && \
         [ "$den_us" != "SKIP" ] && [ "$den_us" != "ERR" ] && \
         [ "$den_us" -gt 0 ] 2>/dev/null && [ "$num_us" -gt 0 ] 2>/dev/null; then
        ratio10=$(( num_us * 10 / den_us ))
        sum_ratio=$(( sum_ratio + ratio10 ))
        count=$(( count + 1 ))
      fi
    done
    if [ "$count" -gt 0 ]; then
      avg10=$(( sum_ratio / count ))
      whole=$(( avg10 / 10 ))
      frac=$(( avg10 % 10 ))
      printf "  $(pad_left $COL_RATIO "${whole}.${frac}x")"
    else
      printf "  $(pad_left $COL_RATIO "N/A")"
    fi
  done
fi
echo ""
echo ""
