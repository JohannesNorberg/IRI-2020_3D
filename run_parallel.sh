#!/bin/bash
#
# run_parallel.sh - Run iri_ne in parallel by splitting param.txt into chunks
#
# Usage: ./run_parallel.sh <year> <mmdd> <hour> <jchoice> [-j N]
#   year, mmdd, hour, jchoice: same arguments as iri_ne
#   -j N: number of parallel processes (default: number of CPU cores)
#
# Example: ./run_parallel.sh 2021 1110 12 1 -j 8
#

set -e

# --- Parse arguments ---
if [ $# -lt 4 ]; then
    echo "Usage: $0 <year> <mmdd> <hour> <jchoice> [-j N]"
    echo "  -j N: number of parallel processes (default: auto-detect CPU cores)"
    exit 1
fi

YEAR="$1"
MMDD="$2"
HOUR="$3"
JCHOICE="$4"
shift 4

# Default: use all available cores
NPROCS=$(sysctl -n hw.logicalcpu 2>/dev/null || nproc 2>/dev/null || echo 4)

while [ $# -gt 0 ]; do
    case "$1" in
        -j) NPROCS="$2"; shift 2 ;;
        *)  echo "Unknown option: $1"; exit 1 ;;
    esac
done

BASEDIR="$(cd "$(dirname "$0")" && pwd)"
PARAM_FILE="$BASEDIR/param.txt"
IRI_NE="$BASEDIR/iri_ne"

if [ ! -f "$PARAM_FILE" ]; then
    echo "Error: param.txt not found in $BASEDIR"
    exit 1
fi
if [ ! -x "$IRI_NE" ]; then
    echo "Error: iri_ne binary not found or not executable in $BASEDIR"
    exit 1
fi

# Count data lines (excluding header)
TOTAL_LINES=$(tail -n +2 "$PARAM_FILE" | wc -l | tr -d ' ')
if [ "$TOTAL_LINES" -eq 0 ]; then
    echo "Error: param.txt has no data lines"
    exit 1
fi

# Adjust NPROCS if more processes than data lines
if [ "$NPROCS" -gt "$TOTAL_LINES" ]; then
    NPROCS=$TOTAL_LINES
fi

LINES_PER_CHUNK=$(( (TOTAL_LINES + NPROCS - 1) / NPROCS ))
HEADER=$(head -1 "$PARAM_FILE")

echo "Parallel IRI Ne computation"
echo "  Grid points:  $TOTAL_LINES"
echo "  Processes:    $NPROCS"
echo "  Points/chunk: ~$LINES_PER_CHUNK"

# --- Create temp directories and split input ---
TMPBASE=$(mktemp -d "${BASEDIR}/.iri_parallel_XXXXXX")
trap "rm -rf '$TMPBASE'" EXIT

PIDS=()
CHUNK_DIRS=()

for i in $(seq 0 $((NPROCS - 1))); do
    CHUNK_DIR="$TMPBASE/chunk_$i"
    mkdir -p "$CHUNK_DIR"
    CHUNK_DIRS+=("$CHUNK_DIR")

    # Extract this chunk's lines from param.txt (1-indexed, skip header)
    START_LINE=$(( i * LINES_PER_CHUNK + 2 ))  # +2 for header offset
    END_LINE=$(( START_LINE + LINES_PER_CHUNK - 1 ))

    # Write header + chunk data to chunk's param.txt
    echo "$HEADER" > "$CHUNK_DIR/param.txt"
    sed -n "${START_LINE},${END_LINE}p" "$PARAM_FILE" >> "$CHUNK_DIR/param.txt"

    # Symlink all required data files into the chunk directory
    for f in "$BASEDIR"/*.asc "$BASEDIR"/*.dat "$BASEDIR"/alt.txt; do
        [ -f "$f" ] && ln -sf "$f" "$CHUNK_DIR/"
    done

    # Symlink the binary
    ln -sf "$IRI_NE" "$CHUNK_DIR/iri_ne"

    # Launch iri_ne in the chunk directory
    (cd "$CHUNK_DIR" && ./iri_ne "$YEAR" "$MMDD" "$HOUR" "$JCHOICE" > /dev/null 2>&1) &
    PID=$!
    PIDS+=($PID)
    LAST=$(( END_LINE - 1 ))
    [ "$LAST" -gt "$TOTAL_LINES" ] && LAST=$TOTAL_LINES
    echo "  Started chunk $i (PID $PID): lines $((START_LINE-1))-$LAST"
done

# --- Wait for all processes ---
echo "Waiting for all processes to finish..."
FAILED=0
for i in "${!PIDS[@]}"; do
    if ! wait "${PIDS[$i]}"; then
        echo "  ERROR: Chunk $i (PID ${PIDS[$i]}) failed"
        FAILED=1
    fi
done

if [ "$FAILED" -eq 1 ]; then
    echo "Some chunks failed. Check error output."
    exit 1
fi

# --- Concatenate results ---
OUTPUT="$BASEDIR/ne.txt"
# Write header from first chunk
head -1 "${CHUNK_DIRS[0]}/ne.txt" > "$OUTPUT"
# Append data from all chunks (skip headers)
for dir in "${CHUNK_DIRS[@]}"; do
    tail -n +2 "$dir/ne.txt" >> "$OUTPUT"
done

TOTAL_OUTPUT=$(tail -n +2 "$OUTPUT" | wc -l | tr -d ' ')
echo "Done. Output: $OUTPUT ($TOTAL_OUTPUT data lines)"
