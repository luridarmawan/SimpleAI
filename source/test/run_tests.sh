#!/bin/sh
# Compile and run the SimpleAI unit tests with instantfpc.
# Usage: ./run_tests.sh [runner options]   (default: -a)
#
# FastPlaz location: set FASTPLAZ_DIR to the FastPlaz folder (the one that
# contains src/ and test/). Default: ../../../../../FastPlaz/git/fastplaz/FastPlaz

cd "$(dirname "$0")" || exit 1
HERE="$(pwd)"

FP="${FASTPLAZ_DIR:-$HERE/../../../../../FastPlaz/git/fastplaz/FastPlaz}"
if [ ! -f "$FP/src/library/common.pas" ]; then
  echo "FastPlaz not found: $FP"
  echo "Set FASTPLAZ_DIR to the FastPlaz folder."
  exit 2
fi
SA="$HERE/../package"

# -B: always rebuild, the cache only tracks the main program source.
export INSTANTFPCOPTIONS="-B -Fi$FP -Fu$FP/test -Fu$FP/../tools/src -Fu$FP/src/systems -Fu$FP/src/library -Fu$FP/src/vendor -Fu$FP/src/model -Fu$FP/src/util -Fu$FP/src/helper -Fu$FP/src/integration -Fu$FP/src/vendor/dcrypt -Fu$SA -Fu$SA/src -Fu$SA/src/library -Fu$SA/src/controller"

echo "Compiling (instantfpc)..."
if [ $# -eq 0 ]; then
  set -- -a
fi
exec instantfpc simpleai_test.lpr "$@"
