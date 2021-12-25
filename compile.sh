#! /bin/sh
set -eu

castle-engine compile ${CASTLE_ENGINE_TOOL_OPTIONS:-}
castle-engine simple-compile ${CASTLE_ENGINE_TOOL_OPTIONS:-} gen_function.lpr
