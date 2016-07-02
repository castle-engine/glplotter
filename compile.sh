#! /bin/sh
set -eu

if [ -d ../castle_game_engine/ ]; then
  cd ../castle_game_engine/
  # Force rebuilding CastleWindow unit with proper backend.
  make --quiet clean-window
  cd ../glplotter/
fi

castle-engine compile
castle-engine simple-compile ../glplotter/gen_function.lpr
