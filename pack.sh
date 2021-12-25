#!/bin/bash
set -eu

# Create release zip for https://github.com/castle-engine/glplotter/releases/ .

do_platform ()
{
  # Building and packaging gen_function alongside glplotter is a little hack.
  # We clean and build gen_function separately, then we add it to glplotter
  # because "gen_function" and "gen_function.exe" are listed in CastleEngineManifest.xml .
  rm -f gen_function gen_function.exe
  castle-engine clean
  castle-engine simple-compile $@ gen_function.lpr

  castle-engine package $@
}

#do_platform --os=linux --cpu=x86_64
do_platform --os=win64 --cpu=x86_64
