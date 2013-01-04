#! /bin/bash
set -eu

# Make fonts sources used only by glplotter.

do_font2pascal ()
{
  font2pascal "$@" --dir .
}

do_font2pascal --font-name 'Bitstream Vera Sans Mono' --font-height -16 --grab-to bitmap
