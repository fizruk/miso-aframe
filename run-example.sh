#!/bin/sh

cd $(dirname $0)

example=$1

bin_path="$(stack path --local-install-root)/bin/miso-aframe-${example}.jsexe"

stack build "miso-aframe:miso-aframe-${example}" \
  && cp -f examples/${example}/index.html ${bin_path}/. \
  && open ${bin_path}/index.html
