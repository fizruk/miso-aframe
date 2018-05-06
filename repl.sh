#!/bin/sh

set -e
set -x

# example project name
name=$1
shift

# optional stack options
STACK_OPTIONS=$@

# specify a directory to serve static files from
export GHCJSI_STATIC_DIR=$(pwd)/examples/$name/static

# run GHCJSi
stack ghci miso-aframe:lib miso-aframe:exe:$name $STACK_OPTIONS \
  --docker-mount "${GHCJSI_STATIC_DIR}" \
  --docker-env "GHCJSI_STATIC_DIR=${GHCJSI_STATIC_DIR}"
