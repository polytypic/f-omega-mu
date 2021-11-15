#!/bin/bash

set -eo pipefail

if [ "$TRAVIS" = true ] || [ "$CI" = true ]; then
  OPTS=(--no-checksums -y)
else
  OPTS=()
fi

opam install "${OPTS[@]}" \
  bignum \
  cohttp-lwt-jsoo \
  cohttp-lwt-unix \
  dune \
  lwt_ssl \
  menhir \
  pprint \
  sedlex \
  uutf
