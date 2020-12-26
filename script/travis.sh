#!/bin/bash

set -eo pipefail

if [ "$TRAVIS" = true ]; then
  folded() {
    FOLD=$((FOLD+1))
    echo -e "travis_fold:start:step.$FOLD\033[33;1m$1\033[0m"
    travis_time_start
    shift
    echo "$@"
    "$@"
    travis_time_finish
    echo -e "\ntravis_fold:end:step.$FOLD\r"
  }

  folded "OPAM init" \
    opam init -y

  eval "$(opam env)"

  folded "Installing packages" \
    opam install -y \
      bignum \
      dune \
      js_of_ocaml \
      js_of_ocaml-ppx \
      menhir \
      pprint \
      sedlex \
      uutf
else
  folded() {
    echo
    echo "RUNNING: $1"
    shift
    "$@"
  }
fi

folded "Build" \
  dune build

folded "Test" \
  dune test
