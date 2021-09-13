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
else
  folded() {
    echo
    echo "RUNNING: $1"
    shift
    "$@"
  }
fi

if [ "$TRAVIS" = true ] || [ "$CI" = true ]; then
  folded "Installing packages" \
    opam install --no-checksums -y \
      bignum \
      cohttp-lwt-jsoo \
      cohttp-lwt-unix \
      dune \
      lwt_ssl \
      menhir \
      pprint \
      sedlex \
      uutf
fi

folded "Build" \
  opam exec -- dune build --root=. --profile release

folded "Test" \
  opam exec -- dune test --root=. --profile release

folded "Run examples" \
  _build/default/src/main/FomCommand/FomCommand.exe examples/*.fom