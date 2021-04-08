#!/bin/bash

set -eo pipefail

build-sandbox() {
  local TARGET=src/main/FomSandbox/FomSandbox.bc.js
  local OUTPUT=docs/FomSandbox.js

  dune build --root=. --profile release ./$TARGET

  echo "'use strict'"           > $OUTPUT
  cat ./_build/default/$TARGET >> $OUTPUT
}

build-examples() {
  local OUTPUT=docs/examples.js

  rm -rf docs/examples
  mkdir -p docs/examples

  echo "const examples = [" > $OUTPUT
  for example in examples/*.fom; do
    cp "$example" docs/examples/
    echo "  '$example'," >> $OUTPUT
  done
  echo "]" >> $OUTPUT
}

build-sandbox
build-examples
