#!/bin/bash

set -eo pipefail

for f in examples/*.fom; do
  echo $f
  _build/default/src/main/FomCommand/FomCommand.exe \
    -stop js \
    $f > examples/.tmp.js
  prettier -w examples/.tmp.js > /dev/null
  diff -y --suppress-common-lines $f.js examples/.tmp.js || true
  mv examples/.tmp.js $f.js
done
