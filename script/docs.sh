#!/bin/bash

set -eo pipefail

TARGET=src/main/FomSandbox/FomSandbox.bc.js
OUTPUT=docs/FomSandbox.js

dune build --root=. --profile release ./$TARGET

echo "'use strict'"           > $OUTPUT
cat ./_build/default/$TARGET >> $OUTPUT
