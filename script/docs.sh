#!/bin/bash

set -eo pipefail

dune build --profile release ./src/main/FomSandbox/FomSandbox.bc.js
cp ./_build/default/src/main/FomSandbox/FomSandbox.bc.js docs/FomSandbox.js
chmod +w docs/FomSandbox.js
