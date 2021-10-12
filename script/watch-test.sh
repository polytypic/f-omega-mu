#!/bin/bash

set -eo pipefail

watchexec -d 250 -i docs -- "(\
  echo $'<<< <<< <<<' && \
  dune build --profile release && \
  ./script/docs.sh && \
  dune test --profile release && \
  ./script/ci.sh ; \
  echo $'\n>>> >>> >>>' \
)"
