#!/bin/bash

set -eo pipefail

watchexec -d 250 -i docs -- "(\
  echo $'<<< <<< <<<' && \
  ./script/ci.sh && \
  echo $'\n>>> >>> >>>' \
)"
