#!/bin/bash

set -eo pipefail

SESSION=f-omega-mu

tmux start-server
tmux new-session -d -s $SESSION

tmux splitw -v -p 90
tmux select-pane -t 0
tmux splitw -h

tmux select-pane -t 0
tmux send-keys "npx livereload docs --wait 100" C-m

tmux select-pane -t 1
tmux send-keys "npx serve docs" C-m

tmux select-pane -t 2
tmux send-keys "watchexec -i docs -- \"(\
  echo $'<<< <<< <<<' && \
  dune build --profile release && \
  ./script/docs.sh && \
  dune test --profile release && \
  ./script/ci.sh ; \
  echo $'\n>>> >>> >>>' \
)\"" C-m

tmux attach-session -t $SESSION
