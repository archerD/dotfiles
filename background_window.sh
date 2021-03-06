#!/bin/bash

# Session name
session="Background"

tmux new-session -d -s $session
tmux send-keys 'btop' C-m
tmux split-window -h -b
tmux send-keys 'cmus' C-m

tmux new-window
tmux send-keys 'phone' C-m

tmux new-window
tmux send-keys 'compton'

tmux next-window
tmux attach-session -d

