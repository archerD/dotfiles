#!/bin/bash
# Script comes from https://davidltran.com/blog/check-tmux-session-exists-script/

# Session name
session="Background"

# Check if the session exists, discarding output
# We can check $? for the exit status (zero for success, non-zero for failure)
tmux has-session -t $session 2>/dev/null

if [ $? != 0 ]; then
    # Set up main window
    tmux new-session -d -s $session
    tmux send-keys 'phone' C-m
fi

# Attach to created session
tmux attach-session -t $session

