#!/usr/bin/env bash

# Install packages
source setup.sh

SESSION_NAME="TMUXXING"

# Check if the session already exists
if tmux has-session -t $SESSION_NAME 2>/dev/null; then
    echo "Session $SESSION_NAME already exists. Attaching to it."
    tmux attach-session -t $SESSION_NAME
else
    # Create a new session
    tmux new-session -d -s $SESSION_NAME
    
    # Define seed
    SEED=$((1))

    # Send a command to the first pane
    tmux send-keys -t "$SESSION_NAME:0.0" "./Scripts/Simulate_AI_3PLM.R $SEED" C-m

    # Attach to the created session
    tmux attach-session -t $SESSION_NAME
fi
