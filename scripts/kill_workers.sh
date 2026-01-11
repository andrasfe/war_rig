#!/bin/bash
# Kill all War Rig worker processes

echo "Killing War Rig processes..."

# Kill run_carddemo processes
pkill -9 -f "python.*run_carddemo" 2>/dev/null

# Kill any war_rig module processes
pkill -9 -f "python.*war_rig" 2>/dev/null

# Check what's left
remaining=$(ps aux | grep -E "[p]ython.*(war_rig|carddemo)" | wc -l | tr -d ' ')

if [ "$remaining" -eq "0" ]; then
    echo "All War Rig processes killed."
else
    echo "Warning: $remaining processes still running:"
    ps aux | grep -E "[p]ython.*(war_rig|carddemo)"
fi
