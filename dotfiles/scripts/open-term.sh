#!/bin/bash

is_running=$(osascript -e 'tell application "System Events" to (name of processes) contains "iTerm2"')

if [ "$is_running" = "true" ]; then
    # iTerm2 is running, create a new window
    osascript -e 'tell application "iTerm2" to create window with default profile'
else
    # iTerm2 is not running, launch it
    open -a iTerm2
fi
