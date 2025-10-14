switch()
{

DESKTOP=$1

# Build the array of desktop keycodes
DESKTOPS=(18 19 20 21 23 22 26 28 25 29)

if [[ $DESKTOP -le 10 ]]; then
    INDEX=$(($DESKTOP - 1))
    KEYCODE=${DESKTOPS[$INDEX]}
    osascript <<EOF
    tell application "System Events"
        key code "$KEYCODE" using control down
    end tell
EOF

elif [[ $DESKTOP -gt 10 ]]; then
    INDEX=$(($DESKTOP - 10))
    KEYCODE=${DESKTOPS[$INDEX]}
    osascript <<EOF
    tell application "System Events"
        key code "$KEYCODE" using {control down, option down}
    end tell
EOF

else
    echo 'your display is not supported'

fi

}

# Allow script to be called directly
if [[ $0 == *switch.sh  ]]; then
    switch $1
fi
