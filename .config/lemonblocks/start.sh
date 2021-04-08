#!/bin/bash

CLICKABLE_AREAS=8
PANEL_WIDTH=566
PANEL_HEIGHT=21
PANEL_HORIZONTAL_OFFSET=800
PANEL_VERTICAL_OFFSET=0
PANEL_FONT="Source Han Sans JP:size=10"
PANEL_ICON_FONT="RobotoMono Nerd Font:style=Regular:size=15"
COLOR_DEFAULT_FG="#FFFFFF"
COLOR_DEFAULT_BG="#171520"
UNDERLINE_HEIGHT=3
PANEL_WM_NAME="lemon"

# Kill potential instances of lemonblocks
#killall "lemonbar"
killall "lemonblocks"

# Make sure the named pipe already exists
mkfifo /tmp/lemonblockspipe

# start xmonad status lemonbar
# Start lemonbar
cat "/tmp/lemonblockspipe" | lemonbar -b -a "$CLICKABLE_AREAS" \
    -g "$PANEL_WIDTH"x"$PANEL_HEIGHT"+"$PANEL_HORIZONTAL_OFFSET"+"$PANEL_VERTICAL_OFFSET" \
    -o -3 -f "$PANEL_FONT" -o 0 -f "$PANEL_ICON_FONT" -F "$COLOR_DEFAULT_FG" -B "$COLOR_DEFAULT_BG" \
    -u "$UNDERLINE_HEIGHT" -n "$PANEL_WM_NAME" | bash &

sleep 0.5

# Make sure lemonbar is hidden below a fullscreen window
## Bspwm
# wid=$(xdo id -a "$PANEL_WM_NAME")
# xdo above -t "$(xdo id -N Bspwm -n root | sort | head -n 1)" "$wid"

if [ $# -eq 0 ]
then
    lemonblocks
else
    ./bin/lemonblocks
fi
