#!/bin/bash

printf " VOL:$(pamixer --get-volume-human)% "
# if [ $volume_level == "muted" ]; then
        # printf "%{F#ff0058}VOL: $(pamixer --get-volume)%{F-}"
# else
        # printf "VOL: $volume_level"
# fi

