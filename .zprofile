#!/bin/sh

[[ -f ~/.config/zsh/.zshenv ]] && source ~/.config/zsh/.zshenv
startpage &

alias startx="startx ~/.xinitrc"
alias xm="startx xmonad"
alias ob="startx openbox"

clear
echo "Welcome, VidhuKant!"
echo "run startx <xm/ob> to open XMonad/Openbox"
