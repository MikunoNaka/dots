export EDITOR="nvim"
export VISUAL="emacs"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export COLORTERM="truecolor"
export PAGER="bat"
export WM="xmonad"

export QT_QPA_PLATFORMTHEME=gtk2
export ZDOTDIR="$HOME/.config/zsh"

if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  /zt/Programs/startpage-server &
  startx
fi

