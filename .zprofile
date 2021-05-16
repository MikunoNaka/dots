export EDITOR="nvim"
export VISUAL="emacs"
export TERMINAL="st"
export BROWSER="librewolf"
export READER="zathura"
export COLORTERM="truecolor"
export PAGER="bat"
export WM="xmonad"

export QT_QPA_PLATFORMTHEME=gtk2
export ZDOTDIR="$HOME/.config/zsh"

if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  /zt/.path/startpage-server &
  startx
fi

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
