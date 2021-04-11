export EDITOR="nvim"
export VISUAL="emacs"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export COLORTERM="truecolor"
export PAGER="vim"
export WM="xmonad"

export QT_QPA_PLATFORMTHEME=gtk2
export ZDOTDIR="$HOME/.config/zsh"

startx
if [[ "$(tty)" = "dev/tty1" ]]; then
	startx
fi
