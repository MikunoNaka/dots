export EDITOR="nvim"
export VISUAL="emacs"
export TERMINAL="st"
export BROWSER="firefox"
export READER="zathura"
export COLORTERM="truecolor"
export PAGER="less"
export WM="xmonad"

export QT_QPA_PLATFORMTHEME=gtk2
export ZDOTDIR="$HOME/.config/zsh"

/zt/Programs/startpage-server &

if [[ "$(tty)" = "dev/tty1" ]]; then
	startx
fi

