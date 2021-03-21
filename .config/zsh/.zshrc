HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep notify
bindkey -v
# autoload -U colors && colors
# source /home/zt/.local/zsh-autocomplete/zsh-autocomplete.plugin.zsh

# paths
# export PATH=/:$PATH
export PATH=/zt/Docs/Go:$PATH
export PATH=/home/zt/.config/scripts:$PATH
export PATH=/zt/Programs:$PATH
export PATH=/home/zt/.local/bin:$PATH
export PATH=/zt/Docs/GoMinder:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/home/zt/.emacs.d/bin:$PATH

export GOPATH="/zt/Docs/Go/"

# aliases
# directories
alias waifu='cd /zt/waifu'
alias docs='cd /zt/Docs'
alias zt='/zt/'
alias progs='/zt/Programs/'
alias scripts='~/.config/scripts/'
alias goproj='/zt/Docs/Go/src/github.com/MikunoNaka'
alias wd='/zt/Docs/web'

# programs
alias pf='clear && pfetch'
alias rm='rm -i'
alias ls='exa -l'
alias la='exa -a'
alias lsa='exa -al'
alias nf='neofetch'
alias adbc='adb connect 10.0.0.51:5555'
alias x='chmod +x'

# git
alias gs='git status'
alias ga='git add'
alias gA='git add -A'
alias gc='git commit'
alias gcm='git commit -m'
alias gi='nvim .gitignore'
alias gp='git push'
alias gcl='git clone'

# git bare
alias dots='git --git-dir=$HOME/.dots/ --work-tree=$HOME'

# ssh
alias ssh-k='eval $(ssh-agent)&& ssh-add /home/zt/.ssh/kalawati_git_key'
alias ssh-v='eval $(ssh-agent)&& ssh-add /home/zt/.ssh/vidhukant'

neofetch
exa
eval "$(starship init zsh)"
# syntax highlighting, needs to be at the end
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
