#!/usr/bin/env zsh

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

zplugin light zdharma/zui
zplugin light zdharma/zplugin-crasis

# 

# autoload -U promptinit && promptinit
# autoload -U compinit compdef && compinit
# autoload colors && colors

# no c-s/c-q output freezing
setopt noflowcontrol
# allow expansion in prompts
setopt prompt_subst
# display PID when suspending processes as well
setopt longlistjobs
# try to avoid the 'zsh: no matches found...'
setopt nonomatch
# report the status of backgrounds jobs immediately
setopt notify
# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all
# not just at the end
setopt completeinword
# use zsh style word splitting
setopt noshwordsplit
# allow use of comments in interactive code
setopt interactivecomments
# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath
# if a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
setopt auto_cd
# make cd push the old directory onto the directory stack.
setopt auto_pushd
# avoid "beep"ing
setopt nobeep
# don't push the same dir twice.
setopt pushd_ignore_dups

# Case-insensitive completion
# #zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
# zinit ice blockf
zinit load "zsh-users/zsh-completions"
zinit load "zsh-users/zsh-history-substring-search"
# # zinit load "zsh-users/zsh-syntax-highlighting"

zinit light zsh-users/zsh-autosuggestions
zinit light zdharma/fast-syntax-highlighting

#zinit load asdf-vm/asdf

zinit load "bobsoppe/zsh-ssh-agent"
# #zinit ice git
zinit snippet PZT::modules/osx
zinit snippet OMZ::plugins/brew
zinit snippet OMZ::plugins/git
zinit snippet OMZ::plugins/ruby
zinit snippet OMZ::plugins/python
zinit snippet OMZ::plugins/virtualenv
zinit snippet OMZ::plugins/golang
zinit snippet OMZ::plugins/colorize
zinit snippet OMZ::plugins/gradle

zinit pack"bgn-binary+keys" for fzf

# zinit load "b4b4r07/enhancd" #, use:init.sh
zinit load "supercrabtree/k"
zinit load "zuxfoucault/colored-man-pages_mod"
zinit load '_local/_path_functions'
zinit ice pick"zsh/fzf-zsh-completion.sh"
# zinit light 'lincheney/fzf-tab-completion'
zinit ice wait"2" lucid from"gh-r" as"program" mv"exa* -> exa"
zinit light ogham/exa
alias ls=exa

# Theme
zinit ice wait'!' lucid atload'source ~/.p10k.zsh; _p9k_precmd' nocd
zinit light romkatv/powerlevel10k


. ${HOME}/.zsh.d/zshrc

bindkey -e

# #path-prepend ${HOME}/local/bin
autoload -U +X compinit && compinit
autoload -U bashcompinit && bashcompinit

#bindkey '^I' fzf_completion
zstyle ':completion:*:*:aws' fzf-search-display true
complete -C aws_completer aws
