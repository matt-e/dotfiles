zmodload zsh/zprof

ZPLUG_ROOT=$HOME/.zplug

# Essential
if [[ ! -d $ZPLUG_ROOT ]]; then
    git clone https://github.com/zplug/zplug $ZPLUG_ROOT
    source $ZPLUG_ROOT/init.zsh && zplug update --self
fi
source $ZPLUG_ROOT/init.zsh

export ZSH=$HOME/.zplug/repos/robbyrussell/oh-my-zsh
export EDITOR=emacsclient

autoload -U promptinit && promptinit
autoload -U compinit compdef && compinit
autoload colors && colors


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

# History
# =======
setopt hist_ignore_all_dups
setopt sharehistory
setopt inc_append_history
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_verify

export HISTSIZE=1000000
export SAVEHIST=1000000
export HISTFILE=~/.zsh_history
export REPORTTIME=10
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# Enhancd configuration
ENHANCD_DISABLE_DOT=1
ENHANCD_FILTER=fzy

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

#zplug "zsh-users/zaw"
zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf
zplug "bobsoppe/zsh-ssh-agent"
zplug "plugins/brew", from:oh-my-zsh
zplug "plugins/brew-cask", from:oh-my-zsh
zplug "plugins/git",   from:oh-my-zsh
#zplug "plugins/rbenv",   from:oh-my-zsh # 500ms
zplug "plugins/ruby",   from:oh-my-zsh
zplug "plugins/python",   from:oh-my-zsh
zplug "plugins/virtualenv",   from:oh-my-zsh
zplug "plugins/virtualenvwrapper",   from:oh-my-zsh
zplug "plugins/golang",   from:oh-my-zsh
zplug "plugins/osx", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
zplug "plugins/zsh_reload", from:oh-my-zsh
zplug "plugins/colorize", from:oh-my-zsh
zplug "b4b4r07/enhancd", use:init.sh
zplug "supercrabtree/k"
zplug "plugins/gradle", from:oh-my-zsh
zplug "zuxfoucault/colored-man-pages_mod"
zplug "littleq0903/gcloud-zsh-completion", use:src
if [[ -d ${HOME}/.kube ]]; then
    zplug "superbrothers/zsh-kubectl-prompt"
    RPROMPT='%{$fg[blue]%}($ZSH_KUBECTL_PROMPT)%{$reset_color%}'
fi

# Theme
zplug romkatv/powerlevel10k, use:powerlevel10k.zsh-theme

# Start zplug
zplug check || zplug install
zplug load # --verbose

. ${HOME}/.zsh.d/zshrc

bindkey -e

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"