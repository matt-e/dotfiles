# Skip the not really helping Ubuntu global compinit
skip_global_compinit=1

# Make shared history
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

# Other configuration
export EDITOR=emacsclient

ENHANCD_DISABLE_DOT=1
ENHANCD_FILTER=fzy
