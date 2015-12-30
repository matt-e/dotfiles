# Load any machine-specific configuration required for this to work
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"
[[ -s "$HOME/.hostid" ]] && export HOSTID=$(cat $HOME/.hostid)

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
ZSH_CUSTOM=$HOME/zsh.d
# Set name of the theme to load.
ZSH_THEME="agnoster"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

for i in $HOME/zsh.d/S*
do
    source $i
done

zstyle :omz:plugins:ssh-agent agent-forwarding on

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git ruby rvm ssh-agent gem svn fasd virtualenvwrapper virtualenv)

source $ZSH/oh-my-zsh.sh

test -e ${HOME}/.iterm2_shell_integration.zsh && source ${HOME}/.iterm2_shell_integration.zsh
