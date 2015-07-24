# Load any machine-specific configuration required for this to work
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"
[[ -s "$HOME/.hostid" ]] && export HOSTID=$(cat $HOME/.hostid)

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="fino"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

for i in $HOME/zsh.d/S*
do
    source $i
done

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git ruby rvm ssh-agent gem svn fasd)

source $ZSH/oh-my-zsh.sh
