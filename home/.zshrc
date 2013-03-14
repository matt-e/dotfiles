# Machine-specific configuration required for this to work
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="fino"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git ruby rvm ssh-agent gem svn)

source $ZSH/oh-my-zsh.sh

export HOSTID=`cat $HOME/.hostid`
export DOTFILES_ROOT=$HOME/.homesick/repos
export GLOBAL_DOTFILES=$DOTFILES_ROOT/global
export SITE_DOTFILES=$DOTFILES_ROOT/site/shared
export HOST_DOTFILES=$DOTFILES_ROOT/site/$HOSTID

# Globally shared prefs
for i in $GLOBAL_DOTFILES/zsh.d/S*
do
    source $i
done
# Site-specific prefs (work, home, etc)
for i in $SITE_DOTFILES/zsh.d/S*
do
    source $i
done
# Machine-local prefs (JAVA_HOME, etc)
for i in $HOST_DOTFILES/zsh.d/S*
do
    source $i
done
