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


REPOS_ROOT=$HOME/.homesick/repos
GLOBAL_REPO=$REPOS_ROOT/global
SITE_REPO=$REPOS_ROOT/site

# Globally shared prefs
[[ -s "$GLOBAL_REPO/zsh.d/zshrc" ]] && source "$GLOBAL_REPO/zsh.d/zshrc"
# Site-specific prefs (work, home, etc)
[[ -s "$SITE_REPO/shared/zsh.d/zshrc" ]] && source "$SITE_REPO/shared/zsh.d/zshrc"
# Machine-local prefs (JAVA_HOME, etc)
[[ -s "$SITE_REPO/`cat $HOME/.hostid`/zsh.d/zshrc" ]] && source "$SITE_REPO/`cat $HOME/.hostid`/zsh.d/zshrc"
