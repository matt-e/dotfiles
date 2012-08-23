# [[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="fino"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
plugins=(git ruby rvm ssh-agent gem svn)

source $ZSH/oh-my-zsh.sh

# Globally shared prefs
[[ -s "$HOME/dotfiles/global/zsh.d/zshrc" ]] && source "$HOME/dotfiles/global/zsh.d/zshrc"
# Site-specific prefs (work, home, etc)
[[ -s "$HOME/dotfiles/site/shared/zsh.d/zshrc" ]] && source "$HOME/dotfiles/site/shared/zsh.d/zshrc"
# Machine-local prefs (JAVA_HOME, etc)
[[ -s "$HOME/dotfiles/site/`cat $HOME/.hostid`/zsh.d/zshrc" ]] && source "$HOME/dotfiles/site/`cat $HOME/.hostid`/zsh.d/zshrc"
