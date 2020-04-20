#!/bin/bash

# Add these directories because we don't want stow to manage the
# the entire root under these paths
mkdir -p ${HOME}/local/bin
mkdir -p ${HOME}/.config
mkdir -p ${HOME}/.config/git
mkdir -p ${HOME}/.emacs.d

stow -Rv -t $HOME public
