# -*- mode: sh; sh-indentation: 4; indent-tabs-mode: nil; sh-basic-offset: 4; -*-

# Copyright (c) 2020 Matthew Eddey

# According to the Zsh Plugin Standard:
# http://zdharma.org/Zsh-100-Commits-Club/Zsh-Plugin-Standard.html

0=${${ZERO:-${0:#$ZSH_ARGZERO}}:-${(%):-%N}}
0=${${(M)0:#/*}:-$PWD/$0}

# Then ${0:h} to get plugin's directory

if [[ ${zsh_loaded_plugins[-1]} != */asdf_loader && -z ${fpath[(r)${0:h}]} ]] {
    fpath+=( "${0:h}" )
}

# Standard hash for plugins, to not pollute the namespace
typeset -gA Plugins
Plugins[ASDF_LOADER_DIR]="${0:h}"

asdf_dir="${asdf_dir:-$HOME/.asdf}"
if [[ -d $asdf_dir ]]; then
    export ASDF_DIR=$asdf_dir
	export ASDF_DATA_DIR=$asdf_dir
    #fpath=( ${asdf_dir} $fpath)
    #autoload -Uz asdf
    source $asdf_dir/asdf.sh
    fpath=( ${asdf_dir}/completions $fpath)
else
    echo "ASDF not found at ${asdf_dir}"
fi

# Use alternate vim marks [[[ and ]]] as the original ones can
# confuse nested substitutions, e.g.: ${${${VAR}}}

# vim:ft=zsh:tw=80:sw=4:sts=4:et:foldmarker=[[[,]]]
