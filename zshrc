#!/usr/bin/env zsh
# vim:fmr=<-,->:ft=zsh:fdm=marker

# only run in zsh
[ ! -n "$ZSH_VERSION" ] && return

# profiling zsh
# zmodload zsh/zprof
#
source $HOME/.config/zsh/kiriline.theme.zsh
source $HOME/.config/zsh/config.zsh
# source $HOME/.config/work_bin/work_config

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# cleanup: remove duplicate PATH entries 
export -U PATH
# eliminate duplicates
typeset -gU cdpath fpath

# profiling
# zprof
