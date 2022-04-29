#!/usr/bin/env zsh
# Thanks to https://thevaluable.dev/zsh-completion-guide-examples/ sharing this knowledge
# some configurations from https://github.com/Phantas0s/.dotfiles/blob/master/zsh/completion.zsh

# setting of zsh completion system
# list of completion functions:
# _completion: core function for completion
# _approximate: try to correct the input (the context) if there is no match
# _expand_alias: expand an alias that you've typed
# _extension: try to expand glob pattern
# _complete_help: show completion help in context

# By default, you can use _expand_alias by CTRL-x a
# By default, you can use _complete_help by CTRL-x h

# zsh options
setopt MENU_COMPLETE        # Automatically highlight first element of completion menu
# setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
# setopt COMPLETE_IN_WORD     # Complete from both ends of a word.

# setup completers
zstyle ':completion:*' completer _extensions _complete _approximate

zstyle ':completion:*' rehash true # refresh autocompletion

# enable cache for speed
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.config/zsh/.zcompcache"
# Complete the alias when _expand_alias is used as a function
zstyle ':completion:*' complete true

# use menu
# select=long     - use menu when screen is large enough
# select=<number> - only use menu for given number of matches
# interactive     - enable filter completion menu
# search          - enable fuzzy search
zstyle ':completion:*' menu select=long search

# descriptions tag - generate descriptions based on the type of matches
# %d             - description
# %F{color_name} - frontground color
# %f             - match
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-> %d %f'
# corrections tag: style for _approximate
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}-> %d (errors: %e) %f'

# matching information style
zstyle ':completion:*:messages' format ' %F{orange}-> %d %f'
zstyle ':completion:*:warnings' format ' %F{red}-> no matches found %f'

# display setting

# color with $LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# group by type
zstyle ':completion:*' group-name ''
# group order
zstyle ':completion:*:*:-command-:*:*' group-order alias builtins functions commands

# display ls -l like file for description
# zstyle ':completion:*' file-list all

# Autocomplete options for cd instead of directory stack
zstyle ':completion:*' complete-options true
zstyle ':completion:*' file-sort modification

# use before compinit
zmodload zsh/complist

# move with vim bindings
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect '^xg' clear-screen
# enable interactive mode
bindkey -M menuselect '^xi' vi-insert
# Hold
bindkey -M menuselect '^xh' accept-and-hold
# Next
bindkey -M menuselect '^xn' accept-and-infer-next-history
# Undo
bindkey -M menuselect '^xu' undo

## completion control
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' keep-prefix true

zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

## Other completion settings
# WIP
