#!/usr/bin/env zsh
# Thanks to https://thevaluable.dev/zsh-completion-guide-examples/ sharing this knowledge
# some configurations from https://github.com/Phantas0s/.dotfiles/blob/master/zsh/completion.zsh

# Guard: prevent reloading
(( ${+_COMPLETION_PLUGIN_LOADED} )) && return
typeset -g _COMPLETION_PLUGIN_LOADED=1

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
# Enter menu and cycle through candidates on first Tab press
setopt MENU_COMPLETE
# Complete from cursor position in word
setopt COMPLETE_IN_WORD
# Don't ask "do you wish to see all N possibilities" - just show the menu
# Set to 0 to disable the prompt entirely, or a high number like 500
LISTMAX=0

# setup completers
# Order matters: standard completion first, then approximate correction
# Note: _extensions at the front can reduce predictability
zstyle ':completion:*' completer _complete _approximate

zstyle ':completion:*' rehash true # refresh autocompletion

# enable cache for speed
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.config/zsh/.zcompcache"
# Complete the alias when _expand_alias is used as a function
zstyle ':completion:*' complete true

# use menu
# select=long     - use menu when screen is large enough
# select=<number> - only use menu for given number of matches
# interactive     - enable filter completion menu with mini-buffer
# search          - typing filters the completion list (conflicts with hjkl navigation)
zstyle ':completion:*' menu select=long

# Show scrolling prompt when list exceeds screen
zstyle ':completion:*' list-prompt '%SAt %p: TAB for more, / to search%s'
# Similar prompt for selection mode
zstyle ':completion:*' select-prompt '%SScrolling: %p%s'

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
# Note: file-sort by modification breaks muscle memory (order changes as files change)
# Use alphabetical (default) for predictable positioning
# zstyle ':completion:*' file-sort modification

# use before compinit
zmodload zsh/complist

# Keybinding notation:
#   ^x  = Ctrl+x (^ is the Ctrl modifier)
#   ^xg = Ctrl+x followed by g (two-key sequence)
#
# menuselect keybindings (active when completion menu is shown)

# Navigation: use arrow keys (hjkl conflicts with isearch character input)
# Arrow keys work by default in menuselect, no explicit binding needed
# bindkey -M menuselect 'h' vi-backward-char        # move left
# bindkey -M menuselect 'j' vi-down-line-or-history # move down
# bindkey -M menuselect 'k' vi-up-line-or-history   # move up
# bindkey -M menuselect 'l' vi-forward-char         # move right

# Ctrl+x prefix commands (may not work in some terminals/tmux - ^x often intercepted)
# bindkey -M menuselect '^xg' clear-screen              # Ctrl+x g: refresh display
# bindkey -M menuselect '^xi' vi-insert                 # Ctrl+x i: enter insert mode (edit in place)
# bindkey -M menuselect '^xh' accept-and-hold           # Ctrl+x h: accept but keep menu open
# bindkey -M menuselect '^xn' accept-and-infer-next-history  # Ctrl+x n: accept and show next completion
# bindkey -M menuselect '^xu' undo                      # Ctrl+x u: undo last selection

# Press / to enter isearch mode in menu (vim-style)
# In isearch: type to filter, Ctrl+g to cancel, Enter to accept
bindkey -M menuselect '/' history-incremental-search-forward

## completion control
# Tightened matcher-list: removed overly permissive 'l:|=* r:|=*'
# Order: exact match -> case-insensitive -> separator-aware
zstyle ':completion:*' matcher-list \
  '' \
  'm:{a-zA-Z}={A-Za-z}' \
  'r:|[._-]=* r:|=*'

# Note: keep-prefix can cause confusion with loose matchers - disabled for now
# zstyle ':completion:*' keep-prefix true

zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

## Other completion settings
# WIP
