#!/usr/bin/env zsh

# Terminal size detection for adaptive display
local term_cols=${COLUMNS:-$(tput cols 2>/dev/null || echo 80)}
local term_rows=${LINES:-$(tput lines 2>/dev/null || echo 24)}
local is_small_term=false
(( term_cols < 65 || term_rows < 20 )) && is_small_term=true

# use ANSI shadow style logo - only show in interactive terminals (not in tmux)
# Logo requires 62 columns, skip on small terminals
if [[ $PROLOGUE_LOGO && $- == *i* && -z $TMUX ]] && ! $is_small_term; then
  echo "\033[94m"
  echo " ████████╗██╗███████╗███████╗███████╗ ███████╗███████╗██╗  ██╗"
  echo " ╚══██╔══╝██║╚══███╔╝██╔════╝██╔════╝ ╚══███╔╝██╔════╝██║  ██║"
  echo "    ██║   ██║  ███╔╝ █████╗  █████╗     ███╔╝ ███████╗███████║"
  echo "    ██║   ██║ ███╔╝  ██╔══╝  ██╔══╝    ███╔╝  ╚════██║██╔══██║"
  echo "    ██║   ██║███████╗███████╗███████╗ ███████╗███████║██║  ██║"
  echo "    ╚═╝   ╚═╝╚══════╝╚══════╝╚══════╝ ╚══════╝╚══════╝╚═╝  ╚═╝"
  echo "\033[m"
fi
# Skip fortune in tmux skill sessions to save tokens (sockets in claude-tmux-sockets dir)
if [[ $PROLOGUE_FORTUNE && $- == *i* && $TMUX != *claude-tmux-sockets* ]]; then
  # alan-kay
  # kk-99
  # song
  # unix
  # learning
  local quotes=(
    naval-on-wealth
    naval-on-happiness
    naval-bonus
    the-art-of-doing-science-and-engineering
  )
  if $is_small_term; then
    # Small terminal: plain fortune with word-wrap, no box decoration
    fortune -s "${quotes[@]}" 2>/dev/null | fold -s -w $((term_cols - 2))
  else
    # Large terminal: use bubblesay box decoration
    fortune "${quotes[@]}" | bubblesay
  fi
fi

# Execute code in the background to not affect the current session
function zwc_watcher() {
    # <https://github.com/zimfw/zimfw/blob/master/login_init.zsh>
    setopt LOCAL_OPTIONS EXTENDED_GLOB
    autoload -U zrecompile
    local ZSH_CONFIG=$HOME/.config/zsh
    local ZSH_PLUGINS=$ZSH_CONFIG/plugins
    local ZSH_FUNCS=$ZSH_CONFIG/autoloaded

    # Compile zcompdump, if modified, to increase startup speed.
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zrecompile -pqn "$zcompdump"
    fi
    # zcompile .zshrc
    zrecompile -pqn ${ZDOTDIR:-${HOME}}/.zshrc
    zrecompile -pqn ${ZDOTDIR:-${HOME}}/.zprofile
    zrecompile -pqn ${ZDOTDIR:-${HOME}}/.zshenv
    # recompile all zsh or sh
    for f in $ZSH_PLUGINS/**/*.*sh
    do
        zrecompile -pqn $f
    done
    for func in $ZSH_FUNCS/**/*
    do
        zrecompile -pqn $func
    done
}
zwc_watcher &!

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
# vim: ft=zsh
