#!/usr/bin/env zsh

# use ANSI shadow style logo
if [[ $PROLOGUE_LOGO ]]; then
  echo "\033[94m"
  echo " ████████╗██╗███████╗███████╗███████╗ ███████╗███████╗██╗  ██╗"
  echo " ╚══██╔══╝██║╚══███╔╝██╔════╝██╔════╝ ╚══███╔╝██╔════╝██║  ██║"
  echo "    ██║   ██║  ███╔╝ █████╗  █████╗     ███╔╝ ███████╗███████║"
  echo "    ██║   ██║ ███╔╝  ██╔══╝  ██╔══╝    ███╔╝  ╚════██║██╔══██║"
  echo "    ██║   ██║███████╗███████╗███████╗ ███████╗███████║██║  ██║"
  echo "    ╚═╝   ╚═╝╚══════╝╚══════╝╚══════╝ ╚══════╝╚══════╝╚═╝  ╚═╝"
  echo "\033[m"
fi
if [[ $PROLOGUE_FORTUNE ]]; then
  local quotes=(
    alan-kay
    mao-ze-dong-anthology
    mao-ze-dong-chronicle
    kk-99
    song
    unix
    learning
  )
  fortune "${quotes[@]}"
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
