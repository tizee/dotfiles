#!/usr/bin/env zsh

# show date
# date

# magicTmux message
if [[ $PROLOGUE_LOGO ]]; then
  echo "\033[94m Kilin is not kirlin"
  echo '    ___       ___       ___       ___       ___'
  echo '   /\__\     /\  \     /\__\     /\  \     /\__\ '
  echo '  /:/ _/_   _\:\  \   /:/  /    _\:\  \   /:| _|_'
  echo ' /::-"\__\ /\/::\__\ /:/__/    /\/::\__\ /::|/\__\'
  echo ' \;:;-",-" \::/\/__/ \:\  \    \::/\/__/ \/|::/  /'
  echo '  |:|  |    \:\__\    \:\__\    \:\__\     |:/  /'
  echo '   \|__|     \/__/     \/__/     \/__/     \/__/'
  echo "\033[m"
fi
if [[ $PROLOGUE_FORTUNE ]]; then
  fortune chinese
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
