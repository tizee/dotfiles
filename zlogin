#!/usr/bin/env zsh

# magicTmux message
date
echo "\033[94m Kilin is not kirlin"
echo '    ___       ___       ___       ___       ___'
echo '   /\__\     /\  \     /\__\     /\  \     /\__\ '
echo '  /:/ _/_   _\:\  \   /:/  /    _\:\  \   /:| _|_'
echo ' /::-"\__\ /\/::\__\ /:/__/    /\/::\__\ /::|/\__\'
echo ' \;:;-",-" \::/\/__/ \:\  \    \::/\/__/ \/|::/  /'
echo '  |:|  |    \:\__\    \:\__\    \:\__\     |:/  /'
echo '   \|__|     \/__/     \/__/     \/__/     \/__/'
echo "\033[m"
fortune chinese

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
        zrecompile -pq "$zcompdump"
    fi
    # zcompile .zshrc
    zrecompile -pq ${ZDOTDIR:-${HOME}}/.zshrc
    zrecompile -pq ${ZDOTDIR:-${HOME}}/.zprofile
    zrecompile -pq ${ZDOTDIR:-${HOME}}/.zshenv
    # recompile all zsh or sh
    for f in $ZSH_PLUGINS/**/*.*sh
    do
        zrecompile -pq $f
    done
    for func in $ZSH_FUNCS/**/*
    do
        zrecompile -pq $func
    done
}
zwc_watcher &!

# vim:syntax=zsh ft=zsh
