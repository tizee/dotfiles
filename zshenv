#!/usr/bin/env zsh

# https://blog.patshead.com/2011/04/improve-your-oh-my-zsh-startup-time-maybe.html
# skip_global_compinit=1

# http://disq.us/p/f55b78
setopt noglobalrcs

export SYSTEM=$(uname -s)
# use $OSTYPE insteal of uname -s
# export SYSTEM=$OSTYPE

export XDG_CONFIG_HOME=$HOME/.config
export XDG_CONFIG_DIR=$XDG_CONFIG_HOME/.config

# Home-made scripts
export PATH=$PATH:${HOME}/.bin

export PROLOGUE_LOGO=1
export PROLOGUE_FORTUNE=1
