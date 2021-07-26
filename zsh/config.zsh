#!/usr/bin/env zsh

export ZSHDIR=$HOME/.config/zsh
# man 1 zmodules

######################
# Options
######################
# zsh default options
# emulate -lLR zsh
# {{{

setopt CORRECT
#setopt CORRECT_ALL
setopt auto_cd # automatic cd to a directory without leading cd
setopt multios
setopt prompt_subst
zstyle ':completion:*' rehash true # refresh autocompletion

## History file configuration {{{
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

## History command configuration
setopt extended_history       # record timestamp and elapsed time of command in HISTFILE
setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
setopt hist_ignore_dups       # ignore duplicated commands history list
setopt hist_ignore_space      # ignore commands that start with space
setopt hist_reduce_blanks     # remove blank lines from history list
setopt hist_find_no_dups      # ignore duplicates when searching
setopt hist_verify            # show command with history expansion to user before running it
setopt share_history          # share command history data across zsh sessions
setopt append_history         # append to history rather than overwrite
# setopt inc_append_history # adds commands while typing commands instead of at shell exit.

# use CTRL-R to search command history reversely 
bindkey -v
bindkey '^R' history-incremental-search-backward
# }}}

# enable diff color if possible.
if command diff --color . . &>/dev/null; then
  alias diff='diff --color'
fi
# }}}

######################
# zsh functions
######################
#{{{
# zsh built-in completion system

autoload -Uz compinit
case $SYSTEM in 
  Darwin)
    # use macOS stat
    if [[ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' $HOME/.zcompdump) ]]; then
      # update
      compinit;
    else
      compinit -C;
    fi
    ;;
  Linux)
    if [[ $(date +'%j') != $(date -r $HOME/.zcompdump +'%j') ]]; then
      # update
      compinit;
    else
      compinit -C;
    fi
    ;;
esac

# bash completion compatibility
autoload -U +X bashcompinit && bashcompinit
function haskell_completion_init(){
# eval "$(stack --bash-completion-script stack)"
_stack()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(stack "${CMDLINE[@]}") )
}

complete -o filenames -F _stack stack
}
# haskell completion
haskell_completion_init

#}}}

######################
# plugins 
######################
# could get some ideas from famous Zsh frameworks
# {{{
source "$ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$ZSHDIR/vendor/zsh-autosuggestions/zsh-autosuggestions.zsh"
# source "$ZSHDIR/vendor/z.sh"
source "$ZSHDIR/../zoxide.zsh"

# lazy load functions
# insert at head
fpath=($ZSHDIR/autoloaded "${fpath[@]}") 
autoload -Uz $fpath[1]/*(.:t)

## plugins
# cost about 60ms
local plugins=($HOME/.config/zsh/plugins/*.plugin.zsh)
for file in $plugins; do
 source "$file"
done

#source ~/dev/grepo_shell/fzf-tab/fzf-tab.plugin.zsh

# debug
#export _ZL_ECHO=1
#export _ZL_LOG_NAME=true
# eval "$(lua ~/.config/zsh/vendor/z.lua --init zsh enhanced)"
# }}}

# vim:ft=zsh ts=2 sw=2 sts=2 et fenc=utf-8
