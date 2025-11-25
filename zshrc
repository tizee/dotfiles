#!/usr/bin/env zsh
# vim:fmr={{{,}}}:ft=zsh:fdm=marker

# only run in zsh
[ ! -n "$ZSH_VERSION" ] && return

# profiling zsh
# zmodload zsh/zprof

# Debug shell loading function
function debug_shell_loading() {
    local log_file="${1:-/tmp/shell_debug.log}"
    echo "Debug: Loading .zshrc, SHLVL=$SHLVL, PPID=$PPID, PID=$$" >> "$log_file"
    echo "Debug: SHELL=$SHELL, TERM_PROGRAM=$TERM_PROGRAM" >> "$log_file"
    echo "Debug: Date=$(date)" >> "$log_file"
    echo "---" >> "$log_file"
    echo "Debug info logged to $log_file"
}

######################
# Prompt
# more details on https://zsh.sourceforge.io/Doc/Release/Prompt-Expansion.html
######################
# Resolve the directory of this file (follow symlinks) and load the prompt config from zsh/prompt.zsh.
prompt_config_file="${${(%):-%N}:A:h}/zsh/prompt.zsh"
[[ -f $prompt_config_file ]] && source "$prompt_config_file"

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
# setopt prompt_subst
# enhanced glob
setopt extendedglob

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

# use CTRL-R to search command history reversely in vim mode
# bindkey -v '^R' history-incremental-search-backward
# }}}

# enable diff color if possible.
# if command diff --color . . &>/dev/null; then
#   alias diff='diff --color'
# fi
# }}}

#}}}

######################
# plugins
######################
# could get some ideas from famous Zsh frameworks
# {{{
# cost about 9ms
[ -e $ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && source "$ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -e $ZSHDIR/vendor/zsh-autosuggestions/zsh-autosuggestions.zsh ] && source "$ZSHDIR/vendor/zsh-autosuggestions/zsh-autosuggestions.zsh"
if [ -d $ZSHDIR/vendor/zsh-completions/src ];then
  fpath=("$ZSHDIR/vendor/zsh-completions/src" ${fpath[@]})
fi
# source "$ZSHDIR/vendor/z.sh"
# [ -e "$ZSHDIR/../zoxide.zsh" ] && source "$ZSHDIR/../zoxide.zsh"
# use zcd instead

# lazy load functions
# insert at head
fpath=($ZSHDIR/autoloaded $HOME/.config/zfunc "${fpath[@]}")
autoload -Uz $fpath[1]/*(.:t)

## plugins
# cost about 60ms
local my_plugins=($HOME/.config/zsh/plugins/*.plugin.zsh)
local my_widgets=($HOME/.config/zsh/widgets/*.widget.zsh)
for file in $my_plugins; do
  source "$file"
done
for file in $my_widgets; do
  source "$file"
done
unset my_widgets
unset my_plugins


# debug
#export _ZL_ECHO=1
#export _ZL_LOG_NAME=true
# eval "$(lua ~/.config/zsh/vendor/z.lua --init zsh enhanced)"
# }}}

# [ -f ~/dev/grepo_shell/fzf-tab/fzf-tab.plugin.zsh ] && source ~/dev/grepo_shell/fzf-tab/fzf-tab.plugin.zsh

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# [ -f ~/.config/zsh/fzf-tab-config.zsh ] && source ~/.config/zsh/fzf-tab-config.zsh

# haskell completion
# function haskell_completion_init(){
# # eval "$(stack --bash-completion-script stack)"
# _stack()
# {
#     local CMDLINE
#     local IFS=$'\n'
#     CMDLINE=(--bash-completion-index $COMP_CWORD)

#     for arg in ${COMP_WORDS[@]}; do
#         CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
#     done

#     COMPREPLY=( $(stack "${CMDLINE[@]}") )
# }

# complete -o filenames -F _stack stack
# }
# haskell_completion_init

# cleanup: remove duplicate PATH entries
export -U PATH
# eliminate duplicates
typeset -gU cdpath fpath

######################
# zsh functions
######################
#{{{
# zsh built-in completion system

autoload -Uz compinit

# zcompdump
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
  *)
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
# }}}

# python manager
export PATH="$HOME/.poetry/bin:$PATH"

# llm

# prepare-commit-msg hook
function toggleLLMCommit {
  if [ -z "$SKIP_LLM_GITHOOK" ]; then
    export SKIP_LLM_GITHOOK=1
    echo "Disable LLM git commit generation"
  else
    unset SKIP_LLM_GITHOOK
    echo "Enable LLM git commit generation"
  fi
}

# pre-commit hook
# Function to enable or disable the secret scan pre-commit hook
function toggleSecretScan {
  if [ -z "$SKIP_SCAN_GITHOOK" ]; then
    export SKIP_SCAN_GITHOOK=1
    echo "Disable credential scan"
  else
    unset SKIP_SCAN_GITHOOK
    echo "Enable credential scan"
  fi
}

# yubikey
# export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
# gpgconf --launch gpg-agent

# zsh hook
function __cd_hook_list_files() {
  # do not override user defined options
  emulate -L zsh
  ls -al
}

# zshbuiltin variable
chpwd_functions=(${chpwd_functions[@]} "__cd_hook_list_files")

# rbenv init make the startup slow down
# eval "$(rbenv init - zsh)"

# >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/Users/tizee/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/Users/tizee/anaconda3/etc/profile.d/conda.sh" ]; then
#         . "/Users/tizee/anaconda3/etc/profile.d/conda.sh"
#     fi
# fi
# unset __conda_setup
# <<< conda initialize <<<

export TMUX_CONF="$HOME/.config/tmux/tmux.conf"
# Commented out automatic tmux session to prevent nested shells
supported_terms=("WezTerm" "ghostty")
if [[ -n "$TERM_PROGRAM" && " ${supported_terms[@]} " =~ " $TERM_PROGRAM " ]]; then
  if command -v tmux &> /dev/null; then
    if ! tmux has -t develop &> /dev/null; then
      tmux new -s develop
    else
      tmux attach -t develop
    fi
  fi
fi

# zsh profiling end
# zprof

# pnpm
# https://pnpm.io/completion
# tabtab source for packages
# uninstall by removing these lines
[[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
source ~/.config/zsh/mangit.zsh

# >>> mamba initialize >>>
# !! Contents within this block are managed by 'mamba shell init' !!
export MAMBA_EXE='/opt/homebrew/opt/micromamba/bin/mamba';
export MAMBA_ROOT_PREFIX='/Users/tizee/mamba';
__mamba_setup="$("$MAMBA_EXE" shell hook --shell zsh --root-prefix "$MAMBA_ROOT_PREFIX" 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__mamba_setup"
else
    alias mamba="$MAMBA_EXE"  # Fallback on help from mamba activate
fi
unset __mamba_setup
# <<< mamba initialize <<<
