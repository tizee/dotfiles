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
# {{{
#     __ __    _            _    __   _
#    / //_/   (_)  _____   (_)  / /  (_)  ____     ___
#   / ,<     / /  / ___/  / /  / /  / /  / __ \   / _ \
#  / /| |   / /  / /     / /  / /  / /  / / / /  /  __/
# /_/ |_|  /_/  /_/     /_/  /_/  /_/  /_/ /_/   \___/
#
#
# modified from gitstaus
# {{{
zmodload zsh/datetime
# Source gitstatus.plugin.zsh from $GITSTATUS_DIR or from the same directory
# in which the current script resides if the variable isn't set.

if $(uname -r | grep 'microsoft' > /dev/null); then
  # wsl
  export GITSTATUS_DIR=${GITSTATUS_DIR:-"/home/linuxbrew/.linuxbrew/opt/gitstatus"}
  # ip
  export LOCAL_IP=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}')
  # x-server
  # export DISPLAY="$LOCAL_IP:0"
  # https://github.com/microsoft/wslg/wiki/Diagnosing-%22cannot-open-display%22-type-issues-with-WSLg
  # WSLg's X server is running on display 0 by default, which is used for GUI apps to connect the right display.
  export DISPLAY=":0"
  # zig lang
  # export PATH="$HOME/zig-linux-x86_64-0.10.0-dev.3685+dae7aeb33/:$PATH"
else
  # macOS
  export GITSTATUS_DIR=${GITSTATUS_DIR:-"/opt/homebrew/opt/gitstatus"}
fi

if [ -d $GITSTATUS_DIR ]; then
  source "${GITSTATUS_DIR:-${${(%):-%x}:h}}/gitstatus.plugin.zsh" || return
else
  echo "gitstatus not found"
fi



# Sets GITSTATUS_PROMPT to reflect the state of the current git repository. Empty if not
# in a git repository. In addition, sets GITSTATUS_PROMPT_LEN to the number of columns
# $GITSTATUS_PROMPT will occupy when printed.
#
# Example:
#
#   GITSTATUS_PROMPT='master ⇣42⇡42 ⇠42⇢42 *42 merge ~42 +42 !42 ?42'
#   GITSTATUS_PROMPT_LEN=39
#
#   master  current branch
#      ⇣42  local branch is 42 commits behind the remote
#      ⇡42  local branch is 42 commits ahead of the remote
#      ⇠42  local branch is 42 commits behind the push remote
#      ⇢42  local branch is 42 commits ahead of the push remote
#      *42  42 stashes
#    merge  merge in progress
#      ~42  42 merge conflicts
#      +42  42 staged changes
#      !42  42 unstaged changes
#      ?42  42 untracked files

# Dracula color scheme
# Background  59
# Current Line  60
# Foreground  231
# Comment  103
# Cyan  159
# Green  120
# Orange  222
# Pink  212
# Purple  183
# Red  210
# Yellow  229

#use extended color palette if available
if [[ $TERM = *256color* || $TERM = *rxvt* ]]; then
   local bg_color="%F{59}"
   local current_color="%F{60}"
   local fg_color="%F{231}"
   local comment_color="%F{103}"
   local turquoise="%F{81}"
   local cyan="%F{159}"
   local orange="%F{172}"
   local yellow="%F{229}"
   local purple="%F{183}"
   local red="%F{210}"
   local pink="%F{212}"
   local limegreen="%F{120}"
else
   local turquoise="$fg[cyan]"
   local orange="$fg[yellow]"
   local purple="$fg[magenta]"
   local pink="$fg[pink]"
   local limegreen="$fg[green]"
   local grey="$fg[grey]"
fi

# Declare the variable
typeset -A ZSH_HIGHLIGHT_STYLES

#ZSH_HIGHLIGHT_STYLES["unknown-token"]='fg=red,bold'

# To differentiate aliases from other command types
ZSH_HIGHLIGHT_STYLES[alias]='fg=magenta,bold'

# To have paths colored instead of underlined
ZSH_HIGHLIGHT_STYLES[path]='fg=cyan,underline'

# To disable highlighting of globbing expressions
# ZSH_HIGHLIGHT_STYLES[globbing]='none'

function gitstatus_prompt_update() {
  setopt localoptions noshwordsplit
  # emulate -L zsh
  typeset -g  GITSTATUS_PROMPT=''
  typeset -gi GITSTATUS_PROMPT_LEN=0

  # Call gitstatus_query synchronously. Note that gitstatus_query can also be called
  # asynchronously; see documentation in gitstatus.plugin.zsh.
  gitstatus_query 'MY'                  || return 1  # error
  [[ $VCS_STATUS_RESULT == 'ok-sync' ]] || return 0  # not a git repo

  local        add=$limegreen
  local      clean=$purple
  local   modified=$yellow
  local  untracked=$orange
  local conflicted=$pink
  local      stash=$fg_color

  local p

  local where  # branch name, tag or commit
  if [[ -n $VCS_STATUS_LOCAL_BRANCH ]]; then
    where=$VCS_STATUS_LOCAL_BRANCH
  elif [[ -n $VCS_STATUS_TAG ]]; then
    p+='%f#'
    where=$VCS_STATUS_TAG
  else
    p+='%f@'
    where=${VCS_STATUS_COMMIT[1,8]}
  fi

  (( $#where > 32 )) && where[13,-13]="…"  # truncate long branch names and tags
  p+="${clean}${where//\%/%%}"             # escape %

  # ⇣42 if behind the remote.
  (( VCS_STATUS_COMMITS_BEHIND )) && p+=" ${add}⇣${VCS_STATUS_COMMITS_BEHIND}"
  # ⇡42 if ahead of the remote; no leading space if also behind the remote: ⇣42⇡42.
  (( VCS_STATUS_COMMITS_AHEAD && !VCS_STATUS_COMMITS_BEHIND )) && p+=" "
  (( VCS_STATUS_COMMITS_AHEAD  )) && p+="${add}⇡${VCS_STATUS_COMMITS_AHEAD}"
  # ⇠42 if behind the push remote.
  (( VCS_STATUS_PUSH_COMMITS_BEHIND )) && p+=" ${add}⇠${VCS_STATUS_PUSH_COMMITS_BEHIND}"
  (( VCS_STATUS_PUSH_COMMITS_AHEAD && !VCS_STATUS_PUSH_COMMITS_BEHIND )) && p+=" "
  # ⇢42 if ahead of the push remote; no leading space if also behind: ⇠42⇢42.
  (( VCS_STATUS_PUSH_COMMITS_AHEAD  )) && p+="${add}⇢${VCS_STATUS_PUSH_COMMITS_AHEAD}"
  # *42 if have stashes.
  (( VCS_STATUS_STASHES        )) && p+=" ${stash}*${VCS_STATUS_STASHES}"
  # 'merge' if the repo is in an unusual state.
  [[ -n $VCS_STATUS_ACTION     ]] && p+=" ${conflicted}${VCS_STATUS_ACTION}"
  # ~42 if have merge conflicts.
  (( VCS_STATUS_NUM_CONFLICTED )) && p+=" ${conflicted}~${VCS_STATUS_NUM_CONFLICTED}"
  # +42 if have staged changes.
  # use ✓
  (( VCS_STATUS_NUM_STAGED     )) && p+=" ${add}✓${VCS_STATUS_NUM_STAGED}"
  # !42 if have unstaged changes.
  (( VCS_STATUS_NUM_UNSTAGED   )) && p+=" ${modified}!${VCS_STATUS_NUM_UNSTAGED}"
  # ?42 if have untracked files. It's really a question mark, your font isn't broken.
  (( VCS_STATUS_NUM_UNTRACKED  )) && p+=" ${untracked}?${VCS_STATUS_NUM_UNTRACKED}"

  GITSTATUS_PROMPT="${p}%f"

  # The length of GITSTATUS_PROMPT after removing %f and %F.
  local invisible='%([BSUbfksu]|([FBK]|){*})'
  local visible_git_prompt=${(S)GITSTATUS_PROMPT//$~invisible}
  # GITSTATUS_PROMPT_LEN="${(m)#${${GITSTATUS_PROMPT//\%\%/x}//\%(f|<->F)}}"
  GITSTATUS_PROMPT_LEN="${(m)#visible_git_prompt}"
}

# Start gitstatusd instance with name "MY". The same name is passed to
# gitstatus_query in gitstatus_prompt_update. The flags with -1 as values
# enable staged, unstaged, conflicted and untracked counters.
gitstatus_stop 'MY' && gitstatus_start -s -1 -u -1 -c -1 -d -1 'MY'
# }}}
# On every prompt, fetch git status and set GITSTATUS_PROMPT.
autoload -Uz add-zsh-hook
add-zsh-hook precmd gitstatus_prompt_update

# command time of execution {{{
function prompt_preexec() {
  typeset -g prompt_cmd_timestamp=$EPOCHREALTIME
}

add-zsh-hook preexec prompt_preexec


function __compress_path() {
  local path="$1"
  local compressed=""
  local dir="${path%/*}"  # Extract directory (dirname)
  local last="${path##*/}"  # Extract basename

  # Handle root path case
  if [[ "$dir" == "$path" ]]; then
    dir=""
  fi

  # Compress parent directories
  if [[ -n "$dir" ]]; then
    for part in $(echo "$dir" | /usr/bin/tr "/" " "); do
      compressed+="/${part:0:1}"
    done
  fi

  # Append the last component
  compressed+="/$last"

  echo "$compressed"
}

function human_time() {
  local human total_seconds=$1 var=$2
  local -ri days=$(( total_seconds / 60 / 60 / 24 ))
  local -ri hours=$(( total_seconds / 60 / 60 % 24 ))
  local -ri minutes=$(( total_seconds / 60 % 60 ))
  local -rF seconds=$(( total_seconds % 60 ))
  (( days > 0 )) && human+="${days}d "
  (( hours > 0 )) && human+="${hours}h "
  (( minutes > 0 )) && human+="${minutes}m "
  if (( seconds >= 1 )); then
    local human_sec
    printf -v human_sec '%.3fs' ${seconds}
    human+="$human_sec"
  else
    local human_ms
    printf -v human_ms '%ims' $(( seconds * 1000 ))
    human+="$human_ms"
  fi
  # Store human readable time in a variable as specified by the caller
  typeset -g "${var}"="${human}"
}

function prompt_check_cmd_exec_time() {
  local -F elapsed
  (( elapsed = EPOCHREALTIME - ${prompt_cmd_timestamp:-$EPOCHREALTIME} ))
  typeset -g prompt_cmd_exec_time=
  # show execution time when larger than 50ms
  (( elapsed * 1000 > ${CMD_MAX_EXEC_TIME:-50} )) && {
    human_time $elapsed "prompt_cmd_exec_time"
  }
}

# Execution time
function cmd_exec_time_helper() {
  prompt_check_cmd_exec_time
  unset prompt_cmd_timestamp
}

add-zsh-hook precmd cmd_exec_time_helper
# }}}

# Enable/disable the right prompt options.
setopt no_prompt_bang prompt_percent prompt_subst

# The current directory gets truncated from the left if the whole prompt doesn't fit on the line.

local err_color="%(?.${limegreen}.${red})"
local return_code="%(?..%?)"
local shell_symbol='➜'
local prompt_symbol="%(!.${shell_symbol}#.${shell_symbol})"
local NEWLINE=$'\n'
local invisible='%([BSUbfksu]|([FBK]|){*})'
# only run once
case $SYSTEM_DISTRO in
  Darwin)
    local sys_icon=' '
    # sys_icon+=$(sw_vers -productVersion)
    ;;
  Arch)
    local sys_icon='󰣇 '
    ;;
  Ubuntu)
    local sys_icon=' '
    ;;
  Dock)
    local sys_icon='󰡨 '
    ;;
  Linux)
    local sys_icon=' '
    # sys_icon+=$(uname -r)
    ;;
  *)
    local sys_icon=' '
    ;;
esac

# source "${${(%):-%x}:h}/fish_like_collapsed.zsh"

# $COLUMNS terminal width
function __update_tz_prompt() {
  typeset -g PROMPT_ZLE_MODE="%{%F{159}%}[insert]%f"

  # Get current directory
  local current_dir="${PWD/#$HOME/~}"

  # Calculate space available for path
  # Other prompt elements (estimate their length without formatting)
  local sys_part="%(!,[ROOT],)"
  # Only include sys_icon if not in TMUX
  [[ -z "$TMUX" ]] && sys_part+=" ${sys_icon}"
  local sys_part_len=${#${(S)sys_part//$~invisible}}

  local git_part="${GITSTATUS_PROMPT:+ $GITSTATUS_PROMPT}"
  local git_part_len=${#${(S)git_part//$~invisible}}

  local mode_part="[insert] "
  local mode_part_len=${#mode_part}

  local time_part=""
  [[ -n $prompt_cmd_exec_time ]] && time_part=" ${prompt_cmd_exec_time} "
  local time_part_len=${#time_part}

  # Calculate available space for path
  local available_width=$(( COLUMNS - sys_part_len - git_part_len - mode_part_len - time_part_len - 5 )) # 5 for safety margin

  # Decide whether to compress path
  local path_display
  if (( ${#current_dir} > available_width )); then
    path_display="$(__compress_path "$current_dir")"
  else
    path_display="$current_dir"
  fi

  # Build the prompt components
  prompt_top_left="%(!,[ROOT],)"
  # Only add sys_icon when not in TMUX
  if [[ -z "$TMUX" ]]; then
    prompt_top_left+="%{$grey%}% ${sys_icon}%f "
  else
    prompt_top_left+="%{$grey%}% %f "
  fi
  prompt_top_left+="%{$cyan%}${path_display}%f"

  prompt_top_right="%B${GITSTATUS_PROMPT:+ $GITSTATUS_PROMPT}%f "

  # Include command time at the end of the prompt line
  [[ -n $prompt_cmd_exec_time ]] && prompt_top_right+="%F{229} ${prompt_cmd_exec_time}%f "

  left=${(S)prompt_top_left//$~invisible}
  right=${(S)prompt_top_right//$~invisible}
  (( prompt_top_len=${#left}+${#right}))

  prompt_input_line="$NEWLINE%b$err_color$prompt_symbol%f "

  PROMPT='${prompt_top_left}${prompt_top_right}${PROMPT_ZLE_MODE}${prompt_input_line}'
  RPROMPT="$err_color${return_code}%f "
}

# trick from 2007!
# http://www.zsh.org/mla/users/2007/msg00944.html
# TMOUT=1
# TRAPALRM() {
    # zle reset-prompt
# }

add-zsh-hook precmd __update_tz_prompt
# }}}

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
