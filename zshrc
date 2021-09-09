#!/usr/bin/env zsh
# vim:fmr={{{,}}}:ft=zsh:fdm=marker

# only run in zsh
[ ! -n "$ZSH_VERSION" ] && return

# profiling zsh
# zmodload zsh/zprof

######################
# Prompt
######################
# {{{
#     __ __    _            _    __   _                
#    / //_/   (_)  _____   (_)  / /  (_)  ____     ___ 
#   / ,<     / /  / ___/  / /  / /  / /  / __ \   / _ \
#  / /| |   / /  / /     / /  / /  / /  / / / /  /  __/
# /_/ |_|  /_/  /_/     /_/  /_/  /_/  /_/ /_/   \___/ 
#                                                      
# modified from gitstaus
# TODO tidy code up

zmodload zsh/datetime
# Source gitstatus.plugin.zsh from $GITSTATUS_DIR or from the same directory
# in which the current script resides if the variable isn't set.
export GITSTATUS_DIR=${GITSTATUS_DIR:-"/usr/local/opt/gitstatus"}
source "${GITSTATUS_DIR:-${${(%):-%x}:h}}/gitstatus.plugin.zsh" || return

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
# Background	59	
# Current Line	60	
# Foreground	231	
# Comment	103	
# Cyan	159	
# Green	120	
# Orange	222	
# Pink	212	
# Purple	183	
# Red	210	
# Yellow	229

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
    turquoise="$fg[cyan]"
    orange="$fg[yellow]"
    purple="$fg[magenta]"
    pink="$fg[pink]"
    limegreen="$fg[green]"
    grey="$fg[grey]"
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

function gitstatus_prompt_update() {
  setopt localoptions noshwordsplit
  prompt_check_cmd_exec_time
  unset prompt_cmd_timestamp
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
  # Execution time
  [[ -n $prompt_cmd_exec_time ]] && p+=" %F{229}${prompt_cmd_exec_time}"

  GITSTATUS_PROMPT="${p}%f"

  # The length of GITSTATUS_PROMPT after removing %f and %F.
  GITSTATUS_PROMPT_LEN="${(m)#${${GITSTATUS_PROMPT//\%\%/x}//\%(f|<->F)}}"
}

# Start gitstatusd instance with name "MY". The same name is passed to
# gitstatus_query in gitstatus_prompt_update. The flags with -1 as values
# enable staged, unstaged, conflicted and untracked counters.
gitstatus_stop 'MY' && gitstatus_start -s -1 -u -1 -c -1 -d -1 'MY'

function prompt_preexec() {
  typeset -g prompt_cmd_timestamp=$EPOCHREALTIME
}

# On every prompt, fetch git status and set GITSTATUS_PROMPT.
autoload -Uz add-zsh-hook
add-zsh-hook preexec prompt_preexec
add-zsh-hook precmd gitstatus_prompt_update

# Enable/disable the right prompt options.
setopt no_prompt_bang prompt_percent prompt_subst

# The current directory gets truncated from the left if the whole prompt doesn't fit on the line.
local execute_color="%(?.${limegreen}.${red})"
local error_code="%(?..×%? )"
local default_path='<…<%6~%<<'
# source "${${(%):-%x}:h}/fish_like_collapsed.zsh"
PROMPT=' %(!,ROOT,)'
PROMPT+='%{$cyan%}% [${SYSTEM}]%f '
PROMPT+='%{$cyan%}%$((-GITSTATUS_PROMPT_LEN-1))${default_path}%f'  # blue current working directory
PROMPT+='%B${GITSTATUS_PROMPT:+ $GITSTATUS_PROMPT}%b'      # git status
PROMPT+=$'\n'                                          # new line
PROMPT+='%{$execute_color%}%{$error_code%}%(!.#.ᐅ)%f '                         # %/# (normal/root); green/red (ok/error)
RPROMPT=""
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

#}}}

######################
# plugins
######################
# could get some ideas from famous Zsh frameworks
# {{{
# cost about 9ms
[ -e $ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && source "$ZSHDIR/vendor/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
[ -e $ZSHDIR/vendor/zsh-autosuggestions/zsh-autosuggestions.zsh ] && source "$ZSHDIR/vendor/zsh-autosuggestions/zsh-autosuggestions.zsh"
# source "$ZSHDIR/vendor/z.sh"
[ -e "$ZSHDIR/../zoxide.zsh" ] && source "$ZSHDIR/../zoxide.zsh"

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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# haskell completion
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
# }}}
# profiling
# zprof
