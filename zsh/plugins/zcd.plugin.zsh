#!/usr/bin/env zsh
# vim:fmr={,}

function __zcd_pwd() {
  builtin pwd -P
}

function __zcd_cd() {
  builtin cd "$@"
  [[ -n $ZCD_ECHO ]] && __zcd_pwd
}

function __zcd_cdi() {
  builtin cd "$@"
  [[ -n $ZCD_ECHO ]] && __zcd_pwd
}

# utils
function __zcd_unset() {
  builtin unalias "$@" &>/dev/null
  builtin unfunction "$@" &>/dev/null
  builtin unset "$@" &>/dev/null
}

# jump to a directory with keywords
function __zcd_z() {
  if [[ "$#" -eq 0 ]]; then
    __zcd_cd ~
  elif [[ "$#" -eq 1 ]] && [[ "$1" = "-" ]]; then
    # jumps to zsh's $OLDPWD
    if [ -n "$OLDPWD" ]; then
      __zcd_cd "${OLDPWD}"
    else
      builtin printf -n 'zcd: $OLDPWD not set'
      return 1
    fi
  elif [[ "$#" -eq 1 ]] && [[ -d "$1" ]]; then
    # use cd directly if $1 is a valid path
    __zcd_cd $1
  else
    # sorting candidates
    local __zcd_result="$(zcd query -- "$@")" && __zcd_cd "$__zcd_result"
    return 0
  fi
}

# query interactively
function __zcd_zi(){
  local __zcd_result="$(zcd list | fzf "$@" --preview="tree {} -L 1")" && __zcd_cd "$__zcd_result)"
}

# zsh hook
function __zcd_insert_or_update() {
  zcd insert -- "$(__zcd_pwd)"
}
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd __zcd_insert_or_update

# zcd
__zcd_unset "z"
function z() {
  __zcd_z "$@"
}

# interactive
function zi() {
  __zcd_zi "$@"
}

