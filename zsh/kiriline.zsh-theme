 # modified from gitstatus 

# Source gitstatus.plugin.zsh from $GITSTATUS_DIR or from the same directory
# in which the current script resides if the variable isn't set.
GITSTATUS_DIR="/usr/local/opt/gitstatus"
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

#* `unknown-token` - unknown tokens / errors
#* `reserved-word` - shell reserved words (`if`, `for`)
#* `alias` - aliases
#* `suffix-alias` - suffix aliases (requires zsh 5.1.1 or newer)
#* `builtin` - shell builtin commands (`shift`, `pwd`, `zstyle`)
#* `function` - function names
#* `command` - command names
#* `precommand` - precommand modifiers (e.g., `noglob`, `builtin`)
#* `commandseparator` - command separation tokens (`;`, `&&`)
#* `hashed-command` - hashed commands
#* `path` - existing filenames
#* `path_pathseparator` - path separators in filenames (`/`); if unset, `path` is used (default)
#* `path_prefix` - prefixes of existing filenames
#* `path_prefix_pathseparator` - path separators in prefixes of existing filenames (`/`); if unset, `path_prefix` is used (default)
#* `globbing` - globbing expressions (`*.txt`)
#* `history-expansion` - history expansion expressions (`!foo` and `^foo^bar`)
#* `command-substitution` - command substitutions (`$(echo foo)`)
#* `command-substitution-unquoted` - an unquoted command substitution (`$(echo foo)`)
#* `command-substitution-quoted` - a quoted command substitution (`"$(echo foo)"`)
#* `command-substitution-delimiter` - command substitution delimiters (`$(` and `)`)
#* `command-substitution-delimiter-unquoted` - an unquoted command substitution delimiters (`$(` and `)`)
#* `command-substitution-delimiter-quoted` - a quoted command substitution delimiters (`"$(` and `)"`)
#* `process-substitution` - process substitutions (`<(echo foo)`)
#* `process-substitution-delimiter` - process substitution delimiters (`<(` and `)`)
#* `single-hyphen-option` - single-hyphen options (`-o`)
#* `double-hyphen-option` - double-hyphen options (`--option`)
#* `back-quoted-argument` - backtick command substitution (`` `foo` ``)
#* `back-quoted-argument-unclosed` - unclosed backtick command substitution (`` `foo ``)
#* `back-quoted-argument-delimiter` - backtick command substitution delimiters (`` ` ``)
#* `single-quoted-argument` - single-quoted arguments (`` 'foo' ``)
#* `single-quoted-argument-unclosed` - unclosed single-quoted arguments (`` 'foo ``)
#* `double-quoted-argument` - double-quoted arguments (`` "foo" ``)
#* `double-quoted-argument-unclosed` - unclosed double-quoted arguments (`` "foo ``)
#* `dollar-quoted-argument` - dollar-quoted arguments (`` $'foo' ``)
#* `dollar-quoted-argument-unclosed` - unclosed dollar-quoted arguments (`` $'foo ``)
#* `rc-quote` - two single quotes inside single quotes when the `RC_QUOTES` option is set (`` 'foo''bar' ``)
#* `dollar-double-quoted-argument` - parameter expansion inside double quotes (`$foo` inside `""`)
#* `back-double-quoted-argument` -  backslash escape sequences inside double-quoted arguments (`\"` in `"foo\"bar"`)
#* `back-dollar-quoted-argument` -  backslash escape sequences inside dollar-quoted arguments (`\x` in `$'\x48'`)
#* `assign` - parameter assignments (`x=foo` and `x=( )`)
#* `redirection` - redirection operators (`<`, `>`, etc)
#* `comment` - comments, when `setopt INTERACTIVE_COMMENTS` is in effect (`echo # foo`)
#* `named-fd` - named file descriptor (`echo foo {fd}>&2`)
#* `arg0` - a command word other than one of those enumerated above (other than a command, precommand, alias, function, or shell builtin command).
#* `default` - everything else
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
  emulate -L zsh
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
  # use ✔ 
  (( VCS_STATUS_NUM_STAGED     )) && p+=" ${add}✔${VCS_STATUS_NUM_STAGED}"
  # !42 if have unstaged changes.
  (( VCS_STATUS_NUM_UNSTAGED   )) && p+=" ${modified}!${VCS_STATUS_NUM_UNSTAGED}"
  # ?42 if have untracked files. It's really a question mark, your font isn't broken.
  (( VCS_STATUS_NUM_UNTRACKED  )) && p+=" ${untracked}?${VCS_STATUS_NUM_UNTRACKED}"

  GITSTATUS_PROMPT="${p}%f"

  # The length of GITSTATUS_PROMPT after removing %f and %F.
  GITSTATUS_PROMPT_LEN="${(m)#${${GITSTATUS_PROMPT//\%\%/x}//\%(f|<->F)}}"
}

# Start gitstatusd instance with name "MY". The same name is passed to
# gitstatus_query in gitstatus_prompt_update. The flags with -1 as values
# enable staged, unstaged, conflicted and untracked counters.
gitstatus_stop 'MY' && gitstatus_start -s -1 -u -1 -c -1 -d -1 'MY'

# On every prompt, fetch git status and set GITSTATUS_PROMPT.
autoload -Uz add-zsh-hook
add-zsh-hook precmd gitstatus_prompt_update

# Enable/disable the right prompt options.
setopt no_prompt_bang prompt_percent prompt_subst

# Customize prompt. Put $GITSTATUS_PROMPT in it to reflect git status.
#
# Example:
#
#   user@host ~/projects/skynet master ⇡42
#   % █
#
# The current directory gets truncated from the left if the whole prompt doesn't fit on the line.
local execute_color="%(?.${limegreen}.${red})"
local error_code="%(?..✘ %? )"
local default_path='<…<%6~%<<'
local OS_NAME="$(uname -s)"
# source "${${(%):-%x}:h}/fish_like_collapsed.zsh"
PROMPT=' %(!,ROOT,)'
PROMPT+='%{$cyan%}% [${OS_NAME}]%f '
PROMPT+='%{$cyan%}%$((-GITSTATUS_PROMPT_LEN-1))${default_path}%f'  # blue current working directory
PROMPT+='%B${GITSTATUS_PROMPT:+ $GITSTATUS_PROMPT}%b'      # git status
PROMPT+=$'\n'                                          # new line
PROMPT+='%{$execute_color%}%{$error_code%}%(!.#.➜)%f '                         # %/# (normal/root); green/red (ok/error)
RPROMPT=""

# vim:ft=zsh ts=2 sw=2 sts=2 et fenc=utf-8
