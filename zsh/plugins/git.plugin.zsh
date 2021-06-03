#!/usr/bin/env zsh
#                                           _ _   
#                         _   _  ___   __ _(_) |_ 
#                        | | | |/ _ \ / _` | | __|
#                        | |_| | (_) | (_| | | |_ 
#                         \__, |\___/ \__, |_|\__|
#                         |___/       |___/       
# by Jeff Chiang(Tizee)
# git >= 2.29
# Have fun ;-)

# This plugin is conflicted with fzf-tab. ;) 

# --no-optional-locks is equivalent to GIT_OPTIONAL_LOCKS=0
# do not perform optional operations require locks.

_yogit_basic_prefix=$YOGIT_BASIC_PREFIX
_yogit_interactive_prefix=$YOGIT_INTERACTIVE_PREFIX
if [[ -z $YOGIT_BASIC_PREFIX  ]]; then
_yogit_basic_prefix='ygg'
fi
if [[ -z $YOGIT_INTERACTIVE_PREFIX ]]; then
_yogit_interactive_prefix='ygi'
fi


function yogit::help() {
  print "${yogit_checkout:-${_yogit_interactive_prefix}co}: checkout with fzf"
  print "${yogit_cherry_pick:-${_yogit_interactive_prefix}cp}: cherry pick with fzf"
  print "${yogit_show:-${_yogit_interactive_prefix}sc}: select commit with fzf"
  print "${yogit_branch:-${_yogit_interactive_prefix}br}: select branch with fzf"
  print "${yogit_diff:-${_yogit_interactive_prefix}diff}: diff with fzf"
  print ""
  # basic
  print "${_yogit_basic_prefix}htest: ssh -T git@github.com"
  print "${_yogit_basic_prefix}st: git status"
  print "${_yogit_basic_prefix}cob: git checkout -b"
  print "${_yogit_basic_prefix}a: git add"
  print "${_yogit_basic_prefix}c: git commit -v"
  print "${_yogit_basic_prefix}c!: git commit --amend"
  print "${_yogit_basic_prefix}push: git push origin current_branch"
  print "${_yogit_basic_prefix}pull: git pull origin current_branch"
  print "${_yogit_basic_prefix}sst: list staged and unstaged file names only"
  print "${_yogit_basic_prefix}ls: git ls-files --others --exclude-standard"
  print "${_yogit_basic_prefix}pickclone: git clone --sparse --filter=blob:none --depth=1"
}

alias "${yogit_help:-${_yogit_interactive_prefix}help}"='yogit::help'
alias "${yogit_help:-${_yogit_basic_prefix}help}"='yogit::help'

# Utilities {{{
# To speed up, use gitstatus :D
# fallback to git if not found

function yogit::is_git_repo(){
  git rev-parse --is-inside-work-tree 2>&1 >/dev/null
}

function yogit::current_branch() {
  yogit::is_git_repo || return 1
  local ref=$(git symbolic-ref --quiet HEAD 2> /dev/null)
  # TODO detached head
  echo ${ref#refs/heads/}
}
# }}}

# basic aliases start with prefix gg {{{
# test github
alias "${_yogit_basic_prefix}htest"='ssh -T git@github.com'

# status
alias "${_yogit_basic_prefix}st"='git status'

# git checkout
alias "${_yogit_basic_prefix}cob"='git checkout -b'


# add
alias "${_yogit_basic_prefix}a"='git add'

# commit
alias "${_yogit_basic_prefix}c"='git commit -v'
alias "${_yogit_basic_prefix}c!"='git commit --amend'

# push
alias "${_yogit_basic_prefix}push"='git push origin "$(yogit::current_branch)"'

# pull
alias "${_yogit_basic_prefix}pull"='git pull origin "$(yogit::current_branch)"'

function yogit::staged_and_unstaged(){
  git status --porcelain | sed 's/^...//'
}
# list both unstaged and staged files
alias "${_yogit_basic_prefix}sst"='yogit::staged_and_unstaged'
# only diff modifled files
# alias "${_yogit_basic_prefix}stdiffm"='yogit::staged_and_unstaged | xargs git diff'
# diff all files that git status given
# alias "${_yogit_basic_prefix}stdiff"='yogit::staged_and_unstaged | xargs git diff'
# list only untracked files
alias "${_yogit_basic_prefix}ls"='git ls-files --others --exclude-standard'
# --sparse initializes the sparse-checkout file so the working directory starts only with the files in the root directory
# --filter=blob:none will exclude files so we could fetch them when needed
# --depth=1 truncate commit history to leave only the latest commit(may cause problems)
alias "${_yogit_basic_prefix}pickclone"='git clone --sparse --filter=blob:none --depth=1'
# use git sparse-checkout add [file]

# restore

# bisect
alias "${_yogit_basic_prefix}bs"="git bisect"
alias "${_yogit_basic_prefix}bsr"="git bisect reset"
alias "${_yogit_basic_prefix}bss"="git bisect start"
alias "${_yogit_basic_prefix}bsg"="git bisect good"
alias "${_yogit_basic_prefix}bsg"="git bisect bad"

# cherry-pick
alias "${_yogit_basic_prefix}cp"="git cherry-pick"
alias "${_yogit_basic_prefix}cpa"="git cherry-pick --abort"
alias "${_yogit_basic_prefix}cpc"="git cherry-pick --continue"

# stash
alias "${_yogit_basic_prefix}sta"='git stash push'
alias "${_yogit_basic_prefix}staclear"='git stash clear'

# diff
# diff unstaged files
alias "${_yogit_basic_prefix}dn"='git diff --name-only'
# diff only staged files
alias "${_yogit_basic_prefix}dns"="git diff --name-only --staged"

# rebase
alias "${_yogit_basic_prefix}b"='git rebase'
alias "${_yogit_basic_prefix}ba"='git rebase --abort'
alias "${_yogit_basic_prefix}bc"='git rebase --continue'

# reset
# reset head
alias "${_yogit_basic_prefix}rh"="git reset"
alias "${_yogit_basic_prefix}rhh"="git reset --hard"

# remote
# list remote branch
alias "${_yogit_basic_prefix}br"='git branch -r'
# list remote url
alias "${_yogit_basic_prefix}url"='git remote -v'

# }}}

# Interactive commands aliases start with prefix gi {{{
# could use forgit as alternate
function yogit::branch(){
  git branch | sed -E 's/\*//'| fzf | sed -E 's/ //g'
}

# select abbrviation commit
function yogit::select::commit(){
  if [[ -n $1 ]];then
    git log $1 --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' | sed  | 2>/dev/null
  else
    # Use current branch
    local branch=$(yogit::branch)
    if [[ -n $branch ]]; then
      git log $branch --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' 2>/dev/null
    fi
  fi
}

# git cherry-pick with fzf
function yogit::cherry_pick(){
  if [ -n $1 ];then
    git cherry-pick -e $(yogit::show $1)
  else
    git cherry-pick -e $(yogit::show)
  fi
}

# git diff with fzf
function yogit::diff() {
  preview="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview $preview
}

function yogit::checkout() {
  git branch | fzf | git checkout
}

if [[ -z "$DISABLE_YOGIT_INTERACTIVE" ]]; then
# checkout with fzf
alias "${yogit_checkout:-${_yogit_interactive_prefix}co}"='yogit::checkout'
alias "${yogit_cherry_pick:-${_yogit_interactive_prefix}cp}"='yogit::cherry_pick'
alias "${yogit_show:-${_yogit_interactive_prefix}sc}"='yogit::select::commit'
alias "${yogit_branch:-${_yogit_interactive_prefix}br}"='yogit::branch'
alias "${yogit_diff:-${_yogit_interactive_prefix}diff}"='yogit::diff'
fi
# }}}
