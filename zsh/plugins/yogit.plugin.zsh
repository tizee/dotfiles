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
# Note that this plugin is conflicted with fzf-tab.

_yogit_basic_prefix=$YOGIT_BASIC_PREFIX
_yogit_interactive_prefix=$YOGIT_INTERACTIVE_PREFIX
if [[ -z $YOGIT_BASIC_PREFIX  ]]; then
_yogit_basic_prefix='ygg'
fi
if [[ -z $YOGIT_INTERACTIVE_PREFIX ]]; then
_yogit_interactive_prefix='ygi'
fi

if [ $SYSTEM = Darwin ]; then
  alias _yogit_open='open'
elif [ $SYSTEM = Linux ]; then
  alias _yogit_open='xdg-open'
fi

function yogit::help() {
  # basic
  printf "--> basic usage\n"
  print "${_yogit_basic_prefix}htest     : ssh -T git@github.com"
  print "${_yogit_basic_prefix}st        : git status"
  print "${_yogit_basic_prefix}sc        : git clone with depth 1 and shallow clone submodules with depth 1"
  print "${_yogit_basic_prefix}cob       : git checkout -b"
  print "${_yogit_basic_prefix}a         : git add"
  print "${_yogit_basic_prefix}c         : git commit -v"
  print "${_yogit_basic_prefix}c!        : git commit --amend"
  print "${_yogit_basic_prefix}cn!       : git commit --amend --no-edit"
  print "${_yogit_basic_prefix}push      : git push origin current_branch"
  print "${_yogit_basic_prefix}pull      : git pull origin current_branch"
  print "${_yogit_basic_prefix}sst       : list staged and unstaged file names only"
  print "${_yogit_basic_prefix}open      : open/xdg-open repo url in browser"
  print "${_yogit_basic_prefix}url       : ${_yogit_basic_prefix}url <remote-name> to print remote url"
  print "${_yogit_basic_prefix}rtags     : list remote tags with 'git ls-remote --tags'"
  print "${_yogit_basic_prefix}ftag      : fetch a remote tag with 'git fetch origin refs/tags/<lname>:refs/tags<rname> --no-tags'"
  # github repos
  print "${_yogit_basic_prefix}ghsize    : get size of github repo"
  print "${_yogit_basic_prefix}ghurl     : github repo worktree url of current commit"
  print "${_yogit_basic_prefix}opengh    : open github repo worktree url of current commit"
  print "${_yogit_basic_prefix}lr        : remote list"
  print "${_yogit_basic_prefix}ls        : git ls-files --others --exclude-standard"
  print "${_yogit_basic_prefix}pickclone : git clone --sparse --filter=blob:none --depth=1 --no-checkout"
  print "after setting up the sparse-checkout, use git read-tree -mu HEAD"
  print "${_yogit_basic_prefix}sub       : git submodule update --init --recursive"
  print "${_yogit_basic_prefix}br        : git branch -r"
  print "${_yogit_basic_prefix}parse     : git rev-parse [input] | cut -d 1-6"
  print "git branch -r --merged          : ${_yogit_basic_prefix}br --merged"
  print "git branch --merged             : ${_yogit_basic_prefix}br --merged"
  print "${_yogit_basic_prefix}br        : git branch -r"
  print "${_yogit_basic_prefix}bru       : git branch -u <remote>/<branch> <local-branch> "
  print "${_yogit_basic_prefix}prune     : git remote prune origin"
  print "${_yogit_basic_prefix}bm        : rename branch and update its tracked origin branch"
  print ""
  printf "--> interactive usage\n"
  print "${yogit_checkout:-${_yogit_interactive_prefix}co}    :  checkout with fzf"
  print "${yogit_cherry_pick:-${_yogit_interactive_prefix}cp} :  cherry pick with fzf"
  print "${yogit_show:-${_yogit_interactive_prefix}sc}        :  select commit with fzf"
  print "${yogit_branch:-${_yogit_interactive_prefix}br}      :  select branch with fzf"
  print "${yogit_diff:-${_yogit_interactive_prefix}diff}      :  diff with fzf"
  print "${_yogit_interactive_prefix}rtags    :  select a tag name with fzf"
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
  local ref=$(git symbolic-ref --quiet HEAD 2> /dev/null | sed -E 's#refs/heads/(.*)#\1#')
  # TODO detached head
  echo ${ref}
}
# }}}

# basic aliases start with prefix gg {{{

function yogit::url() {
  local remote_name="${1:-origin}"
  git config --get remote.$remote_name.url | sed -E 's/^[^@]*@([^:\/]*)[:\/]/https:\/\/\1\//' | sed 's/\.git//'
}

function yogit::open() {
  _yogit_open $(yogit::url)
}

function yogit::list_remote_tags() {
  git ls-remote --tags | awk -F '/tags\/' '{print $2}'
}

alias "${_yogit_basic_prefix}rtags"='yogit::list_remote_tags'

function yogit::fetch_remote_tag() {
  # syntatic sugar: git fetch origin refs/tags/local-tag-name:refs/tags/remote-tag-name --no-tags
  git fetch --depth=1 origin tag $1 --no-tags
}

alias "${_yogit_basic_prefix}ftag"='yogit::fetch_remote_tag'

function yogit::gh_commit_url() {
  local commit=$(git rev-parse --verify ${1:-HEAD})
  local base_url=$(yogit::url)
  if [[ "$base_url" =~ .*"github".* ]]; then
    echo "${base_url}/tree/${commit}"
  else
    echo "Remote isn't not a Github repository" && exit 1
  fi
}

# open Github's permanent url of given commit of a git worktree
function yogit::open_gh_commit() {
  _yogit_open $(yogit::gh_commit_url $1)
}

alias "${_yogit_basic_prefix}ghsize"='yogit::get_gh_repo_size'

# idea from https://stackoverflow.com/questions/2882620/is-it-possible-to-remote-count-object-and-size-of-git-repository
function yogit::get_gh_repo_size() {
  if [[ $# > 0 ]]; then
    local organ=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:)([^/]+)/([^/.]+)(\.git)?#\2#p')
    local repo_name=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:)([^/]+)/([^/.]+)(\.git)?#\3#p')
    local repo_size=$(curl -sL https://api.github.com/repos/${organ}/${repo_name} | grep -m 1 '"size"' | awk -F ':|,' '{print $2}')
    # convert to human readable size
    printf "${organ}/${repo_name} size:\n $(( $repo_size / 1024 )) Mb $(( $repo_size % 1024 )) Kb\n"
  else
    echo 'usage: [repo url]
url format:
    git@github.com:${organ}/${repo_name}.git
    https://github.com/${organ}/${repo_name}.git
    https://github.com/${organ}/${repo_name}
    '
  fi
}

alias "${_yogit_basic_prefix}ghurl"='yogit::gh_commit_url'
alias "${_yogit_basic_prefix}opengh"='yogit::open_gh_commit'

# open url of remote repo
alias "${_yogit_basic_prefix}open"='yogit::open'

# test github
alias "${_yogit_basic_prefix}htest"='ssh -T git@github.com'

# status
alias "${_yogit_basic_prefix}st"='git status'

# shallow clone with --depth 1
function yogit::shallowclone(){
  print "clone with --depth 1 --recurse-submodules -j8 --shallow-submodules"
  # https://stackoverflow.com/questions/3796927/how-do-i-git-clone-a-repo-including-its-submodules
  # https://stackoverflow.com/questions/2144406/how-to-make-shallow-git-submodules
  git clone $@ --depth 1 --recurse-submodules -j8 --shallow-submodules
}

alias "${_yogit_basic_prefix}sc"='yogit::shallowclone'

# git checkout
alias "${_yogit_basic_prefix}cob"='git checkout -b'

# add
alias "${_yogit_basic_prefix}a"='git add'

# commit
alias "${_yogit_basic_prefix}c"='git commit -v'
alias "${_yogit_basic_prefix}c!"='git commit --amend'
alias "${_yogit_basic_prefix}cn!"='git commit --amend --no-edit'

# push
alias "${_yogit_basic_prefix}push"='git push origin "$(yogit::current_branch)"'

# pull
alias "${_yogit_basic_prefix}pull"='git pull origin "$(yogit::current_branch)"'

# change remote branch that current branch tracked
alias "${_yogit_basic_prefix}bru"='yogit::change_current_tracked'

function yogit::change_current_tracked(){
  git branch -u $1 $(yogit::current_branch)
}

# rename and change tracked branch of origin
alias "${_yogit_basic_prefix}bm"='yogit::rename_for_remote_origin_branch'
function yogit::rename_for_remote_origin_branch(){
  git branch -m $1 $2
  git branch -u origin/$2 $2
  print "1. git fetch origin"
  print "2. git remote set-head -a"
}

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
# --no-checkout further accelerates the clone
alias "${_yogit_basic_prefix}pickclone"='git clone --sparse --filter=blob:none --depth=1 --no-checkout'

alias "${_yogit_basic_prefix}sub"='git submodule update --init --recursive'

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
# print current remote url
alias "${_yogit_basic_prefix}url"='yogit::url'
# list remote
alias "${_yogit_basic_prefix}lr"='git remote -v'
# prune unused branches
alias "${_yogit_basic_prefix}prune"='git remote prune origin'

function yogit::parse() {
  local object_name="${1:-HEAD}"
  # always print short sha-1 of commit
  git rev-parse "$object_name" | cut -c 1-7
}
# print current commit
alias "${_yogit_basic_prefix}last"="git log -1 HEAD --pretty='%h'"
# print the commit of given object
alias "${_yogit_basic_prefix}parse"='yogit::parse'
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

function yogit::select_remote_tag() {
  yogit::list_remote_tags | fzf
}

if [[ -z "$DISABLE_YOGIT_INTERACTIVE" ]]; then
# checkout with fzf
alias "${yogit_checkout:-${_yogit_interactive_prefix}co}"='yogit::checkout'
alias "${yogit_cherry_pick:-${_yogit_interactive_prefix}cp}"='yogit::cherry_pick'
alias "${yogit_show:-${_yogit_interactive_prefix}sc}"='yogit::select::commit'
alias "${yogit_branch:-${_yogit_interactive_prefix}br}"='yogit::branch'
alias "${yogit_diff:-${_yogit_interactive_prefix}diff}"='yogit::diff'
alias "${_yogit_interactive_prefix}rtags"='yogit::select_remote_tag'
fi
# }}}
