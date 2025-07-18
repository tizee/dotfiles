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

# Color definitions for better output
_yogit_color_reset="\033[0m"
_yogit_color_green="\033[0;32m"
_yogit_color_yellow="\033[0;33m"
_yogit_color_blue="\033[0;34m"
_yogit_color_red="\033[0;31m"
_yogit_color_cyan="\033[0;36m"
_yogit_color_magenta="\033[0;35m"
_yogit_color_bold="\033[1m"
# Detect the number of CPU cores
if command -v nproc >/dev/null 2>&1; then
    CORES=$(nproc)  # Linux
elif command -v sysctl >/dev/null 2>&1; then
    CORES=$(sysctl -n hw.ncpu)  # macOS
else
    CORES=8  # Fallback to 8 if detection fails
fi

# Check for required commands
function yogit::check_dependencies() {
  local missing_deps=()
  for cmd in git fzf; do
    if ! command -v $cmd &>/dev/null; then
      missing_deps+=($cmd)
    fi
  done

  if [[ ${#missing_deps[@]} -gt 0 ]]; then
    printf "${_yogit_color_yellow}Warning: Missing dependencies: ${missing_deps[*]}${_yogit_color_reset}\n"
    printf "Install them for full functionality.\n"
    [[ " ${missing_deps[@]} " =~ " git " ]] && return 1
  fi
  return 0
}
yogit::check_dependencies

# Improved error handling
function yogit::error() {
  printf "${_yogit_color_red}Error: $1${_yogit_color_reset}\n" >&2
  return 1
}

function yogit::success() {
  printf "${_yogit_color_green}✓ $1${_yogit_color_reset}\n"
}

function yogit::info() {
  printf "${_yogit_color_blue}$1${_yogit_color_reset}\n"
}

function yogit::warning() {
  printf "${_yogit_color_yellow}Warning: $1${_yogit_color_reset}\n" >&2
}

_yogit_basic_prefix=$YOGIT_BASIC_PREFIX
_yogit_interactive_prefix=$YOGIT_INTERACTIVE_PREFIX
if [[ -z $YOGIT_BASIC_PREFIX  ]]; then
  _yogit_basic_prefix='yg'
fi
if [[ -z $YOGIT_INTERACTIVE_PREFIX ]]; then
  _yogit_interactive_prefix='ygi'
fi

# System detection with fallback
if [[ -z $SYSTEM ]]; then
  SYSTEM=$(uname -s)
fi

if [[ $SYSTEM == Darwin ]]; then
  alias _yogit_open='open'
elif [[ $SYSTEM == Linux ]]; then
  alias _yogit_open='xdg-open'
else
  # Fallback detection for other systems
  if command -v xdg-open &>/dev/null; then
    alias _yogit_open='xdg-open'
  elif command -v open &>/dev/null; then
    alias _yogit_open='open'
  else
    function _yogit_open() {
      yogit::error "Could not find a suitable 'open' command for your system"
      echo "URL: $1" # At least show the URL
    }
  fi
fi

function yogit::help() {
  # basic
  printf "${_yogit_color_cyan}${_yogit_color_bold}>>> Basic Usage${_yogit_color_reset}\n"
  printf "\n"

  # Core commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}st${_yogit_color_reset}\n"
  printf "  git status\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}a${_yogit_color_reset}\n"
  printf "  git add\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}c${_yogit_color_reset}\n"
  printf "  git commit -v\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ct${_yogit_color_reset}\n"
  printf "  git commit -v --trailer sign --trailer coauthor\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}c!${_yogit_color_reset}\n"
  printf "  git commit --amend\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}cn!${_yogit_color_reset}\n"
  printf "  git commit --amend --no-edit\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}push${_yogit_color_reset}\n"
  printf "  git push origin current_branch\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}pull${_yogit_color_reset}\n"
  printf "  git pull origin current_branch\n\n"

  # Clone commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}sc${_yogit_color_reset}\n"
  printf "  git clone with depth 1 and shallow clone submodules\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}sc!${_yogit_color_reset}\n"
  printf "  git clone with depth 1 (without submodules)\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ghsc${_yogit_color_reset}\n"
  printf "  git clone a github repo using <username>/<repo-name> with submodules\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ghsc!${_yogit_color_reset}\n"
  printf "  git clone a github repo using <username>/<repo-name> (without submodules)\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}pickclone${_yogit_color_reset}\n"
  printf "  git clone --sparse --filter=blob:none --depth=1 --no-checkout\n"
  printf "  ${_yogit_color_blue}(after setting up sparse-checkout, use git read-tree -mu HEAD)${_yogit_color_reset}\n\n"

  # Branch & Remote commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}cob${_yogit_color_reset}\n"
  printf "  git checkout -b\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}br${_yogit_color_reset}\n"
  printf "  git branch -r\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}bru${_yogit_color_reset}\n"
  printf "  git branch -u <remote>/<branch> <local-branch>\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}bm${_yogit_color_reset}\n"
  printf "  rename branch and update its tracked origin branch\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}lr${_yogit_color_reset}\n"
  printf "  remote list\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}url${_yogit_color_reset}\n"
  printf "  print remote url (usage: ${_yogit_basic_prefix}url <remote-name>)\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}seturl${_yogit_color_reset}\n"
  printf "  git remote set-url origin\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}rnremote${_yogit_color_reset}\n"
  printf "  rename remote (usage: ${_yogit_basic_prefix}rnremote <old-name> <new-name>)\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}rnurl${_yogit_color_reset}\n"
  printf "  change remote URL (usage: ${_yogit_basic_prefix}rnurl <remote-name> <new-url>)\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}rremote${_yogit_color_reset}\n"
  printf "  modify current tracked remote repo\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}prune${_yogit_color_reset}\n"
  printf "  git remote prune origin\n\n"

  # File & Status commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}sst${_yogit_color_reset}\n"
  printf "  list staged and unstaged file names only\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ls${_yogit_color_reset}\n"
  printf "  git ls-files --others --exclude-standard\n\n"

  # Submodule commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}sub${_yogit_color_reset}\n"
  printf "  git submodule update --init --recursive\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}subu${_yogit_color_reset}\n"
  printf "  git submodule update --remote --merge\n\n"

  # Tag commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}rtags${_yogit_color_reset}\n"
  printf "  list remote tags with 'git ls-remote --tags'\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ftag${_yogit_color_reset}\n"
  printf "  fetch a remote tag\n\n"

  # GitHub specific commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}ghsize${_yogit_color_reset}\n"
  printf "  get size of github repo\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}ghurl${_yogit_color_reset}\n"
  printf "  github repo worktree url of current commit\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}opengh${_yogit_color_reset}\n"
  printf "  open github repo worktree url of current commit\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}open${_yogit_color_reset}\n"
  printf "  open/xdg-open repo url in browser\n\n"

  # Utility commands
  printf "${_yogit_color_green}${_yogit_basic_prefix}parse${_yogit_color_reset}\n"
  printf "  git rev-parse [input] | cut -c 1-7\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}resetupdate${_yogit_color_reset}\n"
  printf "  git fetch --depth=1 then reset to FETCH_HEAD\n\n"

  printf "${_yogit_color_green}${_yogit_basic_prefix}htest${_yogit_color_reset}\n"
  printf "  ssh -T git@github.com\n\n"

  # Interactive commands
  printf "${_yogit_color_cyan}${_yogit_color_bold}>>> Interactive Usage${_yogit_color_reset}\n"
  printf "\n"

  printf "${_yogit_color_green}${yogit_checkout:-${_yogit_interactive_prefix}co}${_yogit_color_reset}\n"
  printf "  checkout with fzf\n\n"

  printf "${_yogit_color_green}${yogit_cherry_pick:-${_yogit_interactive_prefix}cp}${_yogit_color_reset}\n"
  printf "  cherry pick with fzf\n\n"

  printf "${_yogit_color_green}${yogit_show:-${_yogit_interactive_prefix}sc}${_yogit_color_reset}\n"
  printf "  select commit with fzf\n\n"

  printf "${_yogit_color_green}${yogit_branch:-${_yogit_interactive_prefix}br}${_yogit_color_reset}\n"
  printf "  select branch with fzf\n\n"

  printf "${_yogit_color_green}${yogit_diff:-${_yogit_interactive_prefix}diff}${_yogit_color_reset}\n"
  printf "  diff with fzf\n\n"

  printf "${_yogit_color_green}${_yogit_interactive_prefix}rtags${_yogit_color_reset}\n"
  printf "  select a tag name with fzf\n\n"
}

alias "${yogit_help:-${_yogit_interactive_prefix}help}"='yogit::help'
alias "${yogit_help:-${_yogit_basic_prefix}help}"='yogit::help'

# Utilities {{{
# To speed up, use gitstatus :D
# fallback to git if not found

function yogit::is_git_repo(){
  if ! git rev-parse --is-inside-work-tree &>/dev/null; then
    yogit::error "Not inside a git repository"
    return 1
  fi
  return 0
}

function yogit::current_branch() {
  yogit::is_git_repo || return 1
  local ref=$(git symbolic-ref --quiet HEAD 2>/dev/null)
  if [[ $? -ne 0 ]]; then
    # Handle detached HEAD state
    ref=$(git rev-parse --short HEAD 2>/dev/null) || return 1
    echo "(detached:$ref)"
    return 0
  fi
  echo ${ref#refs/heads/}
}
# }}}

# basic aliases start with prefix gg {{{
function yogit::update_remote() {
    # Configuration
    local UPSTREAM_REMOTE="upstream"
    local FORK_REMOTE="origin"
    local DEFAULT_BRANCH="main"

    # Verify Git repo
    if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
        echo "Error: Not in a Git repository"
        return 1
    fi

    # Get GitHub username
    local github_username=$(git config --global --get user.name)
    if [ -z "$github_username" ]; then
        echo "Warning: Could not get GitHub username from git config"
        read -p "Please enter your GitHub username: " github_username
    fi

    # Detect current branch
    local current_default=$(git symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | awk -F'/' '{print $NF}')
    [ -z "$current_default" ] && current_default=$DEFAULT_BRANCH

    # Get origin URL
    local origin_url=$(git remote get-url origin 2>/dev/null)
    if [ -z "$origin_url" ]; then
        echo "Error: No origin remote found"
        return 1
    fi

    # Convert SSH URL to consistent format
    if [[ "$origin_url" == git@github.com:* ]]; then
        origin_url="https://github.com/${origin_url#git@github.com:}"
        origin_url=${origin_url%.git}
    fi

    # Determine repo type
    if [[ "$origin_url" == *"github.com/$github_username/"* ]]; then
        echo "Detected: This is a clone of your personal fork"

        # Add upstream remote
        if ! git remote get-url $UPSTREAM_REMOTE >/dev/null 2>&1; then
            echo "Please provide the UPSTREAM repository details:"
            read "org_name?Enter the organization/owner name: "
            read "repo_name?Enter the repository name [${origin_url##*/}]: "
            repo_name=${repo_name:-${origin_url##*/}}

            # Offer URL format choice
            echo -e "\nSelect URL format:"
            echo "1) HTTPS (https://github.com/$org_name/$repo_name.git)"
            echo "2) SSH (git@github.com:$org_name/$repo_name.git)"
            read "url_choice?Enter choice [1/2]: "

            case "$url_choice" in
                1|"")
                    upstream_url="https://github.com/$org_name/$repo_name.git"
                    ;;
                2)
                    upstream_url="git@github.com:$org_name/$repo_name.git"
                    ;;
                *)
                    echo "Invalid choice, using HTTPS format"
                    upstream_url="https://github.com/$org_name/$repo_name.git"
                    ;;
            esac

            git remote add $UPSTREAM_REMOTE "$upstream_url" || {
                echo "Error: Failed to add upstream remote"
                return 1
            }
            echo "Added upstream remote ($UPSTREAM_REMOTE)"
        fi

    else
        echo "Detected: This is a clone of the upstream repository"

        # Rename origin to upstream
        if ! git remote get-url $UPSTREAM_REMOTE >/dev/null 2>&1; then
            git remote rename origin $UPSTREAM_REMOTE || {
                echo "Error: Failed to rename origin to $UPSTREAM_REMOTE"
                return 1
            }
            echo "Renamed origin to $UPSTREAM_REMOTE"
        fi

        # Add fork remote
        if ! git remote get-url $FORK_REMOTE >/dev/null 2>&1; then
            echo "Please provide your FORK repository details:"
            read "fork_url?Enter your fork URL [git@github.com:$github_username/${origin_url##*/}.git]: "
            fork_url=${fork_url:-"git@github.com:$github_username/${origin_url##*/}.git"}

            git remote add $FORK_REMOTE "$fork_url" || {
                echo "Error: Failed to add fork remote"
                return 1
            }
            echo "Added your fork as $FORK_REMOTE"
        fi

        # Update tracking
        if git show-ref --verify --quiet refs/heads/$current_default; then
            # First fetch the fork's branches
            git fetch $FORK_REMOTE

            # Then set upstream tracking
            git branch --set-upstream-to=$FORK_REMOTE/$current_default $current_default || {
                echo "Error: Failed to set upstream tracking"
                return 1
            }
            echo "Updated $current_default to track $FORK_REMOTE/$current_default"
        fi
    fi

    # Fetch all remotes
    echo -e "\nFetching from all remotes..."
    git fetch --all --prune || {
        echo "Warning: Failed to fetch from some remotes"
    }

    # Verify configuration
    echo -e "\nFinal remote configuration:"
    git remote -v
    echo -e "\nBranch tracking:"
    git branch -vv

    # Show workflow instructions
    echo -e "\nRepository setup completed!"
    if git remote get-url $UPSTREAM_REMOTE >/dev/null 2>&1; then
        echo -e "\nWorkflow commands:"
        echo "  git pull $UPSTREAM_REMOTE $current_default  # Get upstream changes"
        echo "  git push $FORK_REMOTE $current_default       # Push to your fork"
        echo "  # Create PRs from your fork to upstream"
    fi
}

# modify current tracked remote
alias "${_yogit_basic_prefix}rremote"='yogit::update_remote'

function yogit::url() {
  local remote_name="${1:-origin}"
  if ! git config --get remote.$remote_name.url &>/dev/null; then
    yogit::error "Remote '$remote_name' not found"
    return 1
  fi
  git config --get remote.$remote_name.url | sed -E 's/^[^@]*@([^:\/]*)[:\/]/https:\/\/\1\//' | sed 's/\.git//'
}

function yogit::open() {
  yogit::is_git_repo || return 1
  local url=$(yogit::url "$@")
  if [[ $? -ne 0 ]]; then
    return 1
  fi
  yogit::info "Opening $url in browser..."
  _yogit_open "$url"
}

function yogit::list_remote_tags() {
  yogit::is_git_repo || return 1

  yogit::info "Fetching remote tags..."
  git ls-remote --tags 2>/dev/null | awk -F '/tags\/' '{print $2}' | grep -v '\^{}$'

  if [[ $? -ne 0 ]]; then
    yogit::error "Failed to fetch remote tags"
    return 1
  fi
}

alias "${_yogit_basic_prefix}rtags"='yogit::list_remote_tags'

function yogit::fetch_remote_tag() {
  if [[ -z "$1" ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}ftag <tag-name>"
    return 1
  fi

  yogit::is_git_repo || return 1
  yogit::info "Fetching tag $1..."
  # syntatic sugar: git fetch origin refs/tags/local-tag-name:refs/tags/remote-tag-name --no-tags
  git fetch --depth=1 origin tag "$1" --no-tags

  if [[ $? -eq 0 ]]; then
    yogit::success "Tag $1 fetched successfully"
  else
    yogit::error "Failed to fetch tag $1"
    return 1
  fi
}

alias "${_yogit_basic_prefix}ftag"='yogit::fetch_remote_tag'

function yogit::gh_commit_url() {
  yogit::is_git_repo || return 1

  local commit=$(git rev-parse --verify ${1:-HEAD} 2>/dev/null)
  if [[ $? -ne 0 ]]; then
    yogit::error "Invalid commit reference: ${1:-HEAD}"
    return 1
  fi

  local base_url=$(yogit::url)
  if [[ $? -ne 0 ]]; then
    return 1
  fi

  if [[ "$base_url" =~ .*"github".* ]]; then
    echo "${base_url}/tree/${commit}"
  else
    yogit::error "Remote isn't a GitHub repository"
    return 1
  fi
}

# open Github's permanent url of given commit of a git worktree
function yogit::open_gh_commit() {
  yogit::is_git_repo || return 1

  local url=$(yogit::gh_commit_url "$1")
  if [[ $? -ne 0 ]]; then
    return 1
  fi

  yogit::info "Opening GitHub commit URL: $url"
  _yogit_open "$url"
}

alias "${_yogit_basic_prefix}ghsize"='yogit::get_gh_repo_size'

# idea from https://stackoverflow.com/questions/2882620/is-it-possible-to-remote-count-object-and-size-of-git-repository
function yogit::get_gh_repo_size() {
  if [[ $# -gt 0 ]]; then
    local organ=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:)([^/]+)/([^/]+)(\.git)?$#\2#p')
    local repo_name=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:)([^/]+)/([^/]+)(\.git)?$#\3#p' | sed -nE 's/\.git$//p')

    if [[ -z "$organ" || -z "$repo_name" ]]; then
      yogit::error "Invalid GitHub repo URL format"
      return 1
    fi

    yogit::info "Fetching repository size..."
    printf "Organization: ${_yogit_color_green}${organ}${_yogit_color_reset}\n"
    printf "Repository  : ${_yogit_color_green}${repo_name}${_yogit_color_reset}\n"

    # Run curl directly without background process
    # see https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#get-a-repository
    local response=$(curl -sL "https://api.github.com/repos/${organ}/${repo_name}")

    # Process the results
    # The size of the repository, in kilobytes. Size is calculated hourly. When a repository is initially created, the size is 0.
    local repo_size=$(echo "$response" | grep -m 1 '"size"' | awk -F ':|,' '{print $2}')

    if [[ -z "$repo_size" ]]; then
      yogit::error "Failed to retrieve repository size. Check the repository URL."
      return 1
    fi

    # convert to human readable size
    printf "${_yogit_color_cyan}${organ}/${repo_name} size:${_yogit_color_reset}\n"
    printf " ${_yogit_color_green}$(( $repo_size / 1024 )) MB $(( $repo_size % 1024 )) KB${_yogit_color_reset}\n"
  else
    printf "${_yogit_color_blue}Usage:${_yogit_color_reset} ${_yogit_basic_prefix}ghsize [repo url]\n"
    printf "${_yogit_color_yellow}URL format:${_yogit_color_reset}\n"
    printf "    git@github.com:\${organ}/\${repo_name}.git\n"
    printf "    https://github.com/\${organ}/\${repo_name}.git\n"
    printf "    https://github.com/\${organ}/\${repo_name}\n"
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
# supports self-hosted git repo url, github, gitlab and etc.
function yogit::shallowclone() {
  yogit::info "Cloning with --depth 1 --recurse-submodules -j${CORES} --shallow-submodules"

  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}sc <repo-url> [destination-dir]"
    return 1
  fi

  yogit::info "Starting clone operation..."
  git clone --depth 1 --recurse-submodules -j${CORES} --shallow-submodules "$@"

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Clone completed successfully!"
}

alias "${_yogit_basic_prefix}sc"='yogit::shallowclone'

function yogit::shallowclone_github_gitlab() {
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}sch <repo-url> [destination-dir]"
    return 1
  fi

  local organ=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:|https://gitlab.com/|git@gitlab.com:)([^/]+)/([^/]+)(\.git)?$#\2#p')
  local repo_name=$(echo $1 | sed -nE 's#(https?://github.com/|git@github.com:|https://gitlab.com/|git@gitlab.com:)([^/]+)/([^/]+)(\.git)?$#\3#p' | sed -nE 's/\.git$//p')

  if [[ -z "$organ" || -z "$repo_name" ]]; then
    yogit::error "Invalid repository URL format"
    return 1
  fi

  yogit::info "Cloning with --depth 1 --recurse-submodules -j${CORES} --shallow-submodules"
  yogit::info "Cloning ${organ}/${repo_name}..."

  local dest_dir="${repo_name}.${organ}"
  if [[ $# -gt 1 ]]; then
    # Use provided destination
    git clone --depth 1 --recurse-submodules -j${CORES} --shallow-submodules "$@"
  else
    # Use default destination
    git clone $1 --depth 1 --recurse-submodules -j${CORES} --shallow-submodules "$dest_dir"
  fi

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Clone completed successfully!"

  # Check if both jq and mangit exist before attempting to add tags
  if command -v jq &>/dev/null && command -v mangit &>/dev/null; then
    # Only process GitHub repos
    if [[ "$1" =~ "github.com" ]]; then
      yogit::info "Fetching repository topics..."

      local response=$(curl -sL "https://api.github.com/repos/${organ}/${repo_name}")
      local topics=$(echo "$response" | jq -r '.topics[]' 2>/dev/null)

      if [[ -n "$topics" ]]; then
        # Convert topics to tags format
        local tags=()
        while IFS= read -r topic; do
          tags+=("$topic")
        done <<< "$topics"

        yogit::info "Adding repository to mangit with topics as tags..."
        printf "Topics: ${_yogit_color_green}${tags[*]}${_yogit_color_reset}\n"

        # Add to mangit with tags
        mangit add "$dest_dir" --tags "${(j:,:)tags}"
        if [[ $? -eq 0 ]]; then
          yogit::success "${repo_name}.${organ} added to mangit with topics as ${tags}"
        else
          yogit::warning "Failed to add repository to mangit"
        fi
      else
        yogit::info "No topics found for this repository"
      fi
    fi
  fi
}

# clone github repo
alias "${_yogit_basic_prefix}sch"='yogit::shallowclone_github_gitlab'

function yogit::shallowclone_without_submodules() {
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}sc! <repo-url> [destination-dir]"
    return 1
  fi

  yogit::info "Cloning with --depth 1 (without submodules)"
  yogit::info "Starting clone operation..."

  git clone --depth 1 "$@"

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Clone completed successfully!"
}

alias "${_yogit_basic_prefix}sc!"='yogit::shallowclone_without_submodules'

alias "${_yogit_basic_prefix}ghsc"='yogit::ghclone'

function yogit::ghclone(){
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}ghsc <username>/<repo> [destination-dir]"
    return 1
  fi

  local username=$(echo "$1" | sed -E 's/(.*)\/(.*)$/\1/')
  local repo=$(echo "$1" | sed -E 's/(.*)\/(.*)$/\2/')

  if [[ -z "$username" || -z "$repo" ]]; then
    yogit::error "Invalid format. Use <username>/<repo>"
    return 1
  fi

  # remove the first parameter
  shift

  yogit::info "Cloning github.com:${username}/${repo}.git with submodules..."

  git clone --depth 1 --recurse-submodules -j${CORES} --shallow-submodules "git@github.com:${username}/${repo}.git" "$@"

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Clone completed successfully!"
}

alias "${_yogit_basic_prefix}ghsc!"='yogit::ghclone_without_submodules'

function yogit::ghclone_without_submodules(){
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}ghsc! <username>/<repo> [destination-dir]"
    return 1
  fi

  local username=$(echo "$1" | sed -E 's/(.*)\/(.*)$/\1/')
  local repo=$(echo "$1" | sed -E 's/(.*)\/(.*)$/\2/')

  if [[ -z "$username" || -z "$repo" ]]; then
    yogit::error "Invalid format. Use <username>/<repo>"
    return 1
  fi

  # remove the first parameter
  shift

  yogit::info "Cloning github.com:${username}/${repo}.git (without submodules)..."

  git clone --depth 1 "git@github.com:${username}/${repo}.git" "$@"

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Clone completed successfully!"
}

# git checkout
alias "${_yogit_basic_prefix}cob"='git checkout -b'

# add
alias "${_yogit_basic_prefix}a"='git add'

# commit
alias "${_yogit_basic_prefix}c"='git commit -v'
# Add Sign-off-by: ....
# Add Co-authored-by: ....
# predefined git-interpret-trailer key, see .config/git/common.gitconfig
alias "${_yogit_basic_prefix}ct"='git commit -v --trailer sign --trailer coauthor'
alias "${_yogit_basic_prefix}c!"='git commit --amend'
alias "${_yogit_basic_prefix}cn!"='git commit --amend --no-edit'

# push
function yogit::push() {
  yogit::is_git_repo || return 1

  local branch=$(yogit::current_branch)
  if [[ $? -ne 0 ]]; then
    return 1
  fi

  yogit::info "Pushing to origin/${branch}..."
  git push origin "$branch" "$@"

  if [[ $? -eq 0 ]]; then
    yogit::success "Push completed"
  else
    yogit::error "Push failed"
    return 1
  fi
}
alias "${_yogit_basic_prefix}push"='yogit::push'

# pull
function yogit::pull() {
  yogit::is_git_repo || return 1

  local branch=$(yogit::current_branch)
  if [[ $? -ne 0 ]]; then
    return 1
  fi

  yogit::info "Pulling from origin/${branch}..."
  git pull origin "$branch" "$@"

  if [[ $? -eq 0 ]]; then
    yogit::success "Pull completed"
  else
    yogit::error "Pull failed"
    return 1
  fi
}
alias "${_yogit_basic_prefix}pull"='yogit::pull'

# change remote branch that current branch tracked
alias "${_yogit_basic_prefix}bru"='yogit::change_current_tracked'

function yogit::change_current_tracked(){
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}bru <remote>/<branch>"
    return 1
  fi

  yogit::is_git_repo || return 1

  local branch=$(yogit::current_branch)
  if [[ $? -ne 0 ]]; then
    return 1
  fi

  yogit::info "Changing tracked branch for ${branch} to $1..."
  git branch -u "$1" "$branch"

  if [[ $? -eq 0 ]]; then
    yogit::success "Tracked branch updated"
  else
    yogit::error "Failed to update tracked branch"
    return 1
  fi
}

# rename and change tracked branch of origin
alias "${_yogit_basic_prefix}bm"='yogit::rename_for_remote_origin_branch'

function yogit::rename_for_remote_origin_branch(){
  if [[ $# -ne 2 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}bm <old-branch-name> <new-branch-name>"
    return 1
  fi

  local current_branch=$(yogit::current_branch)
  if [[ "$current_branch" != "$1" ]]; then
    printf "${_yogit_color_yellow}Warning: You're not on branch '$1'. Do you want to continue? [y/N]: ${_yogit_color_reset}"
    read -r response
    if [[ ! "$response" =~ ^[yY]$ ]]; then
      printf "${_yogit_color_blue}Operation cancelled${_yogit_color_reset}\n"
      return 0
    fi
  fi

  git branch -m "$1" "$2" || return 1
  yogit::success "Branch renamed from '$1' to '$2'"

  # Check if the remote branch exists before setting upstream
  if git ls-remote --heads origin "$2" &>/dev/null; then
    git branch -u "origin/$2" "$2" && \
    yogit::success "Upstream set to origin/$2"
  else
    yogit::warning "Remote branch 'origin/$2' doesn't exist yet."
    yogit::info "Next steps:"
    printf "1. ${_yogit_color_green}git push -u origin $2${_yogit_color_reset} (to create the remote branch)\n"
    printf "2. ${_yogit_color_green}git fetch origin${_yogit_color_reset}\n"
    printf "3. ${_yogit_color_green}git remote set-head -a origin${_yogit_color_reset}\n"
  fi
}

function yogit::staged_and_unstaged(){
  yogit::is_git_repo || return 1
  git status --porcelain | sed 's/^...//'
}

# list both unstaged and staged files
alias "${_yogit_basic_prefix}sst"='yogit::staged_and_unstaged'

# list only untracked files
function yogit::list_untracked_files() {
  yogit::is_git_repo || return 1
  yogit::info "Listing untracked files..."
  git ls-files --others --exclude-standard
}
alias "${_yogit_basic_prefix}ls"='yogit::list_untracked_files'

# --sparse initializes the sparse-checkout file so the working directory starts only with the files in the root directory
# --filter=blob:none will exclude files so we could fetch them when needed
# --depth=1 truncate commit history to leave only the latest commit(may cause problems)
# --no-checkout further accelerates the clone
function yogit::pickclone() {
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}pickclone <repo-url> [destination-dir]"
    return 1
  fi

  yogit::info "Performing optimized sparse clone..."
  yogit::info "After cloning, set up sparse-checkout and use 'git read-tree -mu HEAD'"

  git clone --sparse --filter=blob:none --depth=1 --no-checkout "$@"

  if [[ $? -ne 0 ]]; then
    yogit::error "Clone failed"
    return 1
  fi

  yogit::success "Sparse clone completed successfully!"
}
alias "${_yogit_basic_prefix}pickclone"='yogit::pickclone'

function yogit::submodule_update() {
  yogit::is_git_repo || return 1
  yogit::info "Updating submodules..."

  git submodule update --init --recursive

  if [[ $? -ne 0 ]]; then
    yogit::error "Submodule update failed"
    return 1
  fi

  yogit::success "Submodules updated successfully!"
}
alias "${_yogit_basic_prefix}sub"='yogit::submodule_update'

function yogit::submodule_update_remote() {
  yogit::is_git_repo || return 1
  yogit::info "Updating submodules from remote..."

  git submodule update --remote --merge

  if [[ $? -ne 0 ]]; then
    yogit::error "Submodule remote update failed"
    return 1
  fi

  yogit::success "Submodules updated from remote successfully!"
}
alias "${_yogit_basic_prefix}subu"='yogit::submodule_update_remote'

# bisect
alias "${_yogit_basic_prefix}bs"="git bisect"
alias "${_yogit_basic_prefix}bsr"="git bisect reset"
alias "${_yogit_basic_prefix}bss"="git bisect start"
alias "${_yogit_basic_prefix}bsg"="git bisect good"
alias "${_yogit_basic_prefix}bsb"="git bisect bad"  # Fixed alias name from bsg to bsb

# cherry-pick
alias "${_yogit_basic_prefix}cp"="git cherry-pick"
alias "${_yogit_basic_prefix}cpa"="git cherry-pick --abort"
alias "${_yogit_basic_prefix}cpc"="git cherry-pick --continue"

# stash
function yogit::stash_push() {
  yogit::is_git_repo || return 1
  yogit::info "Stashing changes..."

  if [[ $# -gt 0 ]]; then
    git stash push -m "$*"
  else
    git stash push
  fi

  if [[ $? -eq 0 ]]; then
    yogit::success "Changes stashed successfully"
  else
    yogit::warning "No changes to stash or stash failed"
  fi
}
alias "${_yogit_basic_prefix}sta"='yogit::stash_push'

function yogit::stash_clear() {
  yogit::is_git_repo || return 1

  # Confirm before clearing
  printf "${_yogit_color_yellow}Warning: This will clear all stashed changes. Continue? [y/N]: ${_yogit_color_reset}"
  read -r response
  if [[ ! "$response" =~ ^[yY]$ ]]; then
    yogit::info "Operation cancelled"
    return 0
  fi

  yogit::info "Clearing all stashes..."
  git stash clear
  yogit::success "All stashes cleared"
}
alias "${_yogit_basic_prefix}staclear"='yogit::stash_clear'

# diff
# diff unstaged files
function yogit::diff_names_only() {
  yogit::is_git_repo || return 1
  yogit::info "Listing changed file names:"
  git diff --name-only "$@"
}
alias "${_yogit_basic_prefix}dn"='yogit::diff_names_only'

# diff only staged files
function yogit::diff_names_staged() {
  yogit::is_git_repo || return 1
  yogit::info "Listing staged file names:"
  git diff --name-only --staged "$@"
}
alias "${_yogit_basic_prefix}dns"='yogit::diff_names_staged'

# rebase
alias "${_yogit_basic_prefix}b"='git rebase'
alias "${_yogit_basic_prefix}ba"='git rebase --abort'
alias "${_yogit_basic_prefix}bc"='git rebase --continue'

# reset
# reset head
function yogit::reset_head() {
  yogit::is_git_repo || return 1

  if [[ $# -eq 0 ]]; then
    yogit::info "Resetting HEAD (soft reset)"
    git reset
  else
    yogit::info "Resetting HEAD to $*"
    git reset "$@"
  fi

  if [[ $? -eq 0 ]]; then
    yogit::success "Reset completed"
  else
    yogit::error "Reset failed"
    return 1
  fi
}
alias "${_yogit_basic_prefix}rh"='yogit::reset_head'

function yogit::reset_hard() {
  yogit::is_git_repo || return 1

  # Confirm before hard reset
  printf "${_yogit_color_yellow}Warning: Hard reset will discard all changes. Continue? [y/N]: ${_yogit_color_reset}"
  read -r response
  if [[ ! "$response" =~ ^[yY]$ ]]; then
    yogit::info "Operation cancelled"
    return 0
  fi

  if [[ $# -eq 0 ]]; then
    yogit::info "Performing hard reset to HEAD"
    git reset --hard
  else
    yogit::info "Performing hard reset to $*"
    git reset --hard "$@"
  fi

  if [[ $? -eq 0 ]]; then
    yogit::success "Hard reset completed"
  else
    yogit::error "Hard reset failed"
    return 1
  fi
}
alias "${_yogit_basic_prefix}rhh"='yogit::reset_hard'

# remote
# list remote branch
function yogit::list_remote_branches() {
  yogit::is_git_repo || return 1
  yogit::info "Listing remote branches:"
  git branch -r "$@"
}
alias "${_yogit_basic_prefix}br"='yogit::list_remote_branches'

# print current remote url
alias "${_yogit_basic_prefix}url"='yogit::url'

# copy remote url to clipboard
function yogit::url_copy(){
  yogit::is_git_repo || return 1
  local url=$(yogit::url "$@")
  if [[ $? -ne 0 ]]; then
    return 1
  fi
  echo "$url" | pbcopy
  yogit::success "Remote URL copied to clipboard"
}
alias "${_yogit_basic_prefix}urlcp"='yogit::url_copy'

# list remote
function yogit::list_remotes() {
  yogit::is_git_repo || return 1
  yogit::info "Listing remotes:"
  git remote -v
}
alias "${_yogit_basic_prefix}lr"='yogit::list_remotes'

# prune unused branches
function yogit::prune_origin() {
  yogit::is_git_repo || return 1

  yogit::info "Pruning deleted remote branches..."
  git remote prune origin

  if [[ $? -eq 0 ]]; then
    yogit::success "Prune completed"
  else
    yogit::error "Prune failed"
    return 1
  fi
}
alias "${_yogit_basic_prefix}prune"='yogit::prune_origin'

function yogit::set_remote_url() {
  if [[ $# -lt 1 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}seturl <new-url>"
    return 1
  fi

  # Curry function: call rename_remote_url with 'origin' as first parameter
  yogit::rename_remote_url "origin" "$1"
}
alias "${_yogit_basic_prefix}seturl"='yogit::set_remote_url'

function yogit::rename_remote() {
  if [[ $# -ne 2 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}rnremote <old-name> <new-name>"
    return 1
  fi

  yogit::is_git_repo || return 1

  local old_name="$1"
  local new_name="$2"

  # Check if old remote exists
  if ! git config --get remote.$old_name.url &>/dev/null; then
    yogit::error "Remote '$old_name' not found"
    return 1
  fi

  # Check if new remote name already exists
  if git config --get remote.$new_name.url &>/dev/null; then
    yogit::error "Remote '$new_name' already exists"
    return 1
  fi

  local remote_url=$(git config --get remote.$old_name.url)
  yogit::info "Renaming remote:"
  printf "  Remote: ${_yogit_color_yellow}${old_name}${_yogit_color_reset} → ${_yogit_color_green}${new_name}${_yogit_color_reset}\n"
  printf "  URL:    ${_yogit_color_blue}${remote_url}${_yogit_color_reset}\n"

  git remote rename "$old_name" "$new_name"

  if [[ $? -eq 0 ]]; then
    yogit::success "Remote renamed successfully"

    # Update tracking branches if any
    local current_branch=$(yogit::current_branch)
    if [[ "$current_branch" != *"(detached:"* ]]; then
      local tracking_branch=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)
      if [[ "$tracking_branch" == "$old_name/"* ]]; then
        local branch_name="${tracking_branch#$old_name/}"
        yogit::info "Updating tracking branch: ${old_name}/${branch_name} → ${new_name}/${branch_name}"
        git branch --set-upstream-to="$new_name/$branch_name" "$current_branch"
      fi
    fi
  else
    yogit::error "Failed to rename remote"
    return 1
  fi
}
alias "${_yogit_basic_prefix}rnremote"='yogit::rename_remote'

function yogit::rename_remote_url() {
  if [[ $# -ne 2 ]]; then
    yogit::error "Usage: ${_yogit_basic_prefix}rnurl <remote-name> <new-url>"
    return 1
  fi

  yogit::is_git_repo || return 1

  local remote_name="$1"
  local new_url="$2"

  # Check if remote exists
  if ! git config --get remote.$remote_name.url &>/dev/null; then
    yogit::error "Remote '$remote_name' not found"
    return 1
  fi

  local old_url=$(git config --get remote.$remote_name.url)
  yogit::info "Changing remote '$remote_name' URL:"
  printf "  From: ${_yogit_color_yellow}${old_url}${_yogit_color_reset}\n"
  printf "  To:   ${_yogit_color_green}${new_url}${_yogit_color_reset}\n"

  git remote set-url "$remote_name" "$new_url"

  if [[ $? -eq 0 ]]; then
    yogit::success "Remote '$remote_name' URL updated successfully"
  else
    yogit::error "Failed to update remote '$remote_name' URL"
    return 1
  fi
}
alias "${_yogit_basic_prefix}rnurl"='yogit::rename_remote_url'

function yogit::parse() {
  local object_name="${1:-HEAD}"
  yogit::is_git_repo || return 1

  # Check if the object exists
  if ! git rev-parse --verify "$object_name" &>/dev/null; then
    yogit::error "Invalid git object: $object_name"
    return 1
  fi

  # always print short 7-byte-long sha-1 of commit
  git rev-parse "$object_name" | cut -c 1-7
}
# print current commit
alias "${_yogit_basic_prefix}last"="git log -1 HEAD --pretty='%h'"
# print the commit of given object
alias "${_yogit_basic_prefix}parse"='yogit::parse'

# cd to root path of a git repo
function yogit::cd_to_root() {
  yogit::is_git_repo || return 1

  local root_path=$(git rev-parse --show-toplevel)
  yogit::info "Changing directory to git repository root: $root_path"
  cd "$root_path" || return 1
}
alias "${_yogit_basic_prefix}rt"='yogit::cd_to_root'

function yogit::reset_update() {
  yogit::is_git_repo || return 1

  local branch=$(yogit::current_branch)
  git fetch --depth=1 origin "$branch" && git reset --hard FETCH_HEAD
}
alias "${_yogit_basic_prefix}resetupdate"='yogit::reset_update'
# }}}

# Interactive commands aliases start with prefix gi {{{
# could use forgit as alternate
function yogit::branch(){
  yogit::is_git_repo || return 1

  yogit::info "Select a branch:"
  git branch | sed -E 's/\*//' | fzf --height 40% --border --ansi \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' | \
    sed -E 's/ //g'
}

# select abbreviation commit
function yogit::select::commit(){
  yogit::is_git_repo || return 1

  if [[ -n $1 ]]; then
    if ! git rev-parse --verify "$1" &>/dev/null; then
      yogit::error "Invalid branch or reference: $1"
      return 1
    fi

    yogit::info "Select commit from $1:"
    git log $1 --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' 2>/dev/null
  else
    # Use current branch
    local branch=$(yogit::branch)
    if [[ -n $branch ]]; then
      yogit::info "Select commit from $branch:"
      git log $branch --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' 2>/dev/null
    fi
  fi
}

# git cherry-pick with fzf
function yogit::cherry_pick(){
  yogit::is_git_repo || return 1

  local commit=""
  if [[ -n $1 ]]; then
    commit=$(yogit::select::commit $1)
  else
    commit=$(yogit::select::commit)
  fi

  if [[ -n $commit ]]; then
    yogit::info "Cherry-picking commit: $commit"
    git cherry-pick -e $commit

    if [[ $? -eq 0 ]]; then
      yogit::success "Cherry-pick completed"
    else
      yogit::error "Cherry-pick failed or conflicts occurred"
      return 1
    fi
  else
    yogit::warning "No commit selected"
    return 1
  fi
}

# git diff with fzf
function yogit::diff() {
  yogit::is_git_repo || return 1

  yogit::info "Select files to diff:"
  preview="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview "$preview"
}

function yogit::checkout() {
  yogit::is_git_repo || return 1

  yogit::info "Select branch to checkout:"
  local branch=$(git branch | fzf --height 40% --border --ansi \
    --preview 'git log --oneline --graph --date=short --color=always --pretty="format:%C(auto)%cd %h%d %s" {1} | head -200' | \
    sed -E 's/^[ *]+//')

  if [[ -n $branch ]]; then
    yogit::info "Checking out branch: $branch"
    git checkout "$branch"

    if [[ $? -eq 0 ]]; then
      yogit::success "Checkout successful"
    else
      yogit::error "Checkout failed"
      return 1
    fi
  else
    yogit::warning "No branch selected"
    return 1
  fi
}

function yogit::select_remote_tag() {
  yogit::is_git_repo || return 1

  yogit::info "Select a remote tag:"
  yogit::list_remote_tags | fzf --height 40% --border --ansi
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
