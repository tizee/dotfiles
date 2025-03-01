#!/usr/bin/env zsh
# mangit.zsh - ZSH Integration for mangit
# Source this file in your ~/.zshrc:
# source /path/to/mangit.zsh

# Ensure mangit is available
if ! command -v mangit &> /dev/null; then
    echo "mangit command not found. Please install mangit first."
    return 1
fi

# Function to navigate to a repository
function mgcd() {
    local query="$*"
    local selected

    if [[ -z "$query" ]]; then
        # If no query provided, show all repositories
        selected=$(mangit search "" | fzf --preview 'ls -la $(readlink -f {})' --preview-window=right:60%)
    else
        # Use the provided query
        selected=$(mangit search "$query" | fzf --preview 'ls -la $(readlink -f {})' --preview-window=right:60%)
    fi

    if [[ -n "$selected" ]]; then
        # Record access and change to the directory
        mangit access "$selected" &> /dev/null
        cd "$(readlink -f "$selected")"
    fi
}

# Function to interactively add a repository with tags
function mgadd() {
    local repo_path="$1"
    local tags

    # Default to current directory if no path provided
    [[ -z "$repo_path" ]] && repo_path="$(pwd)"

    # Verify it's a git repository
    if [[ ! -d "$repo_path/.git" ]]; then
        echo "Error: $repo_path is not a git repository"
        return 1
    fi

    # Prompt for tags
    echo "Enter tags for repository (comma-separated):"
    read -r tags

    # Add the repository
    mangit add "$repo_path" --tags "$tags"
    echo "Repository added/updated: $repo_path"
}

# Tab completion for mangit
if (( $+commands[compdef] )); then
    _mangit_completion() {
        local state

        _arguments \
            '1: :->command' \
            '*: :->args'

        case $state in
            command)
                _values "mangit command" \
                    "init[Initialize mangit]" \
                    "add[Add a repository]" \
                    "delete[Delete a repository]" \
                    "update[Update repository tags]" \
                    "search[Search for repositories]" \
                    "access[Access a repository]" \
                    "reset[Reset frequency data]"
                ;;
            args)
                case $words[2] in
                    search|access|delete|update)
                        # For these commands, suggest repositories from mangit
                        _alternative "repositories:repository:_mangit_repositories"
                        ;;
                    *)
                        # For other commands, suggest files/directories
                        _files
                        ;;
                esac
                ;;
        esac
    }

    # Helper function to get repository paths
    _mangit_repositories() {
        local -a repos
        repos=( $(mangit search "") )
        _describe 'repositories' repos
    }

    compdef _mangit_completion mangit
    compdef '_files -/' mgcd
    compdef '_files -/' mgadd
fi

# Aliases
alias mgl="mangit list"
alias mgs="mangit search"
alias mgr="mangit reset"

# Optional: create a function to pull all repos with a specific tag
function mgpull() {
    local tag="$1"
    local repos

    if [[ -z "$tag" ]]; then
        echo "Usage: mgpull <tag>"
        return 1
    fi

    repos=$(mangit search "$tag")

    if [[ -z "$repos" ]]; then
        echo "No repositories found with tag: $tag"
        return 1
    fi

    echo "Pulling repositories with tag: $tag"
    echo "$repos" | while read -r repo; do
        echo "Pulling: $repo"
        (cd "$repo" && git pull)
    done
}

# Optional: function to show status of all repos with a specific tag
function mgstatus() {
    local tag="$1"
    local repos

    if [[ -z "$tag" ]]; then
        echo "Usage: mgstatus <tag>"
        return 1
    fi

    repos=$(mangit search "$tag")

    if [[ -z "$repos" ]]; then
        echo "No repositories found with tag: $tag"
        return 1
    fi

    echo "Status for repositories with tag: $tag"
    echo "$repos" | while read -r repo; do
        echo "=== $repo ==="
        (cd "$repo" && git status -s)
        echo
    done
}

# Create an interactive dashboard using fzf
function mgdash() {
    local selected_action
    local actions=(
        "cd:Navigate to repository"
        "add:Add current directory as repository"
        "list:List all repositories"
        "search:Search repositories by tag"
        "pull:Pull repositories by tag"
        "status:Check status of repositories by tag"
        "reset:Reset frequency data"
    )

    selected_action=$(printf "%s\n" "${actions[@]}" | fzf --prompt="Select action: " --height=~50% | cut -d ':' -f 1)

    case "$selected_action" in
        cd)
            mgcd
            ;;
        add)
            mgadd
            ;;
        list)
            mangit list | less
            ;;
        search)
            echo "Enter search tag:"
            read -r tag
            mangit search "$tag" | less
            ;;
        pull)
            echo "Enter tag to pull repositories:"
            read -r tag
            mgpull "$tag"
            ;;
        status)
            echo "Enter tag to check status:"
            read -r tag
            mgstatus "$tag"
            ;;
        reset)
            mangit reset
            echo "Frequency data reset"
            ;;
        *)
            echo "mangit ZSH integration loaded. Available commands:"
            echo "  - mgcd [query]  : Navigate to a repository"
            echo "  - mgadd [path]  : Add a repository"
            echo "  - mgl           : List repositories"
            echo "  - mgs <tag>     : Search repositories by tag"
            echo "  - mgpull <tag>  : Pull all repositories with tag"
            echo "  - mgstatus <tag>: Check status of repositories with tag"
            echo "  - mgdash        : Interactive dashboard"
            return 0
            ;;
    esac
}

