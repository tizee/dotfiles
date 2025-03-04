#!/usr/bin/env zsh
# mangit.zsh - ZSH Integration for mangit
# Source this file in your ~/.zshrc:
# source /path/to/mangit.zsh

# Define color codes for elegant, visually comfortable output
RED="\033[31m"
GREEN="\033[32m"
YELLOW="\033[33m"
BLUE="\033[34m"
MAGENTA="\033[35m"
CYAN="\033[36m"
BOLD="\033[1m"
RESET="\033[0m"

# Ensure mangit is available
if ! command -v mangit &> /dev/null; then
    echo -e "${RED}mangit command not found. Please install mangit first.${RESET}"
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
    local input_path="$1"
    local repo_path
    local tags

    # Default to current directory if no path provided
    if [[ -z "$input_path" ]]; then
        repo_path="$(pwd)"
    else
        # Convert relative path to absolute path
        repo_path="$(realpath "$input_path")"
    fi

    # Verify it's a git repository
    if [[ ! -d "$repo_path/.git" ]]; then
        echo -e "${RED}Error: ${repo_path} is not a git repository${RESET}"
        return 1
    fi

    # Check if it's a GitHub repository and fetch topics
    if command -v jq &>/dev/null; then
        # Get remote URL
        local remote_url=$(git -C "$repo_path" config --get remote.origin.url)

        # Extract organ and repo_name using the improved approach
        local organ=$(echo "$remote_url" | sed -nE 's#(https?://github.com/|git@github.com:|github.com:)([^/]+)/([^/]+)(\.git)?$#\2#p')
        local repo_name=$(echo "$remote_url" | sed -nE 's#(https?://github.com/|git@github.com:|github.com:)([^/]+)/([^/]+)(\.git)?$#\3#p' | sed 's/\.git$//')

        if [[ -n "$organ" && -n "$repo_name" ]]; then
            echo -e "${CYAN}GitHub repository detected: ${BOLD}$organ/$repo_name${RESET}"
            echo -e "${YELLOW}Fetching repository topics...${RESET}"

            local response=$(curl -sL "https://api.github.com/repos/${organ}/${repo_name}")
            local github_topics=$(echo "$response" | jq -r '.topics | join(",")' 2>/dev/null)

            if [[ -n "$github_topics" ]]; then
                echo -e "${GREEN}GitHub topics found: ${github_topics}${RESET}"
                echo -e "${BLUE}Use GitHub topics as tags? (y/n/a)${RESET}"
                echo -e "${BLUE}  y: Use only GitHub topics${RESET}"
                echo -e "${BLUE}  n: Enter custom tags manually${RESET}"
                echo -e "${BLUE}  a: Use GitHub topics AND add custom tags${RESET}"
                read -r use_topics

                case "$use_topics" in
                    y|Y)
                        tags="$github_topics"
                        ;;
                    n|N)
                        echo -e "${BLUE}Enter tags for repository (comma-separated):${RESET}"
                        read -r tags
                        ;;
                    a|A)
                        echo -e "${BLUE}Enter additional tags for repository (comma-separated):${RESET}"
                        read -r additional_tags
                        tags="$github_topics,$additional_tags"
                        ;;
                    *)
                        echo -e "${YELLOW}Invalid choice. Using GitHub topics.${RESET}"
                        tags="$github_topics"
                        ;;
                esac
            else
                echo -e "${YELLOW}No GitHub topics found.${RESET}"
                echo -e "${BLUE}Enter tags for repository (comma-separated):${RESET}"
                read -r tags
            fi
        else
            echo -e "${YELLOW}Not a GitHub repository or cannot parse remote URL.${RESET}"
            echo -e "${BLUE}Enter tags for repository (comma-separated):${RESET}"
            read -r tags
        fi
    else
        echo -e "${RED}jq not installed. Cannot fetch GitHub topics.${RESET}"
        echo -e "${BLUE}Enter tags for repository (comma-separated):${RESET}"
        read -r tags
    fi

    # Add the repository
    mangit add "$repo_path" --tags "$tags"
    echo -e "${GREEN}Repository added/updated: ${BOLD}$repo_path${RESET}"
    echo -e "${GREEN}Tags: ${tags}${RESET}"
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
                        _alternative "repositories:repository:_mangit_repositories"
                        ;;
                    *)
                        _files
                        ;;
                esac
                ;;
        esac
    }

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
        echo -e "${YELLOW}Usage: mgpull <tag>${RESET}"
        return 1
    fi

    repos=$(mangit search "$tag")

    if [[ -z "$repos" ]]; then
        echo -e "${RED}No repositories found with tag: ${tag}${RESET}"
        return 1
    fi

    echo -e "${CYAN}Pulling repositories with tag: ${BOLD}$tag${RESET}"
    echo "$repos" | while read -r repo; do
        echo -e "${BLUE}Pulling: ${repo}${RESET}"
        (cd "$repo" && git pull)
    done
}

# Optional: function to show status of all repos with a specific tag
function mgstatus() {
    local tag="$1"
    local repos

    if [[ -z "$tag" ]]; then
        echo -e "${YELLOW}Usage: mgstatus <tag>${RESET}"
        return 1
    fi

    repos=$(mangit search "$tag")

    if [[ -z "$repos" ]]; then
        echo -e "${RED}No repositories found with tag: ${tag}${RESET}"
        return 1
    fi

    echo -e "${CYAN}Status for repositories with tag: ${BOLD}$tag${RESET}"
    echo "$repos" | while read -r repo; do
        echo -e "${MAGENTA}=== ${repo} ===${RESET}"
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
            echo -e "${BLUE}Enter search tag:${RESET}"
            read -r tag
            mangit search "$tag" | less
            ;;
        pull)
            echo -e "${BLUE}Enter tag to pull repositories:${RESET}"
            read -r tag
            mgpull "$tag"
            ;;
        status)
            echo -e "${BLUE}Enter tag to check status:${RESET}"
            read -r tag
            mgstatus "$tag"
            ;;
        reset)
            mangit reset
            echo -e "${GREEN}Frequency data reset${RESET}"
            ;;
        *)
            echo -e "${GREEN}mangit ZSH integration loaded. Available commands:${RESET}"
            echo -e "${CYAN}  - mgcd [query]  : Navigate to a repository${RESET}"
            echo -e "${CYAN}  - mgadd [path]  : Add a repository${RESET}"
            echo -e "${CYAN}  - mgl           : List repositories${RESET}"
            echo -e "${CYAN}  - mgs <tag>     : Search repositories by tag${RESET}"
            echo -e "${CYAN}  - mgpull <tag>  : Pull all repositories with tag${RESET}"
            echo -e "${CYAN}  - mgstatus <tag>: Check status of repositories with tag${RESET}"
            echo -e "${CYAN}  - mgdash        : Interactive dashboard${RESET}"
            return 0
            ;;
    esac
}
