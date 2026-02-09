#!/bin/bash
# Custom statusline for llms-agent (FEAT-065)
# Migrated from Claude Code statusline plugin
#
# Non-blocking design: all slow operations (quota API, git stats) use
# stale-while-revalidate -- return cached data immediately and refresh
# in the background. This keeps statusline rendering fast even when
# called at short fixed intervals.
#
# Setup:
#   1. Copy to ~/.config/llms/statusline.sh
#   2. chmod +x ~/.config/llms/statusline.sh
#   3. Add to config.json:
#      { "agent": { "ui": { "statusline": { "command": "~/.config/llms/statusline.sh" } } } }
#
# Output format:
#   myapp | Q:7% 2h 34m | ████████░░░░░░░ 12.5K / 200K (6%) | main (3 files +45 -12)

# Path to the quota checker script (same directory as this script)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
QUOTA_SCRIPT="${SCRIPT_DIR}/claude_quota_curl.py"
QUOTA_PROFILE="rhhzhqpn.dev-edition-default"
QUOTA_DEBOUNCE=120

# Git cache configuration
GIT_CACHE_DIR="/tmp/llms_statusline_cache"
GIT_CACHE_INTERVAL=10  # seconds between git refreshes

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
cwd=$(echo "$input" | jq -r '.cwd')
dir_name=$(basename "$cwd")

# Extract context window information
context_size=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // 0')

# Calculate current tokens from percentage to match llms agent's calculation
# (input_tokens + output_tokens can differ from the agent's context measurement)
current_tokens=$(echo "scale=0; $context_size * $used_pct / 100" | bc)

# Format token count (1K = 1000)
format_tokens() {
    local count=$1
    if (( count >= 1000 )); then
        printf "%.1fK" "$(echo "scale=1; $count / 1000" | bc)"
    else
        echo "$count"
    fi
}

# Format context size (1K = 1000)
format_size() {
    local size=$1
    if (( size >= 1000 )); then
        echo "$((size / 1000))K"
    else
        echo "$size"
    fi
}

tokens_fmt=$(format_tokens "$current_tokens")
size_fmt=$(format_size "$context_size")
# Round percentage to integer for display
pct_int=$(printf "%.0f" "$used_pct")

# Build context progress bar (15 chars wide)
bar_width=15
filled=$((pct_int * bar_width / 100))
(( filled > bar_width )) && filled=$bar_width
empty=$((bar_width - filled))
bar=""
for ((i=0; i<filled; i++)); do bar+="█"; done
for ((i=0; i<empty; i++)); do bar+="░"; done

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
GRAY='\033[0;90m'
NC='\033[0m'

# Color the progress bar based on usage
if (( pct_int < 50 )); then
    BAR_COLOR="$GRAY"
elif (( pct_int < 80 )); then
    BAR_COLOR="$YELLOW"
else
    BAR_COLOR="$RED"
fi

# Fetch 5-hour quota info (debounced, non-blocking via stale-while-revalidate in python script)
quota_info=""
if [ -f "$QUOTA_SCRIPT" ]; then
    quota_json=$(python3 "$QUOTA_SCRIPT" --profile "$QUOTA_PROFILE" --raw --debounce "$QUOTA_DEBOUNCE" 2>/dev/null)
    if [ -n "$quota_json" ]; then
        quota_util=$(echo "$quota_json" | jq -r '.five_hour.utilization // empty')
        if [ -n "$quota_util" ]; then
            quota_pct=$(printf "%.0f" "$quota_util")
            if (( quota_pct < 50 )); then
                QUOTA_COLOR="$GREEN"
            elif (( quota_pct < 80 )); then
                QUOTA_COLOR="$YELLOW"
            else
                QUOTA_COLOR="$RED"
            fi
            resets_in=$(echo "$quota_json" | jq -r '.five_hour.resets_in // empty')
            if [ -n "$resets_in" ]; then
                quota_info="${QUOTA_COLOR}Q:${quota_pct}%${NC} ${GRAY}${resets_in}${NC}"
            else
                quota_info="${QUOTA_COLOR}Q:${quota_pct}%${NC}"
            fi
        else
            quota_info="${GRAY}Q:--${NC}"
        fi
    else
        quota_info="${GRAY}Q:--${NC}"
    fi
fi

# --- Git info with stale-while-revalidate ---
#
# Git operations (status --porcelain, diff --numstat, counting untracked lines)
# can be slow in large repos. We cache the result and refresh in the background.

# Derive a stable cache key from the repo root path
git_cache_file=""
git_cache_lock=""
needs_git_refresh=false

_git_cache_key() {
    local repo_root
    repo_root=$(cd "$cwd" 2>/dev/null && git rev-parse --show-toplevel 2>/dev/null) || return 1
    # Use a hash of the repo path as filename to avoid collisions
    echo "$repo_root" | md5sum 2>/dev/null | cut -d' ' -f1 || echo "$repo_root" | md5 2>/dev/null | cut -d' ' -f1
}

_collect_git_info() {
    # Runs the actual git commands and writes result to cache file.
    # Intended to run either inline (first call) or in a background subshell.
    local target_dir="$1"
    local cache_file="$2"

    cd "$target_dir" 2>/dev/null || return 1

    local branch
    branch=$(git branch --show-current 2>/dev/null || echo "detached")
    local status_output
    status_output=$(git status --porcelain 2>/dev/null)

    local total_files=0 added=0 removed=0

    if [ -n "$status_output" ]; then
        total_files=$(echo "$status_output" | wc -l | xargs)

        local staged_stats unstaged_stats
        staged_stats=$(git diff --numstat --cached 2>/dev/null | awk '{a+=$1; r+=$2} END {print a+0, r+0}')
        unstaged_stats=$(git diff --numstat 2>/dev/null | awk '{a+=$1; r+=$2} END {print a+0, r+0}')

        local staged_added staged_removed unstaged_added unstaged_removed
        staged_added=$(echo "$staged_stats" | cut -d' ' -f1)
        staged_removed=$(echo "$staged_stats" | cut -d' ' -f2)
        unstaged_added=$(echo "$unstaged_stats" | cut -d' ' -f1)
        unstaged_removed=$(echo "$unstaged_stats" | cut -d' ' -f2)

        local untracked_lines
        untracked_lines=$(echo "$status_output" | grep '^??' | cut -c4- | while read -r f; do
            [ -f "$f" ] && wc -l < "$f" || echo 0
        done | awk '{s+=$1} END {print s+0}')

        added=$((staged_added + unstaged_added + untracked_lines))
        removed=$((staged_removed + unstaged_removed))
    fi

    # Write cache as simple key=value
    mkdir -p "$GIT_CACHE_DIR"
    cat > "$cache_file" <<EOF
branch=$branch
total_files=$total_files
added=$added
removed=$removed
cache_time=$(date +%s)
EOF
}

_read_git_cache() {
    local cache_file="$1"
    if [ -f "$cache_file" ]; then
        source "$cache_file" 2>/dev/null
        return 0
    fi
    return 1
}

_is_git_cache_stale() {
    local cache_file="$1"
    if [ ! -f "$cache_file" ]; then
        return 0  # no cache = stale
    fi
    local cache_time=0
    source "$cache_file" 2>/dev/null
    local now
    now=$(date +%s)
    local elapsed=$((now - cache_time))
    (( elapsed >= GIT_CACHE_INTERVAL ))
}

git_info=""
if cd "$cwd" 2>/dev/null && git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    # Branch name is always fast (reads .git/HEAD), so always fetch synchronously
    branch=$(git branch --show-current 2>/dev/null || echo "detached")

    cache_key=$(_git_cache_key)
    if [ -n "$cache_key" ]; then
        git_cache_file="${GIT_CACHE_DIR}/${cache_key}.cache"
        git_cache_lock="${GIT_CACHE_DIR}/${cache_key}.lock"

        if _is_git_cache_stale "$git_cache_file"; then
            # Stale or missing -- read whatever we have, then refresh in background
            _read_git_cache "$git_cache_file"  # ok if this fails (no cache yet)
            needs_git_refresh=true
        else
            # Cache is fresh
            _read_git_cache "$git_cache_file"
        fi

        # Spawn background refresh if needed (with lock to prevent concurrent refreshes)
        if $needs_git_refresh; then
            (
                if mkdir "$git_cache_lock" 2>/dev/null; then
                    _collect_git_info "$cwd" "$git_cache_file"
                    rmdir "$git_cache_lock" 2>/dev/null
                fi
            ) &
            disown 2>/dev/null
        fi

        # Build git_info from sourced cache variables (may be empty on first call)
        # Always use the live branch name fetched above
        if [ "${total_files:-0}" -gt 0 ] 2>/dev/null; then
            git_info=" ${GRAY}|${NC} ${YELLOW}${branch}${NC} ${GRAY}(${total_files} files"
            [ "${added:-0}" -gt 0 ] && git_info+=" ${GREEN}+${added}${NC}"
            [ "${removed:-0}" -gt 0 ] && git_info+=" ${RED}-${removed}${NC}"
            git_info+="${GRAY})${NC}"
        else
            # First call or clean repo -- just show branch
            git_info=" ${GRAY}|${NC} ${YELLOW}${branch}${NC}"
        fi
    fi
fi

# Context info
context_info="${BAR_COLOR}${bar}${NC} ${tokens_fmt} / ${size_fmt} (${pct_int}%)"

# Output the status line
echo -e "${BLUE}${dir_name}${NC} ${GRAY}|${NC} ${quota_info} ${GRAY}|${NC} ${context_info}${git_info}"
