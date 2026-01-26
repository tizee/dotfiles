#!/bin/bash
# see https://code.claude.com/docs/en/statusline

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
model_name=$(echo "$input" | jq -r '.model.display_name')
current_dir=$(echo "$input" | jq -r '.workspace.current_dir')

# Extract context window information
# Use 200K (200,000) as default context size (1K=1000)
context_size=$(echo "$input" | jq -r '.context_window.context_window_size // 200000')
current_usage=$(echo "$input" | jq '.context_window.current_usage')

# Calculate context percentage
if [ "$current_usage" != "null" ]; then
    current_tokens=$(echo "$current_usage" | jq '.input_tokens + .cache_creation_input_tokens + .cache_read_input_tokens')

    # For debugging purposes, show the breakdown
    # input_tokens=$(echo "$current_usage" | jq '.input_tokens')
    # cache_creation=$(echo "$current_usage" | jq '.cache_creation_input_tokens')
    # cache_read=$(echo "$current_usage" | jq '.cache_read_input_tokens')

    # Format context size for display using (1K = 1000), uppercase K
    format_context_size() {
        local size=$1
        if (( size >= 1000 )); then
            echo "$((size / 1000))K"
        else
            echo "$size"
        fi
    }

    # Format current tokens for display using (1K = 1000), uppercase K
    format_token_count() {
        local count=$1
        if (( count >= 1000 )); then
            printf "%.1fK" $(echo "scale=1; $count / 1000" | bc)
        else
            echo "$count"
        fi
    }

    context_percent=$((current_tokens * 100 / context_size))

    # Format values for display
    context_size_k=$(format_context_size $context_size)
    current_tokens_formatted=$(format_token_count $current_tokens)
else
    context_percent=0
    current_tokens=0
    context_size_k="200K"
    current_tokens_formatted="0"
fi

# Build context progress bar (20 chars wide)
bar_width=15
filled=$((context_percent * bar_width / 100))
empty=$((bar_width - filled))
bar=""
for ((i=0; i<filled; i++)); do bar+="█"; done
for ((i=0; i<empty; i++)); do bar+="░"; done

# Get directory name (basename)
dir_name=$(basename "$current_dir")

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
GRAY='\033[0;90m'
NC='\033[0m' # No Color

# Change to the current directory to get git info
cd "$current_dir" 2>/dev/null || cd /

# Get git branch
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    branch=$(git branch --show-current 2>/dev/null || echo "detached")

    # Get git status with file counts
    status_output=$(git status --porcelain 2>/dev/null)

    if [ -n "$status_output" ]; then
        # Count files and get basic line stats (including new files)
        total_files=$(echo "$status_output" | wc -l | xargs)

        # Get stats from both staged and unstaged changes
        staged_stats=$(git diff --numstat --cached 2>/dev/null | awk '{added+=$1; removed+=$2} END {print added+0, removed+0}')
        unstaged_stats=$(git diff --numstat 2>/dev/null | awk '{added+=$1; removed+=$2} END {print added+0, removed+0}')

        staged_added=$(echo $staged_stats | cut -d' ' -f1)
        staged_removed=$(echo $staged_stats | cut -d' ' -f2)
        unstaged_added=$(echo $unstaged_stats | cut -d' ' -f1)
        unstaged_removed=$(echo $unstaged_stats | cut -d' ' -f2)

        # Count lines in untracked files (files starting with ??)
        untracked_lines=$(echo "$status_output" | grep '^??' | cut -c4- | xargs -I {} sh -c 'test -f "{}" && wc -l < "{}" || echo 0' 2>/dev/null | awk '{sum+=$1} END {print sum+0}')

        added=$((staged_added + unstaged_added + untracked_lines))
        removed=$((staged_removed + unstaged_removed))

        # Build status display
        git_info=" ${YELLOW}($branch${NC} ${YELLOW}|${NC} ${GRAY}${total_files} files${NC}"

        [ "$added" -gt 0 ] && git_info="${git_info} ${GREEN}+${added}${NC}"
        [ "$removed" -gt 0 ] && git_info="${git_info} ${RED}-${removed}${NC}"

        git_info="${git_info} ${YELLOW})${NC}"
    else
        git_info=" ${YELLOW}($branch)${NC}"
    fi
else
    git_info=""
fi

# Build context bar display with k notation
context_info="${GRAY}${bar}${NC} ${current_tokens_formatted} / ${context_size_k} (${context_percent}%)"

# Get quota refresh progress bar and remaining time
get_quota_bar() {
    local bar=""
    local remaining=""
    
    # Check if claude-quota is available and running
    if command -v claude-quota >/dev/null 2>&1; then
        local quota_status=$(claude-quota status 2>/dev/null)
        
        if echo "$quota_status" | grep -q "active (running)"; then
            # Extract next refresh time and interval
            local mode_line=$(echo "$quota_status" | grep 'Mode:')
            local refresh_time=$(echo "$mode_line" | grep -oE '[0-9]{2}:[0-9]{2}:[0-9]{2}')
            local interval=$(echo "$mode_line" | grep -oE '\+[0-9]+h' | tr -d '+h')
            
            if [ -n "$refresh_time" ] && [ -n "$interval" ]; then
                # Calculate time difference in seconds
                local current_epoch=$(date +%s)
                local refresh_epoch=$(date -j -f "%H:%M:%S" "$refresh_time" +%s 2>/dev/null)
                
                # If refresh time is earlier than current time, it means it's tomorrow
                if [ "$refresh_epoch" -lt "$current_epoch" ]; then
                    refresh_epoch=$((refresh_epoch + 86400))
                fi
                
                local interval_seconds=$((interval * 3600))
                local elapsed=$((current_epoch - (refresh_epoch - interval_seconds)))
                local remaining_seconds=$((refresh_epoch - current_epoch))
                
                # Calculate percent
                local percent=$((elapsed * 100 / interval_seconds))
                
                # Clamp percent between 0 and 100
                [ "$percent" -lt 0 ] && percent=0
                [ "$percent" -gt 100 ] && percent=100
                
                # Build progress bar (10 chars wide)
                local bar_width=10
                local filled=$((percent * bar_width / 100))
                local empty=$((bar_width - filled))
                
                for ((i=0; i<filled; i++)); do bar+="▮"; done
                for ((i=0; i<empty; i++)); do bar+="▯"; done
                
                # Calculate remaining time in hours and minutes
                local remaining_hours=$((remaining_seconds / 3600))
                local remaining_minutes=$(((remaining_seconds % 3600) / 60))
                
                # Format remaining time
                if [ "$remaining_hours" -gt 0 ]; then
                    remaining="${remaining_hours}h ${remaining_minutes}m"
                else
                    remaining="${remaining_minutes}m"
                fi
                
                echo "${MAGENTA}${bar}${NC} ${percent}% ${GRAY}(${remaining} to reset)${NC}"
            fi
        fi
    fi
    
    echo ""
}

# Get quota bar
quota_bar=$(get_quota_bar)

# Output the status line
if [ -n "$quota_bar" ]; then
    echo -e "${BLUE}${dir_name}${NC} ${GRAY}|${NC} ${CYAN}${model_name}${NC} ${GRAY}|${NC} ${context_info} ${GRAY}|${NC} ${quota_bar}${git_info:+ ${GRAY}|${NC}}${git_info}"
else
    echo -e "${BLUE}${dir_name}${NC} ${GRAY}|${NC} ${CYAN}${model_name}${NC} ${GRAY}|${NC} ${context_info}${git_info:+ ${GRAY}|${NC}}${git_info}"
fi
