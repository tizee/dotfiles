#!/bin/bash
# Custom statusline for llms-agent (FEAT-065)
# Migrated from Claude Code statusline plugin
#
# Non-blocking design: all slow operations (quota API, git stats) use
# stale-while-revalidate -- return cached data immediately and refresh
# in the background. This keeps statusline rendering fast even when
# called at short fixed intervals.
#
# Supports multiple providers: Claude (sonnet/opus), MiniMax, GLM
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
export CLAUDE_OAUTH_TOKEN_CMD="jq -r '.claude' ~/.config/llms/keys.json"
QUOTA_SCRIPT="${SCRIPT_DIR}/quota.py"

# Git cache configuration
GIT_CACHE_DIR="/tmp/llms_statusline_cache"
GIT_CACHE_INTERVAL=10  # seconds between git refreshes

# Read JSON input from stdin
input=$(cat)

# Extract information from JSON
cwd=$(echo "$input" | jq -r '.cwd')
dir_name=$(basename "$cwd")

# Extract model information to determine provider
model_name=$(echo "$input" | jq -r '.model.model_name // ""')
model_display=$(echo "$input" | jq -r '.model.display_name // ""')

# Determine provider based on model name
# Claude: sonnet, opus, or claude models
# MiniMax: minimax models
# GLM: glm models
# Kimi: kimi models
# Codex: codex, gpt, o1, o3 models
determine_provider() {
    local model="$1"
    local model_lower=$(echo "$model" | tr '[:upper:]' '[:lower:]')

    if [[ "$model_lower" == *"claude"* ]] || [[ "$model_lower" == *"sonnet"* ]] || [[ "$model_lower" == *"opus"* ]]; then
        echo "claude"
    elif [[ "$model_lower" == *"minimax"* ]]; then
        echo "minimax"
    elif [[ "$model_lower" == *"glm"* ]]; then
        echo "glm"
    elif [[ "$model_lower" == *"kimi"* ]]; then
        echo "kimi"
    elif [[ "$model_lower" == *"doubao"* ]] || [[ "$model_lower" == *"volcengine"* ]]; then
        echo "doubao"
    elif [[ "$model_lower" == *"codex"* ]] || [[ "$model_lower" == *"gpt"* ]] || [[ "$model_lower" == "o1"* ]] || [[ "$model_lower" == "o3"* ]]; then
        echo "codex"
    else
        echo "claude"  # default to claude
    fi
}

provider=$(determine_provider "$model_name")
# Unified debounce interval for all providers (seconds)
QUOTA_DEBOUNCE=600  # 10 minutes

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

# Helper function to get color based on percentage
get_pct_color() {
    local pct=$1
    if (( pct < 50 )); then
        echo "$GREEN"
    elif (( pct < 80 )); then
        echo "$YELLOW"
    else
        echo "$RED"
    fi
}

# Helper function to extract day of week from ISO date
get_day_of_week() {
    local iso_date="$1"
    local date_part=$(echo "$iso_date" | cut -dT -f1)
    date -j -f "%Y-%m-%d" "$date_part" "+%a" 2>/dev/null || echo ""
}

# Fetch quota info (debounced, non-blocking via stale-while-revalidate in python script)
quota_info=""
if [ -f "$QUOTA_SCRIPT" ]; then
    quota_json=$(QUOTA_PROFILE="$QUOTA_PROFILE" python3 "$QUOTA_SCRIPT" -p "$provider" --json --debounce "$QUOTA_DEBOUNCE" 2>/dev/null)
    # Client errors (curl/network failures) are transient -- force-retry once
    # Server errors (API error responses) are legitimate -- hide normally
    if [ -n "$quota_json" ] && echo "$quota_json" | jq -e 'select(.error_type == "client")' >/dev/null 2>&1; then
        quota_json=$(QUOTA_PROFILE="$QUOTA_PROFILE" python3 "$QUOTA_SCRIPT" -p "$provider" --json --debounce "$QUOTA_DEBOUNCE" --force 2>/dev/null)
    fi
    if [ -n "$quota_json" ]; then
        # Parse quota based on provider
        if [ "$provider" = "claude" ]; then
            # Claude: current (5-hour), weekly, weekly_sonnet
            current_pct=$(echo "$quota_json" | jq -r '.sessions.current.used_percent // empty')
            if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                quota_pct=$(printf "%.0f" "$current_pct")
                QUOTA_COLOR=$(get_pct_color "$quota_pct")
                resets_in=$(echo "$quota_json" | jq -r '.sessions.current.resets_in // empty')
                resets_at_local=$(echo "$quota_json" | jq -r '.sessions.current.resets_at_local // empty')
                if [ -n "$resets_in" ]; then
                    quota_info="${QUOTA_COLOR}Q:${quota_pct}%${NC} ${GRAY}${resets_in}"
                    [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                    quota_info+="${NC}"
                else
                    quota_info="${QUOTA_COLOR}Q:${quota_pct}%${NC}"
                fi
            else
                # No valid data - hide quota info
                quota_info=""
            fi

            # Weekly quota
            weekly_pct=$(echo "$quota_json" | jq -r '.sessions.weekly.used_percent // empty')
            if [ -n "$weekly_pct" ] && [ "$weekly_pct" != "null" ]; then
                weekly_pct_int=$(printf "%.0f" "$weekly_pct")
                WEEKLY_COLOR=$(get_pct_color "$weekly_pct_int")
                weekly_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at // empty')
                weekly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at_local // empty')
                weekly_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_in // empty')
                if [ -n "$weekly_resets_at" ] && [ -n "$weekly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$weekly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${WEEKLY_COLOR}W:${weekly_pct_int}%${NC} ${GRAY}${week_day} @${weekly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$weekly_resets_in" ] && [ "$weekly_resets_in" != "null" ]; then
                            quota_info+=" (in ${weekly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi

            # Sonnet quota (if using sonnet model)
            if [[ "$model_name" == *"sonnet"* ]] || [[ "$model_display" == *"sonnet"* ]] || [[ "$model_display" == *"Sonnet"* ]]; then
                sonnet_pct=$(echo "$quota_json" | jq -r '.sessions.weekly_sonnet.used_percent // empty')
                if [ -n "$sonnet_pct" ] && [ "$sonnet_pct" != "null" ]; then
                    sonnet_pct_int=$(printf "%.0f" "$sonnet_pct")
                    SONNET_COLOR=$(get_pct_color "$sonnet_pct_int")
                    sonnet_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly_sonnet.resets_at // empty')
                    sonnet_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly_sonnet.resets_at_local // empty')
                    sonnet_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly_sonnet.resets_in // empty')
                    if [ -n "$sonnet_resets_at" ] && [ -n "$sonnet_resets_at_local" ]; then
                        week_day=$(get_day_of_week "$sonnet_resets_at")
                        if [ -n "$week_day" ]; then
                            quota_info+=" ${SONNET_COLOR}Son:${sonnet_pct_int}%${NC} ${GRAY}${week_day} @${sonnet_resets_at_local}"
                            # Add remaining time if available (e.g., "in 2d 3h")
                            if [ -n "$sonnet_resets_in" ] && [ "$sonnet_resets_in" != "null" ]; then
                                quota_info+=" (in ${sonnet_resets_in})"
                            fi
                            quota_info+="${NC}"
                        fi
                    fi
                fi
            fi

        elif [ "$provider" = "minimax" ]; then
            # MiniMax: show text model quota (MiniMax-M* is the text/coding model)
            # Token plan includes non-text models (video, speech, image) -- skip those
            first_model=$(echo "$quota_json" | jq -r '.sessions | keys[] | select(startswith("MiniMax-M"))' | head -1)
            if [ -n "$first_model" ]; then
                current_pct=$(echo "$quota_json" | jq -r ".sessions[\"$first_model\"].used_percent // empty")
                mm_used=$(echo "$quota_json" | jq -r ".sessions[\"$first_model\"].used // empty")
                mm_limit=$(echo "$quota_json" | jq -r ".sessions[\"$first_model\"].limit // empty")
                if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                    quota_pct=$(printf "%.0f" "$current_pct")
                    QUOTA_COLOR=$(get_pct_color "$quota_pct")
                    resets_in=$(echo "$quota_json" | jq -r ".sessions[\"$first_model\"].resets_in // empty")
                    resets_at_local=$(echo "$quota_json" | jq -r ".sessions[\"$first_model\"].resets_at_local // empty")
                    # Build prompt usage string (used/limit)
                    prompt_str=""
                    if [ -n "$mm_used" ] && [ "$mm_used" != "null" ] && [ -n "$mm_limit" ] && [ "$mm_limit" != "null" ]; then
                        mm_used_int=$(printf "%.0f" "$mm_used")
                        mm_limit_int=$(printf "%.0f" "$mm_limit")
                        prompt_str=" ${GRAY}${mm_used_int}/${mm_limit_int}${NC}"
                    fi
                    if [ -n "$resets_in" ]; then
                        quota_info="${QUOTA_COLOR}MM:${quota_pct}%${NC}${prompt_str} ${GRAY}${resets_in}"
                        [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                        quota_info+="${NC}"
                    else
                        quota_info="${QUOTA_COLOR}MM:${quota_pct}%${NC}${prompt_str}"
                    fi
                else
                    # No valid data - hide quota info
                    quota_info=""
                fi
            else
                # No MiniMax session found - hide quota info
                quota_info=""
            fi

        elif [ "$provider" = "glm" ]; then
            # GLM: current (5-hour), daily, weekly
            current_pct=$(echo "$quota_json" | jq -r '.sessions.current.used_percent // empty')
            if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                quota_pct=$(printf "%.0f" "$current_pct")
                QUOTA_COLOR=$(get_pct_color "$quota_pct")
                resets_in=$(echo "$quota_json" | jq -r '.sessions.current.resets_in // empty')
                resets_at_local=$(echo "$quota_json" | jq -r '.sessions.current.resets_at_local // empty')
                if [ -n "$resets_in" ]; then
                    quota_info="${QUOTA_COLOR}GLM:${quota_pct}%${NC} ${GRAY}${resets_in}"
                    [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                    quota_info+="${NC}"
                else
                    quota_info="${QUOTA_COLOR}GLM:${quota_pct}%${NC}"
                fi
            else
                # No valid data - hide quota info
                quota_info=""
            fi

            # Daily quota
            daily_pct=$(echo "$quota_json" | jq -r '.sessions.daily.used_percent // empty')
            if [ -n "$daily_pct" ] && [ "$daily_pct" != "null" ]; then
                daily_pct_int=$(printf "%.0f" "$daily_pct")
                DAILY_COLOR=$(get_pct_color "$daily_pct_int")
                daily_resets_in=$(echo "$quota_json" | jq -r '.sessions.daily.resets_in // empty')
                daily_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.daily.resets_at_local // empty')
                if [ -n "$daily_resets_in" ]; then
                    quota_info+=" ${DAILY_COLOR}D:${daily_pct_int}%${NC} ${GRAY}${daily_resets_in}"
                    [ -n "$daily_resets_at_local" ] && quota_info+=" @${daily_resets_at_local}"
                    quota_info+="${NC}"
                fi
            fi

            # Weekly quota
            weekly_pct=$(echo "$quota_json" | jq -r '.sessions.weekly.used_percent // empty')
            if [ -n "$weekly_pct" ] && [ "$weekly_pct" != "null" ]; then
                weekly_pct_int=$(printf "%.0f" "$weekly_pct")
                WEEKLY_COLOR=$(get_pct_color "$weekly_pct_int")
                weekly_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at // empty')
                weekly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at_local // empty')
                weekly_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_in // empty')
                if [ -n "$weekly_resets_at" ] && [ -n "$weekly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$weekly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${WEEKLY_COLOR}W:${weekly_pct_int}%${NC} ${GRAY}${week_day} @${weekly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$weekly_resets_in" ] && [ "$weekly_resets_in" != "null" ]; then
                            quota_info+=" (in ${weekly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi

        elif [ "$provider" = "kimi" ]; then
            # Kimi: 5-hour session, weekly
            current_pct=$(echo "$quota_json" | jq -r '.sessions.current.used_percent // empty')
            if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                quota_pct=$(printf "%.0f" "$current_pct")
                QUOTA_COLOR=$(get_pct_color "$quota_pct")
                resets_in=$(echo "$quota_json" | jq -r '.sessions.current.resets_in // empty')
                resets_at_local=$(echo "$quota_json" | jq -r '.sessions.current.resets_at_local // empty')
                if [ -n "$resets_in" ]; then
                    quota_info="${QUOTA_COLOR}Kimi:${quota_pct}%${NC} ${GRAY}${resets_in}"
                    [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                    quota_info+="${NC}"
                else
                    quota_info="${QUOTA_COLOR}Kimi:${quota_pct}%${NC}"
                fi
            else
                # No valid data - hide quota info
                quota_info=""
            fi

            # Weekly quota
            weekly_pct=$(echo "$quota_json" | jq -r '.sessions.weekly.used_percent // empty')
            if [ -n "$weekly_pct" ] && [ "$weekly_pct" != "null" ]; then
                weekly_pct_int=$(printf "%.0f" "$weekly_pct")
                WEEKLY_COLOR=$(get_pct_color "$weekly_pct_int")
                weekly_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at // empty')
                weekly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at_local // empty')
                weekly_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_in // empty')
                if [ -n "$weekly_resets_at" ] && [ -n "$weekly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$weekly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${WEEKLY_COLOR}W:${weekly_pct_int}%${NC} ${GRAY}${week_day} @${weekly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$weekly_resets_in" ] && [ "$weekly_resets_in" != "null" ]; then
                            quota_info+=" (in ${weekly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi

        elif [ "$provider" = "codex" ]; then
            # Codex: 5-hour session (primary), weekly (secondary)
            current_pct=$(echo "$quota_json" | jq -r '.sessions.current.used_percent // empty')
            if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                quota_pct=$(printf "%.0f" "$current_pct")
                QUOTA_COLOR=$(get_pct_color "$quota_pct")
                resets_in=$(echo "$quota_json" | jq -r '.sessions.current.resets_in // empty')
                resets_at_local=$(echo "$quota_json" | jq -r '.sessions.current.resets_at_local // empty')
                if [ -n "$resets_in" ]; then
                    quota_info="${QUOTA_COLOR}Codex:${quota_pct}%${NC} ${GRAY}${resets_in}"
                    [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                    quota_info+="${NC}"
                else
                    quota_info="${QUOTA_COLOR}Codex:${quota_pct}%${NC}"
                fi
            else
                # No valid data - hide quota info
                quota_info=""
            fi

            # Weekly quota
            weekly_pct=$(echo "$quota_json" | jq -r '.sessions.weekly.used_percent // empty')
            if [ -n "$weekly_pct" ] && [ "$weekly_pct" != "null" ]; then
                weekly_pct_int=$(printf "%.0f" "$weekly_pct")
                WEEKLY_COLOR=$(get_pct_color "$weekly_pct_int")
                weekly_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at // empty')
                weekly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at_local // empty')
                weekly_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_in // empty')
                if [ -n "$weekly_resets_at" ] && [ -n "$weekly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$weekly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${WEEKLY_COLOR}W:${weekly_pct_int}%${NC} ${GRAY}${week_day} @${weekly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$weekly_resets_in" ] && [ "$weekly_resets_in" != "null" ]; then
                            quota_info+=" (in ${weekly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi

        elif [ "$provider" = "doubao" ]; then
            # Doubao: session (5-hour), weekly, monthly
            current_pct=$(echo "$quota_json" | jq -r '.sessions.current.used_percent // empty')
            if [ -n "$current_pct" ] && [ "$current_pct" != "null" ]; then
                quota_pct=$(printf "%.0f" "$current_pct")
                QUOTA_COLOR=$(get_pct_color "$quota_pct")
                resets_in=$(echo "$quota_json" | jq -r '.sessions.current.resets_in // empty')
                resets_at_local=$(echo "$quota_json" | jq -r '.sessions.current.resets_at_local // empty')
                if [ -n "$resets_in" ]; then
                    quota_info="${QUOTA_COLOR}DB:${quota_pct}%${NC} ${GRAY}${resets_in}"
                    [ -n "$resets_at_local" ] && quota_info+=" @${resets_at_local}"
                    quota_info+="${NC}"
                else
                    quota_info="${QUOTA_COLOR}DB:${quota_pct}%${NC}"
                fi
            else
                # No valid data - hide quota info
                quota_info=""
            fi

            # Weekly quota
            weekly_pct=$(echo "$quota_json" | jq -r '.sessions.weekly.used_percent // empty')
            if [ -n "$weekly_pct" ] && [ "$weekly_pct" != "null" ]; then
                weekly_pct_int=$(printf "%.0f" "$weekly_pct")
                WEEKLY_COLOR=$(get_pct_color "$weekly_pct_int")
                weekly_resets_at=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at // empty')
                weekly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_at_local // empty')
                weekly_resets_in=$(echo "$quota_json" | jq -r '.sessions.weekly.resets_in // empty')
                if [ -n "$weekly_resets_at" ] && [ -n "$weekly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$weekly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${WEEKLY_COLOR}W:${weekly_pct_int}%${NC} ${GRAY}${week_day} @${weekly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$weekly_resets_in" ] && [ "$weekly_resets_in" != "null" ]; then
                            quota_info+=" (in ${weekly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi

            # Monthly quota
            monthly_pct=$(echo "$quota_json" | jq -r '.sessions.monthly.used_percent // empty')
            if [ -n "$monthly_pct" ] && [ "$monthly_pct" != "null" ]; then
                monthly_pct_int=$(printf "%.0f" "$monthly_pct")
                MONTHLY_COLOR=$(get_pct_color "$monthly_pct_int")
                monthly_resets_at=$(echo "$quota_json" | jq -r '.sessions.monthly.resets_at // empty')
                monthly_resets_at_local=$(echo "$quota_json" | jq -r '.sessions.monthly.resets_at_local // empty')
                monthly_resets_in=$(echo "$quota_json" | jq -r '.sessions.monthly.resets_in // empty')
                if [ -n "$monthly_resets_at" ] && [ -n "$monthly_resets_at_local" ]; then
                    week_day=$(get_day_of_week "$monthly_resets_at")
                    if [ -n "$week_day" ]; then
                        quota_info+=" ${MONTHLY_COLOR}M:${monthly_pct_int}%${NC} ${GRAY}${week_day} @${monthly_resets_at_local}"
                        # Add remaining time if available (e.g., "in 2d 3h")
                        if [ -n "$monthly_resets_in" ] && [ "$monthly_resets_in" != "null" ]; then
                            quota_info+=" (in ${monthly_resets_in})"
                        fi
                        quota_info+="${NC}"
                    fi
                fi
            fi
        fi
    else
        # No valid quota data - hide quota info entirely
        quota_info=""
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

    # Write cache atomically: write .tmp then mv (prevents partial reads)
    mkdir -p "$GIT_CACHE_DIR"
    local tmp_file="${cache_file}.tmp.$$"
    cat > "$tmp_file" <<EOF
branch=$branch
total_files=$total_files
added=$added
removed=$removed
cache_time=$(date +%s)
EOF
    mv -f "$tmp_file" "$cache_file"
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
            # Cache is stale or missing
            if [ ! -f "$git_cache_file" ]; then
                # First call: collect synchronously so we have data immediately
                _collect_git_info "$cwd" "$git_cache_file"
            else
                # Cache exists but stale: read stale data, refresh in background
                _read_git_cache "$git_cache_file"
                needs_git_refresh=true
            fi
        else
            # Cache is fresh
            _read_git_cache "$git_cache_file"
        fi

        # Spawn background refresh if needed (with lock to prevent concurrent refreshes)
        if $needs_git_refresh; then
            # Stale lock detection: if lock dir is older than 60s, remove it
            if [ -d "$git_cache_lock" ]; then
                lock_age=$(( $(date +%s) - $(stat -f %m "$git_cache_lock" 2>/dev/null || echo 0) ))
                if (( lock_age > 60 )); then
                    rmdir "$git_cache_lock" 2>/dev/null
                fi
            fi
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

# Session identity (FEAT-259)
session_id=$(echo "$input" | jq -r '.session.id // ""')
session_alias=$(echo "$input" | jq -r '.session.alias // ""')

session_info=""
if [ -z "$session_id" ]; then
    session_info=" ${GRAY}|${NC} ${YELLOW}ephemeral${NC}"
else
    if [ -n "$session_alias" ]; then
        session_info=" ${GRAY}|${NC} ${CYAN}${session_alias}${NC} ${GRAY}(${session_id})${NC}"
    else
        session_info=" ${GRAY}|${NC} ${GRAY}${session_id}${NC}"
    fi
fi

# Output the status line
if [ -n "$quota_info" ]; then
    echo -e "${BLUE}${dir_name}${NC} ${GRAY}|${NC} ${quota_info} ${GRAY}|${NC} ${context_info}${git_info}${session_info}"
else
    # No quota info - hide that component
    echo -e "${BLUE}${dir_name}${NC} ${GRAY}|${NC} ${context_info}${git_info}${session_info}"
fi
