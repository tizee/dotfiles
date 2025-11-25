#!/usr/bin/env zsh

# Default key binding
(( ! ${+SMART_SUGGESTION_KEY} )) &&
    typeset -g SMART_SUGGESTION_KEY='^o'

# Prompt recovery key binding
(( ! ${+SMART_SUGGESTION_RECOVER_KEY} )) &&
    typeset -g SMART_SUGGESTION_RECOVER_KEY='^[^o'

# Configuration options
(( ! ${+SMART_SUGGESTION_SEND_CONTEXT} )) &&
    typeset -g SMART_SUGGESTION_SEND_CONTEXT=true

(( ! ${+SMART_SUGGESTION_DEBUG} )) &&
    typeset -g SMART_SUGGESTION_DEBUG=false

# Proxy mode configuration - now enabled by default
(( ! ${+SMART_SUGGESTION_PROXY_MODE} )) &&
    typeset -g SMART_SUGGESTION_PROXY_MODE=true

# Auto-update configuration
(( ! ${+SMART_SUGGESTION_AUTO_UPDATE} )) &&
    typeset -g SMART_SUGGESTION_AUTO_UPDATE=true

# Update interval configuration in days
(( ! ${+SMART_SUGGESTION_UPDATE_INTERVAL} )) &&
    typeset -g SMART_SUGGESTION_UPDATE_INTERVAL=7

# Provider configuration file path
(( ! ${+SMART_SUGGESTION_PROVIDER_FILE} )) &&
    typeset -g SMART_SUGGESTION_PROVIDER_FILE="$HOME/.config/smart-suggestion/config.json"

# Default AI provider (empty means use default_provider from config file)
(( ! ${+SMART_SUGGESTION_AI_PROVIDER} )) &&
    typeset -g SMART_SUGGESTION_AI_PROVIDER=""

# Privacy filter configuration - enabled by default for security
(( ! ${+SMART_SUGGESTION_PRIVACY_FILTER} )) &&
    typeset -g SMART_SUGGESTION_PRIVACY_FILTER=true

# Privacy filter level configuration
(( ! ${+SMART_SUGGESTION_PRIVACY_LEVEL} )) &&
    typeset -g SMART_SUGGESTION_PRIVACY_LEVEL=basic

if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
    touch /tmp/smart-suggestion.log
fi

# Detect binary path
if [[ -z "$SMART_SUGGESTION_BINARY" ]]; then
    candidates=(
        "${0:a:h}/smart-suggestion"
        "$HOME/.config/smart-suggestion/smart-suggestion"
    )
    for bin in "${candidates[@]}"; do
        if [[ -f "$bin" ]]; then
            typeset -g SMART_SUGGESTION_BINARY="$bin"
            break
        fi
    done
    if [[ -z "$SMART_SUGGESTION_BINARY" ]]; then
        echo "No available smart-suggestion binary found. Please ensure that it is installed correctly or set SMART_SUGGESTION_BINARY to a valid binary path."
        return 1
    fi
else
    if [[ ! -f "$SMART_SUGGESTION_BINARY" ]]; then
        echo "smart-suggestion binary not found at $SMART_SUGGESTION_BINARY."
        return 1
    fi
fi

function _run_smart_suggestion_proxy() {
    if [[ $- == *i* ]]; then
        "$SMART_SUGGESTION_BINARY" proxy
    fi
}

function _fetch_suggestions() {
    # Prepare debug flag
    local debug_flag=""
    if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
        debug_flag="--debug"
    fi

    # Prepare context flag
    local context_flag=""
    if [[ "$SMART_SUGGESTION_SEND_CONTEXT" == 'true' ]]; then
        context_flag="--context"
    fi

    # Prepare provider flag (only if SMART_SUGGESTION_AI_PROVIDER is set and non-empty)
    # This allows using default_provider from config file when environment variable is not set
    local provider_args=()
    if [[ -n "$SMART_SUGGESTION_AI_PROVIDER" ]]; then
        provider_args=(--provider "$SMART_SUGGESTION_AI_PROVIDER")
    fi

    # Call the Go binary with proper arguments
    "$SMART_SUGGESTION_BINARY" \
        "${provider_args[@]}" \
        --input "$input" \
        --output "/tmp/smart_suggestion" \
        $debug_flag \
        $context_flag

    return $?
}


function _show_loading_animation() {
    local pid=$1
    local interval=0.1
    local animation_chars=("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
    local i=1

    cleanup() {
        kill $pid
        tput -S <<<"bicr ed rc cnorm"
        touch /tmp/.smart_suggestion_canceled
    }
    trap cleanup SIGINT

    tput -S <<<"sc civis"
    while kill -0 $pid 2>/dev/null; do
        # Display current animation frame
        zle -R "${animation_chars[i]} Press <Ctrl-c> to cancel"

        # Update index, make sure it starts at 1
        i=$(( (i + 1) % ${#animation_chars[@]} ))

        if [[ $i -eq 0 ]]; then
            i=1
        fi

        sleep $interval
    done

    tput cnorm
    trap - SIGINT
}

function _do_smart_suggestion() {
    ##### Get input
    command rm -f /tmp/smart_suggestion
    command rm -f /tmp/.smart_suggestion_canceled
    command rm -f /tmp/.smart_suggestion_error
    local input=$(echo "${BUFFER:0:$CURSOR}" | tr '\n' ';')
    
    ##### Save current input to history for recovery
    echo "$input" > /tmp/smart_suggestion_last_prompt

    _zsh_autosuggest_clear

    ##### Fetch message
    read < <(_fetch_suggestions & echo $!)
    local pid=$REPLY

    _show_loading_animation $pid
    local response_code=$?

    if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
        echo "{\"date\":\"$(date)\",\"log\":\"Fetched message\",\"input\":\"$input\",\"response_code\":\"$response_code\"}" >> /tmp/smart-suggestion.log
    fi

    if [[ -f /tmp/.smart_suggestion_canceled ]]; then
        _zsh_autosuggest_clear
        return 1
    fi

    if [[ ! -f /tmp/smart_suggestion ]]; then
        _zsh_autosuggest_clear
        local error_msg=$(cat /tmp/.smart_suggestion_error 2>/dev/null || echo "No suggestion available at this time. Please try again later.")
        zle -M "$error_msg"
        return 1
    fi

    local message=$(cat /tmp/smart_suggestion)

    ##### Process response

    local first_char=${message:0:1}
    local suggestion=${message:1:${#message}}

    ##### And now, let's actually show the suggestion to the user!

    if [[ "$first_char" == '=' ]]; then
        # Reset user input
        BUFFER=""
        CURSOR=0

        zle -U "$suggestion"
    elif [[ "$first_char" == '+' ]]; then
        _zsh_autosuggest_suggest "$suggestion"
    fi
}

function _recover_last_prompt() {
    ##### Check if last prompt file exists
    if [[ ! -f /tmp/smart_suggestion_last_prompt ]]; then
        zle -M "No previous prompt found"
        return 1
    fi

    ##### Read the last prompt
    local last_prompt=$(cat /tmp/smart_suggestion_last_prompt 2>/dev/null)

    if [[ -z "$last_prompt" ]]; then
        zle -M "No previous prompt found"
        return 1
    fi

    ##### Restore the prompt to buffer, converting back from semicolon format
    local restored_prompt=$(echo "$last_prompt" | tr ';' '\n')
    BUFFER="$restored_prompt"
    CURSOR=${#BUFFER}
    zle redisplay
}

function _check_smart_suggestion_updates() {
    # Check if SMART_SUGGESTION_UPDATE_INTERVAL is a positive integer
    if [[ "$SMART_SUGGESTION_UPDATE_INTERVAL" -le 0 ]]; then
        echo "SMART_SUGGESTION_UPDATE_INTERVAL must be a positive integer. Will be reset to default value."
        SMART_SUGGESTION_UPDATE_INTERVAL=7
    fi

    local update_file="$(dirname $SMART_SUGGESTION_BINARY)/.last_update_check"
    local current_time=$(date +%s)
    local update_interval=$((SMART_SUGGESTION_UPDATE_INTERVAL * 24 * 3600))  # Convert days to seconds

    # Check if we should check for updates
    if [[ -f "$update_file" ]]; then
        local last_check=$(cat "$update_file" 2>/dev/null || echo "0")
        local time_diff=$((current_time - last_check))

        if [[ $time_diff -lt $update_interval ]]; then
            return 0  # Too soon to check again
        fi
    fi

    # Update the last check time
    echo "$current_time" > "$update_file"

    # Check for updates in background
    ("$SMART_SUGGESTION_BINARY" update --check-only 2>/dev/null && \
        echo "Smart Suggestion update available! Run 'smart-suggestion update' to update." || true) &
}

function smart-suggestion() {
    echo "Smart Suggestion is now active. Press $SMART_SUGGESTION_KEY to get suggestions."
    echo "Press $SMART_SUGGESTION_RECOVER_KEY to recover the last prompt."
    echo ""
    echo "Configurations:"
    echo "    - SMART_SUGGESTION_KEY: Key to press to get suggestions (default: ^o, value: $SMART_SUGGESTION_KEY)."
    echo "    - SMART_SUGGESTION_RECOVER_KEY: Key to press to recover last prompt (default: ^[^o, value: $SMART_SUGGESTION_RECOVER_KEY)."
    echo "    - SMART_SUGGESTION_SEND_CONTEXT: If \`true\`, smart-suggestion will send context information (whoami, shell, pwd, etc.) to the AI model (default: true, value: $SMART_SUGGESTION_SEND_CONTEXT)."
    echo "    - SMART_SUGGESTION_AI_PROVIDER: AI provider to use ('openai', 'openai_compatible', 'azure_openai', 'anthropic', 'gemini', or 'deepseek'). If empty, uses default_provider from config file (value: ${SMART_SUGGESTION_AI_PROVIDER:-"(using config file default)"})."
    echo "    - SMART_SUGGESTION_PRIVACY_FILTER: Enable/disable privacy filtering for context data (default: true, value: ${SMART_SUGGESTION_PRIVACY_FILTER:-"true"})."
    echo "    - SMART_SUGGESTION_PRIVACY_LEVEL: Privacy filtering level ('none', 'basic', 'moderate', 'strict') (default: basic, value: ${SMART_SUGGESTION_PRIVACY_LEVEL:-"basic"})."
    echo "    - SMART_SUGGESTION_DEBUG: Enable debug logging (default: false, value: $SMART_SUGGESTION_DEBUG)."
    echo "    - SMART_SUGGESTION_AUTO_UPDATE: Enable automatic update checking (default: true, value: $SMART_SUGGESTION_AUTO_UPDATE)."
    echo "    - SMART_SUGGESTION_UPDATE_INTERVAL: Days between update checks (default: 7, value: $SMART_SUGGESTION_UPDATE_INTERVAL)."
    echo "    - SMART_SUGGESTION_BINARY: Binary path (value: $SMART_SUGGESTION_BINARY)."
}

zle -N _do_smart_suggestion
zle -N _recover_last_prompt
bindkey "$SMART_SUGGESTION_KEY" _do_smart_suggestion
bindkey "$SMART_SUGGESTION_RECOVER_KEY" _recover_last_prompt

if [[ "$SMART_SUGGESTION_PROXY_MODE" == "true" && -z "$TMUX" && -z "$KITTY_LISTEN_ON" ]]; then
    _run_smart_suggestion_proxy
fi

# Add update check to plugin initialization
if [[ "$SMART_SUGGESTION_AUTO_UPDATE" == "true" ]]; then
    _check_smart_suggestion_updates
fi
