#!/usr/bin/env zsh

# Guard: prevent reloading
(( ${+_SMART_SUGGESTION_LOADED} )) && return
typeset -g _SMART_SUGGESTION_LOADED=1

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

# Optional llms model override
(( ! ${+SMART_SUGGESTION_LLMS_MODEL} )) &&
    typeset -g SMART_SUGGESTION_LLMS_MODEL=""

# Resolve path to the Python backend (sibling file)
typeset -g _SMART_SUGGESTION_PY="${0:A:h}/smart-suggestion.py"

if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
    touch /tmp/smart-suggestion.log
fi

if ! command -v python3 >/dev/null 2>&1; then
    echo "python3 not found in PATH."
    return 1
fi

if [[ ! -f "$_SMART_SUGGESTION_PY" ]]; then
    echo "smart-suggestion.py not found at $_SMART_SUGGESTION_PY"
    return 1
fi

function _smart_suggestion_debug_log() {
    if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
        print -r -- "[$(date '+%F %T')] $*" >> /tmp/smart-suggestion.log
    fi
}

# Collect shell-level context that only ZSH can provide and
# build a JSON request for the Python backend.
function _smart_suggestion_build_request() {
    local input="$1"
    local current_user="${USER:-$(whoami 2>/dev/null)}"
    local -a cwd_dirs cwd_files
    local aliases_text=""
    local history_text=""

    cwd_dirs=( *(N-/) )
    cwd_files=( *(N-.) )
    cwd_dirs=("${cwd_dirs[@]:1:40}")
    cwd_files=("${cwd_files[@]:1:80}")

    aliases_text=$(alias 2>/dev/null)
    history_text=$(fc -ln -20 -1 2>/dev/null)

    # Build JSON via python3 to guarantee correct escaping
    python3 -c '
import json, sys
obj = {
    "input": sys.argv[1],
    "cwd": sys.argv[2],
    "shell": "zsh",
    "user": sys.argv[3],
    "model": sys.argv[4],
    "send_context": sys.argv[5] == "true",
    "aliases": sys.argv[6],
    "history": sys.argv[7],
    "cwd_dirs": json.loads(sys.argv[8]),
    "cwd_files": json.loads(sys.argv[9]),
}
json.dump(obj, sys.stdout)
' \
        "$input" \
        "$PWD" \
        "$current_user" \
        "$SMART_SUGGESTION_LLMS_MODEL" \
        "$SMART_SUGGESTION_SEND_CONTEXT" \
        "$aliases_text" \
        "$history_text" \
        "$(printf '%s\n' "${cwd_dirs[@]}" | python3 -c 'import json,sys; json.dump([l.rstrip("\n") for l in sys.stdin if l.strip()], sys.stdout)')" \
        "$(printf '%s\n' "${cwd_files[@]}" | python3 -c 'import json,sys; json.dump([l.rstrip("\n") for l in sys.stdin if l.strip()], sys.stdout)')"
}

function _show_loading_animation() {
    local pid=$1
    local interval=0.1
    local animation_chars=("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
    local i=1

    cleanup() {
        kill $pid 2>/dev/null
        tput -S <<<"bicr ed rc cnorm"
        touch /tmp/.smart_suggestion_canceled
    }
    trap cleanup SIGINT

    tput -S <<<"sc civis"
    while kill -0 $pid 2>/dev/null; do
        zle -R "${animation_chars[i]} Press <Ctrl-c> to cancel"

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
    command rm -f /tmp/.smart_suggestion_canceled

    local input=$(echo "${BUFFER:0:$CURSOR}" | tr '\n' ';')

    print -r -- "$input" > /tmp/smart_suggestion_last_prompt

    _zsh_autosuggest_clear

    local request=$(_smart_suggestion_build_request "$input")

    _smart_suggestion_debug_log "Request: $request"

    # Write request to temp file; pipe-in-background breaks stdin in ZLE context
    local tmpin=$(mktemp /tmp/smart_suggestion_in.XXXXXX)
    local tmpout=$(mktemp /tmp/smart_suggestion_out.XXXXXX)
    print -r -- "$request" > "$tmpin"

    # no_monitor: suppress [1] PID / [1] + done job notifications in ZLE
    # local_options: restore monitor setting on function return
    setopt local_options no_monitor
    python3 "$_SMART_SUGGESTION_PY" < "$tmpin" > "$tmpout" 2>/tmp/.smart_suggestion_error &
    local pid=$!

    _show_loading_animation $pid
    wait $pid 2>/dev/null
    local exit_code=$?
    command rm -f "$tmpin"

    _smart_suggestion_debug_log "Python exit_code=$exit_code"

    if [[ -f /tmp/.smart_suggestion_canceled ]]; then
        command rm -f "$tmpout"
        _zsh_autosuggest_clear
        return 1
    fi

    if (( exit_code != 0 )); then
        command rm -f "$tmpout"
        _zsh_autosuggest_clear
        local error_msg=$(cat /tmp/.smart_suggestion_error 2>/dev/null || echo "Smart suggestion failed.")
        zle -M "$error_msg"
        return 1
    fi

    # Parse JSON response in a single python3 call.
    # shlex.quote guarantees shell-safe values for eval.
    _smart_suggestion_debug_log "Response file: $tmpout"

    local resp_status="" mode="" suggestion="" error_msg=""
    eval "$(python3 -c "
import json, sys, shlex
try:
    with open(sys.argv[1]) as f:
        r = json.load(f)
except Exception as e:
    print('resp_status=error')
    print(f'error_msg={shlex.quote(str(e))}')
    sys.exit(0)
s = r.get('status', 'error')
print(f'resp_status={shlex.quote(s)}')
if s == 'ok':
    print(f'mode={shlex.quote(r.get(\"mode\", \"=\"))}')
    print(f'suggestion={shlex.quote(r.get(\"suggestion\", \"\"))}')
else:
    print(f'error_msg={shlex.quote(r.get(\"message\", \"Unknown error\"))}')
" "$tmpout")"
    command rm -f "$tmpout"

    _smart_suggestion_debug_log "Parsed resp_status=$resp_status mode=$mode suggestion=$suggestion"

    if [[ "$resp_status" != "ok" ]]; then
        _zsh_autosuggest_clear
        zle -M "${error_msg:-Smart suggestion failed.}"
        return 1
    fi

    if [[ "$mode" == '=' ]]; then
        BUFFER=""
        CURSOR=0
        zle -U "$suggestion"
    elif [[ "$mode" == '+' ]]; then
        _zsh_autosuggest_suggest "$suggestion"
    fi
}

function _recover_last_prompt() {
    if [[ ! -f /tmp/smart_suggestion_last_prompt ]]; then
        zle -M "No previous prompt found"
        return 1
    fi

    local last_prompt=$(cat /tmp/smart_suggestion_last_prompt 2>/dev/null)
    if [[ -z "$last_prompt" ]]; then
        zle -M "No previous prompt found"
        return 1
    fi

    local restored_prompt=$(echo "$last_prompt" | tr ';' '\n')
    BUFFER="$restored_prompt"
    CURSOR=${#BUFFER}
    zle redisplay
}

function smart-suggestion() {
    echo "Smart Suggestion is now active. Press $SMART_SUGGESTION_KEY to get suggestions."
    echo "Press $SMART_SUGGESTION_RECOVER_KEY to recover the last prompt."
    echo ""
    echo "Configurations:"
    echo "    - SMART_SUGGESTION_KEY: Key to press to get suggestions (default: ^o, value: $SMART_SUGGESTION_KEY)."
    echo "    - SMART_SUGGESTION_RECOVER_KEY: Key to press to recover last prompt (default: ^[^o, value: $SMART_SUGGESTION_RECOVER_KEY)."
    echo "    - SMART_SUGGESTION_SEND_CONTEXT: If \`true\`, include lightweight shell context for llms (default: true, value: $SMART_SUGGESTION_SEND_CONTEXT)."
    echo "    - SMART_SUGGESTION_LLMS_MODEL: Optional llms model override (default: llms config default, value: ${SMART_SUGGESTION_LLMS_MODEL:-"(using llms default)"})."
    echo "    - SMART_SUGGESTION_DEBUG: Enable debug logging (default: false, value: $SMART_SUGGESTION_DEBUG)."
    echo "Requirements:"
    echo "    - python3 and llms must be installed and available in PATH."
}

zle -N _do_smart_suggestion
zle -N _recover_last_prompt
bindkey "$SMART_SUGGESTION_KEY" _do_smart_suggestion
bindkey "$SMART_SUGGESTION_RECOVER_KEY" _recover_last_prompt
