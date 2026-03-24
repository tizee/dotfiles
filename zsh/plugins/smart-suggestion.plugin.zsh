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

if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
    touch /tmp/smart-suggestion.log
fi

if ! command -v llms >/dev/null 2>&1; then
    echo "llms command not found in PATH."
    return 1
fi

function _smart_suggestion_debug_log() {
    if [[ "$SMART_SUGGESTION_DEBUG" == 'true' ]]; then
        print -r -- "[$(date '+%F %T')] $*" >> /tmp/smart-suggestion.log
    fi
}

function _smart_suggestion_write_error() {
    local message="$1"

    print -r -- "$message" > /tmp/.smart_suggestion_error
    command rm -f /tmp/smart_suggestion
    return 1
}

function _smart_suggestion_xml_escape() {
    local value="$1"

    value=${value//&/&amp;}
    value=${value//</&lt;}
    value=${value//>/&gt;}
    print -r -- "$value"
}

function _smart_suggestion_collect_context() {
    local context=""
    local aliases_text=""
    local history_text=""
    local current_user="${USER:-$(whoami 2>/dev/null)}"
    local escaped_pwd=$(_smart_suggestion_xml_escape "$PWD")
    local escaped_shell=$(_smart_suggestion_xml_escape "zsh")
    local escaped_user=$(_smart_suggestion_xml_escape "$current_user")
    local escaped_aliases=""
    local escaped_history=""
    local -a cwd_dirs cwd_files
    local dir_name
    local file_name

    cwd_dirs=( *(N-/) )
    cwd_files=( *(N-.) )
    cwd_dirs=("${cwd_dirs[@]:1:40}")
    cwd_files=("${cwd_files[@]:1:80}")

    context+="<environment-context>"$'\n'
    context+="  <current-directory>$escaped_pwd</current-directory>"$'\n'
    context+="  <shell>$escaped_shell</shell>"$'\n'
    context+="  <user>$escaped_user</user>"$'\n'

    context+="  <cwd-directories>"$'\n'
    for dir_name in "${cwd_dirs[@]}"; do
        context+="    <dir>$(_smart_suggestion_xml_escape "$dir_name")</dir>"$'\n'
    done
    context+="  </cwd-directories>"$'\n'

    context+="  <cwd-files>"$'\n'
    for file_name in "${cwd_files[@]}"; do
        context+="    <file>$(_smart_suggestion_xml_escape "$file_name")</file>"$'\n'
    done
    context+="  </cwd-files>"$'\n'

    aliases_text=$(alias 2>/dev/null)
    if [[ -n "$aliases_text" ]]; then
        escaped_aliases=$(_smart_suggestion_xml_escape "$aliases_text")
        context+="  <aliases>$escaped_aliases</aliases>"$'\n'
    fi

    history_text=$(fc -ln -20 -1 2>/dev/null)
    if [[ -n "$history_text" ]]; then
        escaped_history=$(_smart_suggestion_xml_escape "$history_text")
        context+="  <recent-history>$escaped_history</recent-history>"$'\n'
    fi

    context+="</environment-context>"
    print -r -- "$context"
}

function _smart_suggestion_build_system_prompt() {
    local context="$1"
    local prompt="You are Smart Suggestion, an AI shell assistant for zsh.
Return exactly one line and nothing else.
The first character of your response must be = or +.
You will receive environment details inside XML tags.
Treat <current-directory> as the default scope for filesystem operations.
Unless the user explicitly asks otherwise, prefer commands that operate inside <current-directory>.
Prefer relative paths and . over ~ or absolute paths when the task can be solved from <current-directory>.
Do not broaden searches to the home directory, root directory, or the whole filesystem unless the user explicitly asks for that scope.
Use <cwd-directories> and <cwd-files> to infer likely targets in the current directory before searching more broadly.
If the current input is empty, predict the most likely next shell command and prefix it with =.
If the current input is a natural language request, convert it to a shell command and prefix it with =.
If the current input is a partial shell command, return only the missing suffix to append at the cursor and prefix it with +.
For + responses, never repeat the existing input.
Valid examples:
=ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
+ -t 10 -c copy output_10s.mp4
Invalid examples:
Here is the command: ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
\`\`\`bash
ffmpeg -i input.mp4 -t 10 -c copy output_10s.mp4
\`\`\`
Do not use markdown, code fences, comments, or explanations.
The command must be valid for zsh on Unix."

    if [[ -n "$context" ]]; then
        prompt+=$'\n\n'"$context"
    fi

    print -r -- "$prompt"
}

function _smart_suggestion_normalize_result() {
    emulate -L zsh
    setopt extendedglob

    local raw="$1"
    local cleaned="${raw//$'\r'/}"
    local line=""
    local -a lines

    lines=("${(@f)cleaned}")
    for line in "${lines[@]}"; do
        line="${line##[[:space:]]#}"
        line="${line%%[[:space:]]#}"
        [[ -z "$line" ]] && continue
        if [[ "$line" == [=+]* ]]; then
            print -r -- "$line"
            return 0
        fi
    done

    cleaned="${cleaned##[[:space:]]#}"
    cleaned="${cleaned%%[[:space:]]#}"
    print -r -- "$cleaned"
}

function _fetch_suggestions() {
    local input="$1"
    local context=""
    local system_prompt=""
    local llms_args=(prompt)
    local result=""
    local exit_code=0

    if [[ "$SMART_SUGGESTION_SEND_CONTEXT" == 'true' ]]; then
        context=$(_smart_suggestion_collect_context)
    fi

    system_prompt=$(_smart_suggestion_build_system_prompt "$context")

    if [[ -n "$SMART_SUGGESTION_LLMS_MODEL" ]]; then
        llms_args+=(-m "$SMART_SUGGESTION_LLMS_MODEL")
    fi

    llms_args+=(-s "$system_prompt")
    result=$(llms "${llms_args[@]}" "$input" 2>/tmp/.smart_suggestion_error)
    exit_code=$?

    if (( exit_code != 0 )); then
        if [[ ! -s /tmp/.smart_suggestion_error ]]; then
            print -r -- "llms prompt failed." > /tmp/.smart_suggestion_error
        fi
        return 1
    fi

    result=$(_smart_suggestion_normalize_result "$result")

    if [[ -z "$result" ]]; then
        _smart_suggestion_write_error "llms returned an empty suggestion."
        return 1
    fi

    if [[ "$result" == *$'\n'* ]]; then
        _smart_suggestion_write_error "llms returned multiple lines. Expected exactly one line prefixed with = or +."
        return 1
    fi

    case "${result:0:1}" in
        '='|'+' )
            print -rn -- "$result" > /tmp/smart_suggestion
            ;;
        * )
            _smart_suggestion_write_error "llms returned an invalid suggestion prefix. Expected = or +."
            return 1
            ;;
    esac

    return 0
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
    command rm -f /tmp/smart_suggestion
    command rm -f /tmp/.smart_suggestion_canceled
    command rm -f /tmp/.smart_suggestion_error

    local input=$(echo "${BUFFER:0:$CURSOR}" | tr '\n' ';')

    print -r -- "$input" > /tmp/smart_suggestion_last_prompt

    _zsh_autosuggest_clear

    read < <(_fetch_suggestions "$input" & echo $!)
    local pid=$REPLY

    _show_loading_animation $pid
    local response_code=$?

    _smart_suggestion_debug_log "Fetched message input=${(qqq)input} response_code=$response_code"

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
    local first_char=${message:0:1}
    local suggestion=${message:1:${#message}}

    if [[ "$first_char" == '=' ]]; then
        BUFFER=""
        CURSOR=0
        zle -U "$suggestion"
    elif [[ "$first_char" == '+' ]]; then
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
    echo "    - llms must be installed and available in PATH."
}

zle -N _do_smart_suggestion
zle -N _recover_last_prompt
bindkey "$SMART_SUGGESTION_KEY" _do_smart_suggestion
bindkey "$SMART_SUGGESTION_RECOVER_KEY" _recover_last_prompt
