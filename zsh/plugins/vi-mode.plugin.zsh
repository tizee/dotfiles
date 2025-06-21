#!/usr/bin/env zsh
# Updates editor information when the keymap changes.
function zle-keymap-select() {
  # update keymap variable for the prompt
  VI_KEYMAP=$KEYMAP
  PROMPT_ZLE_OLDMODE=$PROMPT_ZLE_MODE
   case $KEYMAP in
    (main)
      case $ZLE_STATE in
        (*insert*) PROMPT_ZLE_MODE="%{%F{159}%}[insert]%f";;
        (*) PROMPT_ZLE_MODE="%{%F{210}%}[overwrite]"
      esac;;
    (*) PROMPT_ZLE_MODE="%{%F{212}%}[$KEYMAP]%f"
  esac
  [[ $PROMPT_ZLE_MODE = $PROMPT_ZLE_OLDMODE ]] || zle reset-prompt
  zle -R
}

zle -N zle-keymap-select

# Enhanced accept-line with comprehensive self-redirection protection
function safe-accept-line() {
  local cmdline=$BUFFER

  # Check for output redirection
  if [[ $cmdline =~ '>[[:space:]]*([^[:space:]|&;]+)' ]]; then
    local target_file=${match[1]}
    local words=("${(z)cmdline%%>*}")  # Parse command before redirection
    local cmd_name=${words[1]}

    # Skip built-ins and functions
    if (( ${+builtins[$cmd_name]} )) || (( ${+functions[$cmd_name]} )); then
      zle accept-line
      return
    fi

    # Check all possible input files
    local input_files=()

    case $cmd_name in
      # Script interpreters - check script file arguments
      python*|node|ruby|perl|php|bash|zsh|sh)
        # Find script files in arguments (skip flags)
        local i
        for ((i=2; i<=${#words}; i++)); do
          local arg=${words[i]}
          # Skip flags and options
          [[ $arg == -* ]] && continue
          # If it's a readable file, add to input_files
          [[ -r $arg ]] && input_files+=$arg
        done
        ;;
      # Direct executables
      *)
        # Check if the command itself is a file
        if [[ -x $cmd_name ]]; then
          input_files+=$cmd_name
        elif command -v "$cmd_name" >/dev/null 2>&1; then
          local cmd_path=$(command -v "$cmd_name")
          [[ -f $cmd_path ]] && input_files+=$cmd_path
        fi
        ;;
    esac

    # Check each input file against target
    local input_file
    for input_file in $input_files; do
      local input_real target_real
      input_real=$(readlink -f "$input_file" 2>/dev/null) || input_real=$input_file
      target_real=$(readlink -f "$target_file" 2>/dev/null) || target_real=$target_file

      if [[ $input_real == $target_real ]]; then
        zle -M "ERROR: Self-redirection blocked ($input_file${red} -> $target_file)"
        return
      fi
    done
  fi

  # Normal execution
  VI_KEYMAP=main
  zle accept-line
}

zle -N safe-accept-line

bindkey -v

bindkey -M vicmd '^J' safe-accept-line
bindkey -M vicmd '^M' safe-accept-line
bindkey -M viins '^J' safe-accept-line
bindkey -M viins '^M' safe-accept-line
bindkey -M main '^J' safe-accept-line
bindkey -M main '^M' safe-accept-line

# use custom accept-line widget to update $VI_KEYMAP
#bindkey -M vicmd '^J' vi-accept-line
#bindkey -M vicmd '^M' vi-accept-line

# allow v to edit the command line (standard behaviour)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd 'v' edit-command-line

# allow ctrl-p, ctrl-n for navigate history (standard behaviour)
bindkey '^P' up-history
bindkey '^N' down-history

# allow ctrl-h, ctrl-w, backspace(^?) for char and word deletion (standard behaviour)
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word

# ctrl+f delete char under the cursor
bindkey '^f' delete-char
# ctrl+b move backward one char
bindkey '^b' backward-char


# allow ctrl-r and ctrl-s to search the history
bindkey '^r' history-incremental-search-backward
bindkey '^s' history-incremental-search-forward

bindkey -M vicmd 'gg' beginning-of-buffer-or-history
bindkey -M vicmd 'G' end-of-buffer-or-history

# allow ctrl-a and ctrl-e to move to beginning/end of line
bindkey -M vicmd '0' beginning-of-line
bindkey -M vicmd '$' end-of-line

# if mode indicator wasn't setup by theme, define default
if [[ "$MODE_INDICATOR" == "" ]]; then
  MODE_INDICATOR="%{%F{210}%}<<<%{$reset_color%}"
fi

function vi_mode_prompt_info() {
  echo "${${VI_KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
}

# define right prompt, if it wasn't defined by a theme
if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
  RPS1='$(vi_mode_prompt_info)'
fi
