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

function vi-accept-line() {
  VI_KEYMAP=main
  zle accept-line
}

zle -N vi-accept-line

bindkey -v

# use custom accept-line widget to update $VI_KEYMAP
bindkey -M vicmd '^J' vi-accept-line
bindkey -M vicmd '^M' vi-accept-line

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
