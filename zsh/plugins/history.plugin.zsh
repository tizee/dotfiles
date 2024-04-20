#!/usr/bin/env zsh

## History wrapper
function slim_zsh_history {
  local clear list
  zparseopts -E c=clear l=list

  if [[ -n "$clear" ]]; then
    # if -c provided, clobber the history file
    echo -n >| "$HISTFILE"
    echo >&2 History file deleted. Reload the session to see its effects.
  elif [[ -n "$list" ]]; then
    # if -l provided, run as if calling `fc' directly
    builtin fc "$@"
  else
    # unless a number is provided, show all history events (starting from 1)
    [[ ${@[-1]-} = *[0-9]* ]] && builtin fc -l "$@" || builtin fc -l "$@" 1
  fi
}

# Timestamp format
case ${HIST_STAMPS-} in
  "mm/dd/yyyy") alias history='slim_zsh_history -f' ;;
  "dd.mm.yyyy") alias history='slim_zsh_history -E' ;;
  "yyyy-mm-dd") alias history='slim_zsh_history -i' ;;
  "") alias history='slim_zsh_history' ;;
  *) alias history="slim_zsh_history -t '$HIST_STAMPS'" ;;
esac

