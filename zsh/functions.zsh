#!/usr/bin/env zsh
# Utility functions for zsh

# prepare-commit-msg hook
# Toggle LLM-based commit message generation
# Uses modern LLM_GITHOOK_SKIP naming (backward compatible with SKIP_LLM_GITHOOK)
function toggleLLMCommit {
  if [ -z "$LLM_GITHOOK_SKIP" ]; then
    export LLM_GITHOOK_SKIP=1
    export SKIP_LLM_GITHOOK=1  # Backward compatibility
    echo "Disabled LLM git commit generation"
  else
    unset LLM_GITHOOK_SKIP
    unset SKIP_LLM_GITHOOK  # Backward compatibility
    echo "Enabled LLM git commit generation"
  fi
}

# Force LLM commit message regeneration (bypasses cache and non-interactive checks)
function forceLLMCommit {
  if [ -z "$LLM_GITHOOK_FORCE" ]; then
    export LLM_GITHOOK_FORCE=1
    echo "Enabled forced LLM commit message regeneration"
  else
    unset LLM_GITHOOK_FORCE
    echo "Disabled forced LLM commit message regeneration"
  fi
}

# pre-commit hook
# Function to enable or disable the secret scan pre-commit hook
function toggleSecretScan {
  if [ -z "$SKIP_SCAN_GITHOOK" ]; then
    export SKIP_SCAN_GITHOOK=1
    echo "Disable credential scan"
  else
    unset SKIP_SCAN_GITHOOK
    echo "Enable credential scan"
  fi
}

# Set or cycle through LLM git hook spinner styles
# Usage:
#   setSpinnerStyle              # Show current style and list available styles
#   setSpinnerStyle <style>      # Set specific style (e.g., dots, arrows, circle)
#   setSpinnerStyle next         # Cycle to next style
function setSpinnerStyle {
  local -a SPINNER_STYLES=(
    "classic"
    "dots"
    "arrows"
    "blocks"
    "pulse"
    "bouncing"
    "circle"
    "square"
    "triangle"
    "diamond"
  )

  local current_style="${LLM_GITHOOK_SPINNER_STYLE:-classic}"

  # No argument: show current and list available
  if [[ -z "$1" ]]; then
    echo "Current spinner style: \033[32m${current_style}\033[0m"
    echo "\nAvailable styles:"
    for style in "${SPINNER_STYLES[@]}"; do
      if [[ "$style" == "$current_style" ]]; then
        echo "  * \033[32m${style}\033[0m (current)"
      else
        echo "    ${style}"
      fi
    done
    echo "\nUsage: setSpinnerStyle <style|next>"
    return 0
  fi

  # Cycle to next style
  if [[ "$1" == "next" ]]; then
    local current_index=0
    for i in {1..${#SPINNER_STYLES[@]}}; do
      if [[ "${SPINNER_STYLES[$i]}" == "$current_style" ]]; then
        current_index=$i
        break
      fi
    done

    local next_index=$(( (current_index + 1) % ${#SPINNER_STYLES[@]} ))
    if [[ $next_index -eq 0 ]]; then
      next_index=${#SPINNER_STYLES[@]}
    fi

    local new_style="${SPINNER_STYLES[$next_index]}"
    unset LLM_GITHOOK_SPINNER_STYLE
    export LLM_GITHOOK_SPINNER_STYLE="$new_style"
    echo "Spinner style changed: ${current_style} → \033[32m${new_style}\033[0m"
    return 0
  fi

  # Set specific style
  local requested_style="$1"
  if (( ${SPINNER_STYLES[(I)$requested_style]} )); then
    unset LLM_GITHOOK_SPINNER_STYLE
    export LLM_GITHOOK_SPINNER_STYLE="$requested_style"
    echo "Spinner style set to: \033[32m${requested_style}\033[0m"
  else
    echo "\033[31mError: Unknown spinner style '$requested_style'\033[0m"
    echo "Available styles: ${(j:, :)SPINNER_STYLES}"
    return 1
  fi
}
