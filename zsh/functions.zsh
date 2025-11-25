#!/usr/bin/env zsh
# Utility functions for zsh

# Interactive git hooks configuration manager
# Provides a comprehensive interface to view and manage all git hook options
function githooks {
  local action="${1:-menu}"

  case "$action" in
    status|show)
      _githooks_show_status
      ;;
    menu|interactive)
      _githooks_interactive_menu
      ;;
    help|--help|-h)
      _githooks_show_help
      ;;
    *)
      echo "Unknown action: $action"
      _githooks_show_help
      return 1
      ;;
  esac
}

# Show current status of all git hook options
function _githooks_show_status {
  echo "\n╔═══════════════════════════════════════════════════════════╗"
  echo "║         Git Hooks Configuration Status                   ║"
  echo "╚═══════════════════════════════════════════════════════════╝"

  echo "\n📝 Prepare-Commit-Msg Hook:"
  echo "  └─ LLM Generation:              $(_get_status_badge "$LLM_GITHOOK_SKIP" "inverted")"
  echo "  └─ Force Regeneration:          $(_get_status_badge "$LLM_GITHOOK_FORCE")"
  echo "  └─ No Bypass Amending:          $(_get_status_badge "$LLM_GITHOOK_NO_BYPASS_AMENDING")"
  echo "  └─ Allow Non-Interactive:       $(_get_status_badge "$LLM_GITHOOK_ALLOW_NONINTERACTIVE")"
  echo "  └─ Spinner Style:               \033[36m${LLM_GITHOOK_SPINNER_STYLE:-classic}\033[0m"

  echo "\n🔐 Pre-Commit Hook:"
  echo "  └─ Secret Scan:                 $(_get_status_badge "$SKIP_SCAN_GITHOOK" "inverted")"

  echo "\n⚙️  Advanced Settings:"
  if [[ -n "$LLM_PROGRAM" ]]; then
    echo "  └─ Custom LLM Program:          \033[36m${LLM_PROGRAM}\033[0m"
  else
    echo "  └─ Custom LLM Program:          \033[90m(not set)\033[0m"
  fi

  if [[ -n "$LLM_PREPARE_COMMIT_MSG_PROMPT" ]]; then
    echo "  └─ Custom Prompt Template:      \033[36m${LLM_PREPARE_COMMIT_MSG_PROMPT}\033[0m"
  else
    echo "  └─ Custom Prompt Template:      \033[90m(not set)\033[0m"
  fi
  echo ""
}

# Helper function to display status badge
function _get_status_badge {
  local var_value="$1"
  local inverted="${2:-}"

  if [[ "$inverted" == "inverted" ]]; then
    # For SKIP/DISABLE vars, empty means enabled
    if [[ -z "$var_value" ]]; then
      echo "\033[32m✓ Enabled\033[0m"
    else
      echo "\033[31m✗ Disabled\033[0m"
    fi
  else
    # Normal vars, set means enabled
    if [[ -n "$var_value" ]]; then
      echo "\033[32m✓ Enabled\033[0m"
    else
      echo "\033[90m○ Disabled\033[0m"
    fi
  fi
}

# Interactive menu for managing git hooks
function _githooks_interactive_menu {
  if ! command -v fzf &> /dev/null; then
    echo "\033[33mWarning: fzf not found. Using simple menu instead.\033[0m"
    _githooks_simple_menu
    return
  fi

  while true; do
    local current_llm_status=$(_get_toggle_label "$LLM_GITHOOK_SKIP" "inverted")
    local current_force_status=$(_get_toggle_label "$LLM_GITHOOK_FORCE")
    local current_amend_status=$(_get_toggle_label "$LLM_GITHOOK_NO_BYPASS_AMENDING")
    local current_noninteractive_status=$(_get_toggle_label "$LLM_GITHOOK_ALLOW_NONINTERACTIVE")
    local current_scan_status=$(_get_toggle_label "$SKIP_SCAN_GITHOOK" "inverted")
    local current_spinner="${LLM_GITHOOK_SPINNER_STYLE:-classic}"

    local choice=$(cat <<EOF | fzf --ansi --height=20 --header="Git Hooks Configuration - Press ESC to exit" --prompt="Select option > " --border
📝 Toggle LLM Commit Generation         [${current_llm_status}]
⚡ Toggle Force LLM Regeneration         [${current_force_status}]
🔄 Toggle No Bypass Amending             [${current_amend_status}]
🤖 Toggle Allow Non-Interactive          [${current_noninteractive_status}]
🎨 Change Spinner Style                  [${current_spinner}]
🔐 Toggle Secret Scan                    [${current_scan_status}]
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Show Current Status
❓ Show Help
🚪 Exit
EOF
)

    case "$choice" in
      *"Toggle LLM Commit Generation"*)
        toggleLLMCommit
        ;;
      *"Toggle Force LLM Regeneration"*)
        forceLLMCommit
        ;;
      *"Toggle No Bypass Amending"*)
        toggleNoBypassAmending
        ;;
      *"Toggle Allow Non-Interactive"*)
        toggleAllowNonInteractive
        ;;
      *"Change Spinner Style"*)
        _githooks_spinner_menu
        ;;
      *"Toggle Secret Scan"*)
        toggleSecretScan
        ;;
      *"Show Current Status"*)
        _githooks_show_status
        read -k1 -s "?Press any key to continue..."
        ;;
      *"Show Help"*)
        _githooks_show_help
        read -k1 -s "?Press any key to continue..."
        ;;
      *"Exit"*|"")
        echo "\nExiting git hooks configuration."
        break
        ;;
      *)
        break
        ;;
    esac
  done
}

# Simple menu fallback when fzf is not available
function _githooks_simple_menu {
  while true; do
    echo "\n╔═══════════════════════════════════════════════════════════╗"
    echo "║         Git Hooks Configuration Menu                     ║"
    echo "╚═══════════════════════════════════════════════════════════╝"
    echo ""
    echo "  1) Toggle LLM Commit Generation"
    echo "  2) Toggle Force LLM Regeneration"
    echo "  3) Toggle No Bypass Amending"
    echo "  4) Toggle Allow Non-Interactive"
    echo "  5) Change Spinner Style"
    echo "  6) Toggle Secret Scan"
    echo "  ─────────────────────────────"
    echo "  s) Show Current Status"
    echo "  h) Show Help"
    echo "  q) Exit"
    echo ""

    read "choice?Select option: "

    case "$choice" in
      1) toggleLLMCommit ;;
      2) forceLLMCommit ;;
      3) toggleNoBypassAmending ;;
      4) toggleAllowNonInteractive ;;
      5) _githooks_spinner_menu ;;
      6) toggleSecretScan ;;
      s|S) _githooks_show_status ;;
      h|H) _githooks_show_help ;;
      q|Q) echo "Exiting git hooks configuration."; break ;;
      *) echo "\033[31mInvalid option. Please try again.\033[0m" ;;
    esac
  done
}

# Interactive spinner style selection
function _githooks_spinner_menu {
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

  if command -v fzf &> /dev/null; then
    local selected=$(printf '%s\n' "${SPINNER_STYLES[@]}" | \
      fzf --ansi --height=15 \
          --header="Current: ${current_style} - Select spinner style (ESC to cancel)" \
          --prompt="Style > " \
          --border \
          --preview-window=hidden)

    if [[ -n "$selected" ]]; then
      unset LLM_GITHOOK_SPINNER_STYLE
      export LLM_GITHOOK_SPINNER_STYLE="$selected"
      echo "Spinner style set to: \033[32m${selected}\033[0m"
    fi
  else
    # Fallback without fzf
    echo "\nAvailable spinner styles:"
    for i in {1..${#SPINNER_STYLES[@]}}; do
      local style="${SPINNER_STYLES[$i]}"
      if [[ "$style" == "$current_style" ]]; then
        echo "  $i) \033[32m${style}\033[0m (current)"
      else
        echo "  $i) ${style}"
      fi
    done

    read "choice?Select style (1-${#SPINNER_STYLES[@]}): "
    if [[ "$choice" =~ ^[0-9]+$ ]] && [[ "$choice" -ge 1 ]] && [[ "$choice" -le "${#SPINNER_STYLES[@]}" ]]; then
      local selected_style="${SPINNER_STYLES[$choice]}"
      unset LLM_GITHOOK_SPINNER_STYLE
      export LLM_GITHOOK_SPINNER_STYLE="$selected_style"
      echo "Spinner style set to: \033[32m${selected_style}\033[0m"
    else
      echo "\033[31mInvalid selection.\033[0m"
    fi
  fi
}

# Get toggle label for menu display
function _get_toggle_label {
  local var_value="$1"
  local inverted="${2:-}"

  if [[ "$inverted" == "inverted" ]]; then
    if [[ -z "$var_value" ]]; then
      echo "\033[32mON\033[0m"
    else
      echo "\033[31mOFF\033[0m"
    fi
  else
    if [[ -n "$var_value" ]]; then
      echo "\033[32mON\033[0m"
    else
      echo "\033[90mOFF\033[0m"
    fi
  fi
}

# Show help information
function _githooks_show_help {
  cat <<'EOF'

╔═══════════════════════════════════════════════════════════╗
║              Git Hooks Configuration Help                ║
╚═══════════════════════════════════════════════════════════╝

USAGE:
  githooks [action]

ACTIONS:
  (no args)      Launch interactive menu (default)
  status, show   Display current configuration status
  menu           Launch interactive menu
  help           Show this help message

INDIVIDUAL FUNCTIONS:
  toggleLLMCommit              Toggle LLM commit message generation
  forceLLMCommit               Toggle forced regeneration mode
  toggleNoBypassAmending       Toggle no-bypass-amending mode
  toggleAllowNonInteractive    Toggle non-interactive mode
  toggleSecretScan             Toggle credential scanning
  setSpinnerStyle [style]      Set or cycle spinner style

ENVIRONMENT VARIABLES:
  📝 Prepare-Commit-Msg Hook:
    LLM_GITHOOK_SKIP              Skip LLM generation entirely
    LLM_GITHOOK_FORCE             Force regeneration (bypass cache)
    LLM_GITHOOK_NO_BYPASS_AMENDING  Don't bypass during amend
    LLM_GITHOOK_ALLOW_NONINTERACTIVE  Allow in CI/agents
    LLM_GITHOOK_SPINNER_STYLE     Spinner animation style
    LLM_PROGRAM                   Custom llm executable path
    LLM_PREPARE_COMMIT_MSG_PROMPT Custom prompt template

  🔐 Pre-Commit Hook:
    SKIP_SCAN_GITHOOK             Skip credential scanning
    DISABLE_SECRET_SCAN           Alternative skip flag

EXAMPLES:
  # Launch interactive menu
  githooks

  # Show current status
  githooks status

  # Toggle LLM generation quickly
  toggleLLMCommit

  # Set spinner style
  setSpinnerStyle dots

For more information, see: git/git-hooks/README.md

EOF
}

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

# Toggle no-bypass-amending mode (prevent automatic bypass during commit amend)
function toggleNoBypassAmending {
  if [ -z "$LLM_GITHOOK_NO_BYPASS_AMENDING" ]; then
    export LLM_GITHOOK_NO_BYPASS_AMENDING=1
    echo "Enabled no-bypass-amending (LLM will run even during commit --amend)"
  else
    unset LLM_GITHOOK_NO_BYPASS_AMENDING
    echo "Disabled no-bypass-amending (LLM will skip during commit --amend)"
  fi
}

# Toggle allow-non-interactive mode (allow hook to run in CI/agents)
function toggleAllowNonInteractive {
  if [ -z "$LLM_GITHOOK_ALLOW_NONINTERACTIVE" ]; then
    export LLM_GITHOOK_ALLOW_NONINTERACTIVE=1
    echo "Enabled non-interactive mode (hook will run in CI/automation)"
  else
    unset LLM_GITHOOK_ALLOW_NONINTERACTIVE
    echo "Disabled non-interactive mode (hook will skip in CI/automation)"
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
