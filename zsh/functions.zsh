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

# Render a centered title inside a Unicode box
# Usage: _githooks_box_header "Title Text" [width]
# Default width: 59 (inner content width)
function _githooks_box_header {
  local title="$1"
  local width="${2:-59}"
  local title_len=${#title}

  # Calculate padding
  local total_padding=$((width - title_len))
  local left_pad=$((total_padding / 2))
  local right_pad=$((total_padding - left_pad))

  # Build padding strings
  local left_spaces=""
  local right_spaces=""
  local border=""

  repeat $left_pad; do left_spaces+=" "; done
  repeat $right_pad; do right_spaces+=" "; done
  repeat $width; do border+="═"; done

  # Render the box
  echo "╔${border}╗"
  echo "║${left_spaces}${title}${right_spaces}║"
  echo "╚${border}╝"
}

# Show current status of all git hook options
function _githooks_show_status {
  echo ""
  _githooks_box_header "Git Hooks Configuration Status"

  echo "\n📝 Prepare-Commit-Msg Hook:"
  echo "  └─ LLM Generation:              $(_get_status_badge "$LLM_GITHOOK_SKIP" "inverted")"
  echo "  └─ Force Regeneration:          $(_get_status_badge "$LLM_GITHOOK_FORCE")"
  echo "  └─ No Bypass Amending:          $(_get_status_badge "$LLM_GITHOOK_NO_BYPASS_AMENDING")"
  echo "  └─ Allow Non-Interactive:       $(_get_status_badge "$LLM_GITHOOK_ALLOW_NONINTERACTIVE")"
  echo "  └─ Spinner Style:               \033[36m${LLM_GITHOOK_SPINNER_STYLE:-classic}\033[0m"
  echo "  └─ Animation FPS:               $(_githooks_value_or_default LLM_GITHOOK_FPS 30)"
  echo "  └─ Status Text Override:        $(_githooks_value_or_default_text LLM_GITHOOK_STATUS_TEXT '(default)')"
  echo "  └─ Shimmer Sweep Seconds:       $(_githooks_value_or_default LLM_GITHOOK_SHIMMER_SWEEP_SECONDS 2.0)"
  echo "  └─ Shimmer Padding:             $(_githooks_value_or_default LLM_GITHOOK_SHIMMER_PADDING 10)"
  echo "  └─ Shimmer Band Half-Width:     $(_githooks_value_or_default LLM_GITHOOK_SHIMMER_BAND_WIDTH 5.0)"
  echo "  └─ Color Mode:                  $(_githooks_value_or_default LLM_GITHOOK_COLOR_MODE 256)"
  echo "  └─ Shimmer Base Color:          $(_githooks_color_value LLM_GITHOOK_SHIMMER_BASE_COLOR 255 '255,255,255')"
  echo "  └─ Shimmer Highlight Color:     $(_githooks_color_value LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR 240 '240,240,240')"
  echo "  └─ Spinner Color:               $(_githooks_spinner_color_value)"

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

  if [[ -n "$PYTHONIOENCODING" ]]; then
    echo "  └─ PYTHONIOENCODING:            \033[36m${PYTHONIOENCODING}\033[0m"
  else
    echo "  └─ PYTHONIOENCODING:            \033[90m(not set)\033[0m"
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

# Render a variable value with a default fallback, while indicating whether it is set.
# Usage: _githooks_value_or_default VAR_NAME DEFAULT
function _githooks_value_or_default {
  local var_name="$1"
  local default_value="$2"
  local value="${(P)var_name}"

  if [[ -n "$value" ]]; then
    echo "\033[36m${value}\033[0m"
  else
    echo "\033[90m${default_value} (default)\033[0m"
  fi
}

# Like _githooks_value_or_default, but quotes text values for readability.
function _githooks_value_or_default_text {
  local var_name="$1"
  local default_value="$2"
  local value="${(P)var_name}"

  if [[ -n "$value" ]]; then
    echo "\033[36m\"${value}\"\033[0m"
  else
    echo "\033[90m${default_value}\033[0m"
  fi
}

function _githooks_color_mode {
  local mode="${LLM_GITHOOK_COLOR_MODE:-256}"
  if [[ "$mode" == "truecolor" ]]; then
    echo "truecolor"
  else
    echo "256"
  fi
}

function _githooks_is_uint8 {
  local value="$1"
  [[ "$value" =~ '^[0-9]+$' ]] || return 1
  (( value >= 0 && value <= 255 ))
}

function _githooks_parse_rgb {
  local raw="${1//[[:space:]]/}"
  if [[ ! "$raw" =~ '^([0-9]{1,3}),([0-9]{1,3}),([0-9]{1,3})$' ]]; then
    return 1
  fi

  local r="${match[1]}"
  local g="${match[2]}"
  local b="${match[3]}"

  _githooks_is_uint8 "$r" || return 1
  _githooks_is_uint8 "$g" || return 1
  _githooks_is_uint8 "$b" || return 1

  echo "${r},${g},${b}"
}

function _githooks_color_escape {
  local color_value="$1"
  local mode="$(_githooks_color_mode)"

  if [[ "$mode" == "truecolor" ]]; then
    local parsed="$(_githooks_parse_rgb "$color_value")" || return 1
    local r="${parsed%%,*}"
    local rest="${parsed#*,}"
    local g="${rest%%,*}"
    local b="${rest#*,}"
    echo "\033[38;2;${r};${g};${b}m"
  else
    _githooks_is_uint8 "$color_value" || return 1
    echo "\033[38;5;${color_value}m"
  fi
}

function _githooks_color_value {
  local var_name="$1"
  local default_256="$2"
  local default_truecolor="$3"
  local value="${(P)var_name}"
  local mode="$(_githooks_color_mode)"
  local default_value="$default_256"
  if [[ "$mode" == "truecolor" ]]; then
    default_value="$default_truecolor"
  fi
  local effective="${value:-$default_value}"

  local esc="$(_githooks_color_escape "$effective")"
  if [[ -n "$esc" ]]; then
    if [[ -n "$value" ]]; then
      echo "\033[36m${value}\033[0m ${esc}██\033[0m"
    else
      echo "\033[90m${default_value} (default)\033[0m ${esc}██\033[0m"
    fi
  else
    if [[ -n "$value" ]]; then
      echo "\033[31m${value} (invalid for ${mode})\033[0m"
    else
      echo "\033[31m${default_value} (invalid for ${mode})\033[0m"
    fi
  fi
}

function _githooks_spinner_color_value {
  local mode="$(_githooks_color_mode)"
  local base_default="255"
  if [[ "$mode" == "truecolor" ]]; then
    base_default="255,255,255"
  fi
  local base="${LLM_GITHOOK_SHIMMER_BASE_COLOR:-$base_default}"
  local spinner="${LLM_GITHOOK_SPINNER_COLOR:-}"

  if [[ -z "$spinner" ]]; then
    local esc="$(_githooks_color_escape "$base")"
    if [[ -n "$esc" ]]; then
      echo "\033[90m(same as base)\033[0m ${esc}██\033[0m"
    else
      echo "\033[90m(same as base)\033[0m"
    fi
    return 0
  fi

  local esc="$(_githooks_color_escape "$spinner")"
  if [[ -n "$esc" ]]; then
    echo "\033[36m${spinner}\033[0m ${esc}██\033[0m"
  else
    echo "\033[31m${spinner} (invalid for ${mode})\033[0m"
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
    local current_color_mode="$(_githooks_color_mode)"

    local choice=$(cat <<EOF | fzf --ansi --height=20 --header="Git Hooks Configuration - Press ESC to exit" --prompt="Select option > " --border --preview-window=hidden
📝 Toggle LLM Commit Generation           [${current_llm_status}]
⚡ Toggle Force LLM Regeneration          [${current_force_status}]
🔄 Toggle No Bypass Amending              [${current_amend_status}]
🤖 Toggle Allow Non-Interactive           [${current_noninteractive_status}]
🎨 Change Spinner Style                   [${current_spinner}]
🌈 Configure Colors                       [${current_color_mode}]
🔐 Toggle Secret Scan                     [${current_scan_status}]
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
      *"Configure Colors"*)
        _githooks_color_menu
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
    echo ""
    _githooks_box_header "Git Hooks Configuration Menu"
    echo ""
    echo "  1) Toggle LLM Commit Generation"
    echo "  2) Toggle Force LLM Regeneration"
    echo "  3) Toggle No Bypass Amending"
    echo "  4) Toggle Allow Non-Interactive"
    echo "  5) Change Spinner Style"
    echo "  6) Configure Colors"
    echo "  7) Toggle Secret Scan"
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
      6) _githooks_color_menu ;;
      7) toggleSecretScan ;;
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

# Interactive color configuration (shimmer + spinner)
function _githooks_color_menu {
  local mode="$(_githooks_color_mode)"
  local current_base="${LLM_GITHOOK_SHIMMER_BASE_COLOR:-255}"
  local current_highlight="${LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR:-240}"
  local current_spinner="${LLM_GITHOOK_SPINNER_COLOR:-}"

  if command -v fzf &> /dev/null; then
    local header="Mode: ${mode} | Base: ${current_base} | Highlight: ${current_highlight} | Spinner: ${current_spinner:-same as base}"
    local choice=$(cat <<EOF | fzf --ansi --height=12 --header="$header" --prompt="Colors > " --border --preview-window=hidden
Set color mode (256/truecolor)
Set shimmer base color
Set shimmer highlight color
Set spinner color (or unset)
Reset color settings (unset all)
Back
EOF
)
    case "$choice" in
      "Set color mode (256/truecolor)") _githooks_set_color_mode ;;
      "Set shimmer base color") _githooks_set_color_var LLM_GITHOOK_SHIMMER_BASE_COLOR "Base" ;;
      "Set shimmer highlight color") _githooks_set_color_var LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR "Highlight" ;;
      "Set spinner color (or unset)") _githooks_set_color_var LLM_GITHOOK_SPINNER_COLOR "Spinner" "allow-unset" ;;
      "Reset color settings (unset all)")
        unset LLM_GITHOOK_COLOR_MODE
        unset LLM_GITHOOK_SHIMMER_BASE_COLOR
        unset LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR
        unset LLM_GITHOOK_SPINNER_COLOR
        echo "Reset color settings to defaults."
        ;;
      *) return 0 ;;
    esac
  else
    echo "\nCurrent color mode: \033[36m${mode}\033[0m"
    echo "  1) Set color mode (256/truecolor)"
    echo "  2) Set shimmer base color"
    echo "  3) Set shimmer highlight color"
    echo "  4) Set spinner color (or unset)"
    echo "  5) Reset color settings (unset all)"
    echo "  q) Back"
    echo ""
    read "choice?Select option: "
    case "$choice" in
      1) _githooks_set_color_mode ;;
      2) _githooks_set_color_var LLM_GITHOOK_SHIMMER_BASE_COLOR "Base" ;;
      3) _githooks_set_color_var LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR "Highlight" ;;
      4) _githooks_set_color_var LLM_GITHOOK_SPINNER_COLOR "Spinner" "allow-unset" ;;
      5)
        unset LLM_GITHOOK_COLOR_MODE
        unset LLM_GITHOOK_SHIMMER_BASE_COLOR
        unset LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR
        unset LLM_GITHOOK_SPINNER_COLOR
        echo "Reset color settings to defaults."
        ;;
      q|Q) return 0 ;;
      *) echo "\033[31mInvalid option.\033[0m" ;;
    esac
  fi
}

function _githooks_set_color_mode {
  local current="$(_githooks_color_mode)"
  local selected=""

  if command -v fzf &> /dev/null; then
    selected=$(printf '%s\n' "256" "truecolor" "unset (default: 256)" | fzf --ansi --height=8 --header="Current: ${current}" --prompt="Mode > " --border --preview-window=hidden)
  else
    echo "\nCurrent mode: ${current}"
    echo "  1) 256"
    echo "  2) truecolor"
    echo "  3) unset (default: 256)"
    read "choice?Select mode: "
    case "$choice" in
      1) selected="256" ;;
      2) selected="truecolor" ;;
      3) selected="unset (default: 256)" ;;
      *) echo "\033[31mInvalid selection.\033[0m"; return 1 ;;
    esac
  fi

  case "$selected" in
    256)
      export LLM_GITHOOK_COLOR_MODE=256
      echo "Color mode set to: \033[32m256\033[0m"
      ;;
    truecolor)
      export LLM_GITHOOK_COLOR_MODE=truecolor
      echo "Color mode set to: \033[32mtruecolor\033[0m"
      ;;
    "unset (default: 256)")
      unset LLM_GITHOOK_COLOR_MODE
      echo "Color mode unset (defaults to 256)."
      ;;
    *)
      return 0
      ;;
  esac
}

function _githooks_set_color_var {
  local var_name="$1"
  local label="$2"
  local allow_unset="${3:-}"
  local mode="$(_githooks_color_mode)"

  if [[ "$mode" == "truecolor" ]]; then
    local current="${(P)var_name}"
    echo "Current ${label} color: ${current:-"(not set)"}"
    echo "Enter RGB as R,G,B (0-255), or leave empty to cancel."
    if [[ "$allow_unset" == "allow-unset" ]]; then
      echo "Enter 'unset' to revert to default behavior."
    fi
    local input=""
    read "input?${label} RGB > "

    if [[ -z "$input" ]]; then
      return 0
    fi
    if [[ "$allow_unset" == "allow-unset" && "$input" == "unset" ]]; then
      unset "$var_name"
      echo "${label} color unset."
      return 0
    fi

    local parsed="$(_githooks_parse_rgb "$input")" || {
      echo "\033[31mInvalid RGB format. Expected R,G,B with 0-255.\033[0m"
      return 1
    }
    export "$var_name"="$parsed"
    echo "${label} color set to: \033[32m${parsed}\033[0m"
    return 0
  fi

  # 256-color mode
  local current="${(P)var_name}"
  local selected=""

  if command -v fzf &> /dev/null; then
    local -a items
    items+=("custom")
    if [[ "$allow_unset" == "allow-unset" ]]; then
      items+=("unset")
    fi
    local c
    for c in {0..255}; do
      items+=("$(printf '%3d %s' "$c" "$(printf '\033[38;5;%sm██\033[0m' "$c")")")
    done

    selected=$(printf '%s\n' "${items[@]}" | fzf --ansi --height=18 --header="Current: ${current:-"(not set)"}" --prompt="${label} > " --border --preview-window=hidden)
  else
    echo "\nCurrent ${label} color: ${current:-"(not set)"}"
    if [[ "$allow_unset" == "allow-unset" ]]; then
      echo "Enter 0-255, or 'unset' to clear, or empty to cancel."
    else
      echo "Enter 0-255, or empty to cancel."
    fi
    read "selected?${label} > "
  fi

  if [[ -z "$selected" ]]; then
    return 0
  fi

  if [[ "$allow_unset" == "allow-unset" && "$selected" == "unset" ]]; then
    unset "$var_name"
    echo "${label} color unset."
    return 0
  fi

  if [[ "$selected" == "custom" ]]; then
    local input=""
    read "input?Enter ${label} color (0-255): "
    selected="$input"
  else
    selected="${selected%% *}"
  fi

  _githooks_is_uint8 "$selected" || {
    echo "\033[31mInvalid 256-color value. Expected 0-255.\033[0m"
    return 1
  }

  export "$var_name"="$selected"
  echo "${label} color set to: \033[32m${selected}\033[0m"
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
  echo ""
  _githooks_box_header "Git Hooks Configuration Help"
  cat <<'EOF'

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
	    LLM_GITHOOK_FPS               Control animation FPS (default: 30)
	    LLM_GITHOOK_STATUS_TEXT       Override animated status text
	    LLM_GITHOOK_SHIMMER_SWEEP_SECONDS  Shimmer sweep duration (default: 2.0)
	    LLM_GITHOOK_SHIMMER_PADDING   Shimmer padding characters (default: 10)
	    LLM_GITHOOK_SHIMMER_BAND_WIDTH  Shimmer band half-width (default: 5.0)
	    LLM_GITHOOK_COLOR_MODE        256 (default) or truecolor
	    LLM_GITHOOK_SHIMMER_BASE_COLOR  Base text color (256: 0-255; truecolor: R,G,B)
	    LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR  Wave color (256: 0-255; truecolor: R,G,B)
	    LLM_GITHOOK_SPINNER_COLOR     Spinner color (defaults to base)
	    LLM_PROGRAM                   Custom llm executable path
	    LLM_PREPARE_COMMIT_MSG_PROMPT Custom prompt template
	    PYTHONIOENCODING              Force UTF-8 encoding (Windows compatibility)

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

  # Configure colors (examples)
  export LLM_GITHOOK_COLOR_MODE=256
  export LLM_GITHOOK_SHIMMER_BASE_COLOR=255
  export LLM_GITHOOK_SHIMMER_HIGHLIGHT_COLOR=240

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

