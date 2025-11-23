#!/usr/bin/env bash
set -euo pipefail

# Enhanced tmux session creation with configuration isolation
# Usage: create-session.sh -n NAME -c CONFIG_LEVEL [-p PROGRAM] [-s SOCKET_DIR]

# Get base directory (directory containing this script's parent)
if [[ -n "${BASH_SOURCE[0]:-}" ]]; then
  BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
else
  BASE_DIR="$(cd "$(dirname "$0")/.." && pwd)"
fi

usage() {
  cat <<'USAGE'
Usage: create-session.sh -n NAME -c CONFIG_LEVEL [options]

Create tmux session with specified configuration isolation level.

Options:
  -n, --name       Session name (required)
  -c, --config     Config level: clean, user, custom (required)
  -p, --program    Program to launch in session (optional)
  -s, --socket-dir Socket directory (default: $CLAUDE_TMUX_SOCKET_DIR or /tmp/claude-tmux-sockets)
  -f, --config-file Custom config file path (used with -c custom)
  -w, --wait-for   Wait for pattern before exiting (optional)
  -t, --timeout    Timeout for wait pattern (default: 15)
  -h, --help       Show this help

Config levels:
  clean  - No configuration, maximum performance (-f /dev/null)
  user   - Your tmux config (~/.config/tmux/tmux.basic.conf) with familiar keybindings
  custom - Custom config file (specify with -f)

Examples:
  create-session.sh -n python-repl -c clean -p 'python3 -q'
  create-session.sh -n gdb-debug -c user -p 'gdb --quiet ./program'
  create-session.sh -n custom-env -c custom -f ~/my-tmux.conf -p 'bash'
USAGE
}

# Default values
SESSION_NAME=""
CONFIG_LEVEL=""
PROGRAM=""
SOCKET_DIR=""
CUSTOM_CONFIG=""
WAIT_PATTERN=""
TIMEOUT=15

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    -n|--name)       SESSION_NAME="$2"; shift 2 ;;
    -c|--config)     CONFIG_LEVEL="$2"; shift 2 ;;
    -p|--program)    PROGRAM="$2"; shift 2 ;;
    -s|--socket-dir) SOCKET_DIR="$2"; shift 2 ;;
    -f|--config-file) CUSTOM_CONFIG="$2"; shift 2 ;;
    -w|--wait-for)   WAIT_PATTERN="$2"; shift 2 ;;
    -t|--timeout)    TIMEOUT="$2"; shift 2 ;;
    -h|--help)       usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

# Validate required arguments
if [[ -z "$SESSION_NAME" || -z "$CONFIG_LEVEL" ]]; then
  echo "Error: Session name and config level are required" >&2
  usage
  exit 1
fi

# Validate config level
case "$CONFIG_LEVEL" in
  clean|user|custom) ;;
  *) echo "Error: Invalid config level '$CONFIG_LEVEL'. Use: clean, user, custom" >&2
     exit 1 ;;
esac

# Check custom config file if specified
if [[ "$CONFIG_LEVEL" == "custom" && -z "$CUSTOM_CONFIG" ]]; then
  echo "Error: Custom config level requires -f/--config-file option" >&2
  exit 1
fi

# Set socket directory
if [[ -z "$SOCKET_DIR" ]]; then
  SOCKET_DIR="${CLAUDE_TMUX_SOCKET_DIR:-${TMPDIR:-/tmp}/claude-tmux-sockets}"
fi

# Create socket directory
mkdir -p "$SOCKET_DIR"

# Set socket path
SOCKET="$SOCKET_DIR/$SESSION_NAME.sock"

# Check if session already exists
if tmux -S "$SOCKET" list-sessions -F '#{session_name}' 2>/dev/null | grep -q "^$SESSION_NAME$"; then
  echo "Error: Session '$SESSION_NAME' already exists" >&2
  exit 1
fi

# Determine tmux command based on config level
case "$CONFIG_LEVEL" in
  clean)
    TMUX_CMD="tmux -S '$SOCKET' -f /dev/null"
    CONFIG_DESC="clean (no config)"
    ;;
  user)
    USER_CONFIG="$HOME/.config/tmux/tmux.basic.conf"
    if [[ -f "$USER_CONFIG" ]]; then
      TMUX_CMD="tmux -S '$SOCKET' -f '$USER_CONFIG'"
      CONFIG_DESC="user config: $USER_CONFIG"
    else
      echo "Warning: User config not found at $USER_CONFIG, falling back to clean config" >&2
      TMUX_CMD="tmux -S '$SOCKET' -f /dev/null"
      CONFIG_DESC="clean (user config not found)"
    fi
    ;;
  custom)
    if [[ ! -f "$CUSTOM_CONFIG" ]]; then
      echo "Error: Custom config file not found: $CUSTOM_CONFIG" >&2
      exit 1
    fi
    TMUX_CMD="tmux -S '$SOCKET' -f '$CUSTOM_CONFIG'"
    CONFIG_DESC="custom config: $CUSTOM_CONFIG"
    ;;
esac

echo "Creating session '$SESSION_NAME' with $CONFIG_DESC"
echo "Socket: $SOCKET"

# Create session
eval "$TMUX_CMD new -d -s '$SESSION_NAME' -n shell"
echo "Session created:"
tmux -S "$SOCKET" list-sessions

# Launch program if specified
if [[ -n "$PROGRAM" ]]; then
  echo "Launching program: $PROGRAM"

  # Special handling for Python
  if [[ "$PROGRAM" == *"python"* ]]; then
    export PYTHON_BASIC_REPL=1
  fi

  tmux -S "$SOCKET" send-keys -t "$SESSION_NAME":0.0 -- "$PROGRAM" Enter
fi

# Wait for pattern if specified
if [[ -n "$WAIT_PATTERN" ]]; then
  echo "Waiting for pattern: $WAIT_PATTERN"
  if ! "$BASE_DIR/tools/wait-for-text.sh" \
       -t "$SESSION_NAME":0.0 \
       -p "$WAIT_PATTERN" \
       -s "$SOCKET" \
       -T "$TIMEOUT"; then
    echo "Error: Timeout waiting for pattern '$WAIT_PATTERN'" >&2
    echo "Cleaning up session..."
    tmux -S "$SOCKET" kill-session -t "$SESSION_NAME" 2>/dev/null || true
    exit 1
  fi
  echo "Pattern detected, session ready"
fi

echo "Session '$SESSION_NAME' created successfully"
echo "Monitor with: tmux -S '$SOCKET' attach -t '$SESSION_NAME'"
echo "Capture output: tmux -S '$SOCKET' capture-pane -p -J -t '$SESSION_NAME':0.0 -S -200"
echo "Config level: $CONFIG_LEVEL"

# Provide specific instructions based on config level
case "$CONFIG_LEVEL" in
  user)
    echo "Note: Using your keybindings (prefix: C-a, detach: C-a d)"
    ;;
  clean)
    echo "Note: Clean session, no custom keybindings loaded"
    ;;
  custom)
    echo "Note: Using custom config from $CUSTOM_CONFIG"
    ;;
esac
