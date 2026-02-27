#!/usr/bin/env zsh
# Auto-load .env file into current shell environment
# Usage: source this file from .zshrc

# Path to .env file (can be overridden)
: "${ENV_FILE:=$HOME/.config/llm/.env}"

# Load .env if it exists and is readable
if [[ -f "$ENV_FILE" && -r "$ENV_FILE" ]]; then
  # Use 'set -a' to auto-export variables, then source and turn off
  set -a
  source "$ENV_FILE"
  set +a
fi
