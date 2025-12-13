#!/usr/bin/env zsh

# ========== gemini-cli with dynamic skills ========== {{{
# Runtime composition: generate GEMINI.md with base prompt + skills list
# Maintains Single Source of Truth (CLAUDE.md) while enabling skill discovery

# Configuration
GEMINI_SKILLS_DIR="${GEMINI_SKILLS_DIR:-$HOME/.claude/skills}"
GEMINI_BASE_PROMPT="${GEMINI_BASE_PROMPT:-$HOME/.claude/CLAUDE.md}"
GEMINI_CONFIG_DIR="${GEMINI_CONFIG_DIR:-$HOME/.gemini}"

# Extract skill info from SKILL.md YAML frontmatter
# Usage: __gemini_extract_skill_info /path/to/SKILL.md
function __gemini_extract_skill_info() {
  local skill_file="$1"
  local skill_dir=$(dirname "$skill_file")
  local skill_name=$(basename "$skill_dir")
  local description=""

  # Extract description from YAML frontmatter (between --- markers)
  if [[ -f "$skill_file" ]]; then
    description=$(awk '
      /^---$/ { in_frontmatter = !in_frontmatter; next }
      in_frontmatter && /^description:/ {
        sub(/^description:[[:space:]]*"?/, "")
        sub(/"?[[:space:]]*$/, "")
        print
        exit
      }
    ' "$skill_file")
  fi

  # Output in format: name|description
  if [[ -n "$description" ]]; then
    echo "${skill_name}|${description}"
  fi
}

# Generate skills list in markdown format
# Usage: __gemini_generate_skills_list [skills_dir] [show_path]
# Args:
#   skills_dir - Directory containing skills (default: $GEMINI_SKILLS_DIR)
#   show_path  - Include file paths in output (default: true)
function __gemini_generate_skills_list() {
  local skills_dir="${1:-$GEMINI_SKILLS_DIR}"
  local show_path="${2:-true}"
  local skills_found=0

  # Header
  echo ""
  echo "## Available Skills"
  echo ""
  echo "The following skills are available for specialized tasks. Read the SKILL.md file for detailed instructions:"
  echo ""

  # Find all SKILL.md files and extract info
  if [[ -d "$skills_dir" ]]; then
    for skill_file in "$skills_dir"/*/SKILL.md(N); do
      if [[ -f "$skill_file" ]]; then
        local info=$(__gemini_extract_skill_info "$skill_file")
        if [[ -n "$info" ]]; then
          local name="${info%%|*}"
          local desc="${info#*|}"
          echo "- **${name}**: ${desc}"
          if [[ "$show_path" == "true" ]]; then
            echo "  - Path: \`${skill_file}\`"
          fi
          ((skills_found++))
        fi
      fi
    done
  fi

  if (( skills_found == 0 )); then
    echo "- *No skills found in ${skills_dir}*"
  fi

  echo ""
  echo "To use a skill, read the corresponding SKILL.md file for detailed guidance and tool usage patterns."
}

# Check if GEMINI.md needs regeneration based on source file timestamps
# Returns 0 if regeneration needed, 1 if up-to-date
# Usage: __gemini_needs_regeneration [base_file] [skills_dir] [output_file]
function __gemini_needs_regeneration() {
  local base_file="${1:-$GEMINI_BASE_PROMPT}"
  local skills_dir="${2:-$GEMINI_SKILLS_DIR}"
  local output_file="${3:-$GEMINI_CONFIG_DIR/GEMINI.md}"
  local output_mtime base_mtime skill_mtime dir_mtime

  # Always regenerate if output doesn't exist
  [[ ! -f "$output_file" ]] && return 0

  # Get output file mtime (macOS uses -f %m, Linux uses -c %Y)
  output_mtime=$(stat -f %m "$output_file" 2>/dev/null || stat -c %Y "$output_file" 2>/dev/null)
  [[ -z "$output_mtime" ]] && return 0

  # Check base prompt modification time
  if [[ -f "$base_file" ]]; then
    base_mtime=$(stat -f %m "$base_file" 2>/dev/null || stat -c %Y "$base_file" 2>/dev/null)
    [[ -n "$base_mtime" && "$base_mtime" -gt "$output_mtime" ]] && return 0
  fi

  # Check all SKILL.md files modification time
  if [[ -d "$skills_dir" ]]; then
    for skill_file in "$skills_dir"/*/SKILL.md(N); do
      if [[ -f "$skill_file" ]]; then
        skill_mtime=$(stat -f %m "$skill_file" 2>/dev/null || stat -c %Y "$skill_file" 2>/dev/null)
        [[ -n "$skill_mtime" && "$skill_mtime" -gt "$output_mtime" ]] && return 0
      fi
    done

    # Check if skills directory itself was modified (new skill added/removed)
    dir_mtime=$(stat -f %m "$skills_dir" 2>/dev/null || stat -c %Y "$skills_dir" 2>/dev/null)
    [[ -n "$dir_mtime" && "$dir_mtime" -gt "$output_mtime" ]] && return 0
  fi

  # No regeneration needed
  return 1
}

# Generate combined GEMINI.md with base prompt + skills
# Usage: __gemini_generate_config [base_file] [skills_dir] [output_file] [force]
# Args:
#   force - If "true", skip timestamp check and always regenerate
function __gemini_generate_config() {
  local base_file="${1:-$GEMINI_BASE_PROMPT}"
  local skills_dir="${2:-$GEMINI_SKILLS_DIR}"
  local output_file="${3:-$GEMINI_CONFIG_DIR/GEMINI.md}"
  local force="${4:-false}"

  # Check if regeneration is needed (skip check if force=true)
  if [[ "$force" != "true" ]] && ! __gemini_needs_regeneration "$base_file" "$skills_dir" "$output_file"; then
    return 0
  fi

  # Ensure config directory exists
  mkdir -p "$(dirname "$output_file")"

  # Generate combined config
  {
    # Base prompt content
    if [[ -f "$base_file" ]]; then
      cat "$base_file"
    else
      echo "# Gemini CLI Configuration"
      echo ""
      echo "*Base prompt not found at: ${base_file}*"
    fi

    # Dynamic skills list
    __gemini_generate_skills_list "$skills_dir"
  } > "$output_file"

  return 0
}

# Main gemini wrapper with dynamic skill loading
# Usage: gemini [args...]
# Note: Only wraps if gemini-cli is available
function gemini() {
  # Check if gemini-cli exists
  if ! command -v gemini &>/dev/null; then
    echo "Error: gemini-cli not found. Install with: npm install -g @anthropic-ai/gemini-cli"
    return 1
  fi

  # Generate fresh GEMINI.md with skills before invocation
  __gemini_generate_config

  # Run gemini-cli with all arguments
  command gemini "$@"
}

# List available skills without running gemini
# Usage: gemini-skills
function gemini-skills() {
  echo "Scanning skills directory: ${GEMINI_SKILLS_DIR}"
  echo ""
  __gemini_generate_skills_list "$GEMINI_SKILLS_DIR"
}

# Regenerate GEMINI.md manually without running gemini
# Usage: gemini-refresh [-v|--verbose]
function gemini-refresh() {
  local verbose=false
  [[ "$1" == "-v" || "$1" == "--verbose" ]] && verbose=true

  # Force regeneration when manually refreshing
  __gemini_generate_config "$GEMINI_BASE_PROMPT" "$GEMINI_SKILLS_DIR" "$GEMINI_CONFIG_DIR/GEMINI.md" "true"
  echo "Generated: ${GEMINI_CONFIG_DIR}/GEMINI.md"
  echo ""
  echo "Configuration:"
  echo "  Base prompt: ${GEMINI_BASE_PROMPT}"
  echo "  Skills dir:  ${GEMINI_SKILLS_DIR}"
  echo ""
  echo "Skills included:"
  # Show skills without paths for cleaner summary
  __gemini_generate_skills_list "$GEMINI_SKILLS_DIR" "false" | grep "^-"

  if [[ "$verbose" == "true" ]]; then
    echo ""
    echo "Full generated content:"
    echo "────────────────────────────────────────"
    cat "${GEMINI_CONFIG_DIR}/GEMINI.md"
  fi
}
# }}}

# vim:ft=zsh:foldmarker={{{,}}}
