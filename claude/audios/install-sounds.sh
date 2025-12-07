#!/usr/bin/env bash
#
# install-sounds.sh - Convert MP3 sound effects to AIFF and install to macOS user sounds
#
# Usage:
#   ./install-sounds.sh              # Convert and install all MP3 files in current directory
#   ./install-sounds.sh file.mp3     # Convert and install specific file(s)
#   ./install-sounds.sh -l           # List installed custom sounds
#   ./install-sounds.sh -r name      # Remove installed sound by name
#
# The converted sounds will be available for use with:
#   - terminal-notifier -sound <name>
#   - System Preferences > Sound > Sound Effects
#   - Any macOS app that uses system sounds

set -euo pipefail

SOUNDS_DIR="${HOME}/Library/Sounds"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

info() { echo -e "${BLUE}[INFO]${NC} $*"; }
success() { echo -e "${GREEN}[OK]${NC} $*"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }

# Ensure sounds directory exists
ensure_sounds_dir() {
    if [[ ! -d "$SOUNDS_DIR" ]]; then
        info "Creating ${SOUNDS_DIR}..."
        mkdir -p "$SOUNDS_DIR"
    fi
}

# Convert a single MP3 file to AIFF and install
# Returns: 0=success, 1=error, 2=skipped
convert_and_install() {
    local input_file="$1"
    local force="${2:-false}"
    local basename="${input_file##*/}"
    local name="${basename%.mp3}"
    local output_file="${SOUNDS_DIR}/${name}.aiff"

    if [[ ! -f "$input_file" ]]; then
        error "File not found: $input_file"
        return 1
    fi

    if [[ "${input_file##*.}" != "mp3" ]]; then
        warn "Skipping non-MP3 file: $input_file"
        return 2
    fi

    # Skip if already installed (unless force)
    if [[ -f "$output_file" ]] && [[ "$force" != "true" ]]; then
        info "Skipping (already installed): ${name}"
        return 2
    fi

    info "Converting: ${basename} → ${name}.aiff"

    if afconvert "$input_file" "$output_file" -f AIFF -d BEI16 2>/dev/null; then
        success "Installed: ${name} ($(du -h "$output_file" | cut -f1))"
        return 0
    else
        error "Failed to convert: $input_file"
        return 1
    fi
}

# List installed sounds
list_sounds() {
    echo -e "${BLUE}Installed sounds in ${SOUNDS_DIR}:${NC}"
    echo ""

    if [[ ! -d "$SOUNDS_DIR" ]] || [[ -z "$(ls -A "$SOUNDS_DIR" 2>/dev/null)" ]]; then
        warn "No custom sounds installed"
        return 0
    fi

    printf "%-30s %10s\n" "NAME" "SIZE"
    printf "%-30s %10s\n" "----" "----"

    shopt -s nullglob
    for f in "${SOUNDS_DIR}"/*.aiff; do
        local name="${f##*/}"
        name="${name%.aiff}"
        local size
        size=$(du -h "$f" | cut -f1)
        printf "%-30s %10s\n" "$name" "$size"
    done
    shopt -u nullglob

    echo ""
    info "Use with: terminal-notifier -sound <NAME>"
}

# Remove an installed sound
remove_sound() {
    local name="$1"
    local file="${SOUNDS_DIR}/${name}.aiff"

    if [[ -f "$file" ]]; then
        rm "$file"
        success "Removed: $name"
    else
        error "Sound not found: $name"
        return 1
    fi
}

# Show usage
usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS] [FILE...]

Convert MP3 sound effects to AIFF and install to macOS user sounds directory.

Options:
    -h, --help      Show this help message
    -l, --list      List installed custom sounds
    -r, --remove    Remove an installed sound by name
    -f, --force     Force reinstall (overwrite existing sounds)
    -a, --all       Convert all MP3 files in script directory

Examples:
    $(basename "$0")                    # Convert all MP3s in current directory
    $(basename "$0") alert.mp3          # Convert specific file
    $(basename "$0") -f alert.mp3       # Force reinstall specific file
    $(basename "$0") -l                 # List installed sounds
    $(basename "$0") -r alert           # Remove 'alert' sound

Sound locations:
    Source:  ${SCRIPT_DIR}
    Target:  ${SOUNDS_DIR}
EOF
}

# Main
main() {
    local force="false"

    # Handle options
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                exit 0
                ;;
            -l|--list)
                list_sounds
                exit 0
                ;;
            -r|--remove)
                if [[ -z "${2:-}" ]]; then
                    error "Please specify sound name to remove"
                    exit 1
                fi
                remove_sound "$2"
                exit 0
                ;;
            -f|--force)
                force="true"
                shift
                ;;
            -a|--all)
                shift
                break
                ;;
            -*)
                error "Unknown option: $1"
                usage
                exit 1
                ;;
            *)
                break
                ;;
        esac
    done

    ensure_sounds_dir

    # If no arguments, convert all MP3s in script directory
    if [[ $# -eq 0 ]]; then
        shopt -s nullglob
        set -- "${SCRIPT_DIR}"/*.mp3
        shopt -u nullglob
    fi

    local converted=0
    local skipped=0
    local failed=0

    for file in "$@"; do
        # Handle glob with no matches
        [[ -e "$file" ]] || continue

        local ret=0
        convert_and_install "$file" "$force" || ret=$?

        case $ret in
            0) ((converted++)) ;;
            1) ((failed++)) ;;
            2) ((skipped++)) ;;
        esac
    done

    echo ""
    if [[ $converted -gt 0 ]]; then
        success "Converted $converted sound(s)"
    fi
    if [[ $skipped -gt 0 ]]; then
        info "Skipped $skipped sound(s) (use -f to force reinstall)"
    fi
    if [[ $failed -gt 0 ]]; then
        warn "Failed to convert $failed file(s)"
    fi
    if [[ $converted -eq 0 ]] && [[ $skipped -eq 0 ]] && [[ $failed -eq 0 ]]; then
        warn "No MP3 files found"
    fi
}

main "$@"
