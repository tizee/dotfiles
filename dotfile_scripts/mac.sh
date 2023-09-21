#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker
# failed on error
set -e

script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"

# ====================
# Color Output
# ====================
# {{{
reset_color='\033[0m'
lightgreen='\033[92m'
lightyellow='\033[38;5;228m'
red='\033[38;5;196m'
# }}}

# ====================
# MacOS
# ====================
# {{{
if [[ $(uname -s) = "Darwin" ]]; then
  echo -e "$lightyellow MacOS ${reset_color}setup start"
  "$script_dir/helper.sh" -setup brew || echo -e " install$red homebrew$reset_color failed " || exit 1
  eval "$(/opt/homebrew/bin/brew shellenv)"
  "$script_dir/common_pkgs.sh" || echo -e " install$red packages$reset_color failed " || exit 1
  "$script_dir/mac_apps.sh" || echo -e " install$red apps$reset_color failed " || exit 1
  "$script_dir/helper.sh" -conf || echo -e " install$red dotfiles$reset_color failed " || exit 1
  "$script_dir/mac_defaults.sh" || echo -e "setup sensibles$red macOS defaults$reset_color failed " || exit 1
  echo -e "$lightyellow MacOS ${reset_color}good to go ${lightgreen}✔$reset_color"
fi
# }}}
