#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

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

if [[ $(uname -s) = "Linux" ]]; then
  if [[ -x $(command -v pacman )]]; then
    # ====================
    # Arch Linux
    # ====================
    echo -e "$lightyellow Arch Linux ${reset_color}setup"
    "$script_dir/common_pkgs.sh" || echo -e " $red install packages$reset_color" || exit 1
    "$script_dir/helper.sh" -conf || echo -e " $red install dotfiles$reset_color" || exit 1
  elif $(uname -r | grep 'microsoft' > /dev/null); then
    # ====================
    # Ubuntu WSL
    # ====================
    echo -e "$lightyellow WSL dotfiles ${reset_color}setup"
    "$script_dir/common_pkgs.sh" || echo -e " $red install packages$reset_color" || exit 1
    "$script_dir/helper.sh" -conf || echo -e " $red install dotfiles$reset_color" || exit 1
  fi
fi
