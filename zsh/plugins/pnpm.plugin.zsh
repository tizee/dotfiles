#!/usr/bin/env zsh

# Print the installed package directory for a pnpm global CLI.
# Usage: pnpm-global-path <command> [package]
function pnpm-global-path() {
  if (( $# < 1 || $# > 2 )); then
    print -u2 'Usage: pnpm-global-path <command> [package]'
    return 2
  fi

  local shim target modules package rest package_dir
  shim=$(whence -p "$1") || {
    print -u2 "Command not found: $1"
    return 1
  }
  target=$(sed -n 's/^# cmd-shim-target=//p' "$shim" | tail -n 1)
  if [[ -z $target || $target != */node_modules/* ]]; then
    print -u2 "Not a recognized pnpm global CLI shim: $shim"
    return 1
  fi

  modules=${target%%/node_modules/*}/node_modules
  if (( $# == 2 )); then
    package=$2
  else
    rest=${target#"$modules"/}
    if [[ $rest == @*/*/* ]]; then
      package=${rest%%/*}/${${rest#*/}%%/*}
    else
      package=${rest%%/*}
    fi
  fi

  package_dir=$modules/$package
  if [[ ! -d $package_dir ]]; then
    package_dir=$modules/.pnpm/node_modules/$package
  fi
  if [[ ! -d $package_dir ]]; then
    print -u2 "Package not found in this CLI installation: $package"
    return 1
  fi
  realpath "$package_dir"
}
