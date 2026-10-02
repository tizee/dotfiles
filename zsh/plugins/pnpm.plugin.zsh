#!/usr/bin/env zsh

# Print the cmd-shim target path recorded in a pnpm global CLI shim.
function _pnpm_shim_target() {
  local shim target
  shim=$(whence -p "$1") || {
    print -u2 "Command not found: $1"
    return 1
  }
  target=$(LC_ALL=C sed -n 's/^# cmd-shim-target=//p' "$shim" | tail -n 1)
  if [[ -z $target || $target != */node_modules/* ]]; then
    print -u2 "Not a recognized pnpm global CLI shim: $shim"
    return 1
  fi
  print -r -- "$target"
}

# Print the installed package directory for a pnpm global CLI.
# Usage: pnpm-global-path <command> [package]
function pnpm-global-path() {
  if (( $# < 1 || $# > 2 )); then
    print -u2 'Usage: pnpm-global-path <command> [package]'
    return 2
  fi

  local target modules package rest package_dir
  target=$(_pnpm_shim_target "$1") || return 1

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

# Print the real executable behind a pnpm global CLI.
# Wrapper packages (claude-code, codex, esbuild, ...) ship the native binary in a
# platform optionalDependency such as <pkg>-darwin-arm64; prefer that binary,
# otherwise fall back to the shim target itself.
# Usage: pnpm-global-bin <command>
function pnpm-global-bin() {
  if (( $# != 1 )); then
    print -u2 'Usage: pnpm-global-bin <command>'
    return 2
  fi
  if (( ! $+commands[jq] )); then
    print -u2 'pnpm-global-bin requires jq'
    return 1
  fi

  local target package_dir os arch dep dep_dir bin
  target=$(_pnpm_shim_target "$1") || return 1
  package_dir=$(pnpm-global-path "$1") || return 1

  os=${(L)$(uname -s)}
  arch=$(uname -m)
  case $arch in
    x86_64) arch=x64 ;;
    aarch64) arch=arm64 ;;
  esac

  for dep in ${(f)"$(jq -r '.optionalDependencies // {} | keys[]' "$package_dir/package.json")"}; do
    [[ $dep == *-$os-$arch* ]] || continue
    # Dependencies are siblings of the package inside the same node_modules.
    dep_dir=${package_dir%/node_modules/*}/node_modules/$dep
    [[ -d $dep_dir ]] || continue
    bin=$(find -L "$dep_dir" -type f -name "$1" -perm -u+x -print -quit 2>/dev/null)
    if [[ -n $bin ]]; then
      realpath "$bin"
      return
    fi
  done

  realpath "$target"
}
