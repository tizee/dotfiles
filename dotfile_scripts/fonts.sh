#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"
# failed on error
set -e

declare -a fonts=(
# JetBrains Mono Nerd font
font-jetbrains-mono-nerd-font
# LXGW WenKai font
font-lxgw-wenkai
)

# should install homebrew first
"$script_dir/helper.sh" -font "${pkgs[@]}"
