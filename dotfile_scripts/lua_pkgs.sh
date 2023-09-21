#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"
# failed on error
set -e

declare -a luarocks_pkgs=(
# check
luacheck
)

# should install luarocks first
"$script_dir/helper.sh" -lua "$pkgs"
