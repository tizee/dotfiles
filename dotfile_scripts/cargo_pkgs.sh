#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"
# failed on error
set -e

declare -a cargo_pkgs=(
# better git diff
git-delta
difftastic
# better ls
exa
# better cat
bat
# better xxd replacement - binary to hex
ohx
# cli benchmark tool
hyperfine
# tldr tool
tealdeer
# fast cp tool
fcp
# code count (better cloc)
tokei
# better grep
ripgrep
# better screenfetch for system info
neofetch
)


"$script_dir/helper.sh" -setup "rust"
"$script_dir/helper.sh" -cargo "${cargo_pkgs[@]}"
