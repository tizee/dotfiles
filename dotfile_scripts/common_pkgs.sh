#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker
script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"

# failed on error
set -e

# common homebrew packages
declare -a pkgs=(
# code manager
git
# decompression
unzip
# decompression
brotli
# TUI git repo viewer
tig
# file downloader
curl
# make system
cmake
# programming language
python3
lua
# zig - build from source
# go - build from source

# llvm ecosystem
# this would also install llvm/cmake
clang

# lua manager
luarocks

# http client
httpie

# Node.JS JavaScript ecosystem
node
pnpm

# json tool
jq

# linter
shellcheck

# cli utilities
# debug
gdb
# TUI session
tmux
# interactive search with fuzzy algorithm
fzf
# better htop
btop
# editor
vim
neovim
# video encoder/decoder/downloader/editor
ffmpeg
# free lossless audio codec
flac

# print system info
fastfetch

# library
lzo

#  -- Linter/Formatter -- {{{
#  Swift Style
swiftlint
#  }}}
#
)

declare -a brew_pkgs=(
  # git status widget
  "romkatv/gitstatus/gitstatus"

  # bun
  "oven-sh/bun/bun"

  # library
  openssl@3
)

if [[ -x "$(command -v pacman)" ]]; then
  # Arch Linux
  "$script_dir/helper.sh" -pacman "${pkgs[@]}" && echo -e "$lightyellow common packages ${reset_color} check ${lightgreen}✔$reset_color"

elif [[ -x "$(command -v brew)" ]]; then
  "$script_dir/helper.sh" -setup "brew" && echo -e "$lightyellow homebrew${reset_color} check ${lightgreen}✔$reset_color"
  "$script_dir/helper.sh" -brew "${brew_pkgs[@]}" && echo -e "$lightyellow homebrew-only packages ${reset_color} check ${lightgreen}✔$reset_color"

  "$script_dir/helper.sh" -brew "${pkgs[@]}" && echo -e "$lightyellow common packages ${reset_color} check ${lightgreen}✔$reset_color"

fi
