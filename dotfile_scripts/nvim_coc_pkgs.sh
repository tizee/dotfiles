#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"
# failed on error
set -e

declare -a luarocks_pkgs=(
# check
coc-clangd
coc-css
coc-dictionary
coc-docthis
coc-emmet
coc-emoji
coc-eslint
coc-git
coc-go
coc-highlight
coc-jedi
coc-json
coc-biome
coc-python
coc-rust-analyzer
coc-sh
coc-snippets
# ruby lsp
coc-solargraph
# Swift lsp
coc-sourcekit
# lua
# coc-sumneko-lua
coc-tag
coc-texlab
coc-toml
coc-vetur
coc-vimlsp
coc-word
coc-yaml
coc-tsserver
)

# install coc packages
"$script_dir/helper.sh" -nvim "$pkgs"
