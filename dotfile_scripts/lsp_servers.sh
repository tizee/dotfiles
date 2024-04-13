#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

# failed on error
set -e

_dotfiles_install_lsp_servers() {
  # cmake
  pip install cmake-language-server
  # lua formatter
  cargo install stylua
  # swift
  # xcrun sourcekit-lsp
}
