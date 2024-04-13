#!/usr/bin/env bash
script_path="$(realpath $0)"
script_dir="$(dirname "$script_path")"
# failed on error
set -e

declare -a mac_pkgs=(
  # select default appss for documents and URL schemes
  duti
)

declare -a brew_cask_apps=(
# Terminal emulator
wezterm

# Keyboard
karabiner-elements

# macos desktop automation
hammerspoon

# screenshot
# obs

# flash card
anki

# social media
# telegram
# discord

# note tool
obsidian

# bookmarks manager
# raindropio

# eye candy GUI NeoVim editor
# neovide

# app uninstaller
appcleaner

# credentials manager
1password

# Inspect application bundles
apparency

# Tool for digging into binary files
archaeology

# An Application for Inspecting macOS Installer Packages
suspicious-package
)

# ln -s /Applications/Apparency.app/Contents/MacOS/appy $(brew --prefix)/bin

"$script_dir/helper.sh" -brew "${mac_pkgs[@]}"
"$script_dir/helper.sh" -cask "${brew_cask_apps[@]}"
