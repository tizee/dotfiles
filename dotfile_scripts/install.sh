#!/usr/bin/env bash
# ______
#/\__  _\__
#\/_/\ \/\_\  ____      __     __
#   \ \ \/\ \/\_ ,`\  /'__`\ /'__`\
#    \ \ \ \ \/_/  /_/\  __//\  __/
#     \ \_\ \_\/\____\ \____\ \____\
#      \/_/\/_/\/____/\/____/\/____/
#
# Tizee (Jeff Chiang)
# https://github.com/tizee

# failed on error
set -e

# ====================
# Color Output
# ====================
# {{{
reset_color='\033[0m'
lightgreen='\033[92m'
lightyellow='\033[38;5;228m'
red='\033[38;5;196m'
# }}}

# ====================
# Banner
# ====================
echo -e "${lightgreen}"
echo -e '               __              __                  '
echo -e '              /\ \            /\ \                 '
echo -e ' __  __    ___\ \ \___     ___\ \ \___      __     '
echo -e '/\ \/\ \  / __`\ \  _ `\  / __`\ \  _ `\  /"__`\   '
echo -e '\ \ \_\ \/\ \L\ \ \ \ \ \/\ \L\ \ \ \ \ \/\ \L\.\_ '
echo -e ' \/`____ \ \____/\ \_\ \_\ \____/\ \_\ \_\ \__/.\_\'
echo -e '  `/___/> \/___/  \/_/\/_/\/___/  \/_/\/_/\/__/\/_/'
echo -e '     /\___/                                        '
echo -e '     \/__/                                         '
echo -e "${reset_color}"

# ====================
# Packages List
# ====================
# {{{

# MacOS: brew
# Arch Linux: pacman
if [[ $(uname -s) == 'Darwin' ]]; then
  # PM stands for package manager
  PM="brew"
else
  # I use Arch Linux
  PM="pacman"
fi

# packages
# clang for compiling backend
# rust for writing good program
# gtk for developing gui
declare -a pkgs=(unzip git curl clang gdb vim neovim tmux python3 rust)
brew_cask_apps=()
linux_pkgs=()

# }}}

# ====================
# Utility
# ====================
# {{{
# helper to make sure packages have been installed
function cli_has_installed() {
 command -v $@ > /dev/null 2>&1 && return
}

function pacman_package_has_installed() {
 cli_has_installed 'pacman' && pacman -Qi $@ > /dev/null 2>&1 && return
}

function helper::install_pkgs() {
  # count time eclipsed
  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi
  for pkg in ${pkgs[@]:0}; do
    echo -e " install${lightgreen} ${pkg}$reset_color"
    if [[ $(uname -s) = "Darwin" ]]; then
      $PM list $pkg 1>/dev/null 2>&1 || brew install $pkg || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
    elif [[ $(uname -s) = "Linux" ]]; then
      pacman_package_has_installed $pkg && echo -e " has installled ${lightgreen}$pkg${reset_color}" && continue
      sudo $PM -S $pkg 1>/dev/null 2>&1 || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
    fi
  done
  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#pkgs[@]}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function helper::install_dotfiles() {
  if cli_has_installed 'unzip'; then
    local zip_file="${HOME}/tizee-dotfiles.zip"
    /bin/bash -c "$(curl -fsSL https://github.com/tizee/dotfiles/archive/refs/heads/master.zip -o ${zip_file})"
    if [[ -d $HOME/.config ]]; then
      echo -e " ${lightgreen}$HOME/.config${reset_color} existed! Rename it to ${lightgreen}$HOME/.config_backup${reset_color}"
      mv $HOME/.config $HOME/.config_backup
    fi
    echo -e " ${lightgreen}Install dotfiles${reset_color}"
    unzip ${zip_file} -d ${HOME}/.config
  fi
}

# }}}

# ====================
# Arch Linux
# ====================
# {{{
if [[ $(uname -s) = "Linux" ]]; then
  echo -e "$lightyellow Linux dotfiles ${reset_color}setup"
  cli_has_installed 'pacman' || echo -e " ${red}pacman${reset_color} is not installed" || exit 1
  helper::install_pkgs || echo -e " install packages $red  failed $reset_color" || exit 1
  helper::install_dotfiles || echo -e " install dotfiles $red  failed $reset_color" || exit 1

fi
# }}}

# ====================
# MacOS
# ====================
# {{{
if [[ $(uname -s) = "Darwin" ]]; then
  function install_homebrew() {
    local url="https://brew.sh/"
    cli_has_installed 'brew' && return
    echo -e "install $lightgreen homebrew $reset_color now."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" 2>&1
  }
  echo -e "$lightyellow MacOS dotfiles ${reset_color}setup"
  install_homebrew || echo -e " install homebrew $red failed $reset_color" || exit 1
  helper::install_pkgs || echo -e " install packages $red  failed $reset_color" || exit 1
  echo -e "$lightyellow MacOS ${reset_color}good to go ${lightgreen}✔$reset_color"
  unset install_homebrew
  helper::install_dotfiles || echo -e " install dotfiles $red  failed $reset_color" || exit 1
fi
# }}}

unset cli_has_installed
unset pacman_package_has_installed
unset helper::install_pkgs
unset helper::install_dotfiles
# vim:ft=sh:foldmethod=marker
