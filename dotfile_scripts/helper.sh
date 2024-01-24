#!/usr/bin/env bash
# vim:ft=sh:foldmethod=marker

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
# Utility
# ====================
# {{{
# helper to make sure packages have been installed
function cli_has_installed() {
 command -v "$@" > /dev/null 2>&1 && return
}

function pacman_package_has_installed() {
 cli_has_installed "pacman" && pacman -Qi "$@" > /dev/null 2>&1 && return
}

function __install_emacs() {
  # TODO
  # no-op
  true
}

function __install_rust() {
  cli_has_installed "cargo" && return
  # install Rust with rustup instead Homebrew
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
}

function __install_fonts() {
  if [[ $(uname -s) = "Linux" ]]; then
    exit 1
  fi
  cli_has_installed "brew" || exit 1

  brew tap homebrew/cask-fonts
  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi
  for pkg in "${@}"; do
      echo -e " install font:${lightgreen} ${pkg}$reset_color"
      brew list "$pkg" 1>/dev/null 2>&1 || brew install --cask "$pkg" || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} fonts installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function __pacman_pkgs() {
  cli_has_installed "pacman" || exit 1
  for pkg in "${@}"; do
    echo -e " install${lightgreen} ${pkg}$reset_color"
    if [[ $(uname -s) = "Linux" ]]; then
      pacman_package_has_installed "$pkg" && echo -e "${lightgreen}$pkg${reset_color} has been installed " && continue
      sudo pacman -S "$pkg" 1>/dev/null 2>&1 || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
    fi
  done
}

function __brew_pkgs() {
  cli_has_installed "brew" || exit 1

  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi

  for pkg in "${@}"; do
    echo -e " install${lightgreen} ${pkg}$reset_color"
    brew list "$pkg" 1>/dev/null 2>&1 || brew install "$pkg" || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function __brew_cask_apps() {
  # only supports MacOS
  if [[ $(uname -s) = "Linux" ]]; then
    exit 1
  fi
  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi

  for pkg in "${@}"; do
      echo -e " install${lightgreen} ${pkg}$reset_color"
      brew list "$pkg" 1>/dev/null 2>&1 || brew install --cask "$pkg" || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function __install_cargo_pkgs() {
  cli_has_installed "cargo" || exit 1

  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi

  for pkg in "${@}"; do
    echo -e " install${lightgreen} ${pkg}$reset_color"
    cli_has_installed "$pkg" 1>/dev/null 2>&1 || cargo install "$pkg" || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function __install_lua_pkgs() {
  cli_has_installed "luarocks" || exit 1

  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi

  for pkg in "${@:0}"; do
    echo -e " install${lightgreen} ${pkg}$reset_color"
    cli_has_installed "$pkg" 1>/dev/null 2>&1 || luarocks install "$pkg" || echo -e "\n ${red}${pkg}${reset_color} failed" || exit 1
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}

function __install_dotfiles() {
  if cli_has_installed 'unzip'; then
    local zip_file="${HOME}/tizee-dotfiles.zip"
    /bin/bash -c "$(curl -fsSL https://github.com/tizee/dotfiles/archive/refs/heads/master.zip -o ${zip_file})"
    echo -e " ${lightgreen}Install dotfiles${reset_color}"
    # dotfiles-master
    unzip ${zip_file}
    mv "$HOME/dotfiles-master" "$HOME/tizee-dotfiles"
    rm ${zip_file}
    cd "$HOME/tizee-dotfiles"; make install; make zsh; make git; make nvim; make cargo
  fi
}

function __install_homebrew() {
  cli_has_installed "brew" && return
  echo -e "install $lightgreen homebrew$reset_color now."
  # Following command will install homebrew and XCode Command Line Tools
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" 2>&1
}

function __install_nvim_coc_pkg() {
  cli_has_installed "nvim" && return
  echo -e "install $lightgreen nvim packages$reset_color now."
  if cli_has_installed 'date'; then
    local start_time=$(date +%s)
  fi

  for pkg in "${@}"; do
      echo -e " install${lightgreen} ${pkg}$reset_color"
      nvim -c "CocInstall $pkg" -c "qa!"
  done

  if cli_has_installed 'date'; then
    local end_time=$(date +%s)
    echo -e " ${lightgreen}${#@}${reset_color} packages installed in ${lightgreen}$[$end_time - $start_time]${reset_color} seconds"
  fi
}


# }}}

function __setup() {
  case "$1" in
    brew)
      __install_homebrew
      ;;
    rust)
      __install_rust
      ;;
  esac
}

case "$1" in
  -lua)
    shift
    __install_lua_pkgs "${@}"
    ;;
  -cargo)
    shift
    __setup rust
    __install_cargo_pkgs "${@}"
    ;;
  -pacman)
    shift
    __pacman_pkgs  "${@}"
    ;;
  -brew)
    shift
    __brew_pkgs "${@}"
    ;;
  -coc)
    shift
    __install_nvim_coc_pkg "${@}"
    ;;
  -conf)
    __install_dotfiles
    ;;
  -setup)
    shift
    __setup "$@"
    ;;
  -cask)
    shift
    __brew_cask_apps "${@}"
    ;;
  -font)
    shift
    __install_fonts "${@}"
    ;;
  *)
    echo "-lua: install lua packages via luarocks
-cargo: install Rust packages
-pacman: install ArchLinux packages
-conf: install dotfiles
-setup: install homebrew/rust
-font: install fonts via homebrew
"
    ;;
esac
