#!/usr/bin/env zsh

function resethomebrew(){
  git -C "$(brew --repo)" remote set-url origin https://github.com/Homebrew/brew.git
}
if [[ $SYSTEM = Darwin ]]; then
  # macOS
  function brewreset() {
    resethomebrew || exit $?
    BREW_TAPS="$(brew tap)"
    for tap in core cask{,-fonts,-drivers,-versions}; do
        if echo "$BREW_TAPS" | grep -qE "^homebrew/${tap}\$"; then
            git -C "$(brew --repo homebrew/${tap})" remote set-url origin https://github.com/Homebrew/homebrew-${tap}.git
        fi
    done
    brew update-reset
  }
elif [[ $SYSTEM = Linux ]]; then
  function brewreset() {
    resethomebrew || exit $?
    git -C "$(brew --repo homebrew/core)" remote set-url origin https://github.com/Homebrew/linuxbrew-core.git
    brew update-reset
    }
fi

