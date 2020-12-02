# system
PATH="/usr/bin:$PATH"
PATH="/bin:$PATH"
PATH="/usr/sbin:$PATH"
PATH="/sbin:$PATH"
PATH="/usr/local/sbin:$PATH"
# PATH enhanced
PATH="/usr/local/bin:$HOME/.local/bin:$PATH"
PATH="/usr/local/sbin:$PATH"
# gnu
PATH="/usr/local/opt/make/libexec/gnubin:$PATH"
# curl
PATH="/usr/local/opt/curl/bin:$PATH"
# rust cargo
PATH="$HOME/.cargo/bin:$PATH"
RUSTUP_DIST_SERVER=https://mirrors.tuna.tsinghua.edu.cn/rustup
# ruby
PATH="/usr/local/opt/ruby/bin:$PATH"
PATH="$HOME/.rvm/bin:$PATH"
# yarn
PATH="$HOME/.yarn/bin:$PATH"
PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"
# flutter dart
PATH="$HOME/dev/grepo_dart/flutter/bin:$PATH"
PATH="$HOME/flutter/bin:$PATH"
PUB_HOSTED_URL=https://mirrors.tuna.tsinghua.edu.cn/dart-pub
Flutter_INSTALL=$HOME/dev/grepo_dart/flutter
FLUTTER_STORAGE_BASE_URL=https://mirrors.tuna.tsinghua.edu.cn/flutter
# deno
PATH="$HOME/.deno/bin:$PATH"
# go
PATH="/usr/local/go/bin:$PATH"
PATH="$HOME/dev/go_proj/bin:$PATH"
# llvm
PATH="/usr/local/opt/llvm/bin:$PATH"
# openssl
PATH="/usr/local/opt/openssl@1.1/bin:$PATH"

# Android
# https://stackoverflow.com/questions/52524112/how-do-i-install-java-on-mac-osx-allowing-version-switching
PATH="$HOME/Library/Android/sdk/emulator:$PATH"
PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
PATH="$HOME/Library/Android/sdk/tools/bin:$PATH"
PATH="$HOME/Library/Android/sdk/tools:$PATH"
PATH="/opt/gradle/gradle-6.6/bin:$PATH"

export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
export JAVA_9_HOME=$(/usr/libexec/java_home -v9)
export JAVA_10_HOME=$(/usr/libexec/java_home -v10)
export JAVA_11_HOME=$(/usr/libexec/java_home -v11)
export JAVA_12_HOME=$(/usr/libexec/java_home -v12)
export JAVA_13_HOME=$(/usr/libexec/java_home -v13)
export JAVA_14_HOME=$(/usr/libexec/java_home -v14)
export JAVA_15_HOME=$(/usr/libexec/java_home -v15)
export JAVA_HOME=$JAVA_8_HOME

alias java8='export JAVA_HOME=$JAVA_8_HOME'
alias java9='export JAVA_HOME=$JAVA_9_HOME'
alias java10='export JAVA_HOME=$JAVA_10_HOME'
alias java11='export JAVA_HOME=$JAVA_11_HOME'
alias java12='export JAVA_HOME=$JAVA_12_HOME'
alias java13='export JAVA_HOME=$JAVA_13_HOME'
alias java14='export JAVA_HOME=$JAVA_14_HOME'
alias java15='export JAVA_HOME=$JAVA_15_HOME'

# google 
PATH="$HOME/dev/grepo_cxx/chromium_depot_tools:$PATH"
# vmware
PATH="/Applications/VMware Fusion.app/Contents/Public:$PATH"
# tex
PATH="/Library/TeX/texbin:$PATH"
PATH="/Library/Apple/usr/bin:$PATH"
# wireshark
PATH="/Applications/Wireshark.app/Contents/MacOS:$PATH"
# anaconda3
PATH="$HOME/anaconda3/bin:$PATH"
PATH="$HOME/anaconda3/condabin:$PATH"

# sdkman
SDKMAN_CANDIDATES_API=https://api.sdkman.io/2
SDKMAN_CANDIDATES_DIR=/Users/tizee/.sdkman/candidates
SDKMAN_DIR=$HOME/.sdkman

# terminal
TERM=xterm-256color
COLORTERM=truecolor
LANG=en_US.UTF-8
LC_CTYPE=en_US.UTF-8

MANPAGER="sh -c 'col -bx | bat -l man -p'"
DENO_INSTALL=$HOME/.deno

# fzf
FZF_COMPLETION_TRIGGER=**
FZF_DEFAULT_COMMAND="fd --exclude={.git,.idea,.vscode,.sass-cache,node_modules,build} --type f"
FZF_DEFAULT_OPTS="--reverse --ansi --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {} 2> /dev/null'"

# my zsh config
PATH="$HOME/.config/bin:$PATH"
source $HOME/.config/zsh/kiriline.zsh-theme
source $HOME/.config/zsh/config

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/tizee/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/tizee/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/tizee/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/tizee/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

