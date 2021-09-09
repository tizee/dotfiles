#!/usr/bin/env zsh
# vim:fmr=<-,->:ft=zsh:fdm=marker

# language
if [[ -z "$LANG" ]]; then
  export LANG=en_US.UTF-8
  export LANGUAGE=en_US.UTF-8
fi
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LESSCHARSET=utf-8

local is_macOS=false
local is_Linux=false
case $SYSTEM in
  Darwin) is_macOS=true;;
  Linux) is_Linux=true;;
esac

# ========== 
# ENV
# ========== 


# homebrew
if [[ -n "$HOMEBREW_MIRROR_CN" ]]; then
  if $is_Linux; then BREW_TYPE="linuxbrew"; else BREW_TYPE="homebrew"; fi
  export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git"
  export HOMEBREW_CORE_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/${BREW_TYPE}-core.git"
  export HOMEBREW_BOTTLE_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/${BREW_TYPE}-bottles"
fi

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='nvim'
else
  export EDITOR='nvim'
fi

# Proxy for fucking GFW
# gitclonet/sshtunnel
export GITSSH_PORT=9999
export GPG_TTY=$(tty)

# https://github.com/sorin-ionescu/prezto/blob/master/runcoms/zshenv
# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

# Common <-
# system
path=(
  /usr/local/{bin,sbin}  
  /usr/{bin,sbin}
  /{bin,sbin}
  $HOME/.local/bin
  $path
)

# C/C++ libs
export C_INCLUDE_PATH="/usr/local/include"
export CPLUS_INCLUDE_PATH="/usr/local/include"
# static libs e.g. sdl2
export LIBRARY_PATH="$LIBRARY_PATH:/usr/local/lib"

# yarn
PATH="$HOME/.yarn/bin:$PATH"
PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# deno
PATH="$HOME/.deno/bin:$PATH"
export DENO_INSTALL=$HOME/.deno

# rust cargo
PATH="$HOME/.cargo/bin:$PATH"
export CARGO_HOME="$HOME/.cargo"
export RUSTUP_DIST_SERVER="https://mirrors.tuna.tsinghua.edu.cn/rustup"
export CARGO_NET_GIT_FETCH_WITH_CLI=true
# export RUSTC_WRAPPER=$(which sccache)
# curl
PATH="/usr/local/opt/curl/bin:$PATH"
# ruby
PATH="/usr/local/opt/ruby/bin:$PATH"
PATH="$HOME/.rvm/bin:$PATH"
export GEM_HOME=$HOME/.gem
PATH=$GEM_HOME/bin:$PATH
# flutter dart
PATH="$HOME/dev/grepo_dart/flutter/bin:$PATH"
PATH="$HOME/flutter/bin:$PATH"
# export PUB_HOSTED_URL="https://mirrors.tuna.tsinghua.edu.cn/dart-pub"
# export FLUTTER_STORAGE_BASE_URL="https://mirrors.tuna.tsinghua.edu.cn/flutter"
export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
export FLUTTER_INSTALL="$HOME/dev/grepo_dart/flutter"
export FLUTTER_ROOT="$HOME/dev/grepo_dart/flutter"
# golang
# go modules
export GO111MODULE="on"
PATH=/usr/local/go/bin:$PATH

# my shellscripts
PATH="$HOME/.config/bin:$PATH"
# PATH="$HOME/.config/work_bin:$PATH" # work related scripts
source $HOME/.config/work_bin/work_alias
# terminal
export TERM=xterm-256color
export COLORTERM=truecolor

# replace cat with bat
if [ "$(command -v bat)" ]; then
  unalias -m 'cat'
  alias cat='bat -pp --theme="Nord"'
fi
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# fzf
export FZF_COMPLETION_TRIGGER="**"
export FZF_DEFAULT_COMMAND="fd --exclude={.git,.idea,.vscode,.sass-cache,node_modules,build} --type f"
export FZF_DEFAULT_OPTS="--reverse --ansi --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {} 2> /dev/null' --bind ctrl-c:select-all"

# krb5
PATH="/usr/local/opt/krb5/bin:$PATH"
PATH="/usr/local/opt/krb5/sbin:$PATH"
# ->

# macOS <-
if $is_macOS; then
  # emacs
  PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"
  # plan9port
  export PLAN9=$HOME/dev/plan9/plan9port
  PATH=$PATH:$PLAN9/bin
  # texinfo
  PATH="/usr/local/opt/texinfo/bin:$PATH"
  # harfbuzz
  export HARFBUZZ_CFLAGS="-I/usr/local/opt/harfbuzz/include/harfbuzz"
  export HARFBUZZ_LIBS="-L/usr/local/opt/harfbuzz/lib"
  # freetype
  # export FREETYPE_CFLAGS="-I/usr/local/opt/freetype2/include/freetype2" 
  # haskell
  [ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
  PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
  # riscv
  PATH=$PATH:/usr/local/opt/riscv-gnu-toolchain/bin
  # git-bin
  PATH=$PATH:/Users/tizee/.config/git/bin
  # sdkman
  export SDKMAN_CANDIDATES_API=https://api.sdkman.io/2
  export SDKMAN_CANDIDATES_DIR=/Users/tizee/.sdkman/candidates
  export SDKMAN_DIR=$HOME/.sdkman
  # /etc/paths.d Tex
  PATH=/Library/Tex/texbin:$PATH
  # /etc/paths.d 100-rvictl
  PATH=/Library/Apple/usr:$PATH
  # /etc/paths.d VMware fusion
  PATH="/Applications/VMware\ Fusion.app/Contents/Public:$PATH"
  # /etc/paths.d Wireshark
  PATH="/Applications/Wireshark.app/Contents/MacOS:$PATH"
  # golang modules
  export GOPATH="$HOME/dev/go_proj"
  PATH="$HOME/dev/go_proj/bin:$PATH"
    # gnu
    PATH="/usr/local/opt/make/libexec/gnubin:$PATH"
    PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
  # Android
  # https://stackoverflow.com/questions/52524112/how-do-i-install-java-on-mac-osx-allowing-version-switching
  PATH="$HOME/Library/Android/sdk/emulator:$PATH"
  PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
  PATH="$HOME/Library/Android/sdk/tools/bin:$PATH"
  PATH="$HOME/Library/Android/sdk/tools:$PATH"
  PATH="/opt/gradle/gradle-6.6/bin:$PATH"
  export ANDROID_SDK=$HOME/Library/Android/sdk
  PATH=$ANDROID_SDK/emulator:$ANDROID_SDK/tools:$PATH


  alias java8='export JAVA_HOME=$JAVA_8_HOME'
  # alias java9='export JAVA_HOME=$JAVA_9_HOME'
  # alias java10='export JAVA_HOME=$JAVA_10_HOME'
  # alias java11='export JAVA_HOME=$JAVA_11_HOME'
  # alias java12='export JAVA_HOME=$JAVA_12_HOME'
  # alias java13='export JAVA_HOME=$JAVA_13_HOME'
  # alias java14='export JAVA_HOME=$JAVA_14_HOME'
  # alias java15='export JAVA_HOME=$JAVA_15_HOME'
  # export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
  # export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
  export JAVA_8_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_261.jdk/Contents/Home"
  # export JAVA_9_HOME=$(/usr/libexec/java_home -v9)
  # export JAVA_10_HOME=$(/usr/libexec/java_home -v10)
  # export JAVA_11_HOME=$(/usr/libexec/java_home -v11)
  # export JAVA_12_HOME=$(/usr/libexec/java_home -v12)
  # export JAVA_13_HOME=$(/usr/libexec/java_home -v13)
  # export JAVA_14_HOME=$(/usr/libexec/java_home -v14)
  # export JAVA_15_HOME=$(/usr/libexec/java_home -v15)
  export JAVA_HOME=$JAVA_8_HOME
  # export TOOLCHAINS=swift
  # PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"${PATH}"
  # sw_vers -productVersion 
  export MACOSX_DEPLOYMENT_TARGET=11
  # prevent auto-update whenever run a brew command
  export HOMEBREW_NO_AUTO_UPDATE=1
  # LuaJIT 2.1
  PATH="/usr/local/opt/luajit-openresty/bin:$PATH"
  # llvm
  PATH="/usr/local/opt/llvm/bin:$PATH"
  #export LDFLAGS="-L/usr/local/opt/llvm/lib"
  #export CPPFLAGS="-I/usr/local/opt/llvm/include"

  # ncurses
  PATH="/usr/local/opt/ncurses/bin:$PATH"

  # sqlite
  PATH="/usr/local/opt/sqlite/bin:$PATH"
  export SQLITE_LIBS="-L/usr/local/opt/sqlite/lib"
  export SQLITE_CPPFLAGS="-I/usr/local/opt/sqlite/include"


  # google 
  # PATH="$HOME/dev/grepo_cxx/chromium_depot_tools:$PATH"
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

  # openssl
  PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
  export OPENSSL_CFLAGS="-I/usr/local/opt/openssl@1.1/include"
  export OPENSSL_LIBS="-L/usr/local/opt/openssl@1.1/lib"
  #export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
  #export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"
  #export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

  # gnutls
  export GUILE_TLS_CERTIFICATE_DIRECTORY="/usr/local/etc/gnutls/"
  export LIBGNUTLS_CFLAGS="-I/usr/local/opt/gnutls/include"
  export LIBGNUTLS_LIBS="-L/usr/local/opt/gnutls/lib"

  # libuv
  export LIBUV_CFLAGS="-I/usr/local/opt/libuv/include"
  export LIBUV_LIBS="-L/usr/local/opt/libuv/lib"

  # lissh2
  export LIBSSH2_CFLAGS="-I/usr/local/opt/libssh2/include"
  export LIBSSH2_LIBS="-lssh2"

  # gmp
  export LIBGMP_CFLAGS="-I/usr/local/opt/gmp/include"
  export LIBGMP_LIBS="-L/usr/local/opt/gmp/lib"

  # linettle
  export LIBNETTLE_CFLAGS="-I/usr/local/opt/libnettle/include"
  export LIBNETTLE_LIBS="-L/usr/local/opt/libnettle/lib"

  # jemalloc
  export JEMALLOC_CFLAGS="-I/usr/local/opt/jemalloc/include"
  export JEMALLOC_LIBS="-L/usr/local/opt/jemalloc/lib"

  # skia
  export SKIA_LIBRARY="$HOME/deps/skia/out/Release-x64"

  # ncurses
  PATH="/usr/local/opt/ncurses/bin:$PATH"

  # bison
  PATH="/usr/local/opt/bison/bin:$PATH"
  #export LDFLAGS="-L/usr/local/opt/bison/lib"

  # libxml2
  export PATH="/usr/local/opt/libxml2/bin:$PATH"
  export LIBXML2_CFLAGS="-I/usr/local/opt/libxml2/include"
  export LIBXML2_LIBS="-L/usr/local/opt/libxml2/lib"
  #export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
  #export LDFLAGS="-L/usr/local/opt/libxml2/lib"
  #export CPPFLAGS="-I/usr/local/opt/libxml2/include"

  # lame
  export LIBMP3LAME_CFLAGS="-I/usr/local/opt/lame/include"
  export LIBMP3LAME_LIBS="-L/usr/local/lib"

  # default include path for gcc/clang 
  export CPLUS_INCLUDE_PATH="$CPLUS_INCLUDE_PATH:Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
  # xcrun -sdk macosx --show-sdk-path
  export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.3.sdk"
  export DYLD_LIBRARY_PATH="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.3.sdk/usr/lib"
  export LDFLAGS="-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
  # export LDFLAGS="-L/usr/local/lib"
  # export CPPFLAGS="-I/usr/local/include"
  #
  # expat
  export LIBEXPAT_LIBS="-L/usr/local/opt/expat/lib"
  export LIBEXPAT_CFLAGS="-I/usr/local/opt/expat/include"
    # anaconda3
    # source $HOME/.config/conda_active.zsh
    # keyboard setting for vim
    # if test $(kbaware) = 'Colemak'; then
    #   export COLEMAK_KEYBOARD=1
    # else
    #   export COLEMAK_KEYBOARD=0
    # fi
    
    # hard-coded values of /etc/zprofile


fi
# ->

