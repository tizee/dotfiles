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
# gfw
if $is_Linux; then BREW_TYPE="linuxbrew" ;else BREW_TYPE="homebrew"; fi
export HOMEBREW_BREW_GIT_REMOTE="https://mirrors.tuna.tsinghua.edu.cn/git/homebrew/brew.git"
# Since brew 4.0, homebrew fetch package infos via remote API for better user experience.
# see https://mirrors.tuna.tsinghua.edu.cn/help/homebrew/
export HOMEBREW_API_DOMAIN="https://mirrors.tuna.tsinghua.edu.cn/homebrew-bottles/api"

ubuntu_setup() {
    # Ubuntu uses Linuxbrew
    export HOMEBREW_NO_INSTALL_CLEANUP=1
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    export LINUXBREW_PATH="/home/linuxbrew/.linuxbrew"
    export PATH="/usr/local/bin:$PATH"
    export PATH="/home/linuxbrew/.linuxbrew/bin/:/home/linuxbrew/.linuxbrew/sbin/:$PATH"
    export MANPATH="/home/linuxbrew/.linuxbrew/share/man:$MANPATH"
    export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:$INFOPATH"
    # Please create a new conf file under /etc/ld.so.conf.d to include libraries installed by linuxbrew
    export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-"/usr/lib/wsl/lib"}:$HOME/linuxbrew/.linuxbrew/lib"
    export DYLD_LIBRARY_PATH="/usr/lib:/usr/local/lib:/home/linuxbrew/.linuxbrew/lib"
    # golang modules
    export GOPATH="$HOME/go-projects"
    # go modules
    # Go 1.11 or Go 1.12 default to auto
    # Go >= 1.13 always set to on when outside GOPATH
    # set to off inside GOPATH even there is go.mod
    export GO111MODULE="auto"

    # libxml2
    export PATH="$LINUXBREW_PATH/opt/libxml2/bin:$PATH"
    export LIBXML2_CFLAGS="-I$LINUXBREW_PATH/opt/libxml2/include"
    export LIBXML2_LIBS="-L$LINUXBREW_PATH/opt/libxml2/lib"

    # wsl auto-load ssh key
    eval "$(ssh-agent -s)" >/dev/null
    [ -e "$HOME/.ssh/win_github_id_rsa" ] && $(ssh-add "$HOME/.ssh/win_github_id_rsa" >/dev/null)
    [ -e "$HOME/.ssh/mac_id_rsa" ] && $(ssh-add "$HOME/.ssh/mac_id_rsa" >/dev/null)
}

arch_setup() {
    # golang modules
    export GOPATH="$HOME/go-projects"
    # go modules
    # Go 1.11 or Go 1.12 default to auto
    # Go >= 1.13 always set to on when outside GOPATH
    # set to off inside GOPATH even there is go.mod
    export GO111MODULE="auto"

    # wsl auto-load ssh key
    eval "$(ssh-agent -s)" >/dev/null
    [ -e "$HOME/.ssh/win_github_id_rsa" ] && $(ssh-add "$HOME/.ssh/win_github_id_rsa" >/dev/null)
    [ -e "$HOME/.ssh/win_github_id_rsa" ] && $(ssh-add "$HOME/.ssh/mac_id_rsa" >/dev/null)
}

if command -v lsb_release >/dev/null 2>&1; then
  # Linux
  local distro=$(lsb_release -d | awk '{print $2}')
  export SYSTEM_DISTRO="$distro"
  case $distro in
    Ubuntu)
      ubuntu_setup
      ;;
    Arch)
      arch_setup
      ;;
    *) echo "no configure for $distro"
      ;;
  esac
else
  export SYSTEM_DISTRO="Darwin"
fi

# WSL
if $(uname -r | grep 'microsoft' > /dev/null); then
  echo "WSL $USER at $(date) - $TTY"
  # always use host proxy
  source ~/.config/win_scripts/wsl2/wsl-proxy
  export PATH="$HOME/.config/win_scripts/wsl2:$PATH"
  gpg-agent-relay start
  # use ssh-key from smart-card
  # export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
else
  echo "macOS $USER at $(date) - $TTY"
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


# yarn
# if [[ -d $HOME/.yarn/bin ]]; then
#   PATH="$HOME/.yarn/bin:$PATH"
#   if [[ -d $HOME/.config/yarn/global/node_modules/.bin ]]; then
#   PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"
#   fi
# fi

# deno
if [[ -d $HOME/.deno/bin ]]; then
  PATH="$HOME/.deno/bin:$PATH"
fi
export DENO_INSTALL=$HOME/.deno

# rust cargo
PATH="$HOME/.cargo/bin:$PATH"
export CARGO_HOME="$HOME/.cargo"
export CARGO_NET_GIT_FETCH_WITH_CLI=true
# export RUSTUP_TOOLCHAIN = "stable"
# limit sccache on local disk
export SCCACHE_DIR="/tmp/sccache/"
export SCCACHE_CACHE_SIZE="2G"
# export RUSTUP_UPDATE_ROOT="https://mirrors.tuna.tsinghua.edu.cn/rustup/rustup"
# export RUSTUP_DIST_SERVER="https://mirrors.tuna.tsinghua.edu.cn/rustup"
# export RUSTC_WRAPPER=$(which sccache)

# my shellscripts
PATH="$HOME/.config/bin:$PATH"
# PATH="$HOME/.config/work_bin:$PATH" # work related scripts
# source $HOME/.config/work_bin/work_alias
# terminal
export TERM=xterm-256color
export COLORTERM=truecolor

# replace cat with bat
# if [ "$(command -v bat)" ]; then
  # unalias -m 'cat'
  # alias bat='bat -pp --theme="Nord"'
# fi
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
  ## prevent auto-update whenever run a brew command
  #export HOMEBREW_NO_AUTO_UPDATE=1
  ## Carp - statically typed lisp
  #export CARP_DIR="/usr/local/opt/carp"
  #PATH="$CARP_DIR/bin:$PATH"

  ## mac specific scripts
  PATH="$HOME/.config/mac_scripts:$PATH"
  ## curl
  ## PATH="/usr/local/opt/curl/bin:$PATH"
  ## ruby
  ## export GEM_HOME=$HOME/.gem
  ## PATH="/usr/local/opt/ruby/bin:$PATH"
  #PATH="$PATH:$GEM_HOME/bin"
  ## rvm for ruby
  ## PATH="$HOME/.rvm/bin:$PATH"
  #PATH="$HOME/flutter/bin:$PATH"
  ## flutter dart
  #PATH="$HOME/dev/grepo_dart/flutter/bin:$PATH"
  ## export PUB_HOSTED_URL="https://mirrors.tuna.tsinghua.edu.cn/dart-pub"
  ## export FLUTTER_STORAGE_BASE_URL="https://mirrors.tuna.tsinghua.edu.cn/flutter"
  #export PUB_HOSTED_URL=https://pub.flutter-io.cn
  #export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
  #export FLUTTER_INSTALL="$HOME/dev/grepo_dart/flutter"
  #export FLUTTER_ROOT="$HOME/dev/grepo_dart/flutter"
  ## golang env
  export GOARCH="arm64"
  export GOOS="darwin"
  # https://go.dev/ref/mod#mod-commands
  export GO111MODULE="on"
  export GOPATH="$HOME/projects/gopath"
  export GOBIN="$HOME/projects/gopath/bin"
  ## go bin
  PATH=$HOME/projects/goroot/bin:$PATH
  PATH=$HOME/projects/gopath/bin:$PATH
  ## mame
  #alias mamed='/usr/local/Cellar/mame/0.234/share/mame/mamed'

  ## emacs.app
  PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"

  ## Aseprite.app
  PATH="/Applications/Aseprite.app/Contents/MacOS:$PATH"

  ## plan9port
  # export PLAN9=${PLAN9:-/usr/local/opt/plan9port/libexec}
  ## export PLAN9=$HOME/dev/plan9/plan9port
  ## avoid conflicts with tools under /usr/bin
  ## PATH=$PATH:$PLAN9/bin
  ## texinfo
  #PATH="/usr/local/opt/texinfo/bin:$PATH"
  ## harfbuzz
  #export HARFBUZZ_CFLAGS="-I/usr/local/opt/harfbuzz/include"
  #export HARFBUZZ_LIBS="-L/usr/local/opt/harfbuzz/lib"
  ## freetype
  #export FREETYPE_CFLAGS="-I/usr/local/opt/freetype2/include"
  #export FREETYPE_LIBS="-L/usr/local/opt/freetype2/lib"
  ## ncurses
  PATH="/opt/homebrew/opt/ncurses/bin:$PATH"
  # Need to link header and libs manually
  # build tmux from source in macOS: export PKG_CONFIG_PATH="/opt/homebrew/opt/ncurses/lib/pkgconfig" before ./configure
  # export LIBNCURSES_CFLAGS="-I/opt/homebrew/opt/ncurses/include"
  # it's possible to link libncursers.dylib into /usr/local/lib
  # So gcc should searches via -lncurses to locate the installed one
  # instead of the system's outdated ncurses lib
  # export LIBNCURSES_LIBS="-lncurses"
  # export LIBNCURSES_LIBS="-L/opt/homebrew/opt/ncurses/lib"
  # export CPPFLAGS="-I/opt/homebrew/opt/ncurses/include"
  # export LDFLAGS="-L/opt/homebrew/opt/ncurses/lib"
  ## libevent
  # export LIBEVENT_CFLAGS="-I/usr/local/opt/libevent/include"
  # export LIBEVENT_LIBS="-L/usr/local/opt/libevent/lib"
  # utf8proc
  # export LIBUTF8PROC_CFLAGS="-I/usr/local/opt/utf8proc/include"
  # export LIBUTF8PROC_LIBS="-L/usr/local/opt/utf8proc/lib"
  # haskell
  #[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
  #PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
  ## riscv
  #PATH=$PATH:/usr/local/opt/riscv-gnu-toolchain/bin

  ## git subcommand
  PATH=$PATH:$HOME/.config/git/bin

  ## sdkman
  #export SDKMAN_CANDIDATES_API=https://api.sdkman.io/2
  #export SDKMAN_CANDIDATES_DIR=/Users/tizee/.sdkman/candidates
  #export SDKMAN_DIR=$HOME/.sdkman
  ## /etc/paths.d Tex
  #PATH=/Library/Tex/texbin:$PATH
  ## /etc/paths.d 100-rvictl
  #PATH=/Library/Apple/usr:$PATH
  ## /etc/paths.d VMware fusion
  #PATH="/Applications/VMware\ Fusion.app/Contents/Public:$PATH"
  ## /etc/paths.d Wireshark
  #PATH="/Applications/Wireshark.app/Contents/MacOS:$PATH"
  #PATH="$HOME/dev/go_proj/bin:$PATH"
  ## gnu
  #PATH="/usr/local/opt/make/libexec/gnubin:$PATH"
  #PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"

  ## Android
  ## https://stackoverflow.com/questions/52524112/how-do-i-install-java-on-mac-osx-allowing-version-switching
  #PATH="$HOME/Library/Android/sdk/emulator:$PATH"
  #PATH="$HOME/Library/Android/sdk/platform-tools:$PATH"
  #PATH="$HOME/Library/Android/sdk/tools/bin:$PATH"
  #PATH="$HOME/Library/Android/sdk/tools:$PATH"
  #PATH="/opt/gradle/gradle-6.6/bin:$PATH"
  #export ANDROID_SDK=$HOME/Library/Android/sdk
  #PATH=$ANDROID_SDK/emulator:$ANDROID_SDK/tools:$PATH

  ## mac sdk related
  #alias sdkroot='xcrun --sdk macosx --show-sdk-path'

  #alias java8='export JAVA_HOME=$JAVA_8_HOME'
  ## alias java9='export JAVA_HOME=$JAVA_9_HOME'
  #alias java10='export JAVA_HOME=$JAVA_10_HOME'
  #alias java11='export JAVA_HOME=$JAVA_11_HOME'
  ## alias java12='export JAVA_HOME=$JAVA_12_HOME'
  ## alias java13='export JAVA_HOME=$JAVA_13_HOME'
  ## alias java14='export JAVA_HOME=$JAVA_14_HOME'
  ## alias java15='export JAVA_HOME=$JAVA_15_HOME'
  ## export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
  ## export JAVA_8_HOME=$(/usr/libexec/java_home -v1.8)
  #export JAVA_8_HOME="/Library/Java/JavaVirtualMachines/openjdk-8.jdk/Contents/Home"
  ## export JAVA_9_HOME=$(/usr/libexec/java_home -v9)
  #export JAVA_10_HOME=$(/usr/libexec/java_home -v10)
  #export JAVA_11_HOME=$(/usr/libexec/java_home -v11)
  #export JAVA_18_HOME=$(/usr/libexec/java_home -v18)
  ## export JAVA_12_HOME=$(/usr/libexec/java_home -v12)
  ## export JAVA_13_HOME=$(/usr/libexec/java_home -v13)
  ## export JAVA_14_HOME=$(/usr/libexec/java_home -v14)
  ## export JAVA_15_HOME=$(/usr/libexec/java_home -v15)
  ## PATH="/usr/local/opt/openjdk@8/bin:$PATH"
  #export JAVA_HOME=$JAVA_10_HOME
  ## PATH="$JAVA_HOME/bin:$PATH"
  ## export CLASS_PATH=$JAVA_HOME/lib
  ## export CLASSPATH=$JAVA_HOME/lib

  ## export TOOLCHAINS=swift
  ## PATH=/Library/Developer/Toolchains/swift-latest.xctoolchain/usr/bin:"${PATH}"

  ## sw_vers -productVersion
  ## export MACOSX_DEPLOYMENT_TARGET=11

  ## LuaJIT 2.1
  #PATH="/usr/local/opt/luajit-openresty/bin:$PATH"
  ## llvm
  # To use the bundled libc++ please add the following LDFLAGS:
  # LDFLAGS="-L/opt/homebrew/opt/llvm/lib/c++ -Wl,-rpath,/opt/homebrew/opt/llvm/lib/c++"
  PATH="/opt/homebrew/opt/llvm/bin:$PATH"
  export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
  export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"

  ## ncurses
  #PATH="/usr/local/opt/ncurses/bin:$PATH"

  ## sqlite
  #PATH="/usr/local/opt/sqlite/bin:$PATH"
  #export SQLITE_LIBS="-L/usr/local/opt/sqlite/lib"
  #export SQLITE_CPPFLAGS="-I/usr/local/opt/sqlite/include"


  ## google
  ## PATH="$HOME/dev/grepo_cxx/chromium_depot_tools:$PATH"
  ## vmware
  #PATH="/Applications/VMware Fusion.app/Contents/Public:$PATH"
  ## tex
  #PATH="/Library/TeX/texbin:$PATH"
  #PATH="/Library/Apple/usr/bin:$PATH"
  ## wireshark
  #PATH="/Applications/Wireshark.app/Contents/MacOS:$PATH"
  ## anaconda3
  #PATH="$HOME/anaconda3/bin:$PATH"
  #PATH="$HOME/anaconda3/condabin:$PATH"

  ## openssl
  #PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
  ## rust crate openssl-sys bug
  ## https://stackoverflow.com/questions/49263452/how-do-i-statically-link-the-openssl-sys-crate-into-a-shared-library
  ## https://stackoverflow.com/questions/34612395/openssl-crate-fails-compilation-on-mac-os-x-10-11
  #export OPENSSL_DIR="/usr/local/opt/openssl@1.1"
  #export OPENSSL_LIB_DIR="-L/usr/local/opt/openssl@1.1/lib"
  #export OPENSSL_INCLUDE_DIR="-L/usr/local/opt/openssl@1.1/include"
  ## export OPENSSL_CFLAGS="-I/usr/local/opt/openssl@1.1/include"
  ## export OPENSSL_LIBS="-L/usr/local/opt/openssl@1.1/lib"
  ## export OPENSSL_ROOT_DIR="/usr/local/opt/openssl@1.1"
  ##export OPENSSL_STATIC=1
  ##export LDFLAGS="-L/usr/local/opt/openssl@1.1/lib"
  ##export CPPFLAGS="-I/usr/local/opt/openssl@1.1/include"
  ##export PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig"

  ## gnutls
  #export GUILE_TLS_CERTIFICATE_DIRECTORY="/usr/local/etc/gnutls/"
  #export LIBGNUTLS_CFLAGS="-I/usr/local/opt/gnutls/include"
  #export LIBGNUTLS_LIBS="-L/usr/local/opt/gnutls/lib"

  ## libuv
  #export LIBUV_CFLAGS="-I/usr/local/opt/libuv/include"
  #export LIBUV_LIBS="-L/usr/local/opt/libuv/lib"

  ## lissh2
  #export LIBSSH2_CFLAGS="-I/usr/local/opt/libssh2/include"
  #export LIBSSH2_LIBS="-lssh2"

  ## gmp
  #export LIBGMP_CFLAGS="-I/usr/local/opt/gmp/include"
  #export LIBGMP_LIBS="-L/usr/local/opt/gmp/lib"

  ## linettle
  #export LIBNETTLE_CFLAGS="-I/usr/local/opt/libnettle/include"
  #export LIBNETTLE_LIBS="-L/usr/local/opt/libnettle/lib"

  ## jemalloc
  #export JEMALLOC_CFLAGS="-I/usr/local/opt/jemalloc/include"
  #export JEMALLOC_LIBS="-L/usr/local/opt/jemalloc/lib"

  ## skia
  #export SKIA_LIBRARY="$HOME/deps/skia/out/Release-x64"

  ## ncurses
  #PATH="/usr/local/opt/ncurses/bin:$PATH"

  ## bison
  #PATH="/usr/local/opt/bison/bin:$PATH"
  ##export LDFLAGS="-L/usr/local/opt/bison/lib"

  ## libxml2
  #export PATH="/usr/local/opt/libxml2/bin:$PATH"
  #export LIBXML2_CFLAGS="-I/usr/local/opt/libxml2/include"
  #export LIBXML2_LIBS="-L/usr/local/opt/libxml2/lib"
  ##export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
  ##export LDFLAGS="-L/usr/local/opt/libxml2/lib"
  ##export CPPFLAGS="-I/usr/local/opt/libxml2/include"

  ## lame
  #export LIBMP3LAME_CFLAGS="-I/usr/local/opt/lame/include"
  #export LIBMP3LAME_LIBS="-L/usr/local/lib"

  ## default include path for gcc/clang
  ## export CPLUS_INCLUDE_PATH="$CPLUS_INCLUDE_PATH:Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
  ## xcrun -sdk macosx --show-sdk-path
  #export SDKROOT="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX13.0.sdk"
  ## export DYLD_LIBRARY_PATH="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.3.sdk/usr/lib"
  ## export LDFLAGS="-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
  ## export DYLD_LIBRARY_PATH="/usr/local/lib"

  ## use /usr/lib for Java
  #export DYLD_FALLBACK_LIBRARY_PATH="/usr/local/lib:/usr/lib"
  #export DYLD_LIBRARY_PATH="/usr/lib:/usr/local/lib"
  #export LDFLAGS="-L/usr/local/lib"
  #export CPPFLAGS="-I/usr/local/include"
  ## C/C++ libs
  ## default search paths
  #export C_INCLUDE_PATH="/usr/local/include"
  #export CPLUS_INCLUDE_PATH="/usr/local/include"
  ## static libs e.g. sdl2
  #export LIBRARY_PATH="/usr/local/lib"

  ## expat
  #export LIBEXPAT_LIBS="-L/usr/local/opt/expat/lib"
  #export LIBEXPAT_CFLAGS="-I/usr/local/opt/expat/include"
  ## anaconda3
  ## source $HOME/.config/conda_active.zsh

  ## keyboard setting for vim
  #if [ -f $(command -v kbaware) ]; then
  #  export COLEMAK_KEYBOARD=0
  #  # if test $(kbaware) = 'Colemak'; then
  #  #   export COLEMAK_KEYBOARD=1
  #  # fi
  #fi

  # yubikey
  # export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  # gpgconf --launch gpg-agent
  # hard-coded values of /etc/zprofile

  # pypi
  PATH="$HOME/Library/Python/3.12/bin:$PATH"
  if [[ -f $HOME/.config/python/pypi_personal ]]; then
    source $HOME/.config/python/pypi_personal
  fi

  # bun
  export BUN_INSTALL="/Users/tizee/.bun"
  PATH="$BUN_INSTALL/bin:$PATH"

  # clang address sanitizer
  export ASAN_OPTIONS=detect_leaks=1,symbolize=1
  # https://stackoverflow.com/questions/64126942/malloc-nano-zone-abandoned-due-to-inability-to-preallocate-reserved-vm-space
  export MallocNanoZone=0
  # https://git-scm.com/docs/git/2.35.2#Documentation/git.txt-codeGITCEILINGDIRECTORIEScode
  # Git >=2.35.2
  export GIT_CEILING_DIRECTORIES=/Users

  # nodejs
  # export NODE_PATH="/usr/local/lib/node_modules"
  # roswell for lisp packages
  # PATH="$HOME/.roswell/bin:$PATH"

  # pnpm
  export PNPM_HOME="/Users/tizee/Library/pnpm"
  export PATH="$PNPM_HOME:$PATH"
  # pnpm end

  # homebrew
  eval "$(/opt/homebrew/bin/brew shellenv)"
  # zig lang
  export PATH="$HOME/project-zig/zig/build/stage3/bin:$PATH"
  # figlet font
  export FIGLET_FONTDIR="$HOME/projects/project-doc/figlet-fonts"
  # openFrameworks
  export OF_ROOT="$HOME/projects/project-creative-art/openFrameworks"
  # python llm cli
  export LLM_USER_PATH="$HOME/.config/io.datasette.llm"
  export LLM_PROGRAM="/opt/homebrew/bin/llm"

  # xray-core
  # export XRAY_LOCATION_CONFDIR="$HOME/.config/xray/conf"
fi
