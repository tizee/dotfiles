#!/usr/bin/env zsh
# more info
# set -xeuo pipefall

local is_macOS=false
local is_Linux=false
case $SYSTEM in
   Darwin) is_macOS=true ;;
   Linux) is_Linux=true  ;;
esac

# ========== TITLE EXAMPLE ========== {{{
# }}}

# ========== zsh only Alias suffix ========== {{{
if [[ -e '/usr/bin/zsh' ]];then
  alias -s tgz="tar zxvf"
  alias -s {yml,yaml}="nvim"
  alias -s zip="unzip"
  alias -s 7z="7z"
  # alias -g G='| rg -i'
fi
# }}}

# ========== coreutils =========={{{
alias mkdir='mkdir -v'
alias mv='mv -v'
alias cp='cp -v'
alias rm='rm -v'
alias ln='ln -v'
# }}}

alias socks5="http_proxy=socks5://127.0.0.1:7890 https_proxy=socks5://127.0.0.1:7890 all_proxy=socks5://127.0.0.1:7890"
# print clang included header paths
alias printclang="clang++ -E -x c++ - -v < /dev/null"
alias printcc="cc -E -x c++ - -v < /dev/null"
alias printgcc="gcc -x c -v -E /dev/null"
alias printgccld="gcc -Xlinker -v"
alias printgccplus="gcc -x c++ -v -E /dev/null"
alias printld="ld -x -v /dev/null"
# human-friendly and easy to remember
alias fontlist="fc-list : family file"

# ========== pandoc ========== {{{
function __pandocc(){
  if [[ $# -lt 2 ]]; then
    print "pandocc [input file] [output file]"
    print "pandoc use eisvogel's template"
  else
    local input="$1"
    local output="$2"
    shift 2
    pandoc $input -o $output --template eisvogel $@
  fi
}
alias pandocc="__pandocc"
# ========== emacs ========== }}}

# ========== emacs ========== {{{
alias emacsc="emacs -nw"
# ========== emacs ========== }}}

# ========== tar ========== {{{
alias -g tarsee="tar tvf " # list files
# }}}

# ========== Linux ========== {{{
if [ $is_Linux ];then
  # Archlinux
  # TODO: extract to another script file
  if [ -e "/usr/bin/pacman" ];then
    alias pacrm="pacman -Rns $(pacman -Qdtq)"
    alias pacconf="$EDITOR $HOME/.config/"
    # show keymap
  fi
  if [ -e "/usr/bin/apt" ];then
    alias aup="apt update"
    alias asource="source /etc/apt/sources.list"
  fi
  # check available file descriptors
  alias filedescnum="ulimit -n"
fi
# }}}

# ========== macOS ========== {{{

if [ $is_macOS ];then
  if [ -e "/usr/local/bin/brew" ];then
    alias brewup="brew upgrade"
    # disable homebrew auto update
    export HOMEBREW_NO_AUTO_UPDATE=1
    # no more default ctags
    alias ctags="`brew --prefix`/bin/ctags``"
  fi
  # fix audio bust
  alias audiofix="sudo pkill -9 coreaudiod"
  # print CPU number
  alias lcpunum="sysctl -n hw.ncpu"
  alias pcpunum="sysctl -n hw.physicalcpu"
  # print file descripts number
  # https://stackoverflow.com/questions/795236/in-mac-os-x-how-can-i-get-an-accurate-count-of-file-descriptor-usage
  alias printfd="lsof -d '^txt' -p nnn | wc -l"
  alias printmaxfd="launchctl limit maxfiles"
  alias printsdk="xcrun --show-sdk-path"
  alias printosx="xcrun  --sdk macosx --show-sdk-path"
fi

# }}}

# youtube-dl
# ========== youtube-dl========== {{{
alias ydl="youtube-dl"
# }}}

# show key code
alias showhex="xxd -psd"

# ========== headless Chrome ========== {{{
# requires chrome >= 59
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
# alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
# alias chrome-canary="/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary"
# alias chromium="/Applications/Chromium.app/Contents/MacOS/Chromium"
# }}}

# ========== cmake ========== {{{
if [[ -e /usr/local/bin/cmake ]]; then
  # emit compile_commands.json for clangd
  # see https://clang.llvm.org/docs/JSONCompilationDatabase.html
  # and https://clangd.llvm.org/installation.html#project-setup
 alias cmakecld="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 "
  # update: please use bear to generate compile_commands.json for clangd!!!
fi
# }}}

# ========== neovim  ========= {{{
alias vi="nvim"
alias nvi="nvim"
# }}}

# ========== exa ========= {{{
if [[ "$(command -v exa)" ]]; then
  unalias -m 'ls'
  unalias -m 'la'
  unalias -m 'll'
  alias ls="exa -G --color auto -a -s type"
  alias ll='exa -lG --color auto --icons -s type'
  alias la='exa -alG --color auto --icons -s type'
  alias qcd='cd $(exa -lG | fzf)'
fi
# ls
#alias ll="ls -lG"
#alias la="ls -A"
# }}}

# ========== npm/yarn ========== {{{
alias cnpm="npm --registry=https://registry.npm.taobao.org"
alias cyarn="yarn --registry https://registry.npm.taobao.org"
# }}}

# ========== gun pacakge ========== {{{
alias awk="gawk"
# }}}

# ========== ps ========== {{{
alias psall="ps axw -o nice,pid,ppid,user,%mem,%cpu,vsz,wchan,command"
function ps::search(){
  if [[ $(command -v rg) ]]; then
      # ps aux | rg -v rg | rg $@
      ps aux | rg $@
  else
      ps aux | grep $@
      # ps aux | grep -v grep | grep $@
  fi
}

alias pss='ps::search'
# }}}

# ========== less ========== {{{
alias lesswatch='less +F'
# }}}

# ========== tree ========== {{{
# alias mytree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

# }}}

# ========== tmux ========== {{{
alias ttnew="tmux new -s"
alias ttat="tmux attach -t"
alias tm="tmux attach || tmux new"

# }}}

# ========== find ehancement ========== {{{
alias fdsl="fd . -t l -d 1 -H"
alias fdps='ps aux | fd'
# }}}

# ========== fzf ehancement ========== {{{

# open file in new tmux pane
alias fzftm="fzf-tmux" # fzf

# }}}

# ========== sed ========== {{{

# }}}

# ========== awk ========== {{{

# }}}

# ========== ngnix ========== {{{
alias ngistart="nginx"
alias ngireload="nginx -s reload"
alias ngistop="nginx -s stop"

# }}}

# ========== apache http server ========== {{{
alias ahstop="apachectl stop"

# }}}

# ========== Unix Candy ========== {{{
# tty-clock
alias ttclock="tty-clock -csD"
# nyancat
alias nycat="nyancat"
# pipes.sh
alias ppsh="pipes.sh"
# sl
# cowsay
# cowthink
# toilet
# cmatrix

# figlet
alias -g fgfonts="ls ~/.config/figlet"
# Nice
#

# }}}

# ========== golang ========== {{{
alias httpgodoc="godoc -http=:6060"
# }}}

# ========== rust packages ========== {{{
alias lgenerator="license-generator"
alias rustc21="rustc --edition=2021"
alias rustc18="rustc --edition=2018"
alias rustccfg="touch rust_toolchain.toml"
# }}}

# vim:ft=zsh:foldmarker={{{,}}}
