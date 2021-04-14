#!/usr/bin/env zsh
# more info
# set -xeuo pipefall
PLATFORM=$(uname -s)

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

alias socks5="http_proxy=socks5://127.0.0.1:1080 https_proxy=socks5://127.0.0.1:1080 all_proxy=socks5://127.0.0.1:1080 "
# print clang included header paths
alias clanginclude="clang++ -E -x c++ - -v < /dev/null"

# ========== tar ========== {{{
alias -g tarsee="tar tvf " # list files
# }}} 

# ========== Linux ========== {{{
if [ "$PLATFORM" = Linux ];then
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

if [ "$PLATFORM" = Darwin ];then
  if [ -e "/usr/local/bin/brew" ];then
    alias brewup="brew upgrade"
    # disable homebrew auto update
    export HOMEBREW_NO_AUTO_UPDATE=1
  fi
  # open man in Preview.app
  function preman() { man -t "$@" | open -f -a "Preview" ;}
  # check SIP
  function sipcheck() {
   csrutil status; 
   csrutil authenticated-root status;
  }
  # disable SIP
  function sipdisable(){
    sudo spctl --master-disable 
  } 
  # print CPU number
  alias lcpunum="sysctl -n hw.ncpu"
  alias pcpunum="sysctl -n hw.physicalcpu"
  # print file descripts number 
  # https://stackoverflow.com/questions/795236/in-mac-os-x-how-can-i-get-an-accurate-count-of-file-descriptor-usage
  alias printfd="lsof -d '^txt' -p nnn | wc -l"
  alias printmaxfd="launchctl limit maxfiles"
fi

# }}} 

# youtube-dl
# ========== youtube-dl========== {{{
alias ydl="youtube-dl"
# }}} 

# quick source .aliases
sozsh() {
  source $(fd --glob '*.sh' "$HOME/.config/zsh" | fzf);
}

# alias 'taro1.3'='/usr/local/lib/node_modules/@tarojs_1.3/cli/bin/taro'

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
 alias cmake="cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 " 
 cmake::cldjson(){
   local directory="build"
   if [[ -n $1 ]]; then
     echo "dir:" $1
     directory=$1
   fi
   local path="$(pwd)/${directory}"
   echo "path:" $path
   if [[ -d $path ]]; then
     # search in build
     if [[ -e $path/compile_commands.json ]]; then
       /usr/local/bin/gln -s $path/compile_commands.json compile_commands.json
       echo "find in $path and link!"
     else
       echo "Not found!"
     fi
   elif [[ -e compile_commands.json ]];then
     # link to parent directory
     /usr/local/bin/gln -s $(pwd)/compile_commands.json $(pwd)/../compile_commands.json
     echo "found in current dir and link!"
  else
     echo "Not found!"
   fi
 }
fi
# }}} 

# ========== neovim  ========= {{{
alias vi="nvim"
alias vim="nvim"
alias nvi="nvim"
# }}}

# ========== exa ========= {{{
if [[ "$(command -v exa)" ]]; then
  unalias -m 'ls'
  unalias -m 'la'
  unalias -m 'll'
  alias ls="exa -G --color auto --icons -a -s type"
  alias ll='exa -lG --color always --icons -s type'
  alias la='exa -alG --color always --icons -s type'
  alias qps='ps aux | fd'
  alias qcd='cd $(exa -lG | fzf)'
fi
# ls
#alias ll="ls -lG"
#alias la="ls -A"
# }}}

# ========== npm/yarn ========== {{{
alias cnpm="npm --registry=https://registry.npm.taobao.org"
alias cyarn="yarn --registry https://registry.npm.taobao.org"
# work
alias pnpm="npm --registry=http://npm.piaoniu.com"
alias pyarn="yarn --registry http://npm.piaoniu.com"
# }}}

# ========== gun pacakge ========== {{{
alias awk="gawk"
alias gpgsk="gpg -k --keyid-format SHORT"
alias gpgexp="gpg --armor --export"

# }}}

# ========== ps ========== {{{
alias psall="ps axw -o nice,pid,ppid,user,%mem,%cpu,vsz,wchan,command"

# }}}

# ========== tree ========== {{{
# alias tree="find . -print | sed -e 's;[^/]*/;|____;g;s;____|; |;g'"

# }}}

# ========== tmux ========== {{{
alias ttnew="tmux new -s"
alias ttat="tmux attach -t"
alias tm="tmux attach || tmux new"

# }}}

# ========== find ehancement ========== {{{
alias fdsl="fd . -t l -d 1 -H"
fdfzf(){
  fd $1 | fzf -m
}

# fd quick edit
fdvi(){
  nvim $(fdfzf $1) 
}

# }}}

# ========== fzf ehancement ========== {{{
# search man with fzf
# usgae: fzfman number
fzfman(){
  let number=${1+$@}
  if [ -n $number ];then
    if [ -e /usr/local/bin/fd ];then
      man $number $(fd --glob "*.$number" "/usr/local/share/man/man$number" | sed -E "s/.*\/(.*)\.$number/\1/" | fzf );
    fi
  fi 
}

# fzf history
# fzf preview template zero-based index
# show history context
fzfh(){
  fzf --preview 'tail +$(( {n} - 2 )) ~/.zsh_history | head -100 | sed -E "s/.*;(.*)/\1/"' < ~/.zsh_history
}

# open file in new tmux pane
alias fzftm="fzf-tmux" # fzf

# }}}

# ========== ripgrep ========== {{{
# ripgrep with fzf
rgfzf() {
  rg $1 -l | fzf --multi
}

# rg quick edit
rgvi(){
  vi "$(rgfzf $1)"
}
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

# figlet
alias -g fgfonts="ls ~/.config/figlet"
# Nice 
#

# }}}

# ========== shell script ========== {{{

# ========== quick editing config file ========= {{{

# shorcut map

# helper
vcfg() {
  if [ -z $1 ]; then
    echo "empty target"
  else
    if [ -e '/usr/bin/vi' ]; then
      local tmp_editor=vi
    fi

    if [ -e '/usr/local/bin/nvim' ]; then
      local tmp_editor=nvim
    elif [ -e '/usr/local/bin/vim' ]; then
      local tmp_editor=vim
    fi

    if [[ -n $tmp_editor ]] && [[ -n $1 ]]; then
      case $1 in
      z|zs|zsh)
        $tmp_editor "$HOME/.zshrc"
        ;;
      vi|vim)
        $tmp_editor "$HOME/.config/nvim/init.vim"
        ;;
      nginx)
        $tmp_editor "/usr/local/etc/nginx/nginx.conf"
        # auto reload
        ngireload
        ;;
      hosts)
        sudo $tmp_editor "/etc/hosts"
        ;;
      ssh)
        $tmp_editor "$HOME/.ssh/config"
        ;;
      sshd)
        sudo $tmp_editor "etc/ssh/sshd_config"
        ;;
      tmcfg)
        $tmp_editor "$HOME/.tmux.conf"
        ;;
      alacritty)
        $tmp_editor "$HOME/.config/alacritty/alacritty.yml"
        ;;
      ass)
        $tmp_editor "$HOME/.config/zsh/enhanced.zsh"
        ;;
      *)
        echo "$1 NOT AVAILABLE"
        ;;
      esac
    elif [[ -n $1 ]]; then
      echo "EDITOR NOT AVAILABLE"
    fi

  fi
}

# }}}

# ========== quick cd ========== {{{
# function for quick cd
#alias repo="cd_wrapper() {cd "~/dev/grepo_$1;unset -f cd_wrapper;}; cd_wrapper"
repo() {
  if [[ -z $1 ]];then
    cd $HOME/dev/$(fd -t d -d 1 'grepo_.*' $HOME/dev --exec basename | fzf);
    local repo_name="$(fd -t d -d 1 . | fzf --preview "tree -L 1 {+1}")"
    if [[ -n $repo_name ]];then
      cd "$repo_name"
    fi
  else
    cd "$HOME/dev/grepo_$1";
    local repo_name="$(fd -t d -d 1 . | fzf --preview "tree -L 1 {+1}")"
    if [[ -n $repo_name ]];then
      cd "$repo_name"
    fi
  fi
}

# cd up multiple times
cdk() {
  cd $(printf "%.s../" $(seq $1));
}

# }}}

# }}}

# ========== homebrew ========== {{{
_fzfbrew(){
# print package info in fzf preview 
if [[ -e /usr/local/bin/brew && -e /usr/local/bin/fzf ]];then
  brew list --formula | fzf --multi --preview 'brew info {+1}' | sed -e 's/[% ]//g'
fi
}
fzfbrew() {
  if [[ $# -eq 0 ]]; then
    _fzfbrew
  elif [[ -n $1 ]];then
    if [[ $1 = 'relink' ]];then
      _fzfbrew | xargs -0 -I {} sh -c 'brew unlink "{}"; brew link "{}";' # 'brew unlink {}; brew link {};'
    fi
  fi
}

# fzfbrew relink

# select packages under /usr/local/Cellar/ relink to /usr/local/bin 

# fzfbrew reinstall


# }}}

# ========== golang ========== {{{
alias httpgodoc="godoc -http=:6060"
# }}}

# vim:ft=zsh:
