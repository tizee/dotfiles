#!/usr/bin/env zsh

# more info
# set -xeuo pipefall

PLATFORM=$(uname -s)

# quick source .aliases
sozsh() {
  source $(fd --glob '*.sh' "$HOME/.config/tz-zshconfig" | fzf);
}

alias 'taro1.3'='/usr/local/lib/node_modules/@tarojs_1.3/cli/bin/taro'

# show key code
alias showhex="xxd -psd"

# ========== directory shortcuts ========== {{{
# could use autojump
alias dev="cd ~/dev/"
alias work="cd ~/dev/work/"
alias play="cd ~/dev/playground/"
alias blog="cd ~/dev/SideProject/BlogPosts/"
alias side="cd ~/dev/SideProject/"
alias ohmyzsh="cd ~/.oh-my-zsh"

# }}}

# ========== neovim  ========= {{{
alias vi="nvim"
alias vim="nvim"
alias nvi="nvim"
# }}}

# ========== exa ========= {{{
alias ls="exa"
alias ll='exa -lG'
alias la='exa -al'
alias qps='ps aux | fd'
alias qcd='cd $(exa -lG | fzf)'
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
alias gpg="gpg2"
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

# ========== git ========= {{{
# test github
alias gittest="ssh -T git@github.com"

# git checkout
alias gcob="git checkout -b"

# list remote branch
alias gbr="git branch -r"
alias gurl="git remote -v"

fzfgb(){
  git branch | sed -E 's/\*//'| fzf | sed -E 's/ //g'
}

fzfgshow(){
  if [[ -n $1 ]];then
    git log $1 --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' | sed  | 2>/dev/null
  else
    # Use current branch ?
    branch=$(fzfgb)
    if [[ -n $branch ]]; then
      git log $branch --pretty="%h %cn %s" | fzf --multi --preview 'git show {+1}' | awk '{print $1}' 2>/dev/null
    fi
  fi
}

# git cherry-pick with fzf
fzfgcp(){
  if [ -n $1 ];then
    git cherry-pick -e $(fzfgshow $1)
  else
    git cherry-pick -e $(fzfgshow)
  fi
}

# list unstaged files
alias gdn="git diff --name-only"
# list only staged files
alias gdns="git diff --name-only --staged"
# list both unstaged and staged files
alias gst="git status --porcelain | sed 's/^...//'"
# only diff modifled files
alias gstdiffm="gdn | xargs git diff"
# diff all files that git status given
alias gstdiff="gst | xargs git diff"
# list only untracked files
alias gls="git ls-files --others --exclude-standard"

# git diff with fzf
gdfzf() {
  preview="git diff $@ --color=always -- {-1}"
  git diff $@ --name-only | fzf -m --ansi --preview $preview
}
# checkout with fzf
alias gco="git branch | fzf | git checkout"

# }}}

# Unix {{{
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
fi

# macos
if [ "$PLATFORM" = Darwin ];then
  if [ -e "/usr/local/bin/brew" ];then
    alias brewup="brew upgrade"
  fi
  # man preview
  function preman() { man -t "$@" | open -f -a "Preview" ;}
fi
# }}}

# File search

# find
# ========== find ehancement ========== {{{
alias fdsl="fd . -t l -d 1 -H"
fdfzf(){
  fd $1 | fzf
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
  vi $(rgfzf $1)    
}
# }}}

# ========== sed ========== {{{

# }}}

# ========== awk ========== {{{

# }}}

# Sever

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
# figlet
# toilet

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
        $tmp_editor "$HOME/.config/zsh/ehanced.zsh"
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
    local repo_name=$(fd -t d -d 1 . | fzf --preview "tree -L 1 {+1} | lolcat")
    if [[ -n $repo_name ]];then
      cd "$repo_name"
    fi
  else
    local path="$HOME/dev/grepo_$1";
    local repo_name=$(fd -t d -d 1 . | fzf --preview "tree -L 1 {+1} | lolcat")
    if [[ -n $repo_name ]];then
      cd $path/"$repo_name"
    else
      cd $path
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
_fzfbrew() {
 if [[ -e /usr/local/bin/brew && -e /usr/local/bin/fzf ]]; then
   brew list --formula | fzf --multi --preview 'brew info {+1}' | sed -e 's/[% ]//g'
fi
}

fzfbrew() {
 # print package info in fzf preview 
    if [[ -n $1 ]]; then
      if [[ $1="relink" ]]; then
        $(_fzfbrew) | xargs -0 -I {} sh -c 'echo {} && echo {}22'
      fi
    fi
}

# fzfbrew relink

# select packages under /usr/local/Cellar/ relink to /usr/local/bin 

# fzfbrew reinstall


# }}}
