#!/usr/bin/env zsh

function fzf::find_pid(){
  # format: <pid> <command-args> <time-start> <tty>
  ps -eo pid,args,lstart,tt | sed 1d | sort -r | fzf -m --height 40%  --border=sharp --preview-window=hidden | awk '{print $1}'
}

function fzf::kill_pid() {
  local pid="$(fzf::find_pid)"
  if [ "x$pid" != "x" ]; then
    echo "continue to kill $pid? (y/n)"
    yn=$(read -e -q yn)
      # [Yy]* ) kill -${1:-9} $pid
    case $yn in
     [Yy]* ) kill -${1:-9} "$pid"
        ;;
      * ) return 0
        ;;
    esac
  fi
}

function fzf::find_file() {
    local given_file="$1"
    #fd --type file --follow --hidden --exclude .git | fzf --query="$given_file"
    fzf --query="$given_file"
}

alias fzfp="fzf::find_pid"
alias fzff="fzf::find_fine"
alias fzfk="fzf::kill_pid"
