fzf::kill_pid() {
  pid=$(ps -ef | sed 1d | fzf -m --height 40%  --border=sharp | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    kill -${1:-9} $pid
  fi
}

fzf::find_file() {
    given_file="$1"
    #fd --type file --follow --hidden --exclude .git | fzf --query="$given_file"
    fzf --query="$given_file"
}

alias fzff="fzf::find_fine"
alias fzfk="fzf::kill_pid"
