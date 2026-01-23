#!/usr/bin/env zsh

if command -v tmux &>/dev/null; then
  ## wrapper of tmux commands

  # list
  function tmux_plugin::list(){
    tmux ls
  }

  # attach-session
  function tmux_plugin::attach(){
    tmux attach $@
  }

  # detach-client
  function tmux_plugin::detach(){
    tmux detach $@
  }

  # list clients
  function tmux_plugin::listClients(){
    tmux list-clients $@
  }

  # list commands
  function tmux_plugin::listCommands(){
    tmux list-commands $@
  }

  # list sessions
  function tmux_plugin::listSessions(){
    if [[ $# -lt 0 ]]; then
      tmux list-sessions $@
    else
      tmux list-sessions -F "#{session_name} #{session_path} #{session_windows} windows"
    fi
  }

  # list-panes
  function tmux_plugin::listPanes(){
    if [[ $# -lt 0 ]]; then
      tmux list-panes $@
    else
      tmux list-panes -F "#{pane_title} #{pane_tty}"
    fi
  }

  # list existing sessions
  # select with fzf
  # create new session if there is no such session
  # idea from https://github.com/Junnplus/blog/issues/48
  function tmux_plugin::fzfWorkspace(){
    if command -v fzf &>/dev/null; then
      local target_session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --print-query --height=5 --preview-window=0:noborder)
      if tmux has-session "$target_session" &>/dev/null; then
        tmux attach -t "$target_session"
      else
        tmux new -A -s "$target_session"
      fi
    else
      echo "fzf is not installed!" && exit 1
    fi
  }
  alias tmuxwork='tmux_plugin::fzfWorkspace'

  # has-session
  function tmux_plugin::hasSession(){
    # tmux has -t $@
    tmux has-session -t $@
  }

  # e.g tmux show -s terminal-overrides
  function tmux_plugin::showVariables(){
    tmux show -s $@
  }

  function tmuxrawcapture() {
      local target="%0"
      local start="-"
      local end="-"
      local output=""

      # 第一个非选项参数作为target
      if [[ $# -gt 0 ]] && [[ ! "$1" =~ ^- ]]; then
          target="$1"
          shift
      fi

      # 解析选项参数
      while [[ $# -gt 0 ]]; do
          case $1 in
              -s) start="$2"; shift 2 ;;
              -e) end="$2"; shift 2 ;;
              -o) output="$2"; shift 2 ;;
              *) shift ;;
          esac
      done

      # 获取history大小
      local history_size=$(tmux display-message -p -t "$target" '#{history_size}')

      # 百分比转换
      if [[ "$start" =~ ^[0-9]+%$ ]]; then
          local percent=${start%\%}
          start=$(( -history_size * percent / 100 ))
      fi

      if [[ "$end" =~ ^[0-9]+%$ ]]; then
          local percent=${end%\%}
          end=$(( -history_size * percent / 100 ))
      fi

      # 执行捕获
      if [[ -n "$output" ]]; then
          tmux capture-pane -p -e -J -S "$start" -E "$end" -t "$target" > "$output"
          echo "saved to: $output" >&2
      else
          tmux capture-pane -p -e -J -S "$start" -E "$end" -t "$target"
      fi
  }

fi
