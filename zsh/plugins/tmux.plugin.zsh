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
    tmux capture-pane -p -e -J -t $@
  }
fi
