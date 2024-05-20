#!/usr/bin/env zsh

# cd to given path use only one simple letter
function tp(){
  if [[ $SYSTEM = Darwin ]];then
    case "$1" in
      p) cd "$HOME/projects"
        ;;
      n) cd "$HOME/Documents/obsidian"
        ;;
    esac
  fi
}
