#!/usr/bin/env zsh

local is_macOS=false
local is_Linux=false

case $SYSTEM in
  Darwin) is_macOS=true ;;
  Linux) is_Linux=true ;;
esac

if $is_macOS; then
  function hostip() {
    ifconfig | rg 'inet ' | rg -Fv '127.0.0.1' | awk '{print $2}' 
  }
fi

if $is_Linux; then
  function hostip() {
    ip address | rg 'inet ' | rg -Fv '127.0.0.1' | awk '{print $2}' 
  }
fi
