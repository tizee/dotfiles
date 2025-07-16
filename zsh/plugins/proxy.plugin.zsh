#!/usr/bin/env zsh
# Proxy configuration and related utilities

# ========== Proxy Settings ==========
alias proxy="http_proxy=http://127.0.0.1:1080 https_proxy=http://127.0.0.1:1080 socks5_proxy=socks5://127.0.0.1:1080 all_proxy=http://127.0.0.1:1080"

# Toggle proxy settings
proxytoggle() {
  if [ -z "$http_proxy" ]; then
    # Enable proxies
    export http_proxy=http://127.0.0.1:1080
    export https_proxy=http://127.0.0.1:1080
    export socks5_proxy=socks5://127.0.0.1:1080
    export all_proxy=socks5://127.0.0.1:1080
    echo "Proxy enabled"
  else
    # Disable proxies
    unset http_proxy
    unset https_proxy
    unset socks5_proxy
    unset all_proxy
    echo "Proxy disabled"
  fi
}

# Claude Code over local proxy server
alias pcc="ANTHROPIC_BASE_URL=http://127.0.0.1:8082 claude"

# vim:ft=zsh:foldmarker={{{,}}}