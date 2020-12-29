#!/usr/bin/env zsh

# A quick wrapper for pg_ctl that supports config

if [[ -e /usr/local/var/postgres ]]; then
  alias pgstart="pg_ctl -D /usr/local/var/postgres start"
fi

