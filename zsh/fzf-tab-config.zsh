#!/usr/bin/env zsh

zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup
zstyle ":completion:*:git-checkout:*" sort false
zstyle ":completion:*:descriptions" format '[%d]'
zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
# cd
zstyle ":fzf-tab:complete:cd:*" fzf-preview 'exa -1 --color=always $realpath'
# ps/kill
zstyle ":completion:*:*:*:*:processes" command "ps -u $USER -o pid,user,comm -w -w"
zstyle ":fzf-tab:complete:(kill|ps):argument-rest" fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

# vim:ft=zsh
