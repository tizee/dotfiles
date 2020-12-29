#!/usr/bin/env zsh

zstyle ':fzf-tab:*' fzf-command fzf # ftb-tmux-popup
zstyle ":completion:*:git-checkout:*" sort false
zstyle ":completion:*:descriptions" format '[%d]'
zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}
# vi/nvim/vim
zstyle ":fzf-tab:complete:vim:*" fzf-preview 'bat --color=always --style=header,grid --line-range :300 $realpath'
# zstyle ":fzf-tab:complete:vim:*" fzf-flags --preview-window=down:3:wrap

zstyle ":fzf-tab:complete:nvim:*" fzf-preview 'bat --color=always --style=header,grid --line-range :300 $realpath'

zstyle ":fzf-tab:complete:vi:*" fzf-preview 'bat --color=always --style=header,grid --line-range :300 $realpath' 


# cd
zstyle ":fzf-tab:complete:cd:*" fzf-preview 'exa -1 --color=always $realpath'
# ps/kill
zstyle ":completion:*:*:*:*:processes" command "ps -u $USER -o pid,user,comm -w -w"
zstyle ":fzf-tab:complete:(kill|ps):argument-rest" fzf-preview \
  '[[ $group == "[process ID]" ]] && ps -p $word -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

# vim:ft=zsh
