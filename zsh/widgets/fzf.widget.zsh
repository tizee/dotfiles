#!/usr/bin/zsh

# use fzf to kill proccesses
fzfkill_widget() {
   (command -v fzf::kill_pid) && fzf::kill_pid
   zle reset-prompt
}

zle -N fzf-redraw-prompt
zle -N fzfkill_widget
bindkey '^k'  fzfkill_widget #ctrl+k
