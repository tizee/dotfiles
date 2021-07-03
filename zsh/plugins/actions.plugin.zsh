#!/usr/bin/env zsh

# my predefined actions

function vcfg() {
  local help
  zparseopts -E -D -help=help h=help || exit 1

  if [[ -n "$help" ]]; then
    echo >&2 "vcfg [targets]"
    echo "zsh   - .zshrc"
    echo "vim   - nvim/init.vim"
    echo "nginx - nginx.conf"
    echo "hosts - /etc/hosts"
    echo "ssh   - .ssh/config"
    echo "sshd  - /etc/ssh/sshd_config"
    echo "tmux  - .tmux.conf"
  elif [ -z $1 ]; then
    echo "empty target"
  else
    if [ -e '/usr/bin/vi' ]; then
      local tmp_editor=vi
    fi

    if [ -e '/usr/local/bin/nvim' ]; then
      local tmp_editor=nvim
    elif [ -e '/usr/local/bin/vim' ]; then
      local tmp_editor=vim
    fi

    if [[ -n $tmp_editor ]] && [[ -n $1 ]]; then
      case $1 in
      z|zs|zsh)
        $tmp_editor "$HOME/.zshrc"
        ;;
      vi|vim)
        $tmp_editor "$HOME/.config/nvim/init.vim"
        ;;
      nginx)
        $tmp_editor "/usr/local/etc/nginx/nginx.conf"
        # auto reload
        ngireload
        ;;
      hosts)
        sudo $tmp_editor "/etc/hosts"
        ;;
      ssh)
        $tmp_editor "$HOME/.ssh/config"
        ;;
      sshd)
        sudo $tmp_editor "/etc/ssh/sshd_config"
        ;;
      tmux)
        $tmp_editor "$HOME/.tmux.conf"
        ;;
      *)
        echo "$1 NOT AVAILABLE"
        ;;
      esac
    elif [[ -n $1 ]]; then
      echo "EDITOR NOT AVAILABLE"
    fi
  fi

}

# vim:fmr={,}:foldmethod=marker
