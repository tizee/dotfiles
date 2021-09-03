  augroup ft_yaml
    " Remove all vimrc autocommands
    autocmd!
    autocmd FileType yaml setlocal foldmethod=indent foldlevel=3
  augroup END
