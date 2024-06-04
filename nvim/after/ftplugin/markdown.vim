set expandtab
set cole=0
set tabstop=2
set softtabstop=4

augroup ft_markdown
  autocmd! |
  " show all text
  autocmd BufNewFile,BufRead *.md set expandtab cole=0 tabstop=2 softtabstop=4
augroup END
