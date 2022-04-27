" set filetype for config files explicitly
augrou CONFIG_FT 
  autocmd! BufRead,BufNewFile *.clang-format,.clang-format set ft=yaml
  autocmd! BufRead,BufNewFile .irbrc,irbrc set ft=ruby
  autocmd! BufRead,BufNewFile .gemrc,gemrc set ft=yaml
augroup END
