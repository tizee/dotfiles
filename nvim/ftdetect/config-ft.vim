" set filetype for config files explicitly
augrou CONFIG_FT
	autocmd! | " clean previous settings
  autocmd! BufRead,BufNewFile *.clang-format,.clang-format set ft=yaml
  autocmd! BufRead,BufNewFile .irbrc,irbrc set ft=ruby
  autocmd! BufRead,BufNewFile .gemrc,gemrc set ft=yaml
  autocmd! BufRead,BufNewFile *.code-snippets set ft=json
  autocmd! BufRead,BufNewFile *.h set ft=c
  autocmd! BufRead,BufNewFile *.hpp set ft=cpp
augroup END
