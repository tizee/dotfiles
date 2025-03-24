" autogroup for lua files
aug ft_lua
  autocmd!         | " Deletes all auto-commands in the current group
  autocmd BufNewFile,BufRead *.lua setlocal foldmethod=syntax foldlevel=3 ts=2 sts=0 sw=0 noexpandtab
  autocmd FileType lua setlocal foldmethod=syntax foldlevel=3 ts=2 sts=0 sw=0 noexpandtab
aug END

