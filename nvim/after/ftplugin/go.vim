" autogroup for go files
aug ft_golang
  autocmd!         | " Deletes all auto-commands in the current group
  autocmd FileType go setlocal ts=2 sts=0 sw=0 noexpandtab
  autocmd BufNewFile,BufRead *.go setlocal foldmethod=syntax foldlevel=3
aug END
