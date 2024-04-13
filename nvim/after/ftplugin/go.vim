" autogroup for go files
aug ft_golang
  autocmd!         | " Deletes all auto-commands in the current group
  autocmd BufNewFile,BufRead *.go setlocal foldmethod=syntax foldlevel=3 ts=2 sts=0 sw=0 noexpandtab
  autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')
  autocmd FileType go autocmd BufWritePre <buffer> Format
  autocmd FileType go nmap gtj :CocCommand go.tags.add json<cr>
  autocmd FileType go nmap gty :CocCommand go.tags.add yaml<cr>
  autocmd FileType go nmap gtx :CocCommand go.tags.clear<cr>
aug END

