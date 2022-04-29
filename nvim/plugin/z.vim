" zcd.nvim
if exists('loaded_z_vim') || v:version < 700
  finish
endif
let g:loaded_z_vim = 1

function! s:add_or_update_entry(entry)
  if len(system("command -v zcd")) == 0
    return
  endif 
  echomsg '[Z.vim]: update ' . a:entry
  let cmd = ['zcd','insert'] + map([a:entry], {_, arg -> shellescape(arg)})
  let res = systemlist(join(cmd))
  " echomsg '[Z.vim]: use ' . join(cmd)
  if v:shell_error
    echohl ErrorMsg | echo join(res,'\n') | echohl None
  endif
endfunction

augroup ZVIM
  autocmd! 
    " autocmd DirChanged * if v:event['changed_window'] | call <SID>add_or_update_entry(v:event['cwd']) | endif
    autocmd DirChanged window,tabpage,global call <SID>add_or_update_entry(expand('<afile>:p'))
augroup END "ZVIM
