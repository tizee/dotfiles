if exists('loaded_z_vim') || v:version < 700
  finish
endif
let g:loaded_z_vim = 1

function! s:add_or_update_entry(entry)
  if !len(system("command -v zdb"))
    return
  endif 
  echo '[Z.vim]: update ' . a:entry
  let cmd = ['zdb','add'] + map(copy(a:entry), {_, arg -> shellescape(arg)})
  let res = systemlist(join(cmd))
  if v:shell_error
    echohl ErrorMsg | echo join(res,'\n') | echohl None
  endif
endfunction

augroup ZVIM
  autocmd! 
  if has('nvim')
    autocmd DirChanged * if v:event['changed_window'] | call <SID>add_or_update_entry(v:event['cwd']) | endif
  else
    autocmd DirChanged window,tabpage call <SID>add_or_update_entry(expand('<afile>'))
  endif 
augroup END "ZVIM
