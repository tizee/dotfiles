" File: astyle.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: astyle config resolver plugin based on find-root.vim

if exists('loaded_astyle_vim')
  finish
endif
let g:loaded_astyle_vim = 1
let g:astyle_auto_disabled = get(g:,"astyle_auto_disabled", 0)
let g:astyle_config_fallback= get(g:,"astyle_auto_disabled", "~/.config/global.astylerc")

function! s:handle_c_cpp_file(path)
  if exists("*FindRootWindow")
    let config_path = g:FindRootWindow(".astylerc")
    let project_root = substitute(config_path,".astylerc","","")
    let cmd = "cd " . '"'. project_root .'"' . " && astyle --project=.astylerc" . " " . a:path
    call system(cmd)
    execute ":e"
  endif 
endfunction

augroup astyle_group
  if g:astyle_auto_disabled == 0
    autocmd! BufWritePost *.{c,cpp,h,hpp} call <SID>handle_c_cpp_file(expand("<afile>:p"))
  endif 
augroup END "file_vim
