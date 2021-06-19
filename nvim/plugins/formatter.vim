" File: formatter.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: formatter plugin based on find-root.vim

if exists('loaded_formatter_vim')
  finish
endif

let g:clang_format_path=get(g:,"clang_format_path","/usr/local/opt/llvm/Toolchains/LLVM11.0.0.xctoolchain/usr/share/clang")
let g:loaded_formatter_vim = 1
let g:formatter_auto_disabled = get(g:,"astyle_auto_disabled", 0)
let g:astyle_config_fallback= get(g:,"astyle_config_fallback", "~/.config/global.astylerc")

function! s:handle_c_cpp_file(path)
  if exists("*FindRootWindow") && exists("*readdir")
    let astyle_config_path = g:FindRootWindow(".astylerc")
    " prefer clang-format
    let clang_format_config_path = g:FindRootWindow(".clang-format")
    if !empty(clang_format_config_path)
      let project_root = substitute(astyle_config_path,".clang-format","","")
      let clang_format_py_path = g:clang_format_path . "/clang-format.py"
      pyf clang_format_py_path
    elseif !empty(astyle_config_path)
      let project_root = substitute(astyle_config_path,".clang-format","","")
      let cmd = "cd " . '"'. project_root .'"' . " && astyle --project=.astylerc" . " " . a:path
      call system(cmd)
    endif
    execute ":e"
    echo "Format: " . a:path
  endif 
endfunction


augroup astyle_group
  if g:formatter_auto_disabled == 0
    autocmd! BufWritePost *.{c,cpp,h,hpp} call <SID>handle_c_cpp_file(expand("<afile>:p"))
  endif 
augroup END "file_vim
