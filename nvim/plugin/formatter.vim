" File: formatter.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: formatter plugin based on find-root.vim

if exists('loaded_formatter_vim')
  finish
endif

let g:loaded_formatter_vim = 1
let default_path=substitute(system("which clang-format"),"\n$",'','')
let s:clang_format=get(g:,"clang_format_path",default_path)
" clang-format.py:
" g:clang_format_path - path to clang-format
" g:clang_format_fallback_style - fallback style options
let s:clang_format_py_path=get(g:,"clang_format_py_path","/usr/local/opt/llvm/share/clang/clang-format.py")
let s:astyle_config_fallback= get(g:,"astyle_config_fallback", "~/.config/global.astylerc")

" clang-format based on https://clang.llvm.org/docs/ClangFormat.html#vim-integration
function! s:handle_c_based_lang_file(path) abort
  if exists("*FindFirst") && exists("*readdir")
    let astyle_config_path = g:FindFirst(".astylerc")
    " prefer clang-format
    let clang_format_config_path = g:FindFirst(".clang-format")
    if !len(clang_format_config_path)
      let clang_format_config_path=expand("~/.clang-format")
    endif
    if !empty(clang_format_config_path)
      let project_root = substitute(clang_format_config_path,".clang-format","","")
      " clang-format.py for vim is not customized
      " pyf s:clang_format_py_path
      " clang-format has implemented the searching logic itself
      let cmd =s:clang_format.' '.'--style=file'.' '.'-i'.' '.a:path
      echomsg cmd
      call system(cmd)
    elseif !empty(astyle_config_path)
      let project_root = substitute(astyle_config_path,".clang-format","","")
      let cmd = "cd " . '"'. project_root .'"' . " && astyle --project=.astylerc" . " " . a:path
      call system(cmd)
    else
      " should be no-op
      " use global astyle config by default on pwd
      " let opts = ["-s4",
      "       \"--indent-classes",
      "       \"--indent-namespaces",
      "       \"--attach-closing-while",
      "       \'--indent-cases',
      "       \"--break-blocks",
      "       \"--add-braces",
      "       \"--break-return-type",
      "       \"--mode=c",
      "       \"--suffix=none",
      "       \"--pad-comma",
      "       \"--pad-oper",
      "       \"--suffix=none",
      "       \"--formatted"]
      " let cmd = 'astyle '.join(opts,' ').' '.a:path
      " echom cmd
      " call system(cmd)
    endif
    execute ":e"
    echo "Format: " . a:path
  endif 
endfunction


augroup C_BASED_LANG_FORMATTER_GROUP
  autocmd! BufWritePost *.{cc,m,mm,c,cpp,cxx,h,hpp} call <SID>handle_c_based_lang_file(expand("<afile>:p"))
augroup END "file_vim
