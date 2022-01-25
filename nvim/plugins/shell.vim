if exists('loaded_shell_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_shell_vim = 1
" execute current buffer as zsh script
command! -nargs=0 ZshExec set splitright | vnew | set filetype=zsh | read !zsh #
command! -nargs=0 BashExec set splitright | vnew | set filetype=bash | read !bash #

function! s:read_shell_output() range
  " let old_val=getreg("s")
  let cmd=join(getline(a:firstline,a:lastline),"\n")
  echom "Execute " . cmd
  execute("read !" . cmd)
  " call setreg("s", system("zsh".cmd))
  " execute('normal "sp')
  " call setreg("s",old_val)
endfunction

command! -range=% -nargs=0 ZshExec :<line1>,<line2>call <SID>read_shell_output()
command! -range=% -nargs=0 BashExec :<line1>,<line2>call <SID>read_shell_output()
