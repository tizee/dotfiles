scriptencoding utf-8
if exists('loaded_repo_vim')
  finish
endif
let g:loaded_repo_vim = 1

" open commands
if has('win16') || has('win32') || has('win64')
  let s:repo_open_command='start '
  let s:repo_copy_command='' " not support yet
elseif has('mac') || has('macunix') || has('gui_macvim')
  let s:repo_open_command='open '
  let s:repo_copy_command='pbcopy'
elseif executable('xdg-open')
  let s:repo_open_command='xdg-open '
  let s:repo_copy_command='xclip -selection clipboard' " not support
endif 

function! s:stripNL(path)
  return substitute(a:path,'\n$','','')
endfunction

function! s:stripSuffix(path,suffix)
  return substitute(a:path,a:suffix.'$', '', '')
endfunction

" transfrom ssh://git@github.com:suffix or git@github.com/suffix to
" https://github.com/suffix
function! s:ssh_to_https(path)
  if len(a:path) == 0
    return
  endif
  " use \/ to escape / in shell script, use \\ to escape \ in vim
  let l:sed_cmd = "sed -E 's\/^[^@]*@([^:\\\/]*)[:\\\/]\/https:\\\/\\\/\\1\\\/\/'"
  let l:url = system("echo " . a:path . " | " . l:sed_cmd)
  return l:url
endfunction

function! s:GitRemoteRepo(url)
  let l:repo = s:stripSuffix(a:url,'.git')
  let l:repo = s:ssh_to_https(l:repo)
  return l:repo
endfunction

" get url of remote repo
function! s:remote_repo()
  let l:current_dir = resolve(expand("%:p:h"))
  let l:cd_command = "cd '" . l:current_dir . "'; "
  let l:remote_url= system(l:cd_command . "git config --get remote.origin.url")
  let l:remote_url = s:stripNL(l:remote_url)
  let l:url = s:GitRemoteRepo(remote_url)
  return l:url
endfunction

" open remote repo
function! s:open_remote_repo()
  let l:url = s:remote_repo()
  let l:cmd = s:repo_open_command . l:url
  call system(l:cmd)
endfunction

" open remote repo
function! s:copy_remote_repo()
  let l:url = s:remote_repo()
  if len(s:repo_copy_command)
    let l:cmd = "echo " . "\"". l:url ."\"" . " | " . s:repo_copy_command
    call system(l:cmd)
  else
    call setreg('*', l:url)
  endif
endfunction

function! s:echo_remote_repo()
  let l:url = s:remote_repo()
  echom l:url
endfunction

nnoremap <silent> <Plug>(open-repo-url) :call <SID>open_remote_repo()<CR>
nnoremap <silent> <Plug>(copy-repo-url) :call <SID>copy_remote_repo()<CR>
" could map with <leader>
nnoremap <silent> <leader>go <Plug>(open-repo-url)
nnoremap <silent> <leader>gp <Plug>(copy-repo-url)

command! -nargs=0 RepoOpen call <SID>open_remote_repo()
command! -nargs=0 RepoCopy call <SID>copy_remote_repo()
command! -nargs=0 RepoEcho call <SID>echo_remote_repo()
