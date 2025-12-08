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

" check if remote is a GitHub repo
function! s:is_github_repo()
  let l:current_dir = resolve(expand("%:p:h"))
  let l:cd_command = "cd '" . l:current_dir . "'; "
  let l:remote_url = system(l:cd_command . "git config --get remote.origin.url")
  let l:remote_url = s:stripNL(l:remote_url)
  " match github.com in SSH or HTTPS URLs
  return l:remote_url =~# 'github\.com'
endfunction

" get current commit hash
function! s:git_commit_hash()
  let l:current_dir = resolve(expand("%:p:h"))
  let l:cd_command = "cd '" . l:current_dir . "'; "
  let l:hash = system(l:cd_command . "git rev-parse HEAD")
  return s:stripNL(l:hash)
endfunction

" get file path relative to repo root
function! s:git_relative_path()
  let l:file_path = resolve(expand("%:p"))
  let l:current_dir = resolve(expand("%:p:h"))
  let l:cd_command = "cd '" . l:current_dir . "'; "
  let l:rel_path = system(l:cd_command . "git ls-files --full-name '" . l:file_path . "'")
  return s:stripNL(l:rel_path)
endfunction

" build permalink URL with commit hash for current file
function! s:file_permalink_url()
  let l:repo_url = s:stripNL(s:remote_repo())
  let l:hash = s:git_commit_hash()
  let l:rel_path = s:git_relative_path()
  let l:line = line('.')
  " format: https://github.com/owner/repo/blob/<hash>/path/to/file#L<line>
  return l:repo_url . '/blob/' . l:hash . '/' . l:rel_path . '#L' . l:line
endfunction

" copy file permalink to clipboard (GitHub only)
function! s:copy_file_permalink()
  if !s:is_github_repo()
    echoerr 'RepoFilePermalink: Not a GitHub repository'
    return
  endif
  let l:url = s:file_permalink_url()
  if len(s:repo_copy_command)
    let l:cmd = "echo " . "\"". l:url ."\"" . " | " . s:repo_copy_command
    call system(l:cmd)
  else
    call setreg('*', l:url)
  endif
  echom 'Copied: ' . l:url
endfunction

nnoremap <silent> <Plug>(open-repo-url) :call <SID>open_remote_repo()<CR>
nnoremap <silent> <Plug>(copy-repo-url) :call <SID>copy_remote_repo()<CR>
nnoremap <silent> <Plug>(copy-file-permalink) :call <SID>copy_file_permalink()<CR>
" could map with <leader>
nnoremap <silent> <leader>go <Plug>(open-repo-url)
nnoremap <silent> <leader>gp <Plug>(copy-repo-url)
nnoremap <silent> <leader>gl <Plug>(copy-file-permalink)

command! -nargs=0 RepoOpen call <SID>open_remote_repo()
command! -nargs=0 RepoCopy call <SID>copy_remote_repo()
command! -nargs=0 RepoEcho call <SID>echo_remote_repo()
command! -nargs=0 RepoFilePermalink call <SID>copy_file_permalink()
