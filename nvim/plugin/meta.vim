" File: meta.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: git config utilities
if exists('loaded_meta_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_meta_vim = 1

function! s:stripNL(path)
  return substitute(a:path,'\n$','','')
endfunction

let g:meta_prefer_https=get(g:,'meta_prefer_https',1)

function! s:ssh2https(url)
  if g:meta_prefer_https == 1
    return system("echo " . a:url .  " | " . "sed -E 's\/^[^@]*@([^:\\\/]*)[:\\\/]\/https:\\\/\\\/\\1\\\/\/'")
  endif
endfunction

function! s:stripSuffix(url,suffix)
  return substitute(a:url,a:suffix.'$','','')
endfunction

function! g:MetaUserName()
  return s:stripNL(system("git config --get user.name"))
endfunction

function! g:MetaUserEmail()
 return s:stripNL(system("git config --get user.email"))
endfunction

function! g:MetaGitRemoteUrl(remote)
  let url = s:stripNL(system("git config --get remote." . a:remote . ".url"))
  let url = s:ssh2https(url)
  return s:stripSuffix(url, '.git')
endfunction

function! g:MetaGitOriginUrl()
  return g:MetaGitRemoteUrl('origin')
endfunction

