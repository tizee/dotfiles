" File: meta.vim
" Author: tizee
" Email: 33030965+tizee@users.noreply.github.com
" Description: A gitconfig plugin
if exists('loaded_meta_vim') || &cp || v:version < 700
  finish
endif
let g:loaded_meta_vim = 1

function! s:stripNL(path)
  return substitute(a:path,'\n$','','')
endfunction

function! g:MetaUserName()
  return s:stripNL(system("git config --get user.name"))
endfunction

function! g:MetaUserEmail()
 return s:stripNL(system("git config --get user.email"))
endfunction

function! g:MetaGitRepo()
  let l:url = system("git config --get remote.origin.url")
  " remove linebreak
  let l:git_url=system("echo " . l:url .  " | " . "sed -E 's\/^[^@]*@([^:\\\/]*)[:\\\/]\/https:\\\/\\\/\\1\\\/\/'")
  let l:url = substitute(l:url,'\n$', '','')
  return substitute(l:url,'.git$','','')
endfunction

