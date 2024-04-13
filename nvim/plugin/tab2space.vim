" File: tab2space.vim
" Author: tizee
" Description: convert tabs in selected content to spaces
command! -range=% -nargs=0 Tab2Space :<line1>,<line2>s/	/ /g
