if exists('loaded_diff_vim')
  finish
endif
let g:loaded_diff_vim = 1

if has('nvim')
  set diffopt+=context:20
endif
let g:diff_algorithms = [
      \ "myers",
      \ "minimal",
      \ "patience",
      \ "histogram",
      \ ]
let g:diff_algorithm = "patience"

func! DiffSwitchAlgorithm()
  let l:total_diff_algos = len(g:diff_algorithms)
  let l:i = 0
  while l:i < l:total_diff_algos && g:diff_algorithms[l:i] !=# g:diff_algorithm
  let l:i += 1
  endwhile
  if l:i < l:total_diff_algos
  let g:diff_algorithm = g:diff_algorithms[(l:i + 1) % l:total_diff_algos]
  else
  let g:diff_algorithm = "patience"
    endif
  for l:algo in g:diff_algorithms
    exec "set diffopt-=algorithm:" . l:algo
  endfor
  exec "set diffopt+=algorithm:" . g:diff_algorithm
  echo "Diff algorithm switched to " . g:diff_algorithm
  windo diffupdate
endfunc

func! DiffUpdateContext(contextLines)
  let l:opt = substitute(&diffopt, '\v(^\|,)context:\d+', '', 'g') . ",context:" . a:contextLines
  exec "set diffopt=" . l:opt
  windo diffupdate
endfunc

func! ToggleDiffWhiteSpace()
  if stridx(&diffopt, "iwhite") >= 0
    set diffopt-=iwhite
    echo "Not ignoring whitespaces in diff"
  else
    set diffopt+=iwhite
    echo "Whitespaces ignored in diff"
  endif
  windo diffupdate
endfunc

command! DiffSwitchAlgorithm call DiffSwitchAlgorithm()
command! DiffToggleWhiteSpace call DiffToggleWhiteSpace()
command! -nargs=1 DiffContext call DiffUpdateContext(<f-args>)

if has('nvim')
  set &diffopt+=internal,algorithm:patience
endif
