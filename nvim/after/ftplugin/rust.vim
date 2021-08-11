" vim builtin termdebug plugin
let termdebugger="rust-gdb"
" coc
nmap <silent> <leader>oc :<C-u>CocCommand rust-analyzer.run<CR>
" create breakpoint in current line
nmap <silent> <leader>ob :<C-u>call vimspector#ToggleBreakpoint()<CR>
" run to cursor
nmap <silent> <leader>od :<C-u>call vimspector#RunToCursor()<CR>
" run to cursor
nmap <silent> <leader>ol :<C-u>call vimspector#ListBreakpoints()<CR>
" clear breakpoints
nmap <silent> <leader>oc :<C-u>call vimspector#ClearBreakpoints()<CR>
nmap <silent> <leader>o6 <Plug>VimspectorContinue
nmap <silent> <leader>o7 <Plug>VimspectorStepInto
nmap <silent> <leader>o8 <Plug>VimspectorStepOver
nmap <silent> <leader>o9 <Plug>VimspectorStepOut
nmap <silent> <leader>o0 <Plug>VimspectorStop
nmap <silent> <leader>op <Plug>VimspectorBallonEval
xmap <silent> <leader>op <Plug>VimspectorBallonEval
