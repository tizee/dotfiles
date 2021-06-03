" QWERTY to COLEMAK
" 
" maintain the same finger positions for both of QWERTY layout and COLEMAK
" layout 
"
" QWERTY:
" q w e r t y u i o p
" COLEMAK:
" q w f p g j l u y ; [ ]
"
" QWERTY:
" w a s d f h j k l ; '
" COLEMAK:
" a r s t d h n e i o ' '
"
" QWERTY:
" z x c v b n m , . /
" COLEMAK:
" z x c v b k m , . /
"

" MODES {{{
"     COMMANDS                    MODES
":map   :noremap  :unmap     Normal, Visual, Select, Operator-pending
":nmap  :nnoremap :nunmap    Normal
":vmap  :vnoremap :vunmap    Visual and Select
":smap  :snoremap :sunmap    Select
":xmap  :xnoremap :xunmap    Visual
":omap  :onoremap :ounmap    Operator-pending
":map!  :noremap! :unmap!    Insert and Command-line
":imap  :inoremap :iunmap    Insert
":lmap  :lnoremap :lunmap    Insert, Command-line, Lang-Arg
":cmap  :cnoremap :cunmap    Command-line
":tmap  :tnoremap :tunmap    Terminal
"
" }}}
" MOTIONS {{{
" 1. Motions and operators				*operator*
" The motion commands can be used after an operator command, to have the command
" operate on the text that was moved over.  That is the text between the cursor
" position before and after the motion.  Operators are generally used to delete
" or change text.  The following operators are available:
" 
" 	|c|	c	change
" 	|d|	d	delete
" 	|y|	y	yank into register (does not change the text)
" 	|~|	~	swap case (only if 'tildeop' is set)
" 	|g~|	g~	swap case
" 	|gu|	gu	make lowercase
" 	|gU|	gU	make uppercase
" 	|!|	!	filter through an external program
" 	|=|	=	filter through 'equalprg' or C-indenting if empty
" 	|gq|	gq	text formatting
" 	|gw|	gw	text formatting with no cursor movement
" 	|g?|	g?	ROT13 encoding
" 	|>|	>	shift right
" 	|<|	<	shift left
" 	|zf|	zf	define a fold
" 	|g@|	g@	call function set with the 'operatorfunc' option
" 
" }}}

" if $COLEMAK_KEYBOARD == 0
"  finish
"endif

  " Move {{{
  " h -> h
  " j -> n
  " k -> e
  " l -> i
  " nnoremap h h|xnoremap h h|onoremap h h
  nnoremap n gj|xnoremap n j|onoremap n j
  nnoremap e gk|xnoremap e k|onoremap e k
  nnoremap i l|xnoremap i l|onoremap i l
  " move between split windows
  nnoremap <C-h> <nop>
  nnoremap <C-n> <nop>
  nnoremap <C-e> <nop>
  nnoremap <C-i> <nop>
  nnoremap <C-h> <C-w>h
  nnoremap <C-n> <C-w>j
  nnoremap <C-e> <C-w>k
  nnoremap <C-i> <C-w>l

    " Tab navigation
  nnoremap th :tabp<CR>
  nnoremap ti :tabn<CR>

  " move tabs
  nnoremap tmi :+tabmove<CR>
  nnoremap tmh :-tabmove<CR>

  " }}}
  " Verbs {{{
  " use left hand of home row for most verbs
  " Insert
  nnoremap s i
  nnoremap S I
  
  " Replace 
  " nnoremap r r
  
  " Append
  " nnoremap A A
  
  " Change
  " nnoremap c c
  
  " Copy
  " nnoremap f y
  " nnoremap F Y
  " Paste
  " nnoremap t p

  " Undo
  " nnoremap l u
  " nnoremap L U
  " Redo 
  " nnoremap Z <C-R>

  " Select (Normal Mode->Visual Mode)
  
  " }}}
 
  " Word motion {{{
  "
  
  "
  " }}}

" vim:ft=vim foldmethod=marker
