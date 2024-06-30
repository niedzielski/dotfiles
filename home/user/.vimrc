" Set the swap dir to a prominent place.
se dir=~

" Tabs occupy two characters.
se ts=2

" Show typos.
se spell

" Use Bash style file tab completion.
se wim=list:longest

" Route middle mouse single, double, triple, and quadruple clicks to left click
" instead of paste. I frequently click the middle mouse button accidentally.
map <MiddleMouse>   <LeftMouse>
im  <MiddleMouse>   <LeftMouse>
map <2-MiddleMouse> <LeftMouse>
im  <2-MiddleMouse> <LeftMouse>
map <3-MiddleMouse> <LeftMouse>
im  <3-MiddleMouse> <LeftMouse>
map <4-MiddleMouse> <LeftMouse>
im  <4-MiddleMouse> <LeftMouse>

" Some shortcuts for command mode.
cm $f <c-r>=expand('%:p')<cr>
cm $d <c-r>=expand('%:p:h').'/'<cr>

" Search.
se hls  " Highlight matches.
se is   " As typed.
se ic   " Ignore case.
se scs  " Unless capital(s) are used.
se nows " And don't wrap back to the top of buffer.
