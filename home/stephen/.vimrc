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

" Highlight current line.
se cul

" Maximum number of tabs open simultaneously (default is too few).
se tpm=100

" Remove the toolbar.
se go-=T

" Remove the scrollbars.
se go-=rL

" Don't force newlines at end of files.
se noeol

" ------------------------------------------------------------------------------
" Behave Like Windows

" Sourcing this file handles the lion's share. But there's some (mostly) subtle
" supplements needed on the following lines.
so $VIMRUNTIME/mswin.vim

" Allow cursor to position one past the last character in a line. This allows
" for im c-s-left to work properly when positioned one past the last character.
se ve=onemore

" Forward word deletion.
map <c-del> dw
im <c-del> <c-o>dw

" Backward word deletion.
map <c-bs> db
im <c-bs> <c-o>db

" Backspace in visual mode deletes selection (for consistency with delete).
nm <bs> d<left>

" Left word selection.
vm <c-s-left> b<c-g>
nm <c-s-left> v<c-s-left>
im <c-s-left> <c-o><c-s-left>
" TODO: investigate adding command mode support. I could use setpos for <c-left>
" behavior, but not sure how to do <c-s-left> selecting. Maybe something like
" cmap <c-s-left> <c-\>e".

" Right word selection.
vm <c-s-right> w<c-g>
nm <c-s-right> v<c-s-right>
im <c-s-right> <c-o><c-s-right>

" Page up selection.
vm <s-pgup> <c-u>
nm <s-pgup> v<s-pgup>
im <s-pgup> <c-o><s-pgup>

" Tab navigation.
map <C-tab> gt
im  <C-tab> <C-O>gt
map <C-S-tab> gT
im  <C-S-tab> <C-O>gT
map <C-t> :tabe<cr>
im  <C-t> <C-O>:tabe<cr>