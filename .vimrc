" ------------------------------------------------------------------------------
" .vimrc
" Stephen Niedzielski

so $VIMRUNTIME/mswin.vim



" TODO: gvim can't paste out to chrome in ubuntu


" TODO: no backup ~ thing



" ------------------------------------------------------------------------------
" Keyboard Shortcuts
" See: 1:3 Mapping and Modes.

" Indent / unindent. Don't want im.
vm <tab>   :><cr>gv
vm <s-tab> :<<cr>gv

" Comment / uncomment.
vm <c-k> :call rc:slc(1)<cr>
im <c-k> <c-o>v<c-k>
vm <c-l> :call rc:slc(0)<cr>
im <c-l> <c-o>v<c-l>

" Left word selection.
vm <c-s-left> b<c-g>
nm <c-s-left> v<c-s-left>
im <c-s-left> <c-o><c-s-left>
" TODO: investigate adding command mode support. I could use setpos for <c-left>
" behavior, but not sure how to do <c-s-left> selecting. Maybe something like
" "cmap <c-s-left> <c-\>e".

" Right word selection.
vm <c-s-right> w<c-g>
nm <c-s-right> v<c-s-right>
im <c-s-right> <c-o><c-s-right>

" Page up selection.
vm <s-pgup> <c-u>
nm <s-pgup> v<s-pgup>
im <s-pgup> <c-o><s-pgup>




" Backspace in Visual mode deletes selection
nm <bs> d

" not sure about backspace here in nm

" Forward word deletion.
map <c-del> dw
im <c-del> <c-o>dw

" Backward word deletion.
map <c-bs> db
im <c-bs> <c-o>db

cm $f <c-r>=expand('%:p')<cr>
cm $d <c-r>=expand('%:p:h').'/'<cr>


set expandtab

" Act something like Windows. Word selection and deletion don't behave
" consistently by default.

" Allow cursor to position one past the last character in a line. This allows
" for im c-s-left to work properly when positioned one past the last character.
se ve=onemore


"don't use .vimrc



" For some reason page down is already handled properly.





" ------------------------------------------------------------------------------
" Search

" Match term as typed.
se is

" Ignore term case.
se ic

" Override ignorecase when a capital appears in the term.
se scs

" Highlight matches.
se hls

" Don't wrap searches. I never catch the wrap warning.
se nows

" ------------------------------------------------------------------------------
" Wrapping

" Wrap long lines (display only).
se wrap

" Don't modify buffer to wrap long lines.
se tw=0

" Break at chars spec by brk.
" TODO: brk doesn't seem to be working right. It's breaking mid-word.
se lbr

" Wrap cursor movements at EOL & SOL.
se ww=<,>,h,l,[,]

" ------------------------------------------------------------------------------
" Display

" Show non-printing characters.
se list
se lcs=tab:»\ ,trail:·

" Select Pablo colorscheme.
"colo pablo

" Highlight current line.
"se cul
"hi cursorline gui=standout

se nocul " this must be getting picked up from vim file... need to make changes there

" Set selection highlighting color.
"hi visual guibg=darkblue

" Set status bar (multiple windows) colors.
"hi statusline ctermbg=white ctermfg=black

" Set status line (single or non-selected window).
"hi statuslinenc ctermbg=lightgrey ctermfg=black

" Do not allow modified buffers to be hidden. Need to explore this more.
se nohid

" Show line number gutter.
"se nu

" Startup width and height.
se co=80
se lines=40

" Remove toolbar (icons) and menubar (file, edit, ...).
se go-=T
se go-=m

if has('win32')
  " Set font size.
  " To bring up selector dialog: se gfn=*
  se gfn=Fixedsys:h10
en

" Maximum number of tabs open simultaneously.
se tpm=100


" ------------------------------------------------------------------------------
" Indentation

" Tabs occupy two characters.
se ts=2

" Each indent level is two characters.
se sw=2

" Disable indentation rules based on filetype.
filet indent off

" Something like 'just copy the previous line's indent'.
se ai
se ci
se pi

" ------------------------------------------------------------------------------
" Keys

" Tab navigation.
map <C-tab> gt
im  <C-tab> <C-O>gt
map <C-S-tab> gT
im  <C-S-tab> <C-O>gT
map <C-t> :tabe<cr>
im  <C-t> <C-O>:tabe<cr>

"close tab

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

" ------------------------------------------------------------------------------
" Miscellaneous

" Use forward slashes in filenames.
se ssl

" Disable audible and visual bells.
se novb

" Configure to use bash.
se sh=bash

" Argument specifying a command follows is '-c'.
se shcf=-c

if has('win32')
  " Wrap commands in double quotes.
  se sxq=\"
en

" Use Bash style autocompletion.
se wim=list:longest

" Disable autocompletion for files matching...
"se wig=*~,*.swp

" Default selection highlighting is nearly invisible. Especially when viewed at
" an angle.
hi Visual guibg=#b0c0f0

" ------------------------------------------------------------------------------
" More Miscellaneous
fu! rc:cc()
  " TODO: route errors somewhere or fork or something.
  if bufname('') == '' " The current buffer.
    :w !g++ -x c++ -
  el
    :w|!g++ -c %
  en
endf
" mak --- map to what key? map other things..., ctrlf
" cope

se spell

" ------------------------------------------------------------------------------
" Single line comment.
fu! rc:slc(b_comment)
  " TODO: s doesn't support variable interpolation. Use substitute().
  " TODO: get the comment character from the syntax definition.
  if    &syntax == 'c'
    if a:b_comment
      s_\v^([\t ]*)([^\t ])_\1//\2_e
    el
      s_\v^([\t ]*)//_\1_e
    en
  elsei &syntax == 'cpp'
    if a:b_comment
      s_\v^([\t ]*)([^\t ])_\1//\2_e
    el
      s_\v^([\t ]*)//_\1_e
    en
  elsei &syntax == 'vim'
    if a:b_comment
      s_\v^([\t ]*)([^\t ])_\1"\2_e
    el
      s_\v^([\t ]*)"_\1_e
    en
  elsei &syntax == 'txt' " TODO: need to set syntax to text on autoload.
    " Special case.
    if a:b_comment
      s_\v^([\t ]*)(-+)? ?(.*[^\t ])_\1-\2 \3_e
    el
      s_\v^([\t ]*)((- )|-)_\1_e
    en
  el " Default.
    if a:b_comment
      s_\v^([\t ]*)([^\t ])_\1#\2_e
    el
      s_\v^([\t ]*)#_\1_e
    en
  en
endf

" ------------------------------------------------------------------------------
" Cscope

" Use Cygwin Cscope.
se csprg=mlcscope

" Use Cscope, not Ctags.
se cst

" Notify the user when cs add fails.
se csverb

" Notes:
" cs add cscope.out S:/b/M7630AAAEQMWSA1573_0
" <c-]>

" ------------------------------------------------------------------------------
" Notes

" :se tw=79 insert wrap
" prefix \v (very magic) extended regular expressions, similar to sed -r
" se syntax=python
" se enc=utf-8
" se ff=unix
" se fdm=indent
" # to highlight all matches udner cur
" %!sort -u
" 1!echo -e foo\\\nbar
" ctrl-w q (unsplit)
" |
" elsei has('os2')

" http://vim.wikia.com/wiki/Hex_dump
" %!xxd
" %!xxd -r - %
" how not to lose contents on output?
" ma, `a
" . repeats
"ino ;; <esc>
"icno ;; <c-c>
" other meta or special keys... do a search on minimal mappings
" switch to .vrimc and vim
" se co=161
" ctrl w c, :clo to close frame... alias to unsplit? no... not really sure how to well manage these... just need to practice
" TODO: paste should just affect RBUFFER, not paste before line in insert
" mode.
" tab all or tab ball?
" :nnoremap gf <C-W>gf
":cab      e  tabe
":tab sball                           : retab all files in buffer (repair) *N*


" commenting is broken

" Open files for every nonempty line in a file. This is kind of broken.
" TODO: str_file_ls arg.
fu! rc:tabe_ls()
  " This isn't really what I want, but the following only works on the first
  " line:
  " g_^_tabe `=getline('.')`

  " Hose args.
  sil! argd *

  " Add each nonempty line to args.
  g_._arga `=getline('.')`

  " Open each arg in a tab.
  tab ba
endf
" execute "cd" fnameescape(pathname). `=expr`

se cul
" :se switchbuf=usetab,newtab
" map gf :tabe <cfile><CR>
" ctrl-z, etc. in command mode
" \<word\>


se bdir=~/.swp
",$usernamedesktop
se dir=~/.swp
" $bdir
" TODO: mkdir if not exist
