# config.fish, stephen@niedzielski.com

# ------------------------------------------------------------------------------
# simple supplements

alias cp 'cp -ai' # prompt on overwrite, preserve all

alias rm 'rm -i' # always prompt

alias mv 'mv -i' # prompt on overwrite
alias m mv

alias c cd

alias t touch

alias e echo

# alphabetical, do not list . & .., mark directories with a trailing slash, &
# colored
alias l 'ls -Ap --color=auto'

alias timestamp date\ +%F-%H-%M-%S-%N

# extended regex, skip binaries, devices, sockets, & dirs, colored, & line
# -buffered. use a non- canonical alias instead of GREP_OPTIONS which may wreck
# poorly written scripts
alias g 'grep -EID skip -d skip --color=auto --line-buffered'

# smart-case
alias ag 'ag -S'

# extended regex
alias s 'sed -r'

alias rsync 'rsync -azv --partial-dir=.rsync-partial --partial' # see also zsync

alias abspath realpath\ -ms

alias v gvim\ -p

alias d2u dos2unix