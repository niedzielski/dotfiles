# .bashrc, stephen@niedzielski.com

# ------------------------------------------------------------------------------
# util

[ -f ~/.sh_util ] && . ~/.sh_util

# ------------------------------------------------------------------------------
# globbing

# enabled extended globbing
shopt -s extglob

# enable recursive globbing using **
shopt -s globstar

# ignore case
shopt -s nocaseglob

# ------------------------------------------------------------------------------
# history
# preserve the past as completely as possible

# don't record adjacent duplicates or lines starting with a space
HISTCONTROL=ignoredups:ignorespace

# set max commands and lines in Bash history
unset HISTSIZE
unset HISTFILESIZE

# timestamp entries
HISTTIMEFORMAT='%F-%H-%M-%S '

# preserve multi-line commands in one history entry
shopt -s cmdhist

# append to the history file, don't overwrite it
shopt -s histappend

# allow failed history substitution reedits
shopt -s histreedit

# don't escape variables
shopt -s direxpand

# ------------------------------------------------------------------------------
# misc options

# don't overwrite an existing file when using redirection
set -C

# the return value of a pipeline is that of the rightmost command to exit with a
# non-zero status, or zero if all commands exit successfully
set -o pipefail

# check for active jobs before exiting
shopt -s checkjobs

# notify of job termination immediately
set -b

# check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS
shopt -s checkwinsize

# if a command is unrecognized but matches a directory, cd into it
shopt -s autocd

# ------------------------------------------------------------------------------
# prompt
# "
# /home/stephen ────────────────────────────────────────────────────────────────
# $ "

# padding is 32 dashes long initially
prompt_pad='────────────────────────────────'

# effectively multiply padding by 32 = 1024 dashes, which should be sufficiently
# long enough for most terminals
prompt_pad="${prompt_pad//─/$prompt_pad}"

print_prompt_pad() {
  # TODO: clean up
  declare -i prompt_stat_len="$(python -c 'import re, sys; sys.stdout.write(str(len(re.sub("\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]", "", sys.stdin.read(), 0))))' <<< "$PWD")"
  echo ${prompt_pad:0:$(($COLUMNS - $prompt_stat_len % ${COLUMNS:-1}))}
}

PS1='\n'
PS1+='\[\e[0;104m\]$PWD\[\e[0m\] '
PS1+='$(declare -i status=$?; print_prompt_pad; [[ $status -eq 0 ]] && echo "\[\e[22;32m\]" || echo "\[\e[22;31m\]")'
PS1+='\$ '
PS1+='\[\e[0m\]'

# ------------------------------------------------------------------------------
# simple supplements

alias cp=cp\ -ai # prompt on overwrite, preserve all

alias rm=rm\ -i # always prompt

alias mv=mv\ -i # prompt on overwrite
alias m=mv

alias c=cd

alias t=touch

alias e=echo

# alphabetical, do not list . & .., mark directories with a trailing slash, &
# colored
alias l='ls -Ap --color=auto'

alias timestamp=date\ +%F-%H-%M-%S-%N

# extended regex, skip binaries, devices, sockets, & dirs, colored, & line
# -buffered. use a non- canonical alias instead of GREP_OPTIONS which may wreck
# poorly written scripts
alias g='grep -EID skip -d skip --color=auto --line-buffered'

# TODO: clean up
f() {
  # the trouble is that -regextype must appear after path but before expression
  # HACK: "-D debugopts" unsupported and -[HLPO] options assumed to before dirs
  local a=()
  while [[ -n "$1" ]] &&
        ( [[ ! "${1:0:1}" =~ [-!(),] ]] || [[ "${1:0:2}" =~ -[HLPO] ]] ); do
    a+=("$1")
    shift
  done

  find -O3 ${a:+"${a[@]}"} -nowarn -regextype egrep ${@:+"$@"}
}

# smart-case
alias ag=ag\ -S

# extended regex
alias s=sed\ -r

alias x='xargs -rd\\n ' # \n delimited, don't run on empty in, + expansion
#                    ^-- this space is for expanding a subsequent alias
# copy ex: x cp --parents -t"$dst"
# - TODO: Figure out general case. x -i implies -L1. Use
#   xargs --show-limits -r < /dev/null? See also ulimit builtin?
alias map=x\ -I{}

alias rsync='rsync -azv --partial-dir=.rsync-partial --partial' # see also zsync

alias abspath=realpath\ -ms

alias v=gvim\ -p

alias d2u=dos2unix

# find file in pwd or upwards
# $1 - file
upfile() {
  declare dir="$PWD"
  while :; do
    declare file="$dir/$1"
    if [[ -e "$file" ]]; then
      echo "$file"
      return 0
    elif [[ "$dir" == / ]]; then
      return 1
    fi
    dir="$(dirname "$dir")"
  done
  return 1
}

# ------------------------------------------------------------------------------
# android

if [[ -f ~/.bashrc_android ]]; then
  . ~/.bashrc_android
fi

# ------------------------------------------------------------------------------
# completion

# don't attempt to complete empty command lines otherwise it'll hang the prompt
# for a bit
shopt -s no_empty_cmd_completion

# complete host names
shopt -s hostcomplete

if [[ -f /etc/bash_completion ]]; then
  . /etc/bash_completion
fi

# ------------------------------------------------------------------------------
# autojump

if [[ -f /usr/share/autojump/autojump.bash ]]; then
  export AUTOJUMP_IGNORE_CASE=1
  export AUTOJUMP_KEEP_SYMLINKS=1
  . /usr/share/autojump/autojump.bash
fi