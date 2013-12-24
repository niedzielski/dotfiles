#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Copyright 2009 - 2013 Stephen Niedzielski. Licensed under GPLv3.

# ------------------------------------------------------------------------------
# Interactive shell check.
# TODO: I think this was in here because I needed some horrible screen ssh login
# hack. Remember why.
[[ "$-" == *i* ]] || return

# ------------------------------------------------------------------------------
# History

# Don't record adjacent duplicate commands or command lines starting with a
# space.
HISTCONTROL=ignoredups:ignorespace

# Set max commands and lines in Bash history.
HISTSIZE=999999
HISTFILESIZE=999999

# Timestamp entries.
HISTTIMEFORMAT='%F-%H-%M-%S '

# Preserve multi-line commands in one history entry.
shopt -s cmdhist

# Append to the history file, don't overwrite it.
shopt -s histappend

# Don't replace newlines with semicolons in multi-line commands.
shopt -s lithist

shopt -s histreedit

# ------------------------------------------------------------------------------
# Globbing

# Enabled extended globbing.
shopt -s extglob

# Enable recursive globbing using **.
shopt -s globstar

# Ignore case.
shopt -s nocaseglob

# ------------------------------------------------------------------------------
# Completion

# Don't attempt to complete empty command lines otherwise it'll hang the prompt
# for a bit.
shopt -s no_empty_cmd_completion

# Source available Bash completions.
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

# Complete host names.
shopt -s hostcomplete

# ------------------------------------------------------------------------------
# Miscellaneous Options

# Don't overwrite an existing file when using redirection.
set -C

# The return value of a pipeline is that of the rightmost command to exit with a
# non-zero status, or zero if all commands exit successfully.
set -o pipefail

# Use VI style command line editing.
set -o vi

# Check for active jobs before exiting.
shopt -s checkjobs

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# ------------------------------------------------------------------------------
# Logging
echo_err() { echo "$@" >&2; } # Echo to stderr.
log()      { echo "$@"|tee -a ${log_file:+"$log_file"}; }
log_ok()   {   echo -en '\033[22;32m'; log "$@"; echo -en '\033[00m';        } # Green
log_hl()   {   echo -en '\033[22;36m'; log "$@"; echo -en '\033[00m';        } # Blue
log_warn() { { echo -en '\033[22;33m'; log "$@"; echo -en '\033[00m'; } >&2; } # Yellow
log_err()  { { echo -en '\033[22;31m'; log "$@"; echo -en '\033[00m'; } >&2; } # Red
prompt()    { read -p '<Enter> to continue, <ctrl-c> to abort: '; }




# ------------------------------------------------------------------------------
# Prompt and Window Title

# HACK: Bash won't read updates to PS1 made in readline.
#force_update_term_title() { echo -en "\033]0;$USER@${debian_chroot:-$HOSTNAME}:$PWD\007"; }

declare __VERBOSE_PS1=1
if [[ -n "$__VERBOSE_PS1" ]]
then
  # Title: $(basename "$PWD")
  PS1='\[\e]0;\W\a\]'

  declare -i ps1lastTime=$(date +%s)
  declare -i ps1deltaTime=0
  export PROMPT_COMMAND='ps1deltaTime=$(($(date +%s) - $ps1lastTime)) ; ps1lastTime=$(date +%s)'

  declare -r __PS1_TEXT='⎼ $USER@${debian_chroot:-$HOSTNAME}:$PWD [${ps1deltaTime}s]'

  # Make a long line buffer (420 chars). This should be >= $COLUMNS.
  declare __PS1_PAD='⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼⎼'; __PS1_PAD+=${__PS1_PAD//⎼/$__PS1_PAD}
  # Consider also "─" which is ANSI safe? Unfortunately, it's too heavy. Is
  # there a lighter horizontal pipe?
  __ps1_pad()
  {
    declare -ir err=$?
    # Hacky
    declare w=$(( $COLUMNS + 1 ))
    declare x=$(eval echo -n "$__PS1_TEXT"|wc -c)
    #echo_err $w $x
    [[ $w -ge $x ]] && x=$(($w - $x)) || x=$(($w - $x % $w - 1))
    #echo_err $w $x
    echo "${__PS1_PAD:0:$x}"
    return $err
  }

  # A more verbose prompt with the window title prepended in blue.
  PS1+='\n\033[22;36m'"$__PS1_TEXT"' $(__ps1_pad)\n'
else
  # Title: user@hostname:$PWD
  PS1='\[\e]0;\u@${debian_chroot:-\h}:\w\a\]'
fi

# "$ " colored green for zero exit status, red otherwise.
PS1+='$( [[ $? -eq 0 ]] && echo "\[\033[22;32m\]" || echo "\[\033[22;31m\]" )'

# Back to colorless.
PS1+='\$ \[\033[00m\]'

# ------------------------------------------------------------------------------
# Simple Shell Supplements

# Safety defaults for copy, remove, and move.
alias cp='cp -ai' # Prompt on overwrite, preserve all.
alias rm='rm -i'  # Always prompt.
alias mv='mv -i'  # Prompt on overwrite.

# Some simple shorthands.
alias m=mv
alias t=touch
alias e=echo

# Extended regex, color, line num, skip binaries, devices, sockets, and dirs.
# Better use a non-canonical alias instead of GREP_OPTIONS which may wreck
# poorly written scripts.
alias g='grep --color=auto -nEID skip -d skip'

# Use extended regex. Again use a silly alias to avoid wrecking poorly written
# scripts. Also, Sed doesn't handle repeated "-r" flags gracefully.
alias s='sed -r'

alias x='xargs -rd\\n ' # \n delimited, don't run on empty in, + expansion.
                   # ^--- This space is for expanding a subsequent alias arg.
# Copy ex: x cp --parents -t"$dst"
# - TODO: Figure out general case. x -i implies -L1. Use
#   xargs --show-limits -r < /dev/null? See also ulimit builtin?

# Directory listing. See also: lsattr -a, getfattr.
alias  ls='ls -Ap --color=auto' # Alphabetically.
alias   l=ls
alias lex='l -X'                   # By extension.
alias lsi='l -S'                   # By size.
alias lct='l -c'                   # By creation time.
alias lmt='l -t'                   # By mod time.
alias lat='l -u'                   # By access time.
type dircolors &> /dev/null && eval "$(dircolors -b)" # Enable color support.



alias rsync='rsync -azv --partial-dir=.rsync-partial --partial' # See also zsync
alias timestamp='date +%F-%H-%M-%S-%N'



alias dif='colordiff -d --speed-large-files --suppress-common-lines'
alias vdif='dif -yW$COLUMNS' # Side by side (vertical) diff.

export LESS='-ir' # Smart ignore-case + output control chars. Probably safe to export.
alias pdfgrep='pdfgrep --color=auto -n'
alias abspath='readlink -m' # Note: pwd -P = readlink -e .


# ------------------------------------------------------------------------------
# Directory navigation.
d()
{
  declare color=3
  builtin dirs -p|while read path
  do
    printf "\033[22;3${color}m%s\033[00m " "$path"

    # Alternate the color after the first path.
    [[ $color -eq 4 ]] && color=5 || color=4;
  done
  echo # Newline.
}
c() { cd "$@"; d; }
p() { pushd "$@" > /dev/null; d; } # Change directory.
alias pb='p +1' # Previous directory.
alias pf='p -0' # Next directory.
P() { popd > /dev/null; d; }



# ------------------------------------------------------------------------------
# Less Simple Shell Supplements


# Use "eval $(antialias foo)" to invoke as a command.
antialias()
{
  # "alias" does escaping, so use "type" instead. # TODO: eval without quotes?
  type "$1"|
  sed -r 's%'"$1"' is aliased to `(.*)'\''%\1%' #' # HACK: Vim highlighting gets screwed up here.
}

# Dereference supplied indirection. Accepts arrays, too. For ex:
#   declare -i MIN_DEPTH=0
#   declare -i MAX_DEPTH=9
#   eval declare row_{$MIN_DEPTH..$MAX_DEPTH}='\(\)'
#   declare -i row_i=$MIN_DEPTH
#   row_i=(a b c d)
#   deref row_$row_i[@]
#   -> a b c d
deref() { eval echo \"\${$1}\"; } # deref $__PS1_TITLE fails where eval echo "$__PS1_TITLE" succeeds

#deref() { eval echo -n \"\$$1\"; }
__test_deref()
{
  declare -i err=0

  declare t0=t1
  declare t1=bar
  if [[ "$(deref "$t0")" != bar ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO failed."
  fi

  return $err
}

requote() { printf '%q ' "$@"; }
__test_requote()
{
  declare -i err=0

  if [[ "$(requote '')" != "'' " ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO failed empty argument test."
  fi

  return $err
}


#  These don't seem to correct prior to passing to prog.
shopt -s autocd cdable_vars # cdspell dirspell


shopt -s checkjobs
shopt -s checkwinsize



shopt -u huponexit
shopt -s no_empty_cmd_completion

set -b
#set -u


v() { gvim -p "$@" 2> /dev/null; } # One tab per file.

lynx() { command lynx -accept_all_cookies "$@" 2> /dev/null; }


# Clipboard for GUI integration.
cb()
{
  if [[ ! -t 0 ]] # ifne
  then
    xclip -sel c
  else
    xclip -sel c -o
  fi
}

case "$OSTYPE" in
  cygwin)      gui() { explorer "${@:-.}"; } ;;
  darwin*)     gui() { open "${1:-.}"; } ;;
  linux-gnu|*) : ;;
esac

if pgrep Thunar &> /dev/null
then
  gui() { thunar "${@:-.}"; }
elif pgrep nautilus  &> /dev/null
then
  gui() { nautilus "${@:-.}"; }
elif pgrep explorer &> /dev/null
then
  gui() { explorer "${@:-.}"; }
fi

# ssh-keygen -t rsa
ssh_auth()
{
  ssh "$1" '[[ -d .ssh ]] || { mkdir .ssh && chmod 700 .ssh; cat >> .ssh/authorized_keys' < ~/.ssh/id_rsa.pub
}

# Find with a couple defaults.
f()
{
  # The trouble is that -regextype must appear after path but before expression.
  # HACK: "-D debugopts" unsupported and -[HLPO] options assumed to before dirs.
  local a=()
  while [[ -n "$1" ]] && ( [[ ! "${1:0:1}" =~ [-!(),] ]] || [[ "${1:0:2}" =~ -[HLPO] ]] )
  do
    a+=("$1")

    # Eliminate arg from @.
    shift
  done

  find -O3 "${a[@]}" -nowarn -regextype egrep "$@"
}
# Notes:
# - Pruning ex: find rubadub moon \( -path moon/.git -o -path rubadub/.git \) -prune -o \( -type f -o -type l \)
#   Note: still prints pruned dir.

# Find non-binary files.
ftxt() { f "$@"|file --mime-encoding -Nf-|grep -v binary\$|s 's_(.+): .+$_\1_';}

# f -iregex '.*(ehci|usb).*\.(c|h|inf)'|x sed -ri 's_Portions Copyright_Portions copyright_' # search and replace
# f \( -ipath './edk2/Build' -o -ipath './edk2/Conf' \) -prune -o -iname 'usb*' -print # find exclude
#fxchpp -o -path ./doxygen/ -prune
# f -path ./doxygen/ -prune -o -print|x g -i F22E -- why doesn't this work
# f -path ./doxygen -prune -o -type f -iregex '.*(\.c|\.cpp|\.h|\.hpp)$' -print|x g 'Handle FW status:' -- note print is needed to avoid printing pruned dirs.

# ------------------------------------------------------------------------------
# Find Files with Extension

# $1  Optional iregex for file extension.
# ... Optional arguments passed to find
fx()
{
  local rex=
  if [[ $# -gt 0 ]]
  then
    rex="($1)"
    shift
  fi

  f -type f -iregex '^.*'"${rex}"'$' "$@"
}

# TODO: it would be nice if this cursed find was more easily scriptable. I need
# to add a way to pass dirs and stuff.

# C / C++.
alias  fxcpp="fx '\.c|\.cpp'"
alias  fxhpp="fx '\.h|\.hpp'"
alias fxchpp="fx '\.c|\.cpp|\.h|\.hpp'"

# Sometimes I distinguish similar directories with a little nothing file. For
# example, eng_build_verbose.dsc.
alias fxdsc='f -iname "*.dsc" -maxdepth 2'

# ------------------------------------------------------------------------------
# Misc

[[ -f ~/.bashrc_udev ]] && . ~/.bashrc_udev

# Source Android configuration, if present.
[[ -f ~/.bashrc_android ]] && . ~/.bashrc_android

# Source Perforce configuration, if present.
#[[ -f ~/.bashrc_p4 ]] && . ~/.bashrc_p4

# Source QEMU configuration, if present.
[[ -f ~/.bashrc_qemu ]] && . ~/.bashrc_qemu

# Source SG configuration, if present.
[[ -f ~/.bashrc_sg ]] && . ~/.bashrc_sg

# Source private configuration, if present.
[[ -f ~/.bashrc_home ]] && . ~/.bashrc_home

# Source work configuration, if present.
[[ -f ~/.bashrc_work ]] && . ~/.bashrc_work

up_file()
{
  local f="$1"
  local pwd="$PWD"
  local err=1

  while :
  do
    if [[ -f "$f" ]]
    then
      err=0
      echo "$PWD/$f"
      break
    fi
    [[ "$PWD" != "/" ]] && builtin cd .. || break
  done

  builtin cd "$pwd" && return $err
}

# TODO: use up_file to find existing db.
alias udb='updatedb -l0 -omlocate.db -e $(abspath ./.git) -U .'
loc()
{
  locate -d"$(up_file mlocate.db)" -i --regex "${@:-.}"
}


# dic() { ! wn "$@" -over; } # Dictionary definition.
# alias gd='aspell dump master|g -i' # Grep mediocre dictionary.

# ------------------------------------------------------------------------------
# Behave Like Windows

# Mimic Explorer style directory navigation. I just really don't like having to
# type distinct commands to change directories. Set a-up, down, right, and left
# to unused keys.
# See also: http://unix.stackexchange.com/questions/9664/how-to-configure-inputrc-so-altup-has-the-effect-of-cd
# Note: these cause a potential mismatch between PS1's \w and actual PWD.
case "$OSTYPE" in
  cygwin)
    # HACK: I'm not sure what program to use to get key codes on Windows. I
    # used Zsh and switched to the rudimentary .safe keymap which just prints
    # control sequences directly on the prompt: bindkey -A .safe main. Try
    # "cat -A" next time.

    # Map <Alt + CursorKey> bindings to unused keys, then map the unused keys to
    # functions. (You can also use "cat -A" or "<Ctrl-V><Keys>" to dump the key
    # sequences. There's no builtin termcap in infocmp / tput AFIK.)

    bind '"\e\e[A":"\201"'
    bind '"\e\e[B":"\202"'
    bind '"\e\e[C":"\203"'
    bind '"\e\e[D":"\204"'
  ;;
  linux-gnu|*)
    bind '"\e[1;3A":"\201"'
    bind '"\e[1;3B":"\202"'
    bind '"\e[1;3C":"\203"'
    bind '"\e[1;3D":"\204"'
  ;;
esac

# Map unused keys to push parent, pop, next, and previous directory.
bind -x '"\201":..'
bind -x '"\202":P'
bind -x '"\203":pf'
bind -x '"\204":pb'

# TODO: reverse-menu-complete

#TODO: pull in old zsh
#alias cls=printf\ '\033\143' # TODO: figure out alternative sln for screen.
# consider reset
# TODO: see also "tput reset"
alias cls=reset

# shopt's huponexit is only applicable to login shells. The following covers
# nonlogin shells too.
# trap 'kill -HUP -$$' exit
# trap 'kill -9 -$$' exit

# huponexit is only applicable to login shells, this covers nonlogin too.
# trap_and_hup() { trap 'kill -HUP -$$' exit; } # kill -9
# trap_and_hup
# ps --ppid $$|wc -l -gt 2
# kill -0 $pid

# No way to catch change directory and update term title? Is that Zsh only?
# Assume command names that are directory names are the arguments to cd.
#shopt -s autocd
# Assume that non-directory arguments to cd are variables with directory values.
#shopt -s cdable_vars

# TODO: fix this up nice like retry.
# job& spin& fg %job
# $1 pid
spin()
{
  # HACK: I don't know of a way to guarantee the PID or job isn't recycled in
  # Bash.

  local pid=$1
  local rest=${2:-10}
  local cmd="$3"
  while kill -0 $pid 2> /dev/null
  do
    local log="$(last_log)"
    # Periodically update the user.
    echo "$(date +%F-%H-%M) $(wc -l "$log"|s 's_([0-9]+).*_\1_'): $(tail -qn1 "$log")"
    sleep $rest
  done
}

# proc&
# spin&
# wait pid
# Prefix args. "${@/#/$i}"
alias ps='ps -eF fe'
xprop_pid()
{
  # Only print the PID if it's not a dead parent.
  \ps $(xprop -f _NET_WM_PID 0c ' = $0\n' _NET_WM_PID|
        sed -r 's_.* = ([0-9]+)_\1_')
}
#pidof
#TODO: declare / readonly / local

alias top=htop

#naut() { nautilus "${@:-.}"; }

# rm -f cscope.* tags tags.files mlocate.db
index_pwd()
{
  time \
  {
    # Generate a nice index for future file lookups.
    udb &&

    # Updatedb doesn't track sufficient file information to generate a tidy code
    # listing. There are numerous ways to sift out binaries and other unwanted
    # cruft. If Grep doesn't work, consider file, or find + sed.
    #loc|sed -r '/\.git\/|\.repo\/|doxygen\/|cscope|\.(lst|d|o|dbo|a|so|jar|png|jpg|pdf|map|dep|sym|exe)$/ d; s_\\_\\\\_g; s_"_\\"_g; s_^|$_"_g' >| cscope.files
    grep -lrID skip \
      --exclude-dir={.repo,.git,doxygen} \
      --exclude=\*.{d,dep,lst,map,sym,log} \
      --exclude=\*{cscope,tags}\* \
      . . >| tags.files && #2> /dev/null

    # HACK: workaround "ctags: "tags" doesn't look like a tag file; I refuse to
    # overwrite it."
    rm -f tags &&

    # Generate a Ctags database.
    ctags -L tags.files --fields=+iaS --extra=+q &&

    # Generate a Cscope database.
    sed -r 's_\\_\\\\_g; s_"_\\"_g; s_^|$_"_g' tags.files >| cscope.files &&
    cscope -eq < /dev/null #&&
  }
}
dumb_dox()
{
  PROJECT_NAME="$(basename "$PWD")" time doxygen ~/.doxygen/Doxyfile_c
}

# ------------------------------------------------------------------------------
# Notes
# du -sh, df -h /
# [ ! -t 0 ] # stdin
# paste, join, cut
# rsync -vruK audio barnacle:~
# g '\<foo\>' # grep word foo
# ls -Ad --color=always */ .*/ # List directories in PWD.
# strings -a
# pdftotext -layout
# tac: reverse list
# TODO: email
# f -type f -printf '%T@ %p\n'|sort
# TODO: git grep
#cat *|sort|uniq -dc|sort  -r
# increntmal multi-line search working? not very practical so far, not to stop anyway
# dd bs=1M count=1 if=/dev/zero of=1M.bin - gen big fat file
# find duplicates - sort|uniq -d
# 2>&1 stderr to stdout
# &> stderr and stdout to file
# echo {1..5} # print 1 2 3 4 5
# dd if=/dev/dvd of=dvd.iso
# cat -v

# TODO: javac colorize / format warnings and errors.
# sudo lshw -C network; lspci|grep -i eth; lspci -nn|grep Eth
# git, gitk
# info crontab @reboot
# ${BASH_REMATCH[0]} 
# gnu moreutils
# echo -e 'a\nb\nc'|sed -r -e '$a\foo' -e '$a\bar'
# gnu parallel

# TODO: man ps, man bash -> LC_TIME
# echo foo{,,,,,,}
# {1..10} --> 1 2 3 4 5 6 7 8 9 10
# man watch
# echo() { printf "%b\n" "$*"; }
# complete -G
#watch -n 1 --precise 'df -h /'
# TODO: rename files or functions with -?
# vboxmanage list runningvms
# VBoxManage controlvm "<name>" savestate
# unp -u * # unpack (extract, decompress, unzip) archive
# aspell dump master # dump mediocre dictionary

#sudo chroot ~/work/chroot/lucid/ su - $USER
#last-log -> last-glob-n, keybind to shift tab or something.
#time mm -j -l2.5
alias mtime='date +%s'
# trap 'kill -HUP -$$' exit
#udevadm
#lsb_release --short --codename
#dpkg --print-architecture
#dpkg -l pbuilder or dpkg-query -l pbuilder? wildcard?
# TODO: need an alias for sudo screen /dev/ttuUSB0 115200. TODO: need udev rules too.
# git gui
# nohup
# sudo netstat -lepunt, lsof -i
# find -exec vs xargs. which is fastest
# awk, columns
# getopts

# (Last sorted argument.)
larg()
{
  [[ "${1:0:1}" == "-" ]] &&
  {
    local a=("$@")
    local i=0
    local n=$(($# - 1))

    while [[ $i -lt $n ]] && [[ "${a[$i]}" != "--" ]] && shift
    do
      shift
      let i+=1
    done

    shift
    let i+=1
    a=("${a:0:$i}")
  }

e "${a[@]}"
e "$@"

  printf "%s\n" "$@"|sort "${a[@]}"|tail -n1
}
#--, - arg parsing







# dpkg-query -L udev | grep rules
# tail -f -n 0 /var/log/kern.log
# find /sys ! -type l -iname ttyUSB0
# udevadm info --attribute-walk --path=/sys/class/tty/ttyUSB0
# udevadm monitor --environment --property
# udevadm info -a --attribute-walk --root --name=/dev/ttyUSB0
# screen /dev/ttyUSB0 115200
# diff <(cmd1) <(cmd2)
# apt-cache search jar

# coproc
# IFS= read -r
# LC_COLLATE=C tr A-Z a-z OR tr '[:upper:]' '[:lower:]'
# mapfile
# flock

# Note: this will kill your nice .bash_hist file.
alias noenv='env -i bash --noinit-file --noediting --noprofile --norc '

# TODO: gnu readline lib settings.
# TODO: see command execution environment and environment in bash man.
# Prints Bash environment.
envy()
{
  alias
  declare -p
  declare -f
  hash -l
  trap -p
  bind -psv
  infocmp
  # TODO: go through Bash source to understand what other attributes a shell
  # maintains.

  # TODO: copy / overwrite env.
  # Shopt Options (all stored in read-only variable BASHOPTS?)
  # shopt -p
  # Set Options (all stored in read-only variable SHELLOPTS? Superset of -?)
  # TODO: how to set readonly vars?
  # TODO: PWD
}

# readonly
# mapfile
# compgen
# complete
# getopts
# coproc
# compopt
# bind
# help read, read -r, ifs
# #!/usr/bin/env foo params?
# TODO: init link warty
# TODO: PS for pwd on ssh / chroot / name only cd broken.
# TODO: NDK location should link to versioned location.

# From Matthew Mead. TODO: review.
export LESS_TERMCAP_mb=$'\E[01;31m'      # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'      # begin bold
export LESS_TERMCAP_me=$'\E[0m'          # end mode
export LESS_TERMCAP_se=$'\E[0m'          # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'   # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'          # end underline
export LESS_TERMCAP_us=$'\E[01;32m'      # begin underline

# cal
# iodine
# truncate - shrink or extend the size of a file to the specified size.
# gnome-specimen - fonts
# at aspell, emacs completion
# sg3-utils, lsscsi, lspci -- sg_scan, sg_ses, sg_inq /dev/sg5, dmesg, /var/log/syslog
# pwd -P = readlink -m .
#git --no-pager show --pretty="format:" --name-only f38f3c27d6461d8ae5a97f79c6b41bfd27675bf2
#PWD="$(cygpath -w $(pwd))" p4 -x- edit
#cb|xargs -rd\\n git add -n
# pushd "$(git rev-parse --show-toplevel)" && { git status --porcelain|sed -rn 's_ M (.+)_\1_p'|xargs -rd\\n git add -n; popd; }

#rdp() {
#  # HACK: Xmonad doesn't support _NET_WORKAREA.
#  declare -ai wh=( $(xwininfo -root|sed -nr '/^  Width: /N; s_^  Width: ([0-9]+)\n  Height: ([0-9]+)_\1 \2_p') )
#  let wh[1]-=25 # HACK: allow for dock bar.
#  rdesktop -g${wh[0]}x${wh[1]} -z -xm -P "$@"
#}
alias rdp='rdesktop -zKPxm -gworkarea'
#iodine, pkg.sh
#git update-index --assume-unchanged $files
#git ls-files -v|grep '^h'
#git update-index --no-assume-unchanged $files
#usbip

youtube-dl-mp3()
{
  # HACK: when --extract-audio is used with -o-, avprobe doesn't seem to close
  # the pipe. Maybe it's non-reentrant?
  youtube-dl --audio-quality best -qo- "$1"|
  avconv -v warning -i pipe: "$(youtube-dl -qe "$1").mp3"
}
# youtube-dl-mp3 "$(cb)"
# youtube-dl --max-quality 37 --title --playlist-start=1 --playlist-end=100 --ignore-errors youtube-playlist-url # Download playlist vids
# groove



# gdb -batch -x foo.gdb
#google-chrome --user-data-dir=/tmp --incognito
#chronic
# csplit, split, cat
# join
#tog() { while :; do echo gpiooutput $1 $2 0 1 > /dev/ttyUSB0; sleep ${3:-.15}; echo gpiooutput $1 $2 0 0 > /dev/ttyUSB0; sleep ${3:-.15}; done; }


# google docs list -f audio
#declare -r user=me
#declare -r folder=foo
#cd "$folder"
#sort <(google -u "$user" docs list -f "$folder"|sed -r 's_(.+),https://.+_\1_') <(ls -1 *)|
#uniq -u|
# HACK: google-cl doesn't handle files with unusual extensions.
#xargs -rd\\n google -u "$user" docs upload --no-convert -f "$folder"
#while IFS= read -r file
#do
#  declare tmpfile="$(mktemp tmp.XXX)" &&
#  ln -f "$file" "$tmpfile" &&
#
#  time google -u "$user" docs upload --no-convert -f "$folder" --src "$tmpfile" --title "$file"
#
#  rm -f "$tmpfile"
#done

# at command vs cron
# nl
#while IFS= read -ru9 -d $'\0' file; do
#  :
#done 9< <(cmd)

# Investigate synclient as touchegg replacement http://uselessuseofcat.com/?p=74
# watch sensors
# wodim vs dd
# DISPLAY=:0.0 gnome-calculator
# mkfs.vfat -I /dev/sdX
# clonezilla
# info -f grub
# moreutils, at
# unbuffer
# blkid -o value -s UUID

alias ack='ack-grep --smart-case'
alias cack='ack --ignore-dir=doxygen --cpp --cc --asm'

incognito()
{
  google-chrome --user-data-dir=/tmp --incognito --no-default-browser-check "$@" &> /dev/null&
}
#dislocate
export PYTHONSTARTUP=~/.pystartup

# echo stty cols $COLUMNS rows $LINES

alias xwid="xwininfo|sed -rn '/^xwininfo: Window id: / s_^xwininfo: Window id: ([x0-9a-fA-F]+).*_\1_p'"
# strace, nm, ldd
# rlwrap
# sed 3{p;q}
# script, readelf, objdump, gdb, ndisasm, strings -t
# sg_map

# udevadm info --query=all --name=/dev/sg21
# mapfile -t < <(echo -e 'foo bar boo\nbaz') && echo "${MAPFILE[@]}"

gccsh() #<<EOF
{
  declare t="$(mktemp)" &&
  # TODO: eval flags.
  g++ -xc++ -ansi -pedantic -Wall -Wextra -Wconversion -Werror - -o"$t" && "$t" "$@"
  rm -f "$t"
}


# man: errno

# TODO: retry n "$@"
# TODO: timeout
retry()
{
  declare i=0
  declare -r retries=10

  while [[ $i -lt $retries ]] && ! "$@"
  do
    sleep .5
    let i+=1
  done

  if [[ $i -eq $retries ]]
  then
    echo "fail: $@"
  fi
}

# Consider pyp as an alternative.
# http://docs.python.org/2/library/re.html
reggie()
{
:
#  python2.7 -c '
#import sys, re

#    file = open( sys.stdin )
#    assert( file )
#    str = file.read()
#    file.close()
#
#for line in :
#  sys.stdout.write(re.sub('PATTERN', 'SUBSTITUTION', line))"
#'

}
# awk ' !x[$0]++'
# TODO python sub: Python regex grep / substituion (without sub, use match?).
# TODO python sub: python -c 'import re, sys; sys.stdout.write(re.sub("/\*(.|\r?\n)*?\*/", "", sys.stdin.read(), 0))'




#ioctl
#{
#  gccsh "$@" <<EOF
##include <stdio.h>
##include <fcntl.h>
##include <errno.h>
##include <string.h>
##include <sys/ioctl.h>
##include <unistd.h>
##include <linux/usbdevice_fs.h>
#
#int main(int argc, char ** argv)
#{
#  int ret = ~0;
#  const char * filename = 0;
#  int fd = 0;
#
#  if (!argc)
#  {
#    ret = -2;
#    goto ret;
#  }
#  filename = argv[1];
#
#  fd = open(filename, O_WRONLY);
#  if (fd <= 0)
#  {
#    ret = -3;
#    goto ret;
#  }
#
#  ret = ioctl(fd, USBDEVFS_RESET, 0);
#  if (ret)
#  {
#    fprintf(stderr, "ioctl failed.\n");
#    goto ret;
#  }
#
#  ret = close(fd);
#  if (ret)
#  {
#    fprintf(stderr, "close failed.\n");
#    goto ret;
#  }
#
#  ret:
#  if (ret)
#  {
#    fprintf(stderr,
#      "%s:%u ret %d, errno %d, strerror %s\n",
#      __FUNCTION__,
#      __LINE__,
#      ret,
#      errno,
#      strerror(errno));
#  }
#  return ret;
#}
#EOF
#}


syslog() { tail -f /var/log/syslog; }
cpu_cnt() { grep -c processor /proc/cpuinfo; }

# until [[ $# -eq 0 ]]
# do
# shift
# done

cv() {
  cdargs "$1"" && cd ""'cat ""$HOME/.cdargsresult""'"" ;"
}

export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1
[[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && . ~/.autojump/etc/profile.d/autojump.sh


pgrep_kill()
{
  declare pids=( $(pgrep "$@") ) && kill -s kill ${pids[@]}
}
# fdisk -l, lsblk
# strace -e open lsblk

# TODO: wrap rm.
# lspci -v
#fdupes
# nmap -sP 192.168.1.0-255, nbtscan
# TODO:usb_id_to_bind doesn't work
#python -c 'import fcntl, sys, usb; print fcntl.fcntl(open(sys.argv[1]), USBDEVFS_RESET)' /dev/bus/usb/003/002
#fdisk, hdparm, hwinfo --ide
# dmidecode -- bios ver. see also: http://www.dufault.info/blog/a-better-way-to-find-your-bios-version-in-linux/
# use functions not aliases in general for bash?
# comm, nohup



hex2bin()
{
  #sed -r 's_[[:xdigit:]]_\U\0_g; s_$_\n_; 1i obase=2\; ibase=16'|
  #BC_LINE_LENGTH=0 bc
  # it expects zero padding on input but no way to zero pad on output :( :( :(
  # HACK: bc expects zero padded input but there's no way to pad output.
  sed -r 's_0_0000_g;    s_1_0001_g;    s_2_0010_g;    s_3_0011_g;
          s_4_0100_g;    s_5_0101_g;    s_6_0110_g;    s_7_0111_g;
          s_8_1000_g;    s_9_1001_g;    s_[aA]_1010_g; s_[bB]_1011_g;
          s_[cC]_1100_g; s_[dD]_1101_g; s_[eE]_1110_g; s_[fF]_1111_g;'
}
__test_hex2bin()
{
  declare -i err=0

  if [[ "$(hex2bin <<< 0a)" != 00001010 ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO bad conversion."
  fi

  if [[ "$(hex2bin <<< A)" != 1010 ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO bad conversion."
  fi

  return $err
}
__test_hex2bin

str2hex(){ echo -n "$*"|hexdump -ve '1/1 "%02x"'; }
__test_str2hex()
{
  declare -i err=0

  if [[ "$(str2hex A BA)" != 41204241 ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO bad conversion."
  fi

  return $err
}
__test_str2hex

#Dumps a hex representation of stdin.
#  [n0] - Byte offset to begin dump at.
#  [n1] - Number of bytes to dump.'
seek()
{
  hexdump -ve '1/1 "%02x"' ${1:+"-s$1"} ${2:+"-n$2"}
}
__test_seek()
{
  declare -i err=0

  if [[ "$(echo -n A|seek)" != 41 ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO bad conversion."
  fi

  return $err
}
__test_seek

#declare __help_bseek='[n0] [n1] [n2] [n3] - Dumps a binary textual representation of stdin.
#    [n0] - Byte offset to begin dump at.
#    [n1] - Number of bytes to dump.
#    [n2] - Bit offset to begin dump at (after byte offset, if supplied).
#    [n3] - Number of bits to dump (within number of bytes, if supplied).'
bseek()
{
  seek "$1" "$2"|
  hex2bin|
  sed -r -e s_^__ ${3:+ -e "s_^[01]{$3}__"} ${4:+-e "s_^([01]{$4}).*_\1_"}
}
__test_bseek()
{
  declare -i err=0

  if [[ "$(echo -n A|bseek)" != 01000001 ]]
  then
    err=1
    log_err "$FUNCNAME:$LINENO bad conversion."
  fi

  return $err
}
__test_bseek


[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"
return


takes a stall value
retry()
{
  declare -i ret=0
  declare -i i=0

  while :
  do
    # TODO: we may want to capture stderr and only output it when the comamand
    # exit status is not stall.
    "$@"
    ret=$?

    if [[ $ret -eq 2 ]] || [[ $ret -eq 6 ]]
    then
      let i+=1
      [[ $i -lt $sg_retry_limit ]] || break
      sleep 1
    else
      # Success or non-retryable failure.
      break
    fi
  done

  if   [[ $ret -eq 2 ]]
  then
    echo_warn "\"$*\" consistently returned not ready state after $sg_retry_limit retries."
  elif [[ $ret -eq 6 ]]
  then
    echo_warn "\"$*\" consistently returned unit attention state after $sg_retry_limit retries."
  elif [[ $ret -ne 0 ]]
  then
    echo_warn "\"$*\" returned $ret"
  fi

  return $ret
}







#nm -gC
#readelf -Ws



alias py=python

alias G=git

# vlc -I dummy v4l2:///dev/video0 --video-filter scene --no-audio --scene-path ~/foo --scene-prefix image_prefix --scene-format png vlc://quit --run-time=1


cfg_serial_tty()
{
stty -F "$1" \
  eof ^D eol ^\? eol2 ^\? erase ^\? intr ^C kill ^U lnext ^V quit ^\\ rprnt ^R start ^Q stop ^S susp ^Z swtch ^\? werase ^W \
  cols $COLUMNS ispeed 115200 line 0 min 1 ospeed 115200 rows $LINES time 5 \
  clocal cread -crtscts cs8 -cstopb -hup -parenb \
  brkint -icrnl -ignbrk igncr -ignpar -imaxbel -inlcr -inpck -istrip -iutf8 -iuclc ixany -ixoff -ixon -parmrk \
  bs0 cr0 ff0 nl0 -ocrnl -ofdel -ofill -olcuc onlcr -onlret -onocr -opost \
  crterase crtkill ctlecho -echo echok -echonl -echoprt -icanon iexten isig -noflsh -tostop -xcase
}
# HACK:
# up arrow doesn't work... you need expect to translate or have to remap. best to use console program
#cat /dev/ttyUSB0& cat > /dev/ttyUSB0

# tftp_dwld 10.188.117.2 sxp_evbd_rom.bin a
# ipconfig dhcp 0 ip 10.188.117.80 nm 255.255.252.0 gw 10.188.116.1
# ipconfig macb verbose
# sudo atftpd --daemon --no-fork --port 69 -v7 --trace .

# Very dangerous. For build servers only.
# git reset --hard HEAD && git clean -dfqx
# exec 3<> /dev/ttyXXX
# cat <&3>
# # close
# exec 3>&-

# socat /dev/ttyUSB0,nonblock,raw,echo=1 STDIO

# man od dump in other formats
# expect, unbuffer, stdbuf, script

# screen, minicom, cu
# sx
# socat
# open, chvt
# script
# mkfifo test

#pdfnup
#pdfposter
#pdfjam
#time wget -nv -np -p -k -r -l8 --follow-ftp --retry-connrefused -t5 -c -N --http-user xxx --http-password yyy zzz
#find -L / -samefile /dev/sg6 2>/dev/null