#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Copyright 2009 - 2012 Stephen Niedzielski. Licensed under GPLv3.

# ------------------------------------------------------------------------------
# Interactive Shell Check
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

# Attempt to correct directory names when completing.
#  These don't seem to correct prior to passing to prog.
# shopt -s dirspell

# Source default Bash completions.
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

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
log() { echo "$@"|tee -a ${log_file:+"$log_file"}; }
log_ok()   {   printf '\033[22;32m'; log "$@"; printf '\033[00m';        } # Green
log_warn() { { printf '\033[22;33m'; log "$@"; printf '\033[00m'; } >&2; } # Yellow
log_err()  { { printf '\033[22;31m'; log "$@"; printf '\033[00m'; } >&2; } # Red

# ------------------------------------------------------------------------------
# Prompt and Window Title

# HACK: Bash won't read updates to PS1 made in readline.
#force_update_term_title() { echo -en "\033]0;$USER@${debian_chroot:-$HOSTNAME}:$PWD\007"; }
# TODO: consider the following for strapping into a CD hook: 
# http://stackoverflow.com/questions/3276247/is-there-a-hook-in-bash-to-find-out-when-the-cwd-changes
PS1='\[\e]0;\u@${debian_chroot:-\h}:\w\a\]'

# "$ " colored green for zero exit status, red otherwise.
PS1+='$( [[ $? -eq 0 ]] && echo "\[\033[22;32m\]" || echo "\[\033[22;31m\]" )'

# Back to colorless.
PS1+='\$ \[\033[00m\]'

# ------------------------------------------------------------------------------
# Simple Shell Supplements

alias     e='echo'
alias    cp='cp -ai' # Prompt on overwrite, preserve all.
alias    rm='rm -i' # Always prompt.
alias     t='touch'

alias    mv='mv -i' # Prompt on overwrite.
alias     m='mv'

# Extended regex, color, line num, skip binaries, devices, sockets, and dirs.
alias     g='grep --color=auto -nEID skip -d skip'
# TODO: switch to GREP_OPTIONS?

alias     s='sed -r'
alias   rsync='rsync -azv --partial-dir=.rsync-partial --partial' # See also zsync



alias     x='xargs -rd\\n ' # \n delimited, don't run on empty in, + expansion.
# Notes:
# - Copy ex: x cp --parents -t"$dst"
# - TODO: Figure out general case. x -i implies -L1. Use
#   xargs --show-limits --no-run-if-empty < /dev/null?

alias dif='colordiff -d --speed-large-files --suppress-common-lines'
alias vdif='dif -yW$COLUMNS' # Side by side (vertical) diff.

export LESS='-ir' # Smart ignore-case + output control chars.
alias pdfgrep='pdfgrep --color=auto'
alias timestamp='date +%F-%H-%M-%S-%N'


# TODO: visible stats.
# Directory listing.
alias  ls='ls -Ap --color=auto' # Alphabetically.
alias   l=ls
alias lex='l -X'                   # By extension.
alias lsi='l -S'                   # By size.
alias lct='l -c'                   # By creation time.
alias lmt='l -t'                   # By mod time.
alias lat='l -u'                   # By access time.
[[ -x /usr/bin/dircolors ]] && eval "$(dircolors -b)" # Enable color support.

# ------------------------------------------------------------------------------
# Less Simple Shell Supplements

# Directory navigation.
dirs()
{
  local color=3
  builtin dirs -p|while read path
  do
    printf "\033[22;3${color}m%s\033[00m " "$path"

    # Alternate the color after the first path.
    [[ $color -eq 4 ]] && color=5 || color=4;
  done
  echo # Newline.
}
alias d=dirs
c() { cd "$@"; d; }
p() { pushd "$@" > /dev/null; d; } # Change directory.
alias pb='p +1' # Previous directory.
alias pf='p -0' # Next directory.
P() { popd > /dev/null; d; }

#  These don't seem to correct prior to passing to prog.
shopt -s autocd cdable_vars # cdspell dirspell


shopt -s checkjobs
shopt -s checkwinsize


shopt -s hostcomplete

shopt -u huponexit
shopt -s no_empty_cmd_completion

set -b
#set -u

alias abspath='readlink -m'

v() { gvim -p "$@" 2> /dev/null; } # One tab per file.

lynx() { command lynx -accept_all_cookies "$@" 2> /dev/null; }

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
  linux-gnu|*)
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
  ssh "$1" '[[ -d .ssh ]] || mkdir .ssh; cat >> .ssh/authorized_keys' < ~/.ssh/id_rsa.pub
}

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
    if [[ -e "$f" ]]
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
alias udb='updatedb -l0 -oindex.db -U .'
loc()
{
  locate -d"$(up_file index.db)" --regex "${@:-.}"
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

# Use "eval $(antialias foo)" to invoke as a command.
antialias()
{
  # "alias" does escaping, so use "type" instead.
  type "$1"|
  sed -r 's%'"$1"' is aliased to `(.*)'\''%\1%' #' # HACK: Vim highlighting gets screwed up here.
}

index_pwd()
{
  time \
  {
    udb &&
    loc|sed -r '/\.git\/|doxygen\/|cscope|\.(lst|d|o|dbo|a|so|png|jpg|pdf|map|dep|sym|exe)$/ d; s_\\_\\\\_g; s_"_\\"_g; s_^|$_"_g' >| cscope.files &&
    echo -n|cscope -eq
  }
}

# ------------------------------------------------------------------------------
# Notes
# du -sh, df -h /
# [ ! -t 0 ] # stdin
# f -iregex '.*(ehci|usb).*\.(c|h|inf)'|x sed -ri 's_Portions Copyright_Portions copyright_' # search and replace
# f \( -ipath './edk2/Build' -o -ipath './edk2/Conf' \) -prune -o -iname 'usb*' -print # find exclude
# paste, join, cut
# rsync -vruK audio barnacle:~
# g '\<foo\>' # grep word foo
# ls -Ad --color=always */ .*/ # List directories in PWD.
# TODO python sub: Python regex grep / substituion (without sub, use match?).
# TODO python sub: python -c 'import re, sys; sys.stdout.write(re.sub("/\*(.|\r?\n)*?\*/", "", sys.stdin.read(), 0))'
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
# ctags -R

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


# Prints Bash environment.
envy()
{
  alias
  declare -p
  declare -f
  hash -l
  trap -p
  bind -psv
  # TODO: go through Bash source to understand what other attributes a shell
  # maintains.

  # TODO: copy / overwrite env.
  # Shopt Options (all stored in read-only variable BASHOPTS?)
  # shopt -p
  # Set Options (all stored in read-only variable SHELLOPTS? Superset of -?)
  # TODO: how to set readonly vars?
  # TODO: PWD
  # env -i bash --norc ... empty shell
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

# gdb -batch -x foo.gdb
#fxchpp -o -path ./doxygen/ -prune
#google-chrome --user-data-dir=/tmp --incognito
#chronic
# f -path ./doxygen/ -prune -o -print|x g -i F22E -- why doesn't this work
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
# f -path ./doxygen -prune -o -type f -iregex '.*(\.c|\.cpp|\.h|\.hpp)$' -print|x g 'Handle FW status:' -- note print is needed to avoid printing pruned dirs.
# blkid -o value -s UUID

alias ack='ack-grep --smart-case'
alias cack='ack --ignore-dir=doxygen --cpp --cc --asm'

incognito()
{
  google-chrome --user-data-dir=/tmp --incognito "$@" &> /dev/null&
}
#dislocate
export PYTHONSTARTUP=~/.pystartup
# PROJECT_NAME="$(basename "$PWD")" time doxygen ~/.doxygen/Doxyfile_c

# echo stty cols $COLUMNS rows $LINES

alias xwid="xwininfo|sed -rn '/^xwininfo: Window id: / s_^xwininfo: Window id: ([x0-9a-fA-F]+).*_\1_p'"
# strace, nm, ldd
# rlwrap
# sed 3{p;q}
# script, readelf, objdump, gdb, ndisasm, strings -t
# sg_map

# udevadm info --query=all --name=/dev/sg21
# mapfile -t < <(echo -e 'foo bar boo\nbaz') && echo "${MAPFILE[@]}"
deref() { eval echo -n \$"$1"; }

gccsh() #<<EOF
{
  declare t="$(mktemp)"
  {
    gcc -xc - -o"$t"
  } && "$t"
  rm -f "$t"
}

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
