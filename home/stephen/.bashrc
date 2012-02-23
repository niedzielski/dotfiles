#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Copyright 2009 - 2011 Stephen Niedzielski. Licensed under GPLv3+.

# ------------------------------------------------------------------------------
# Interactive Shell Check
[[ "$-" == *i* ]] || return

# ------------------------------------------------------------------------------
# History

# Don't record adjacent duplicate commands or command lines starting with a
# space.
HISTCONTROL=ignoredups:ignorespace

# Set max commands and lines in Bash history.
HISTSIZE=50000
HISTFILESIZE=50000

# Timestamp entries.
HISTTIMEFORMAT='%F-%H-%M-%S '

# Preserve multi-line commands in one history entry.
shopt -s cmdhist

# Append to the history file, don't overwrite it.
shopt -s histappend

# Don't replace newlines with semicolons in multi-line commands.
shopt -s lithist

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
shopt -s dirspell

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
# Prompt and Window Title

update_term_title() { echo -en "\033]0;$USER@$HOSTNAME:$PWD\007"; }

# Initial update.
update_term_title &&

# "$ " colored green for zero exit status, red otherwise.
PS1='$( [[ $? -eq 0 ]] && echo "\[\033[22;32m\]" || echo "\[\033[22;31m\]" )' &&

# Colorless.
PS1+='\$ \[\033[00m\]'

# ------------------------------------------------------------------------------
# Simple Shell Supplements

# Miscellaneous.
alias   cp='cp -i' # Prompt on overwrite.
alias    e='echo'
alias grep='grep --color=auto -EID skip -d skip' # Extended regex, color, skip
                                                 # bin, dev, sockets, and dirs.
alias    g=grep
alias   mv='mv -i' # Prompt on overwrite.
alias    m='mv'
alias   md='mkdir'
alias   rm='rm -i' # Always prompt.
alias    r='rm'
#alias  sed='sed -r' # Extended regular expression... but Sed won't permit -rrrrrrrrrr.
alias    s='sed -r'
alias    t='touch'
alias    x='xargs -d\\n ' # Xargs newline delimited + expansion.
# Notes:
# - Copy ex: x cp --parents -t"$dst"
# - TODO: Figure out general case. x -i implies -L1. Use
#   xargs --show-limits --no-run-if-empty < /dev/null?
alias head='head -n$(($LINES - 5))'
alias tail='tail -n$(($LINES - 5))'
alias diff='colordiff -d --speed-large-files --suppress-common-lines -W$COLUMNS -y'
alias less='less -ir' # Smart ignore-case + output control chars.

alias timestamp='date +%F-%H-%M-%S-%N'

# Directory listing.
alias  ls='ls -Ap --color=auto' # Alphabetically.
alias   l=ls
alias lex='l -X'                   # By extension.
alias lsi='l -S'                   # By size.
alias lct='l -c'                   # By creation time.
alias lmt='l -t'                   # By mod time.
alias lat='l -u'                   # By access time.
[[ -x /usr/bin/dircolors ]] && eval "$(dircolors -b)" # Enable color support.

alias rsync='rsync -azv --partial-dir=.rsync-partial --partial'

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
cd() { builtin cd "$@"; update_term_title; }
alias c=cd
pushd() { builtin pushd "$@" > /dev/null; d; update_term_title; } # Change directory.
alias p=pushd
alias pb='pushd +1' # Previous directory.
alias pf='pushd -0' # Next directory.
popd() { builtin popd > /dev/null; d; update_term_title; }
alias P=popd

alias abspath='readlink -m'

gvim() { command gvim -p "$@" 2> /dev/null; } # One tab per file.
alias v=gvim

lynx() { command lynx -accept_all_cookies "$@" 2> /dev/null; }

# TODO: fix find \! -user stephen fails.
# Find with a couple defaults.
find()
{
  # The trouble is that -regextype must appear after path but before expression.
  local d=()
  while [[ -n "$1" ]] && [[ "${1:0:1}" != '-' ]]
  do
    # Non-dash parameter, use it.
    d+=("$1")

    # Eliminate dir from @.
    shift
  done

  command find -O3 "${d[@]}" -nowarn -regextype egrep "$@"
}
alias f=find
# Notes:
# - Pruning ex: find rubadub moon \( -path moon/.git -o -path rubadub/.git \) -prune -o \( -type f -o -type l \)
#   Note: still prints pruned dir.

# Find non-binary files.
ftxt() { f "$@"|file --mime-encoding -Nf-|grep -v binary\$|s 's_(.+): .+$_\1_';}

# Clipboard for GUI integration.
cb()
{
  if [[ ! -t 0 ]]
  then
    xclip -sel c
  else
    xclip -sel c -o
  fi
}

gui() { nautilus "${1:-.}"; }

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

# C / C++.
alias  fxcpp="fx '\.c|\.cpp'"
alias  fxhpp="fx '\.h|\.hpp'"
alias fxchpp="fx '\.c|\.cpp|\.h|\.hpp'"

# Sometimes I distinguish similar directories with a little nothing file. For
# example, eng_build_verbose.dsc.
alias fxdsc='f -iname "*.dsc" -maxdepth 2'

# ------------------------------------------------------------------------------
# Misc

# Source Android configuration, if present.
[[ -f ~/.bashrc_android ]] && . ~/.bashrc_android

# Source Perforce configuration, if present.
[[ -f ~/.bashrc_p4 ]] && . ~/.bashrc_p4

# Source private configuration, if present.
[[ -f ~/.bashrc_home ]] && . ~/.bashrc_home

# Source work configuration, if present.
[[ -f ~/.bashrc_work ]] && . ~/.bashrc_work


echo_ok()  {   printf '\033[22;32m'; echo "$@"; printf '\033[00m';        } # Green
echo_wrn() { { printf '\033[22;33m'; echo "$@"; printf '\033[00m'; } >&2; } # Yellow
echo_ng()  { { printf '\033[22;31m'; echo "$@"; printf '\033[00m'; } >&2; } # Red
prompt() { read -p '<Enter> to continue, <ctrl-c> to abort: '; }

rubadub_root="$(readlink -e "$(dirname "$(readlink -e "$BASH_SOURCE")")/../..")"
init_links()
{
  # TODO: rename function.
  # TODO: make this pretty and maybe isolate these to files?
  dpkg-query -l autossh vim-gnome git-all valgrind xclip build-essential samba smbfs meld openssh-server winbind gmrun gconf-editor nfs nfs-kernel-server cachefilesd curl ccache colordiff dos2unix gimp html2text libdevice-usb-perl lynx p7zip screen htop > /dev/null

  [[ -d "$rubadub_root" ]] || return

  # Make some directories.
  [[ -d ~/bin ]] || mkdir ~/bin
  [[ -d ~/opt ]] || mkdir ~/opt

  echo_wrn 'etc/default/cachefilesd requires manual linking'
  echo_wrn 'etc/nsswitch.conf requires manual linking'
  echo_wrn 'etc/udev/rules.d/51-android.rules requires manual linking and a system reboot (sudo ln -s ~/work/rubadub/etc/udev/rules.d/51-android.rules /etc/udev/rules.d/51-android.rules)'
  echo_wrn 'etc/bash_completion.d/android requires manual linking (sudo ln -s ~/work/rubadub/etc/bash_completion.d/android /etc/bash_completion.d/android)'

  # Generate null shorthand link.
  ln -s /dev/null /0

  # Link home files.
  local target_home="$rubadub_root/home/stephen"
  for f in \
    .android/ddms.cfg \
    .bashrc \
    .bashrc_android \
    .bashrc_p4 \
    bin/4tw \
    bin/snap \
    .gconf/apps/metacity \
    .gitconfig \
    .inputrc \
    .profile \
    .screenrc \
    .vimrc \
    opt/eclipse/eclipse.ini \
    opt/signapk \
    work/.metadata/.plugins/org.eclipse.core.runtime/.settings/com.android.ide.eclipse.ddms.prefs \
    .Xmodmap
  do
    ln -s "$target_home/$f" ~/"$(dirname $f)"
  done

  ln -s /usr/bin/google-chrome ~/bin/chrome
  ln -s ~/opt/eclipse/eclipse ~/bin/eclipse
}

#which xclip
#ctags, vim

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
alias updatedb='updatedb -l0 -oindex.db -U .'
loc()
{
  locate -Pd"$(up_file index.db)" --regex "${@:-.}"
}

# Power cycles the embedded webcam on my System76 Gazelle Professional laptop,
# which malfunctions regularly. Since it's builtin, I can't cycle the cable
# manually and the bus and port stay the same (2-1.6).
# TODO: how to unbind /sys/bus/usb/drivers/uvcvideo/*?
reset_webcam()
{
  echo '2-1.6'|sudo tee /sys/bus/usb/drivers/usb/unbind > /dev/null
  sleep 1
  echo '2-1.6'|sudo tee /sys/bus/usb/drivers/usb/bind  > /dev/null
}
# TODO: reset_usb for when the whole stack tanks.

# dic() { ! wn "$@" -over; } # Dictionary definition.
# alias gd='aspell dump master|g -i' # Grep mediocre dictionary.

# ------------------------------------------------------------------------------
# Behave Like Windows

# Mimic Explorer style directory navigation. I just really don't like having to
# type distinct commands to change directories. Set a-up, down, right, and left
# to unused keys.
# See also: http://unix.stackexchange.com/questions/9664/how-to-configure-inputrc-so-altup-has-the-effect-of-cd
case "$OSTYPE" in
  cygwin)
    # HACK: I'm not sure what program to use to get key codes on Windows. I
    # used Zsh and switched to the rudimentary .safe keymap which just prints
    # control sequences directly on the prompt: bindkey -A .safe main.
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
bind -x '"\201":c ..; printf "\033[22;34m%s\033[00m\n" "$PWD"'
bind -x '"\202":P'
bind -x '"\203":pf'
bind -x '"\204":pb'

# c-left, c-right.
bind '"\e[1;5C": forward-word'
bind '"\e[1;5D": backward-word'

# TODO: investigate highlighting / marking for c-s-left, c-s-right, ...
#"\e[1;6C": ...
#"\e[1;6D": ...

# c-del, c-] (c-bs == bs and c-\ is some kind of signal).
bind '"\e[3;5~": kill-word'
bind '"\C-]": backward-kill-word'

# TODO: quote param.
#"\C-xq": "\eb\"\ef\""

# TODO: info rluserman

#TODO: pull in old zsh
alias cls=printf\ '\033\143' # TODO: figure out alternative sln for screen.

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
alias ps='ps -F f e'
xprop_pid()
{
  # Only print the PID if it's not a dead parent.
  ps $(xprop -f _NET_WM_PID 0c ' = $0\n' _NET_WM_PID|
       sed -r 's_.* = ([0-9]+)_\1_')
}

alias top=htop

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

# TODO: javac colorize / format warnings and errors.
# sudo lshw -C network; lspci|grep -i eth; lspci -nn|grep Eth
# git, gitk
# info crontab @reboot

alias ..='cd ..'

