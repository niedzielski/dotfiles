#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Copyright 2009 - 2011 Stephen Niedzielski. Licensed under GPLv3+.

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

update_term_title() { [[ "$-" == *i* ]] && echo -en "\033]0;$USER@$HOSTNAME:$PWD\007"; }

# Initial update for interactive shells.
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
alias grep='grep -E --color=auto -ID skip -d skip' # Extended regex, color, skip
                                                   # bin, dev, sockets, and dir.
alias    g=grep
alias   mv='mv -i' # Prompt on overwrite.
alias    m='mv'
alias   md='mkdir'
alias   rm='rm -i' # Always prompt.
alias    r='rm'
alias  sed='sed -r' # Extended regular expression.
alias    s=sed
alias    t='touch'
alias    x='xargs -d\\n' # Xargs newline delimited.
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

# Xargs grep.
alias xg="x $(alias grep|sed "s_alias grep='(.*)'_\1_")"

# Find with a couple defaults.
find()
{
  # The trouble is that -regextype must appear after path but before expression.
  local d='.' # Default to current directory.
  if [[ -n "$1" ]] && [[ "${1:0:1}" != '-' ]]
  then
    # Non-dash parameter, use it.
    d="$1"

    # Eliminate dir from @.
    shift
  fi

  command find -O3 "$d" -nowarn -regextype egrep "$@"
}
alias f=find

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

# ssh-keygen -t rsa # no passphrase
#ssh_auth()
#{
#  ssh "$1" '[[ -d .ssh ]] || mkdir .ssh; cat >> .ssh/authorized_keys' < ~/.ssh/id_rsa.pub
#}

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

init_links()
{
  [[ -d ~/bin ]] || mkdir ~/bin
  [[ -d ~/opt ]] || mkdir ~/opt

  # Generate null shorthand link.
  [[ -e /0 ]] || case "$OSTYPE" in
         cygwin) ln -s /dev/null /0 ;;
    linux-gnu|*) sudo ln -s /dev/null /0 ;;
  esac

  # A couple shortcuts.
  ln -fs /usr/bin/google-chrome ~/bin/chrome
  ln -fs /usr/bin/gnome-terminal ~/bin/term
  ln -fs {~/opt/eclipse,~/bin}/eclipse

  # Use version controlled files from PWD.
  ln -fs {"$PWD",~/bin}/4tw
  ln -fs {"$PWD",~}/.bashrc
  ln -fs {"$PWD",~}/.bashrc_android
  ln -fs {"$PWD",~}/.bashrc_p4
  ln -fs {"$PWD",~}/.inputrc
  ln -fs {"$PWD",/etc}/nsswitch.conf
  ln -fs {"$PWD",~}/.profile
  ln -fs {"$PWD",~}/.screenrc
  ln -fs {"$PWD",~/bin}/snap
  ln -fs {"$PWD",~}/.vimrc
  ln -fs {"$PWD",~}/.Xmodmap
# what to do about .gitconfig... if work = false? may need gitconfig_work
}

#which xclip
#ctags, vim
#eclipse

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
    echo "$(date +%F-%H-%M) $(wc -l "$log"|sed 's_([0-9]+).*_\1_'): $(tail -qn1 "$log")"
    sleep $rest
  done
}

# proc&
# spin&
# wait pid

