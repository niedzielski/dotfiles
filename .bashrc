#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Copyright 2011 Stephen Niedzielski. Licensed under GPLv3+.

# ------------------------------------------------------------------------------
# History

# Don't record adjacent duplicate commands or command lines starting with a
# space.
HISTCONTROL=ignoredups:ignorespace

# Set max command entries and file size in history.
HISTSIZE=5000
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

# Don't attempt to complete empty command lines.
shopt -s no_empty_cmd_completion

# Source default Bash completions.
[[ -f /etc/bash_completion ]] && . /etc/bash_completion

# ------------------------------------------------------------------------------
# Miscellaneous Options

# Don't overwrite an existing file when using redirection.
set -C

# Use VI style command line editing.
set -o vi

# Check for active jobs before exiting.
shopt -s checkjobs

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# ------------------------------------------------------------------------------
# Prompt and Window Title
# Not great support for this... Keep it simple.

# "\nuser@host:pwd\n" colored blue.
PS1='\n\[\033[22;34m\]\u@\h:\w\n'

# "$ " colored green for zero exit status, red otherwise.
PS1+='$( [[ $? -eq 0 ]] && echo "\[\033[22;32m\]" || echo "\[\033[22;31m\]" )'

# Colorless.
PS1+='\$ \[\033[00m\]'

# ------------------------------------------------------------------------------
# Simple Shell Supplements

# Miscellaneous.
alias cp='cp -i'
alias  e='echo'
alias  g='grep -E --color=auto'
alias mv='mv -i'
alias  m='mv'
alias md='mkdir'
alias rm='rm -i'
alias  r='rm'
alias  s='sed -r'
alias  t='touch'
alias  v='gvim -p'
alias  x='xargs -d\\n' # Xargs newline delimited.

alias timestamp='date +%F-%H-%M-%S-%N'

# Directory listing.
alias  ls='command ls -Ap --color=auto' # Alphabetically.
alias   l=ls
alias lex='l -X'                   # By extension.
alias lsi='l -S'                   # By size.
alias lct='l -c'                   # By creation time.
alias lmt='l -t'                   # By mod time.
alias lat='l -u'                   # By access time.
[[ -x /usr/bin/dircolors ]] && eval "$(dircolors -b)" # Enable color support.

alias rsync='command rsync -azv --partial-dir=.rsync-partial --partial'

# ------------------------------------------------------------------------------
# Less Simple Shell Supplements

# Directory navigation.
dirs()
{
  local color=1
  builtin dirs -p|while read path
  do
    printf "\033[22;3${color}m%s\033[00m " "$path"

    # Alternate the color after the first path.
    [[ $color -eq 5 ]] && color=4 || color=5;
  done
  echo # Newline.
}
alias d=dirs
alias c=cd
pushd() { builtin pushd "$@" > /dev/null; d; } # Change directory.
alias p=pushd
alias pb='pushd +1' # Previous directory.
alias pf='pushd -0' # Next directory.
popd() { builtin popd > /dev/null; d; }
alias P=popd

alias abspath='readlink -m'

# Xargs grep.
alias xg='x grep -E --color=auto'

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

# Source private configuration, if present.
[[ -f ~/.bashrc_home ]] && . ~/.bashrc_home

# Source work configuration, if present.
[[ -f ~/.bashrc_work ]] && . ~/.bashrc_work

init()
{
  [[ -d ~/bin ]] || mkdir ~/bin

  # Generate null shorthand link.
  [[ -e /0 ]] || case "$OSTYPE" in
         cygwin) ln -s /dev/null /0 ;;
    linux-gnu|*) sudo ln -s /dev/null /0 ;;
  esac

  # A couple shortcuts.
  ln -fs /usr/bin/google-chrome ~/bin/chrome
  ln -fs /usr/bin/gnome-terminal ~/bin/term
  ln -fs {~/opt/eclipse,~/bin}/eclipse

#  ln -fs {"$PWD",~/bin}/4tw
#  ln -fs {"$PWD",~}/.bashrc
#  ln -fs {"$PWD",~}/.inputrc
#ln -fs nsswitch
#ln -fs {"$PWD",~}/.profile
#ln -fs {"$PWD",~/bin}/snap
#ln -fs {"$PWD",~}/.vimrc
#ln -fs {"$PWD",~}/.Xmodmap
# what to do about .gitconfig...

}

#which xclip
#ctags, vim
#eclipse

up_file()
{
  local f="$1"
  local pwd="$PWD"
  local err=1

  while [[ "$PWD" != "/" ]] && builtin cd ..
  do
    if [[ -e "$f" ]]
    then
      err=0
      echo "$PWD/$f"
      break
    fi
  done

  builtin cd "$pwd" && return $err
}

alias updatedb='command updatedb -l0 -oindex.db -U .'
loc()
{
  locate -Pd"$(up_file index.db)" --regex "$@"
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

# colordiff -d --speed-large-files --suppress-common-lines -W$COLUMNS -y
# dic() { ! wn "$@" -over; } # Dictionary definition.
# alias gd='aspell dump master|g -i' # Grep mediocre dictionary.

