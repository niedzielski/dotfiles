#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# .bashrc
# Stephen Niedzielski

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
alias  g='egrep --color=always'
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
alias  ls='command ls -Ap --color=always' # Alphabetically.
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
cd() { builtin cd "$@" > /0; }
alias c=cd
pushd() { builtin pushd "$@" > /0; d; } # Change directory.
alias p=pushd
alias pb='pushd +1' # Previous directory.
alias pf='pushd -0' # Next directory.
popd() { builtin popd > /0; d; }
alias P=popd

alias abspath='readlink -m'

# Xargs grep.
alias xg='x egrep --color=always'

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

# Generate null shorthand link.
[[ -e /0 ]] || case "$OSTYPE" in
       cygwin) ln -s /dev/null /0 ;;
  linux-gnu|*) sudo ln -s /dev/null /0 ;;
esac

# A couple shortcuts.
if [[ "$OSTYPE" == "linux-gnu" ]]
then
  [[ -e /usr/bin/chrome ]]  || sudo ln -s /{usr/bin/google-,usr/bin/}chrome
  [[ -e /usr/bin/term ]]    || sudo ln -s /usr/bin/gnome-terminal /usr/bin/term
  [[ -e /usr/bin/eclipse ]] || sudo ln -s /{opt/eclipse,usr/bin}/eclipse
fi

#which xclip
#ctags, vim
#eclipse
#time rsync -azvu --partial-dir=.rsync-partial --partial sn@192.168.1.147:audio .


up_file()
{
  local f="$1"
  local pwd="$PWD"

  while [[ ! -e "$f" ]] && [[ "$PWD" != "/" ]] && builtin cd ..
  do
    :
  done

  if [[ -e "$f" ]]
  then
    echo "$PWD"
  fi

  builtin cd "$pwd"
}

alias updatedb='command updatedb -l0 -oindex.db -U .'
loc()
{
  local index="$(up_file index.db)/index.db"
  locate -Pd"$index" --regex "$@"
}

alias rsync='command rsync -azv --partial-dir=.rsync-partial --partial'
# colordiff -d --speed-large-files --suppress-common-lines -W$COLUMNS -y
