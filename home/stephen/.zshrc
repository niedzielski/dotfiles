#!/usr/bin/env zsh
# ------------------------------------------------------------------------------
# .zshrc
# Copyright 2009 - 2013 Stephen Niedzielski. Licensed under GPLv3.

source ~/.profile

# ------------------------------------------------------------------------------
if is_mac; then
  alias sed=gsed
  alias grep=ggrep
  alias find=gfind
  alias locate=glocate
  alias updatedb=gupdatedb
  alias xargs=gxargs
  alias tar=gtar
  alias awk=gawk
  alias du=gdu
  alias ls=gls
  alias parallel=gparallel
fi

# ------------------------------------------------------------------------------
# Prompt

setopt promptsubst

# TODO: do we need ${debian_chroot:+($debian_chroot)}?
PROMPT_PAD='────────────────────'; PROMPT_PAD+=${PROMPT_PAD//─/$PROMPT_PAD}
PROMPT_PWD='%K{blue}%n@%m%k%B %F{cyan}%~ '
# TODO: handle wrap around case using modulo
print_prompt_pad() {
  declare -i promptlen="$(python -c 'import re, sys; sys.stdout.write(str(len(re.sub("\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]", "", sys.stdin.read(), 0))))' <<< "${(%)PROMPT_PWD}")" # |wc -m
  echo -n ${PROMPT_PAD:0:$(($COLUMNS - $promptlen % $COLUMNS))}
}
PROMPT="
$PROMPT_PWD\$(print_prompt_pad)
%(?.%F{green}.%F{red}) %# %b%f%k"

setopt histignorespace histignorealldups sharehistory histfindnodups histlexwords

setopt checkjobs notify

setopt autocd

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=100000000
SAVEHIST=100000000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
#eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

#autoload predict-on
#predict-on

setopt dotglob

# color partial completions
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34=34}:${(s.:.)LS_COLORS}")';

# ------------------------------------------------------------------------------

# Allow inline comments. I use this to store commands in the history buffer
# without executing them.
setopt interactivecomments

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
alias  ls='ls -ApG' # Alphabetically.
alias   l=ls

alias rsync='rsync -azv --partial-dir=.rsync-partial --partial' # See also zsync


# Clipboard for GUI integration.
cb()
{
  if [[ ! -t 0 ]] && [[ $# -eq 0 ]]
  then
    # No stdin and no call for --help, blow away the current clipboard and copy.
    if is_mac; then
      pbcopy
    elif is_win; then
      putclip
    else
      # Several options here... Xclip seems to work.
      xclip -sel c
    fi
  else
    # Paste
    if is_mac; then
      pbpaste
    elif is_win; then
      getclip
    else
      # Several options here... Xclip seems to work.
      xclip -sel c -o
    fi
  fi
}

alias v=gvim\ -p


export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1
[[ -s ~/.autojump/etc/profile.d/autojump.zsh ]] && . ~/.autojump/etc/profile.d/autojump.zsh

alias ack='ack --smart-case'
alias ag='ag -S'
alias d2u=dos2unix


# TODO: PS1. ohmyzsh completion. auto-cd
alias dif='colordiff -d --speed-large-files --suppress-common-lines'

setopt noclobber

export LESS='-ir' # Smart ignore-case + output control chars. Probably safe to export.
alias pdfgrep='pdfgrep --color=auto -n'

d()
{
  declare path
  declare color=3
  dirs -p|while IFS= read -r path
  do
    printf "\033[22;3${color}m%s\033[00m " "$path"

    # Alternate the color after the first path.
    [[ $color -eq 4 ]] && color=5 || color=4;
  done
  echo # Newline.
}
cd() { builtin cd "$@"; d; }
alias c=cd
p() { pushd "$@" > /dev/null; d; } # Change directory.
P() { popd > /dev/null; d; } # Change directory.
pb() { p +1; } # Previous directory.
pf() { p -0; } # Next directory.
..() { cd ..; }



setopt BRACE_CCL







source ~/.zshrc_kbd
source ~/.zshrc_android


ncui() { cd ~/.juniper_networks/network_connect && sudo ./ncui -h ntc.remote.aol.com -c "$1" -f ~/.vpn/ssl.crt -x }

targz() {
  declare o="$1-$(timestamp).tar.gz"
  shift

  echo "output: $o"
  echo "inputs: $*"

  declare -i sz=0
#  if (( $+isMac )); then
#    sz=$(BLOCKSIZE=512 du -cs "$@"|tail -n1|cut -f1)
#    sz=$(( $sz * 512 ))
#  else
    sz=$(du -bcs "$@"|tail -n1|cut -f1)
#  fi
  echo "size: $sz"

  time {
    tar c "$@"|
    pv -s $sz|
    pigz -1 > "$o"
    sync
  }
}




# TODO: suppress jump chdir msg.

# Find with a couple defaults.
f()
{
  # The trouble is that -regextype must appear after path but before expression.
  # HACK: "-D debugopts" unsupported and -[HLPO] options assumed to before dirs.
  a=()
  while [[ ${+1} -ne 0 ]] &&
        ( [[ "${1:0:1}" =~ '[^-!(),]' ]] || [[ "${1:0:2}" =~ -[HLPO] ]] )
  do
    a+=("$1")

    if [[ -d "$1" ]]; then
      local prependDirNotNeeded=1
    fi

    # Eliminate arg from @.
    shift
  done

  if ! (( $+prependDirNotNeeded )); then
    # PWD is not implied on older versions of find.
    a+=(.)
  fi

#  if (( $+isMac )); then
#    find ${a:+"${a[@]}"} ${@:+"$@"}
#  else
    find -O3 ${a:+"${a[@]}"} -nowarn -regextype egrep ${@:+"$@"}
#  fi
}

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
alias cls=reset


# rm -f cscope.* tags tags.files mlocate.db
#index_pwd()
#{
  #time \
#  {
    # Generate a nice index for future file lookups.
#    udb &&

    # Updatedb doesn't track sufficient file information to generate a tidy code
    # listing. There are numerous ways to sift out binaries and other unwanted
    # cruft. If Grep doesn't work, consider file, or find + sed.
    #loc|sed -r '/\.git\/|\.repo\/|doxygen\/|cscope|\.(lst|d|o|dbo|a|so|jar|png|jpg|pdf|map|dep|sym|exe)$/ d; s_\\_\\\\_g; s_"_\\"_g; s_^|$_"_g' >| cscope.files
 #   grep -lrID skip \
 #     --exclude-dir={.repo,.git,doxygen} \
 #     --exclude=\*.{d,dep,lst,map,sym,log} \
 #     --exclude=\*{cscope,tags}\* \
 #     . . >| tags.files && #2> /dev/null

    # HACK: workaround "ctags: "tags" doesn't look like a tag file; I refuse to
    # overwrite it."
#    rm -f tags &&

    # Generate a Ctags database.
#    ctags -L tags.files --fields=+iaS --extra=+q &&

    # Generate a Cscope database.
#    sed -r 's_\\_\\\\_g; s_"_\\"_g; s_^|$_"_g' tags.files >| cscope.files &&
#    cscope -eq < /dev/null #&&
#  }
#}
#dumb_dox()
#{
#  PROJECT_NAME="$(basename "$PWD")" time doxygen ~/.doxygen/Doxyfile_c
#}

# From Matthew Mead. TODO: review.
export LESS_TERMCAP_mb=$'\E[01;31m'      # begin blinking
export LESS_TERMCAP_md=$'\E[01;34m'      # begin bold
export LESS_TERMCAP_me=$'\E[0m'          # end mode
export LESS_TERMCAP_se=$'\E[0m'          # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'   # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'          # end underline
export LESS_TERMCAP_us=$'\E[01;32m'      # begin underline

youtube-dl-mp3()
{
  # HACK: when --extract-audio is used with -o-, avprobe doesn't seem to close
  # the pipe. Maybe it's non-reentrant?
  youtube-dl --audio-quality 0 -qo- "$1"|
  avconv -v warning -i pipe: "$(youtube-dl -qe "$1").mp3"
}

incognito()
{
  google-chrome --user-data-dir=/tmp --incognito --no-default-browser-check "$@" &> /dev/null&
}

# TODO: checkwinsize

case "$OSTYPE" in
     cygwin) gui() { explorer "${1:-.}"; } ;;
  linux-gnu) gui() { nautilus "${1:-.}"; } ;;
    darwin*) gui() { open     "${1:-.}"; } ;;
          *) echo 'Unknown file manager.' >&2 ;;
esac

setopt extendedglob
unsetopt caseglob
# http://www.rlazo.org/2010/11/18/zsh-case-insensitive-completion/
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' \
    'r:|[._-]=* r:|=*' 'l:|=* r:|=*'


xpid() {
  ps $(xprop -f _NET_WM_PID 0c ' = $0\n' _NET_WM_PID|
        sed -r 's_.* = ([0-9]+)_\1_')
}
alias timestamp='date +%F-%H-%M-%S-%N'

if is_tux; then
  source /etc/zsh_command_not_found
fi

fpath=(/usr/local/share/zsh-completions /usr/local/share/zsh/site-functions $fpath)

autoload bashcompinit
bashcompinit
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion 2> /dev/null
fi


#dashes over underscores. not available for varaibles or is that dash only?

. /usr/local/etc/bash_completion.d

[[ -s `brew --prefix`/etc/autojump.zsh ]] && XDG_DATA_HOME="$HOME/.local/share" . `brew --prefix`/etc/autojump.zsh


# ldd ./BCompare|grep 'not found'


#declare commonName="src/com/mapquest/android/guidance"
#files=("ACEPrototypeTest/$commonName"
#"AceTest/$commonName")

#for file in "${files[@]}"; do
#  git --no-pager log --follow --find-copies-harder --pretty=tformat:%H -- "$file"|
#  while read i; do
#    echo "$i"
#  done|
#  tail -r|
#  while read commit; do
#    git --no-pager format-patch --stdout --pretty=email --patch-with-stat -1 $commit -- "$file"
#  done
#done|cb
#git difftool -C -M
#nmap -sP 172.17.44.0-255
