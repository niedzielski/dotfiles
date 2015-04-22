# .bashrc, stephen@niedzielski.com

# ------------------------------------------------------------------------------
# util

[[ -f ~/.sh_util ]] && . ~/.sh_util
[[ -f ~/.sh_aliases ]] && . ~/.sh_aliases

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

[[ -f /etc/bash_completion ]] && . /etc/bash_completion

# ------------------------------------------------------------------------------
# autojump

[[ -f /usr/share/autojump/autojump.bash ]] && . /usr/share/autojump/autojump.bash