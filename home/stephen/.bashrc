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

# unlimited history
HISTSIZE=999999
HISTFILESIZE=999999

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

# record each line as it is entered
PROMPT_COMMAND='history -a'

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

# prompt
PS1='$(prompt $? ${COLUMNS:-1} 1)'

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
