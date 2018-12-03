# ------------------------------------------------------------------------------
# util

if [[ -f ~/.shrc ]]; then . ~/.shrc; fi

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
HISTSIZE=
HISTFILESIZE=

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

# must appear before PROMPT_COMMAND
if [[ -f /usr/share/autojump/autojump.bash ]]; then . /usr/share/autojump/autojump.bash; fi

# record each line as it is entered
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;} history -a"

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
PS1='\[\e]0;\u@\h:\w\a\]$(prompt $? 1)'

# ------------------------------------------------------------------------------
# completion

# don't attempt to complete empty command lines otherwise it'll hang the prompt
# for a bit
shopt -s no_empty_cmd_completion

# complete host names
shopt -s hostcomplete

if [[ -f /usr/share/bash-completion/bash_completion ]]; then
  . /usr/share/bash-completion/bash_completion
elif [[ -f /etc/bash_completion ]]; then
  . /etc/bash_completion
fi

if [[ -d ~/Applications/fzf ]]; then
  . ~/Applications/fzf/shell/completion.bash
#  . ~/Applications/fzf/key-bindings.bash
fi

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
#export PATH="$PATH:$HOME/.rvm/bin"

alias p=pushd
alias nvm-use='. ~/.nvm/nvm.sh && nvm use'
