# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

if [[ -f ~/.shrc ]]; then . ~/.shrc; fi

# ------------------------------------------------------------------------------
# globbing

# enabled extended globbing.
shopt -s extglob

# enable recursive globbing using **.
shopt -s globstar

# ignore case.
shopt -s nocaseglob

# ------------------------------------------------------------------------------
# history
# preserve the past as completely as possible

# don't record adjacent duplicates or lines starting with a space
HISTCONTROL=ignoreboth

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

# record each line as it is entered
# to-do: deduplicate repeated commands. `fc -ln -1` yields the last executed command but omits empties.
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND;} history -a; fc -ln -1|sed -r \"s%^\s+(.*)%\1⋮\$PWD%\" >> ~/.bash_history_jog"

# ------------------------------------------------------------------------------
# misc options

# don't overwrite an existing file when using redirection
set -C

# the return value of a pipeline is that of the rightmost command to exit with a
# non-zero status, or zero if all commands exit successfully
set -o pipefail

# check the window size after each command update LINES and COLUMNS.
shopt -s checkwinsize

# check for active jobs before exiting
shopt -s checkjobs

# notify of job termination immediately
set -b

# if a command is unrecognized but matches a directory, cd into it
shopt -s autocd

# prompt.
PS1='\[\e]0;\u@\h:\w\a\]$(prompt $? 1 $COLUMNS)'


# input.
bind '"\e[1;3A":"pushd ..\n"' # alt+up
bind '"\e[1;3C":"pushd -0\n"' # alt+right
bind '"\e[1;3B":"popd\n"'     # alt+down
bind '"\e[1;3D":"pushd +1\n"' # alt+left


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

export FZF_DEFAULT_OPTS='--height=30'
if [[ -d ~/bin/.fzf/shell ]]; then
  . ~/bin/.fzf/shell/completion.bash
  . ~/bin/.fzf/shell/key-bindings.bash
fi

alias p=pushd

[[ -f ~/.config/broot/launcher/bash/br ]] && . ~/.config/broot/launcher/bash/br

eval "$(zoxide init --cmd j bash)"

eval "$(deno completions bash)"

[[ -f ~/.bashrc_private ]] && . ~/.bashrc_private
