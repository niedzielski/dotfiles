# .profile, stephen@niedzielski.com

# ------------------------------------------------------------------------------
# os

case "$OSTYPE$(uname)" in
  [lL]inux*) export TUX_OS=1 ;;
 [dD]arwin*) export MAC_OS=1 ;;
  [cC]ygwin) export WIN_OS=1 ;;
          *) echo "unknown os=\"$OSTYPE$(uname)\"" >&2 ;;
esac

# ------------------------------------------------------------------------------
# util

[ -f ~/.sh_util ] && . ~/.sh_util

# ------------------------------------------------------------------------------
# sh

[ -f ~/.shrc ] && export ENV="$HOME/.shrc"

# ------------------------------------------------------------------------------
# dart

if [ -d ~/opt/dart/dart-sdk/bin ]; then
  export DART_SDK="$HOME/opt/dart/dart-sdk"
  set_path_prepend_dirs "$DART_SDK/bin"
fi

# ------------------------------------------------------------------------------
# android

[ -f ~/.profile_android ] && . ~/.profile_android

# ------------------------------------------------------------------------------
# other program exports

export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/default-java
export LESS=-ir # smart ignore-case + output control chars
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1
export SPARK_TICKS=' _▁▂▃▄▅▆▇█'

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ------------------------------------------------------------------------------
# path

set_path_prepend_dirs ~/bin
