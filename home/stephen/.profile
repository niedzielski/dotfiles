#!/usr/bin/env sh

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
# ruby
# gem install --user-install jekyll

ruby_dir="$HOME/.gem/ruby"
if [ -d "$ruby_dir" ]; then
  set_path_prepend_dirs "$ruby_dir/"*"/bin"
fi

# ------------------------------------------------------------------------------
# go

GOROOT="$HOME/opt/go"
if [ -d "$GOROOT" ]; then
  export GOROOT
  export GOBIN=$GOROOT/bin
  export GOPATH=$HOME/golang
  set_path_prepend_dirs "$GOBIN"
fi

# ------------------------------------------------------------------------------
# python (pip)
# pip install --user sh

local_bin="$HOME/.local/bin"
if [ -d "$local_bin" ]; then
  set_path_prepend_dirs "$local_bin/bin"
fi

# ------------------------------------------------------------------------------
# node
# npm install -g gulp

node_root="$HOME/.node"
if [ -d "$node_root" ]; then
  set_path_prepend_dirs "$node_root/bin"
fi

# ------------------------------------------------------------------------------
# dart

DART_SDK="$HOME/opt/dart/dart-sdk"
if [ -d "$DART_SDK" ]; then
  export DART_SDK
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