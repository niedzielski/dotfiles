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

GOROOT="/usr/lib/go"
if [ -d "$GOROOT" ]; then
  export GOROOT
  export GOBIN="$GOROOT/bin"
  export GOPATH="$HOME/.go"
  set_path_prepend_dirs "$GOBIN"
fi

# ------------------------------------------------------------------------------
# haskell (cabal)

cabal_bin="$HOME/.cabal/bin"
if [ -d "$cabal_bin" ]; then
  set_path_prepend_dirs "$cabal_bin"
fi

# ------------------------------------------------------------------------------
# python (pip)
# pip install --user sh

local_bin="$HOME/.local/bin"
if [ -d "$local_bin" ]; then
  set_path_prepend_dirs "$local_bin"
  export PYTHONPATH="$(prepend_path_to_search_paths "$PYTHONPATH" "$HOME/.local/lib/"*"/site-packages")"
fi

# ------------------------------------------------------------------------------
# node

node_root="$HOME/.node"
if [ -d "$node_root" ]; then
  export NODE_PATH="$node_root/lib/node_modules:$HOME/opt/node/lib/node_modules"
  set_path_prepend_dirs "$node_root/bin" "node_modules/.bin"
fi

# ------------------------------------------------------------------------------
# dart

DART_SDK="$HOME/opt/dart/dart-sdk"
if [ -d "$DART_SDK" ]; then
  export DART_SDK
  set_path_prepend_dirs "$DART_SDK/bin"
fi

# ------------------------------------------------------------------------------
# pebble

PEBBLE="$HOME/opt/pebble"
if [ -d "$PEBBLE" ]; then
  export PEBBLE
  set_path_prepend_dirs "$PEBBLE/arm-cs-tools/bin" "$PEBBLE/Pebble" "$PEBBLE/bin"
fi

# ------------------------------------------------------------------------------
# android

[ -f ~/.profile_android ] && . ~/.profile_android

# ------------------------------------------------------------------------------
# other program exports

export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk-amd64
export LESS=-ir # smart ignore-case + output control chars
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ------------------------------------------------------------------------------
# path

set_path_prepend_dirs ~/bin
