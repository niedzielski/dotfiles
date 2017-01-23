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
# mediawiki
export MW_INSTALL_PATH="$HOME/dev/wmf/vagrant/mediawiki"
export MW_SERVER=http://localhost:8080
export MW_SCRIPT_PATH=/w

# ------------------------------------------------------------------------------
# ruby
# gem install --user-install jekyll

ruby_dir="$HOME/.gem/ruby"
set_path_prepend_dirs "$ruby_dir/"*"/bin"

# ------------------------------------------------------------------------------
# python (pip)
# pip install sh

export PYTHONPATH="$(prepend_path_to_search_paths "$PYTHONPATH" "$HOME/.local/lib/"*"/site-packages")"

# ------------------------------------------------------------------------------
# node

export NODE_PATH="$node_root/lib/node_modules:$HOME/opt/node/lib/node_modules"
set_path_prepend_dirs "$HOME/.node/bin" "node_modules/.bin"

# ------------------------------------------------------------------------------
# emscripten
# variables as reported by $EMSDK/emsdk activate latest
# http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html

export EMSDK="$HOME/opt/emsdk"
set_path_prepend_dirs \
  "$EMSDK/emscripten/master" \
  "$EMSDK/node/4.1.1_64bit/bin" \
  "$EMSDK/clang/fastcomp/build_master_64/bin" \
  "$EMSDK"

# ------------------------------------------------------------------------------
# android

export ANDROID="$HOME/opt/android/sdk"
export ANDROID_SDK="$ANDROID"

set_path_append_dirs "$ANDROID/tools" \
                     "$ANDROID/platform-tools" \
                     "$(glob_last_dir "$ANDROID/build-tools/"*)"

# ------------------------------------------------------------------------------
# other program exports

export EDITOR=vim
export TERM=xterm-256color
export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export LESS=-ir # smart ignore-case + output control chars
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ------------------------------------------------------------------------------
# path

set_path_prepend_dirs "$HOME/.local/bin" "$HOME/bin"