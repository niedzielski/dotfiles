# os
case "$OSTYPE$(uname)" in
  [lL]inux*) export TUX_OS=1 ;;
 [dD]arwin*) export MAC_OS=1 ;;
  [cC]ygwin) export WIN_OS=1 ;;
          *) echo "unknown os=\"$OSTYPE$(uname)\"" >&2 ;;
esac

# util
if [ -f "$HOME/.sh_util" ]; then . "$HOME/.sh_util"; fi

# sh
if [ -f "$HOME/.shrc" ]; then export ENV="$HOME/.shrc"; fi

# Default is:
# if [ -n "$BASH_VERSION" ]; then
#     # include .bashrc if it exists
#     if [ -f "$HOME/.bashrc" ]; then
#   . "$HOME/.bashrc"
#     fi
# fi

# protobuf
export PROTOC_DIR="$HOME/opt/protoc-21.9"
export PROTOC="$PROTOC_DIR/bin/protoc"
export PROTOC_INCLUDES="$PROTOC_DIR/include"

# google cloud
set_path_append_dirs "$HOME/opt/google-cloud-sdk/bin"

# go
export GOROOT="$HOME/opt/go"
export GOBIN="$GOROOT/bin"
export GOPATH="$HOME/.go"
set_path_prepend_dirs "$GOBIN"

# node
set_path_prepend_dirs "$HOME/opt/node/bin" # "node_modules/.bin"

# android
export ANDROID="$HOME/opt/android/sdk"
export ANDROID_SDK="$ANDROID"
export ANDROID_HOME="$ANDROID"
set_path_append_dirs "$ANDROID/tools" \
                     "$ANDROID/platform-tools"
if [ -d "$ANDROID/build-tools" ]; then
  set_path_append_dirs "$(glob_last_dir "$ANDROID/build-tools/"*)"
fi

# rust
# export PATH="$HOME/.cargo/bin:$PATH"
#. "$HOME/.cargo/env"

# less
# make less understand some binary inputs such as tar, see lesspipe(1)
if which lesspipe > /dev/null; then eval "$(SHELL=/bin/sh lesspipe)"; fi
# smart ignore-case + output control chars
export LESS=-iR

export EDITOR=vim

export TERM=xterm-256color

# path
set_path_prepend_dirs "$HOME/.local/bin" "$HOME/bin"

if [ -f "$HOME/.profile_private" ]; then . "$HOME/.profile_private"; fi

