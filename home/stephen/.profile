# os
case "$OSTYPE$(uname)" in
  [lL]inux*) export TUX_OS=1 ;;
 [dD]arwin*) export MAC_OS=1 ;;
  [cC]ygwin) export WIN_OS=1 ;;
          *) echo "unknown os=\"$OSTYPE$(uname)\"" >&2 ;;
esac

# util
if [ -f ~/.sh_util ]; then . ~/.sh_util; fi

# sh
if [ -f ~/.shrc ]; then export ENV="$HOME/.shrc"; fi

# mediawiki
export MW_INSTALL_PATH="$HOME/dev/wmf/vagrant/mediawiki"
export MW_SERVER=http://localhost:8080
export MW_SCRIPT_PATH=/w

# ruby & gem
if which ruby gem >/dev/null; then
  export GEM_HOME="$(ruby -rubygems -e 'puts Gem.user_dir')"
  PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
  set_path_prepend_dirs "$GEM_HOME/bin"
fi

# python & pip
export PYTHONPATH="$(prepend_path_to_search_paths "$PYTHONPATH" "$HOME/.local/lib/"*"/site-packages")"

# node
export NODE_PATH="$node_root/lib/node_modules:$HOME/opt/node/lib/node_modules"
set_path_prepend_dirs "$HOME/.node/bin" "node_modules/.bin"

# emscripten
# variables as reported by $EMSDK/emsdk activate latest
# http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html
export EMSDK="$HOME/opt/emsdk"
set_path_prepend_dirs \
  "$EMSDK/emscripten/master" \
  "$EMSDK/node/4.1.1_64bit/bin" \
  "$EMSDK/clang/fastcomp/build_master_64/bin" \
  "$EMSDK"

# android
export ANDROID="$HOME/opt/android/sdk"
export ANDROID_SDK="$ANDROID"
set_path_append_dirs "$ANDROID/tools" \
                     "$ANDROID/platform-tools" \
                     "$(glob_last_dir "$ANDROID/build-tools/"*)"

export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# less
# make less understand some binary inputs such as tar
if which lesspipe > /dev/null; then eval "$(SHELL=/bin/sh lesspipe)"; fi
export LESS=-ir # smart ignore-case + output control chars

export EDITOR=vim

# ls
if [[ -f "$HOME/.dir_colors/dircolors" ]]; then eval "$(dircolors "$HOME/.dir_colors/dircolors")"; fi

export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1

export TERM=xterm-256color

# path
set_path_prepend_dirs "$HOME/.local/bin" "$HOME/bin"