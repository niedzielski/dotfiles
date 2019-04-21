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

# try to fix shiz
# https://trac.torproject.org/projects/tor/ticket/9353
# https://bugs.launchpad.net/ubuntu/+source/ibus/+bug/1421483
#export GTK_IM_MODULE=xim


# mediawiki
#export MW_INSTALL_PATH="$HOME/Code/wmf/vagrant/mediawiki"
#export MW_SERVER=http://localhost:8080
#export MW_SCRIPT_PATH=/w
#export MEDIAWIKI_USER=Selenium_user
#export MEDIAWIKI_PASSWORD=vagrant

# ruby & gem
# bundle install --path .gem
#if which ruby gem >/dev/null; then
#  export GEM_HOME="$(ruby -rubygems -e 'puts Gem.user_dir')"
  # PATH="$(ruby -rubygems -e 'puts Gem.user_dir')/bin:$PATH"
#  set_path_prepend_dirs "$GEM_HOME/bin" ".gem/ruby/$(ruby -e 'puts RUBY_VERSION')/bin"
#fi

# python & pip
#export PYTHONPATH="$(prepend_path_to_search_paths "$PYTHONPATH" "$HOME/.local/lib/"*"/site-packages")"

# node
#export NODE_PATH="$node_root/lib/node_modules:$HOME/Applications/node/lib/node_modules"
set_path_prepend_dirs "$HOME/opt/node/bin" # "node_modules/.bin"

# The default is ~/.babel.json which confuses me.
export BABEL_CACHE_PATH="$HOME/.babel.cache.json"

# emscripten
# variables as reported by $EMSDK/emsdk activate latest
# http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html
#export EMSDK="$HOME/opt/emsdk"
#set_path_prepend_dirs \
#  "$EMSDK/emscripten/master" \
#  "$EMSDK/node/4.1.1_64bit/bin" \
#  "$EMSDK/clang/fastcomp/build_master_64/bin" \
#  "$EMSDK"

# android
export ANDROID="$HOME/opt/android/sdk"
export ANDROID_SDK="$ANDROID"
export ANDROID_HOME="$ANDROID"
set_path_append_dirs "$ANDROID/tools" \
                     "$ANDROID/platform-tools"
if [ -d "$ANDROID/build-tools" ]; then
  set_path_append_dirs "$(glob_last_dir "$ANDROID/build-tools/"*)"
fi

#export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64

# https://unix.stackexchange.com/questions/315004/where-does-gnome-keyring-set-ssh-auth-sock/319796
#export GSM_SKIP_SSH_AGENT_WORKAROUND=1

#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# less
# make less understand some binary inputs such as tar
if which lesspipe > /dev/null; then eval "$(SHELL=/bin/sh lesspipe)"; fi
export LESS=-iR
#r # smart ignore-case + output control chars

export EDITOR=vim

export AUTOJUMP_IGNORE_CASE=1
export AUTOJUMP_KEEP_SYMLINKS=1

export TERM=xterm-256color

# path
set_path_prepend_dirs "$HOME/.local/bin" "$HOME/bin"

if [ -f "$HOME/.profile_private" ]; then . "$HOME/.profile_private"; fi
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
#export PATH="$PATH:$HOME/.rvm/bin"

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

export BAT_THEME=GitHub

# node version manager
#export NVM_DIR="$HOME/.nvm"

