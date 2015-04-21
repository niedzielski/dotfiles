# .profile, stephen@niedzielski.com

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
# java
export JAVA_HOME=/usr/lib/jvm/default-java

# ------------------------------------------------------------------------------
# path
set_path_prepend_dirs ~/bin