# .profile, stephen@niedzielski.com

# ------------------------------------------------------------------------------
# util

if [ -f ~/.sh_util ]; then
  . ~/.sh_util
fi

# ------------------------------------------------------------------------------
# sh

if [ -f ~/.shrc ]; then
  export ENV="$HOME/.shrc"
fi

# ------------------------------------------------------------------------------
# dart

if [ -d ~/opt/dart/dart-sdk/bin ]; then
  export DART_SDK="$HOME/opt/dart/dart-sdk"
  set_path_prepend_dirs "$DART_SDK/bin"
fi

# ------------------------------------------------------------------------------
# android

if [ -f ~/.profile_android ]; then
  . ~/.profile_android
fi

# ------------------------------------------------------------------------------
# java
export JAVA_HOME=/usr/lib/jvm/default-java

# ------------------------------------------------------------------------------
# path
set_path_prepend_dirs ~/bin