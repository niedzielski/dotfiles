# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# ------------------------------------------------------------------------------
# if running bash
#if [ -n "$BASH_VERSION" ]; then
#    # include .bashrc if it exists
#    if [ -f "$HOME/.bashrc" ]; then
#	. "$HOME/.bashrc"
#    fi
#fi

# ------------------------------------------------------------------------------
if [ -d "$HOME/bin" ]; then
  PATH="$HOME/bin:$PATH"
  export PATH
fi

if [ -d "$HOME/opt/android-sdk" ]; then
  ANDROID_HOME="$HOME/opt/android-sdk"; export ANDROID_HOME
  ANDROID_SDK_ROOT="$ANDROID_HOME"; export ANDROID_SDK_ROOT

  PATH="$PATH:$ANDROID_HOME/tools"
  PATH="$PATH:$ANDROID_HOME/platform-tools"
  PATH="$PATH:$ANDROID_HOME/build-tools/latest"
  export PATH
fi

if [ -d "$HOME/opt/jdk1.6-64b" ]; then
  JAVA_HOME="$HOME/opt/jdk1.6-64b"; export JAVA_HOME
  PATH="$JAVA_HOME/bin:$PATH"
  export PATH
fi

if [ -d "$HOME/opt/android-ndk" ]; then
  ANDROID_NDK_HOME="$HOME/opt/android-ndk"; export ANDROID_NDK_HOME
  NDK_HOME="$ANDROID_NDK_HOME"; export NDK_HOME
  NDK_PATH="$ANDROID_NDK_HOME"; export NDK_PATH
  PATH="$PATH:$ANDROID_NDK_HOME"
  export PATH
fi

if [ -d "$HOME/opt/ant" ]; then
  ANT_HOME="$HOME/opt/ant"; export ANT_HOME
  PATH="$ANT_HOME/bin:$PATH"
  export PATH
fi

if [ -d "$HOME/opt/groovy/bin" ]; then
  PATH="$HOME/opt/groovy/bin:$PATH"
  export PATH
fi

if [ -d "$HOME/opt/gradle" ]; then
  GRADLE_HOME="$HOME/opt/gradle"; export GRADLE_HOME
  PATH="$GRADLE_HOME/bin:$PATH"
  export PATH
fi

#[ -d "$HOME/.android" ] && ANDROID_SDK_HOME="$HOME/.android" && export ANDROID_SDK_HOME ||:

NDK_CCACHE=ccache   ; export NDK_CCACHE
USE_CCACHE=1        ; export USE_CCACHE
CCACHE_DIR=~/.ccache; export CCACHE_DIR


# ------------------------------------------------------------------------------
# Magic for Remote Android Debugging

# Use or create ADB server. Needed by ADB, DDMS, and ADT / Eclipse.
#idu=2048 # $(id -u)
#export ANDROID_ADB_SERVER_PORT=$((10000 + $idu))
#export ADBHOST=$ANDROID_ADB_SERVER_PORT

# Note: for Eclipse, modify or add .metadata/.plugins/org.eclipse.core.runtime/
# .settings/com.android.ide.eclipse.ddms.prefs:
# - com.android.ide.eclipse.ddms.adbDebugBasePort=ADT_BASE_PORT
# - com.android.ide.eclipse.ddms.debugSelectedPort=ADT_SELECTED_PORT
# Also modify for DDMS in .android/ddms.cfg:
# - adbDebugBasePort=ADT_BASE_PORT
# - debugSelectedPort=ADT_SELECTED_PORT
# TODO: can I use setprop?
# TODO: remove when env var support is available.
# HACK: wouldn't need to export either of these if we kept them in .bashrc.
#export ADT_BASE_PORT=$((13000 + ($idu - 2048) * 200)) # Eclipse base port.
#export ADT_SELECTED_PORT=$(($ADT_BASE_PORT + 100)) # Eclipse VM port.
#unset idu
