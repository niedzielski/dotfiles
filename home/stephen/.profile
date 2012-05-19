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
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# ------------------------------------------------------------------------------
[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH" || :
[ -d "$HOME/opt/android-sdk/tools" ] && PATH="$PATH:$HOME/opt/android-sdk/tools" || :
[ -d "$HOME/opt/android-sdk/platform-tools" ] && PATH="$PATH:$HOME/opt/android-sdk/platform-tools" || :
[ -d "$HOME/opt/p4v-2011.1.405081/bin" ] && PATH="$PATH:$HOME/opt/p4v-2011.1.405081/bin" || :
[ -d "$HOME/opt/jdk1.6.0_32" ] && export JAVA_HOME="$HOME/opt/jdk1.6.0_32" && PATH="$PATH:$JAVA_HOME/bin"
[ -d "$HOME/opt/android-ndk-r7c" ] && PATH="$PATH:$HOME/opt/android-ndk-r7c"

# TODO: consider putting home bin before existing PATH.

# ------------------------------------------------------------------------------
# Magic for Remote Android Debugging

# Use or create ADB server. Needed by ADB, DDMS, and ADT / Eclipse.
idu=2048 # $(id -u)
export ANDROID_ADB_SERVER_PORT=$((10000 + $idu))
export ADBHOST=$ANDROID_ADB_SERVER_PORT

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
export ADT_BASE_PORT=$((13000 + ($idu - 2048) * 200)) # Eclipse base port.
export ADT_SELECTED_PORT=$(($ADT_BASE_PORT + 100)) # Eclipse VM port.
unset idu
