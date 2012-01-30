# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

[ -d "$HOME/bin" ] && PATH="$HOME/bin:$PATH"
[ -d "$HOME/opt/android-sdk-linux/tools" ] && PATH="$PATH:$HOME/opt/android-sdk-linux/tools"
[ -d "$HOME/opt/android-sdk-linux/platform-tools" ] && PATH="$PATH:$HOME/opt/android-sdk-linux/platform-tools"
[ -d "$HOME/opt/p4v-2011.1.405081/bin" ] && PATH="$PATH:$HOME/opt/p4v-2011.1.405081/bin"
