#!/usr/bin/env bash
# ------------------------------------------------------------------------------
# Copyright 2012 Stephen Niedzielski. Licensed under GPLv3.

# For transparency support.
#xcompmgr&

# TODO: investigate Ginn as a replacement if touchegg keeps chomping my CPU.
#touchegg &> /dev/null&

# TODO: switch to Dzen or Xmobar + Trayer when I can make these prettier and
# more functional. Also consider using Docky if it matures some.
# trayer --widthtype request --align right --height 14 --edge top --transparent true  --tint 0 --padding 0 --margin 0 --distance 0 --SetDockType true --SetPartialStrut true&
gnome-panel&

# HACK: dmenu requires an Xmonad restart for some reason.
sleep 2
xmonad --restart

dropbox start -i

#hamster &
