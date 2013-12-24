
# Update Chrome settings. Sync chrome://plugins manually.

# Install AOSP recommended pkgs from http://s.android.com/source/initializing.html

#valgrind build-essential openssh-server html2text libdevice-usb-perl gnuplot autossh ssh-import-id bash-completion ttf-bitstream-vera ccache cheese cscope doxygen mscgen curl wget dmenu dos2unix g++ gcc gdb gimp git git-gui gitk git-svn gnome-specimen gparted unetbootin graphviz htop ia32-libs idle ipython imagemagick iodine iperf lrzsz lsb-release lshw lsscsi lsof lynx make meld colordiff moreutils rar unrar p7zip p7zip-rar pbuilder picard pdfgrep playonlinux idle python-pip python-beautifulsoup qemu qemu-kvm qemubuilder rsync zsync screen tmux sg3-utils sqlitebrowser telnet usbutils vim-gnome xmonad gnome-panel darcs vlc winbind xclip xdotool wmctrl youtube-dl libav-tools ubuntu-restricted-extras minicom socat cu python-django python-gpgme libqt4-dev libx11-dev libxtst-dev inkscape ack-grep fio aptitude debconf-utils ffmpeg gconf-editor ginn i2c-tools pbuilder-uml puppet python2.7-dev rake openjdk-6-jre ruby1.8-dev scsitools sdparm slirp smartmontools smp-utils spew vagrant whois myunity

# Download dropbox, chrome, virtualbox (using dpkg -???), deadbeef
#sudo adduser stephen vboxusers
#sudo adduser stephen dialout
#echo fs.inotify.max_user_watches=500000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p

# switch to classic wm or xmonad + gnome panel

#sudo apt-get purge unity-lens-shopping ubuntuone-client* python-ubuntuone-* totem deja-dup rhythmbox transmission* thunderbird

sudo apt-get update
sudo apt-get upgrade



http://ubuntuforums.org/showthread.php?t=2039799 / http://pof.eslack.org/archives/files/mba42/post-install-precise.sh



sudo apt-get purge rhythmbox rhythmbox-data thunderbird* evolution-data-server

# Update mouse settings, power settings, background, folder settings. Change file associations for scripts to gVim.



# Update terminal.
# Update gnome-panel.
# Update wallpaper.
# Copy over ~/opt, configuration fiels, etc.

sg utils on vm

echo touchegg '(and )'

echo 'echo options hid_apple fnmode=2 | sudo tee -a /etc/modprobe.d/hid_apple.conf'
echo 'sudo update-initramfs -u'
echo 'compare dpkg -l'





















#!/usr/bin/env bash

#valgrind
#build-essential samba smbfs
#openssh-server
#nfs-common nfs-kernel-server cachefilesd
#html2text
#libdevice-usb-perl
#gnuplot

# TODO: investigate why "sudo dpkg --set-selections" fails.
pkgs+=(autossh ssh-import-id)
pkgs+=(bash-completion)
pkgs+=(ttf-bitstream-vera)
pkgs+=(ccache)
pkgs+=(cheese)
pkgs+=(cscope doxygen mscgen)
pkgs+=(curl wget)
pkgs+=(dmenu)
pkgs+=(dos2unix)
#pkgs+=(espeak)
pkgs+=(g++)
pkgs+=(gcc)
pkgs+=(gdb)
pkgs+=(gimp)
pkgs+=(git git-gui gitk github-cli git-svn)
pkgs+=(gnome-specimen)
#pkgs+=(googlecl)
pkgs+=(gparted unetbootin)
pkgs+=(graphviz)
pkgs+=(htop)
pkgs+=(ia32-libs)
pkgs+=(idle ipython)
pkgs+=(imagemagick)
pkgs+=(iodine)
pkgs+=(iperf)
pkgs+=(lrzsz)
pkgs+=(lsb-release)
pkgs+=(lshw lsscsi) # lspci
pkgs+=(lsof)
pkgs+=(lynx)
pkgs+=(make)
pkgs+=(meld colordiff)
pkgs+=(moreutils)
pkgs+=(rar unrar p7zip p7zip-rar)
pkgs+=(pbuilder)
pkgs+=(picard)
pkgs+=(pdfgrep)
pkgs+=(playonlinux)
pkgs+=(idle python-pip python-beautifulsoup)
pkgs+=(qemu qemu-kvm qemubuilder)
pkgs+=(rsync zsync)
pkgs+=(screen tmux)
#pkgs+=(skype)
pkgs+=(sg3-utils)
pkgs+=(sqlitebrowser)
pkgs+=(telnet)
pkgs+=(usbutils)
pkgs+=(vim-gnome)
#pkgs+=(virtualbox virtualbox-guest-additions-iso)
pkgs+=(vlc)
pkgs+=(winbind)
pkgs+=(xclip)
pkgs+=(xdotool wmctrl)
pkgs+=(xmonad gnome-panel darcs docky xcompmgr)
pkgs+=(youtube-dl libav-tools ubuntu-restricted-extras)
pkgs+=(minicom)
pkgs+=(socat)
#pkgs+=(python-django)

echo groove-dl deadbeef touchegg '(and libqt4-dev libx11-dev libxtst-dev utouch)' dropbox '(python-gpgme)'
echo 'echo fs.inotify.max_user_watches=500000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p'
echo 'update terminal and meld settings'
echo 'change file associations for scripts to gvim'
sudo adduser stephen vboxusers
sudo adduser stephen dialout
echo 'echo options hid_apple fnmode=2 | sudo tee -a /etc/modprobe.d/hid_apple.conf'
echo 'sudo update-initramfs -u'
echo 'update mouse settings, power settings, background'
echo 'update folder settings'
echo 'sign into chrome'
echo 'change notification bar'
echo 'compare dpkg -l'

# sudo dd if=iso of=/dev/sd...
#sudo mkfs.vfat /dev/sdX -I
#Clear out Chrome tabs
#dpkg -l > dpkgl.txt
# ghc-pkg list > ghc-pkg_list.txt
# pip freeze &> pip_freeze.txt
#Empty trash.
#time cp -a /home/stephen /media/...
# dropbox start -i
# bluetooth

#ghc-pkg list >| ghc-pkg_list.txt 2>&1
#pip freeze >| pip_freeze.txt 2>&1
#dpkg -l >| dpkgl.txt 2>&1


# http://ubuntuforums.org/showthread.php?t=2039799

# eagle logic
# openscad inkscape blender

sudo apt-get install "${pkgs[@]}" &&
sudo apt-get purge rhythmbox rhythmbox-data thunderbird*
