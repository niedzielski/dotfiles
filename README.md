Dotfiles
========

Back Up
-------
1. Empty trash.
1. Copy `/home` with tarpipe.
1. Back up browser tabs and unsaved editor files.
1. Back up packages (and check contents):
  1. `dpkg -l &> dpkg.txt`
  1. `ghc-pkg list &> ghc-pkg.txt`
  1. `pip freeze &> pip.txt`
  1. `gem list -a &> gem.list`
  1. `npm ls --global=true &> npm.ls`
  1. `apm list > apm.list`
  1. TODO: Sublime Text 3.
1. Download the latest Ubuntu 64b beta or release and check `md5sum`.
1. Build Ubuntu boot disk with UNetbootin.

Ubuntu Set Up
-------------

1. If Wi-Fi support is unavailable, obtain a USB to Ethernet adapter.
1. Install Wi-Fi driver for MacBook Pro: `sudo apt-get update && sudo apt-get install bcmwl-kernel-source`
1. Install browser: `sudo apt-get install chromium-browser`
1. Upgrade to the latest packages and release: `sudo apt-get upgrade && sudo apt-get dist-upgrade`
1. Install a big pile of packages:

        sudo add-apt-repository ppa:webupd8team/atom
        sudo apt-add-repository ppa:numix/ppa
        sudo apt-get update
        sudo apt-get install acpi ant aptitude aspell atom audacious audacious-plugins audacity autojump automake autossh bash-completion bc blender build-essential bzip2 calibre cachefilesd ccache cheese cmake colordiff command-not-found compiz-plugins compizconfig-settings-manager cool-retro-term cscope cu curl debconf-utils docker dos2unix doxygen edgy-wallpapers ethtool exuberant-ctags espeak fbreader feisty-wallpapers ffmpeg fio fish fonts-cantarell fonts-droid fonts-tuffy g++ gcc gconf-editor gdb gimp ginn git-all git-review gnome-session-flashback gnome-specimen gnuplot gparted gpick gradle graphviz groovy gutsy-wallpapers gzip hdparm hfsprogs html2text htop i2c-tools idle imagemagick indent inkscape iodine iperf ipython jq libav-tools librsvg2-bin libsox-fmt-all lmodern lrzsz lsb-release lshw lsof lsscsi luakit lynx make makehuman mame maven meld mercurial mess minicom mosh mscgen nfs-common nfs-kernel-server nmap nodejs npm numix-icon-theme-circle nvidia-346 openjdk-7-jdk openssh-server p7zip p7zip-rar pandoc pbuilder pbuilder-uml pdfgrep perl picard pigz playonlinux pngcrush powertop puppet pv python-beautifulsoup python-demjson python-dev python-django python-pip qemu qemubuilder qemu-kvm radare2 rake rsync ruby samba scsitools sdparm sed sg3-utils silversearcher-ag slirp smartmontools smp-utils socat sox spew sqlitebrowser ssh-import-id steam suckless-tools surf telnet tig tmux toilet tree ttf-aenigma ttf-bitstream-vera ttf-georgewilliams ttf-sjfonts tv-fonts ubuntu-restricted-extras ubuntustudio-font-meta ubuntu-wallpapers-\* udisks unetbootin unrar-free usbutils vagrant valgrind vim vim-gnome virtualbox virtualbox-guest-additions-iso vlc weechat wget whereami whois winbind wine wmctrl wordnet xclip xdotool xmonad xsane xubuntu-wallpapers youtube-dl zsh zsync

1. Remove some broken packages: `sudo apt-get purge runit git-daemon-run`
1. Approxmiate the decruft instructions [here](https://fixubuntu.com/).
1. Add login to VirtualBox group: `sudo adduser stephen vboxusers`
1. Reboot.
1. Test suspend, resume, virtual machines, and OpenGL.

Apple Keyboard
--------------
Invert the function key:

    echo options hid_apple fnmode=2 | sudo tee -a /etc/modprobe.d/hid_apple.conf
    sudo update-initramfs -u
    # sudo shutdown -r 0

From [Change Function Key Behavior](https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior).

Chromium
--------

### chrome://settings
- Check On startup -> Continue where you left off
- Check Appearance -> Use Classic theme
- Uncheck Appearance -> Use system title bar and borders
- Make Chromium the default.
- Uncheck Use hardware acceleration when available (causes high CPU usage).

### chrome://extensions
- [Android SDK Search](https://chrome.google.com/webstore/detail/android-sdk-search/hgcbffeicehlpmgmnhnkjbjoldkfhoin)
- [ARC Welder](https://chrome.google.com/webstore/detail/arc-welder/emfinbmielocnlhgmfkkmkngdoccbadn)
- [Chrome Apps & Extensions Developer Tools](https://chrome.google.com/webstore/detail/chrome-apps-extensions-de/ohmmkhmmmpcnpikjeljgnaoabkaalbgc)
- [Chrome Dev Editor](https://chrome.google.com/webstore/detail/chrome-dev-editor-develop/pnoffddplpippgcfjdhbmhkofpnaalpg)
- [EditThisCookie](https://chrome.google.com/webstore/detail/editthiscookie/fngmhnnpilhplaeedifhccceomclgfbg)
- [Google Dictionary](https://chrome.google.com/webstore/detail/google-dictionary-by-goog/mgijmajocgfcbeboacabfgobmjgjcoja)
- [Hacker News Enhancement Suite](https://chrome.google.com/webstore/detail/hacker-news-enhancement-s/bappiabcodbpphnojdiaddhnilfnjmpm)
- [Postman REST Client](https://chrome.google.com/webstore/detail/postman-rest-client/fdmmgilgnpjigdojojpjoooidkmcomcm)
- [Reddit Enhancement Suite](https://chrome.google.com/webstore/detail/reddit-enhancement-suite/kbmfpngjjgdllneeigpgjifpgocmfgmb)
- [uBlock](https://chrome.google.com/webstore/detail/ublock/cjpalhdlnbpafiamejdnhcphjbkeiagm)
- [Vimium](https://chrome.google.com/webstore/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb)
- [Web Cache](https://chrome.google.com/webstore/detail/web-cache/coblegoildgpecccijneplifmeghcgip)
- [Web Developer](https://chrome.google.com/webstore/detail/web-developer/bfbameneiokkgbdmiekhjnmfkcnldhhm)

#### Vimium

Use the following under Vimium -> Options -> Show advanced options... -> CSS for link hints*:

    div > .vimiumHintMarker {
    background: white;
    border: none;
    }

    div > .vimiumHintMarker span {
    color: black;
    font-weight: normal;
    font-size: 8pt;
    text-transform: lowercase;
    font-family: tahoma;
    }

    div > .vimiumHintMarker > .matchingCharacter {
    color: green;
    }

    div > .vimiumHintMarker > .matching {
    background: pink;
    }

*Note: indentation seems to break the override.