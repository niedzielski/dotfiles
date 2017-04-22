# Dotfiles
My Ubuntu system configuration

![Screenshot](desktop.png)

## Back Up
1. Empty trash; consider dumping Steam and Wine games
1. Copy files: `cd / && tarpipe /media/stephen/disk/home home`
1. Backup with rsnapshot too: `backup alpha`
1. Back up browser tabs, unsaved editor files (check Code, Sublime Text, and
   DeaDBeeF)
1. Back up packages and verify contents (see rsnapshot config)
1. Take screenshot of launcher
1. Download the latest Ubuntu release and check `md5sum`
1. Copy to USB thumbdrive (not SD Card): `mkfs.fat -I -n FOO /dev/sdX && time dd bs=4M if=foo.iso of=/dev/sdX`

## Restore

The network connection keeps dropping in v17.04. Disable IPv6 as a workaround:
  - https://askubuntu.com/questions/440649/how-to-disable-ipv6-in-ubuntu-14-04
  - https://askubuntu.com/questions/886107/google-chrome-error-21-neterr-network-changed

### Miscellaneous
- Check [stability](https://discourse.codinghorror.com/t/is-your-computer-stable)
- Decompress backup: `time tar xf /media/stephen/disk/home-2016-01-01-00-00-00-000000000.tar.gz -I pigz`
- Enable all proprietary drivers
- Link system dotfiles
- Update launcher to screenshot

### Packages

#### APT
```bash
sudo add-apt-repository ppa:dr-graef/pd-l2ork.zesty &&
sudo add-apt-repository ppa:graphics-drivers/ppa &&

curl https://dl.winehq.org/wine-builds/Release.key|sudo apt-key add &&
sudo apt-add-repository 'https://dl.winehq.org/wine-builds/ubuntu/' &&

curl https://riot.im/packages/debian/repo-key.asc|sudo apt-key add &&
sudo apt-add-repository 'https://riot.im/packages/debian/'

sudo apt update &&

sudo apt dist-upgrade &&
sudo apt upgrade &&

sudo apt install autojump blender chromium-browser clang cmake dos2unix feh ffmpeg fontforge fontforge-extras fonts-roboto gimp git-gui gitk gnome-specimen gparted htop imagemagick inkscape jq libgnome-keyring-dev libimage-exiftool-perl llvm meld mplayer nmap nvidia-settings pigz pitivi potrace purr-data pv python-dev python-pip python3-dev python3-pip qemu qemu-kvm riot-web rsnapshot ruby-dev sg3-utils sox vim vim-gnome vlc whois wmctrl xclip xdotool xvfb &&

sudo apt install dolphin-emu fceux mame retroarch winehq-staging winetricks

# mediawiki
sudo apt install hhvm nfs-common nfs-kernel-server

sudo apt purge rhythmbox
```

#### Manual Downloads
- [Aseprite](https://www.aseprite.org/)
- [Chrome](https://www.google.com/chrome/browser/desktop/)
- [DeaDBeeF](http://deadbeef.sourceforge.net/download.html)
- [Hyper](https://hyper.is/)
- [itch](https://itch.io/app)
- [Node.js](https://nodejs.org/en/)
- [Postman](https://www.getpostman.com/apps)
- [ripgrep](https://github.com/BurntSushi/ripgrep/releases)
- [Steam](http://store.steampowered.com/about/)
- [Sublime Text](https://www.sublimetext.com/3)
- [Tiled Map Editor](https://thorbjorn.itch.io/tiled)
- [Vagrant](https://www.vagrantup.com/downloads.html)
- [VirtualBox + extensions](https://www.virtualbox.org/wiki/Downloads)
- [Visual Studio Code](https://code.visualstudio.com/download)

#### Compare Packages
```bash
alias strip='sed -r "1,5 d; s%^(ii|rc)\s+([^ ]+).*%\2%"' &&
meld <(strip dpkg.txt|sort) <(dpkg -l|strip|sort)
# other comparisons are manual
```

### Appearance
```bash
# background
gsettings get org.gnome.desktop.background picture-uri &&
gsettings set org.gnome.desktop.background picture-uri file:///usr/share/backgrounds/gnome/adwaita-timed.xml

gsettings get org.gnome.desktop.screensaver picture-uri &&
gsettings set org.gnome.desktop.screensaver picture-uri file:///usr/share/backgrounds/gnome/adwaita-timed.xml

# todo: show hidden files
# todo: change clock to 12hr and enable automatic timezone
# todo: disable calendar, contacts, documents, files, photos, and software from
#       appearing in search results and disable all search locations
# todo: enable icons on desktop
# todo: enable minimize titlebar button
# todo: change window action key to super
# todo: enable extensions:
# - Alternatetab
# - Hide top bar (https://extensions.gnome.org/extension/545/hide-top-bar/)
# - Openweather (https://extensions.gnome.org/extension/750/openweather/)
# - Pixelsaver (https://extensions.gnome.org/extension/723/pixel-saver/)
# - Topicons (https://extensions.gnome.org/extension/495/topicons/)
# - Dash to Dock (https://extensions.gnome.org/extension/307/dash-to-dock/)
# - Transparent Top Bar (https://extensions.gnome.org/extension/857/transparent-top-bar/)
# turn off blutetooth
```

### Keybindings
```bash
# todo: ctrl-alt-p screenshot
# todo: disable mouse middle-click paste
# todo: change trackpad click method to fingers (for right click)
# invert function key on apple keyboard
echo options hid_apple fnmode=2 | sudo tee -a /etc/modprobe.d/hid_apple.conf
sudo update-initramfs -u
```

### Terminal
- Set scrollback to unlimited
- Unset show menu bar
- Unset terminal bell

### Chromium
#### about://settings
- Sign in
- Check On startup -> Continue where you left off
- Uncheck Appearance -> Use system title bar and borders
- Make Chromium the default

#### chrome://extensions
- [uBlock](https://chrome.google.com/webstore/detail/ublock/cjpalhdlnbpafiamejdnhcphjbkeiagm)

### Git
```bash
sudo apt install libgnome-keyring-dev &&
cd /usr/share/doc/git/contrib/credential/gnome-keyring &&
sudo make
```

## License (GPLv3)
© 2017 Stephen Niedzielski.

### GPLv3
This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, version 3 of the License.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <http://www.gnu.org/licenses/>.