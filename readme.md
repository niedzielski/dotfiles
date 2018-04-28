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
1. Enable all proprietary drivers
1. Reboot
1. `sudo apt install mesa-utils pigz`
1. Run `glxgears` and check the frame rate and tearing
1. Decompress backup: `time tar xf /media/stephen/disk/home-2016-01-01-00-00-00-000000000.tar.gz -I pigz`
1. `sudo apt install chromium-browser`
1. Sign into Chromium
1. Install remaining packages
1. Configure
1. Link dotfiles
1. Check [stability](https://discourse.codinghorror.com/t/is-your-computer-stable)

### Additional Packages
#### Manual Downloads
- [Aseprite](https://www.aseprite.org/)
- [Chrome](https://www.google.com/chrome/browser/desktop/)
- [DeaDBeeF](http://deadbeef.sourceforge.net/download.html)
- [Hyper](https://hyper.is/)
- [Node.js](https://nodejs.org/en/)
- [Postman](https://www.getpostman.com/apps)
- [ripgrep](https://github.com/BurntSushi/ripgrep/releases)
- [Steam](http://store.steampowered.com/about/)
- [Vagrant](https://www.vagrantup.com/downloads.html) (for MediaWiki)
- [VirtualBox + extensions](https://www.virtualbox.org/wiki/Downloads)
- [Visual Studio Code](https://code.visualstudio.com/download)

#### Removals
```bash
sudo apt purge gnome-calendar rhythmbox* thunderbird* ubuntu-web-launchers
```

#### APT Installs
```bash
# Purr Data
sudo add-apt-repository ppa:dr-graef/pd-l2ork.bionic

# Sublime Text
# https://www.sublimetext.com/docs/3/linux_repositories.html#apt
curl https://download.sublimetext.com/sublimehq-pub.gpg|
sudo apt-key add -
echo 'deb https://download.sublimetext.com/ apt/stable/'|
sudo tee /etc/apt/sources.list.d/sublime-text.list

# Itch.io
# https://itch.io/docs/itch/installing/linux/ubuntu-and-debian.html
curl https://dl.itch.ovh/archive.key|
sudo apt-key add -
echo 'deb https://dl.bintray.com/itchio/deb bionic main'|
sudo tee /etc/apt/sources.list.d/itchio.list

# Wine
# https://wiki.winehq.org/Ubuntu
sudo dpkg --add-architecture i386
curl https://dl.winehq.org/wine-builds/Release.key|
sudo apt-key add -
sudo apt-add-repository https://dl.winehq.org/wine-builds/ubuntu/
sudo apt update
sudo apt-get install --install-recommends winehq-stable

sudo apt upgrade

sudo apt install autojump blender calibre colordiff flac fontforge fontforge-extras fonts-roboto gimp git-gui gitk gpick htop imagemagick inkscape intel-microcode krita libgnome-keyring-dev libimage-exiftool-perl meld mplayer nmap obs-studio pitivi potrace pv python-dev qemu qemu-kvm rsnapshot sg3-utils sox tmux tree ttf-bitstream-vera ubuntu-restricted-extras vlc whois wmctrl xclip xdotool xvfb sublime-text itch purr-data vim
```

##### Compare Previously Installed Packages
```bash
alias strip='sed -r "1,5 d; s%^(ii|rc)\s+([^ ]+).*%\2%"' &&
meld <(strip dpkg.txt|sort) <(dpkg -l|strip|sort)
```

#### NPM Installs
```bash
npm i -g css-validator diff-so-fancy html-validator-cli jsonlint live-server npm-check-updates source-map-explorer
```

### Configuration

#### Appearance
```bash
# Set the theme to light.
gsettings get org.gnome.desktop.wm.preferences theme &&
gsettings set org.gnome.desktop.wm.preferences theme Radiance
gsettings get org.gnome.desktop.interface gtk-theme &&
gsettings set org.gnome.desktop.interface gtk-theme Radiance
gsettings get org.gnome.desktop.interface icon-theme &&
gsettings set org.gnome.desktop.interface icon-theme ubuntu-mono-light

# Set the background and lock screen.
gsettings get org.gnome.desktop.background picture-uri &&
gsettings set org.gnome.desktop.background picture-uri file:///home/stephen/.bg
gsettings get org.gnome.desktop.screensaver picture-uri &&
gsettings set org.gnome.desktop.screensaver picture-uri file:///home/stephen/.bg

# Show battery percentage.
gsettings get org.gnome.desktop.interface show-battery-percentage &&
gsettings set org.gnome.desktop.interface show-battery-percentage true

# Use 12h clock format.
gsettings get org.gnome.desktop.interface clock-format &&
gsettings set org.gnome.desktop.interface clock-format 12h

# Enable automatic timezone.
gsettings get org.gnome.desktop.datetime automatic-timezone &&
gsettings set org.gnome.desktop.datetime automatic-timezone true

gsettings get org.gnome.desktop.interface toolbar-icons-size &&
gsettings set org.gnome.desktop.interface toolbar-icons-size small

# Disable bell.
gsettings get org.gnome.desktop.wm.preferences audible-bell &&
gsettings set org.gnome.desktop.wm.preferences audible-bell false

# Pin favorite programs to the dock.
gsettings get org.gnome.shell favorite-apps &&
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'chromium-browser.desktop', 'org.gnome.Terminal.desktop', 'sublime_text.desktop', 'code.desktop', 'deadbeef.desktop', 'aseprite.desktop', 'gimp.desktop', 'blender.desktop', 'fontforge.desktop', 'pomolectron.desktop', 'inkscape.desktop', 'org.kde.krita.desktop', 'pitivi.desktop', 'purr-data.desktop', 'obs.desktop', 'hyper.desktop', 'steam.desktop']"

# Set the dock icon dimensions.
gsettings get org.gnome.shell.extensions.dash-to-dock dash-max-icon-size &&
gsettings set org.gnome.shell.extensions.dash-to-dock dash-max-icon-size 32

# Enable the dock to autohide.
gsettings get org.gnome.shell.extensions.dash-to-dock dock-fixed &&
gsettings set org.gnome.shell.extensions.dash-to-dock dock-fixed false

# Don't expand the dock.
gsettings get org.gnome.shell.extensions.dash-to-dock extend-height &&
gsettings set org.gnome.shell.extensions.dash-to-dock extend-height false

# Don't show update notifications.
gsettings get com.ubuntu.update-notifier no-show-notifications &&
gsettings set com.ubuntu.update-notifier no-show-notifications true

# Allow file trees to be show inline.
gsettings get org.gnome.nautilus.list-view use-tree-view &&
gsettings set org.gnome.nautilus.list-view use-tree-view true

# Set the default file zoom to tiny.
gsettings get org.gnome.nautilus.list-view default-zoom-level &&
gsettings set org.gnome.nautilus.list-view default-zoom-level small

# Set the file columns.
gsettings get org.gnome.nautilus.list-view default-visible-columns &&
gsettings set org.gnome.nautilus.list-view default-visible-columns "['name', 'size', 'type', 'mime_type', 'date_modified_with_time', 'date_accessed']"

# Always show the file location bar instead of the descendent GUI.
gsettings get org.gnome.nautilus.preferences always-use-location-entry &&
gsettings set org.gnome.nautilus.preferences always-use-location-entry true

# Show hidden files.
gsettings get org.gnome.nautilus.preferences show-hidden-files &&
gsettings set org.gnome.nautilus.preferences show-hidden-files true
```

#### Keybindings
```bash
# Press fnc + esc to invert the function keys.

# Flip command and alt keys on Apple keyboards.
echo options hid_apple fnmode=2 swap_opt_cmd=1|
sudo tee -a /etc/modprobe.d/hid_apple.conf
sudo update-initramfs -u

# Disable tap to click.
gsettings get org.gnome.desktop.peripherals.touchpad tap-to-click &&
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click false

# Disable middle click to paste.
gsettings get org.gnome.desktop.interface gtk-enable-primary-paste &&
gsettings set org.gnome.desktop.interface gtk-enable-primary-paste false

# Screenshot is ctrl-super-p / ctrl-super-alt-p.
gsettings get org.gnome.settings-daemon.plugins.media-keys screenshot &&
gsettings set org.gnome.settings-daemon.plugins.media-keys screenshot '<Ctrl><Super>P'
gsettings get org.gnome.settings-daemon.plugins.media-keys window-screenshot &&
gsettings set org.gnome.settings-daemon.plugins.media-keys window-screenshot '<Ctrl><Alt><Super>P'
```

#### Terminal
```bash
# Clone the default profile as "Default".

# Hide the menu bar. It's available on right click.
gsettings get org.gnome.Terminal.Legacy.Settings default-show-menubar &&
gsettings set org.gnome.Terminal.Legacy.Settings default-show-menubar false

profile=$(gsettings get org.gnome.Terminal.ProfilesList default)
profile=${profile:1:-1}

# Do not limit scrollback.
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ scrollback-unlimited &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ scrollback-unlimited true

# Disable bell.
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ audible-bell &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ audible-bell false

# Disable blinking text.
gsettings get org.gnome.Terminal.Legacy.Profile:/:$profile/ text-blink-mode &&
gsettings set org.gnome.Terminal.Legacy.Profile:/:$profile/ text-blink-mode never

# Install a light theme and ~/.dir_colors:
  https://github.com/denysdovhan/one-gnome-terminal
  https://github.com/Anthony25/gnome-terminal-colors-solarized
```

#### Git
```bash
sudo make -C /usr/share/doc/git/contrib/credential/gnome-keyring
```

#### Meld
```bash
gsettings get org.gnome.meld indent-width &&
gsettings set org.gnome.meld indent-width 2

gsettings get org.gnome.meld highlight-syntax &&
gsettings set org.gnome.meld highlight-syntax true
```

#### Image Viewer (eog)
```bash
# Keep it pixelated.
gsettings get org.gnome.eog.view extrapolate &&
gsettings set org.gnome.eog.view extrapolate false
gsettings get org.gnome.eog.view interpolate &&
gsettings set org.gnome.eog.view interpolate false
```

#### VirtualBox
```bash
# Enable USB support.
sudo adduser stephen vboxusers
```

#### Chromium
##### about://settings
- Check On startup -> Continue where you left off
- Uncheck Appearance -> Use system title bar and borders
- Make Chromium the default

##### chrome://extensions
- [uBlock](https://chrome.google.com/webstore/detail/ublock/cjpalhdlnbpafiamejdnhcphjbkeiagm)
- [WikimediaDebug](https://chrome.google.com/webstore/detail/wikimediadebug/binmakecefompkjggiklgjenddjoifbb?utm_source=chrome-app-launcher-info-dialog)

#### DeaDBeeF
- Enable GTK3 theme
- Enable file browser plugin

## License (GPLv3)
© Stephen Niedzielski.

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
