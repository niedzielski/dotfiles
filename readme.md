# .files

My Debian laptop configuration.

## Hardware

Essentially, a ThinkPad X1 Yoga Gen 6 with maxed out specs, part number
20XYCTO1WW:

- 11th Generation Intel® Core™ i7-1185G7 Processor with vPro™ (3.00 GHz, up to
  4.80 GHz with Turbo Boost, 4 Cores, 8 Threads, 12 MB Cache)
- Linux Ubuntu 20.04
- Linux Ubuntu World Wide Multiple Language
- 32 GB LPDDR4x 4266MHz (Soldered)
- 256 GB PCIe SSD, OPAL
- 14.0" UHD+ (3840 x 2400) IPS, anti-reflective, anti-smudge, touchscreen, 500
  nits
- Integrated Intel® Iris® Xe Graphics
- Grey
- IR & 720p HD
- Lenovo Integrated Pen
- Intel® Wi-Fi 6 AX201 802.11AX with vPro™ (2 x 2) & Bluetooth® 5.2
- Fingerprint Reader
- Backlit - US English
- Enabled Discrete TPM2.0
- BIOS Absolute Enabled
- 4 Cell Li-Polymer 57Wh
- 65W AC
- 14.0" WQUXGA (3840x2400) IPS Anti Reflection/Anti Smudge 500nit MultiTouch
  Narrow Bezel 100% DCI-P3 HDR, IR and HD Camera, Mic
- Retail Packaging
- Publication-English
- 1 Year Depot or Carry-in

With the following service plans:
- 3Y Courier/Carry-in upgrade from 1Y Courier/Carry-in, part number 5WS0E97328
- 3Y Accidental Damage Protection Add On, part number 5PS0F15928

I wished to try a WWAN, as my partner's carrier, Google Fi, provides data SIMs
at no additional charge, but
[PSREF lists them as unavailable in the US](https://psref.lenovo.com/Product/ThinkPad/ThinkPad_X1_Yoga_Gen_6). I've read one cannot be retroactively added due to a
missing antenna assembly.

I ordered the cheapest hard drive available and replaced it with a
"[Samsung 2TB 980 PRO PCIe 4.0 x4 M.2 Internal SSD](https://www.bhphotovideo.com/c/product/1624326-REG/samsung_mz_v8p2t0b_am_2tb_980_pro_pcie.html)"
according to this
[replacement guide](https://www.youtube.com/watch?v=j6zhenaLjho).

The builtin webcam is poor. The keyboard is lovely.

The machine is incapable of full-speed rendering even the oldest games in
full-screen due to the modest integrated GPU which maximizes battery life. I am
attempting to try an [AMD eGPU configuration](https://egpu.io) but cannot source
a GPU at a fair price.

There seemed to be able to be a perpetual sale of some sort and I was also able
to use a partner discount.

### Peripherals

- CalDigit TS3 Plus
- [8BitDo SF30 Pro](https://www.8bitdo.com/sn30pro-sf30pro)

## Back Up
1. Empty trash; consider dumping Steam and Wine games.
2. Export bookmarks and settings from Firefox.
3. Copy files: `cd / && tarpipe /media/stephen/disk/home home`.
4. Backup with rsync too: `backup`.
5. Back up browser tabs, unsaved editor files (check Code, Sublime Text, and
   DeaDBeeF).
6. Back up packages and verify contents.
7. Take screenshot of launcher.
8. Download the latest Debian release and check `md5sum *.iso`.
9. Copy to USB thumbdrive (not SD Card): `mkfs.fat -I -n FOO /dev/sdX && time dd bs=4M if=foo.iso of=/dev/sdX`.
10. Check Grub options.

## Install

Download [Debian stable](https://cdimage.debian.org/images/unofficial/non-free/images-including-firmware/11.0.0+nonfree/amd64/iso-dvd/). To-do: try testing.

### BIOS

Set the sleep state to "Windows and Linux" for now. S3 seems to cause the
trackpad to stutter after sleep. I don't remember what else I changed but it was
a number of things, including, I think, enabling secure boot.

### Debian Installer

I wanted to increase the swap partition size but it didn't seem customizable in
itself. I haven't notice any out-of-memory issues so far.

![Disk partition configuration](partman_choose_partition_0.png)

![Optional software configuration](tasksel_first_0.png)

### Grub

Edit /etc/default/grub with the following
[temporary changes for keyboard lag](https://forums.lenovo.com/t5/ThinkPad-X-Series-Laptops/Lag-and-stuttering-on-X1-Carbon-Gen-9-while-running-Linux-untested-on-Windows/m-p/5082352):

```grub
GRUB_CMDLINE_LINUX_DEFAULT="i915.enable_psr=0"
```

### Firmware

The following grabbed the latest upgrades listed on the manufacturer's website:

```bash
fwupdmgr refresh
fwupdmgr get-devices
sudo fwupdmgr update
```

I had trouble getting fwupd to recognize my 8BitDo controller and I'm unsure
if changing the controller's boot mode (maybe L1 + R1 + Start) or using a custom
fwupdmgr install fixed it.

### Enable sudo

```bash
su -c 'usermod -aG sudo stephen' && echo ok
```

### Restore

1. `sudo apt install pigz vim`.
2. Decompress backup: `time tar xf /media/stephen/disk/home-2016-01-01-00-00-00-000000000.tar.gz -I pigz && echo ok`.

### APT

1. Open Software & Updates.
2. View Other Software.
3. Disable cdrom as a software source.

```bash
sudo apt update &&
sudo apt upgrade &&
sudo apt install \
  aseprite blender calibre code build-essential chromium code colordiff \
  command-not-found csvtool curl diffpdf duf fd-find flac flameshot fontforge \
  fonts-roboto fzf gimp git gpick gthumb htop imagemagick inkscape \
  libimage-exiftool-perl lm-sensors lshw meld mesa-utils moreutils mpv nmap \
  nodejs npm obs-studio opus-tools peek picard potrace powertop pv ripgrep \
  rsync scrcpy sg3-utils sox tmux tree ttf-bitstream-vera whois wmctrl xclip \
  xdotool zoxide &&
sudo apt remove evolution rhythmbox &&
sudo apt autoremove
```

#### Compare Previously Installed Packages
```bash
alias strip='sed -r "1,5 d; s%^(ii|rc)\s+([^ ]+).*%\2%"' &&
meld <(strip dpkg.txt|sort) <(dpkg -l|strip|sort)
```

### NPM

```bash
npm i -g create-react-app live-server npm-check-updates source-map-explorer
```

### Wine

```bash
sudo apt install wine winetricks &&
sudo dpkg --add-architecture i386 &&
sudo apt update && sudo apt install wine32
```

#### Exact Audio Copy

```bash
# Select the default prefix and install just dotnet20 without the service packs.
WINEARCH=win32 WINEPREFIX="$HOME/opt/eac" winetricks
WINEARCH=win32 WINEPREFIX="$HOME/opt/eac" wine ~/dl/eac-1.6.exe
```

The program settings are not easily isolated. They're smattered across the
Windows registry and elsewhere, unfortunately. Additionally, some are CD drive
specific.

### Chromium

1. Sign in
2. Set chrome://flags/#force-color-profile to sRGB.
3. Disable cache in the DevTools network tab.

##### about://settings

- Check On startup -> Continue where you left off.
- Make Chromium the default.
- Change search to DuckDuckGo.

#### chrome://extensions

- [uBlock](https://chrome.google.com/webstore/detail/ublock/cjpalhdlnbpafiamejdnhcphjbkeiagm)
- [React](https://chrome.google.com/webstore/detail/react-developer-tools/fmkadmapgofadopljbjfkapdkoienihi)
- [Redux](https://chrome.google.com/webstore/detail/redux-devtools/lmhkpmbekcpmknklioeibfkpmmfibljd)

### Firefox

I use Firefox as a bookmark manager presently. Debian stable is ancient so I had
to export my bookmarks as JSON and reimport them on the old version.

### Sublime Text

```bash
curl https://download.sublimetext.com/sublimehq-pub.gpg |
sudo apt-key add -
echo 'deb https://download.sublimetext.com/ apt/stable/' |
sudo tee /etc/apt/sources.list.d/sublime-text.list
sudo apt update &&
sudo apt install sublime-text
# Register.
```

### Manual Downloads

- [Aseprite](https://www.aseprite.org)
- [bat](https://github.com/sharkdp/bat/releases)
- [DeaDBeeF](http://deadbeef.sourceforge.net/download.html)
- [Delta](https://github.com/dandavison/delta/releases)
- [entr](https://github.com/eradman/entr/releases)
- [Foliate](https://github.com/johnfactotum/foliate/releases)
- [ripgrep](https://github.com/BurntSushi/ripgrep/releases)

### GNOME Extensions

Install via Firefox:

- [Emoji Selector](https://extensions.gnome.org/extension/1162/emoji-selector/)

### UI

```bash
# Show battery percentage.
gsettings get org.gnome.desktop.interface show-battery-percentage &&
gsettings set org.gnome.desktop.interface show-battery-percentage true

# Disable bell.
gsettings get org.gnome.desktop.wm.preferences audible-bell &&
gsettings set org.gnome.desktop.wm.preferences audible-bell false

# Allow file trees to be show inline.
gsettings get org.gnome.nautilus.list-view use-tree-view &&
gsettings set org.gnome.nautilus.list-view use-tree-view true

# Set the default file zoom to tiny.
gsettings get org.gnome.nautilus.list-view default-zoom-level &&
gsettings set org.gnome.nautilus.list-view default-zoom-level small

# Set the file columns.
# [to-do] Add crtime: https://gitlab.gnome.org/GNOME/nautilus/-/issues/1566.
gsettings get org.gnome.nautilus.list-view default-visible-columns &&
gsettings set org.gnome.nautilus.list-view default-visible-columns "['name', 'size', 'type', 'mime_type', 'date_modified_with_time', 'date_accessed']"

# Always show the file location bar instead of the descendent GUI.
gsettings get org.gnome.nautilus.preferences always-use-location-entry &&
gsettings set org.gnome.nautilus.preferences always-use-location-entry true

# Show hidden files.
gsettings get org.gnome.nautilus.preferences show-hidden-files &&
gsettings set org.gnome.nautilus.preferences show-hidden-files true
```

### Keybindings

```bash
# Press fnc + esc to invert the function keys.

# Flip command and alt keys on Apple keyboards.
echo options hid_apple fnmode=2 swap_opt_cmd=1 |
sudo tee -a /etc/modprobe.d/hid_apple.conf
sudo update-initramfs -u

# Disable middle click to paste.
gsettings get org.gnome.desktop.interface gtk-enable-primary-paste &&
gsettings set org.gnome.desktop.interface gtk-enable-primary-paste false

# Screenshot is ctrl-super-p / ctrl-super-alt-p.
gsettings get org.gnome.settings-daemon.plugins.media-keys screenshot &&
gsettings set org.gnome.settings-daemon.plugins.media-keys screenshot "['<Ctrl><Super>P']"
gsettings get org.gnome.settings-daemon.plugins.media-keys window-screenshot &&
gsettings set org.gnome.settings-daemon.plugins.media-keys window-screenshot "['<Ctrl><Alt><Super>P']"
```

### Meld

```bash
gsettings get org.gnome.meld indent-width &&
gsettings set org.gnome.meld indent-width 2

gsettings get org.gnome.meld highlight-syntax &&
gsettings set org.gnome.meld highlight-syntax true
```

### MakeMKV

1. Download the latest: https://www.makemkv.com/forum/viewtopic.php?f=3&t=224.
2. Install dependencies:
```
sudo apt install libbluray-dev libbluray2 libbluray-bdj libaacs0 libaacs-dev libbdplus0 libbdplus-dev
```
3. Follow the forum directions (I couldn't get a prefix install to work).
4. Register.

### Powertop

To-do: How can I run configure mode non-interactively until it has enough
samples?

```bash
sudo powertop -c
sudo powertop --auto-tune
```

### lm_sensors

Attempt most and automatically to modules:

```bash
sudo sensors-detect &&
sensors -u
```

### Terminal

```bash
# Clone the default profile as "Default".

profile=$(gsettings get org.gnome.Terminal.ProfilesList default)
profile=${profile:1:-1}

# Do not limit scrollback.
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ scrollback-unlimited &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ scrollback-unlimited true

# Disable bell.
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ audible-bell &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ audible-bell false

# Set default size to 100x40.
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-columns &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-columns 100
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-rows &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-rows 40
```

### Image Viewer (eog)

```bash
# Keep it pixelated.
gsettings get org.gnome.eog.view extrapolate &&
gsettings set org.gnome.eog.view extrapolate false
gsettings get org.gnome.eog.view interpolate &&
gsettings set org.gnome.eog.view interpolate false
```

### DeaDBeeF

- Enable GTK3 theme.
- Enable file browser plugin.

### Picard

### Calibre / Kindle

### Miscellaneous

- Disable suspend when plugged in.
- Add user icon.
- Set lock screen to 10 min.
- Enable location services.
- Add terminal shortcut for `<Super>T`.
- Enable tap to click for trackpad.

## License (GPLv3)

© Stephen Niedzielski.

### GPL-3.0-only

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, version 3.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.
