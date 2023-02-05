# .files

My system configuration.

## Hardware

### Laptop

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

I wished to try a WWAN, as my carrier, Google Fi, provides data SIMs at no
additional charge, but
[PSREF lists them as unavailable in the US](https://psref.lenovo.com/Product/ThinkPad/ThinkPad_X1_Yoga_Gen_6).
I've read one cannot be retroactively added due to a missing antenna assembly.

I ordered the cheapest hard drive available and replaced it with a
"[Samsung 2TB 980 PRO PCIe 4.0 x4 M.2 Internal SSD](https://www.bhphotovideo.com/c/product/1624326-REG/samsung_mz_v8p2t0b_am_2tb_980_pro_pcie.html)"
according to this
[replacement guide](https://www.youtube.com/watch?v=j6zhenaLjho).
[I upgraded the firmware on Feb, 2023 following these notes](https://news.ycombinator.com/item?id=34649376).

The builtin webcam is poor. The keyboard is lovely.

I haven't made much use of its convertible functionality yet.

There seemed to be able to be a perpetual sale of some sort and I was also able
to use a partner discount.

### External GPU

- [AMD Radeon RX 6900 XT Graphics processing unit](https://www.amd.com/en/direct-buy/5458372200/us)
- [Razer Core X Chroma enclosure](https://www.razer.com/gaming-egpus/razer-core-x/RC21-01430100-R3U1)
- [Noctua NF-A12x25 5V 120 mm fan](https://noctua.at/en/products/fan/nf-a12x25-5v)
- [Corsair SF750 80 Plus Platinum SFX Power Supply unit](https://www.corsair.com/us/en/Categories/Products/Power-Supply-Units/Power-Supply-Units-Advanced/SF-Series/p/CP-9020186-NA)
- [100 mm² x 1 mm thermal pad](https://www.amazon.com/dp/B086W119DK)

I used the [egpu.io buyer's guide and forum](https://egpu.io) to inform.

The GPU fans don't ever see to run at a very fast rate but when they were above
a low speed, they're noisy.

The original power supply's fan is always on. The replacement's doesn't turn on
until the load exceeds 300 W. I would replace the fan in it but I don't think
I've ever heard it turn on and I don't want to risk working near large
capacitors. The replacement supply is so much smaller than the original and is
unable to mount to anything so it rests on the thermal pad which I divided in
half and stacked to align the plug with the opening. I don't think a thermal
pad is necessary over a piece of appropriately sized metal but it's tacky which
helps keep the unit in place.

I had replaced the side fan with a 12 V
[Noctua NF-S12B redux-700](https://noctua.at/en/nf-s12b-redux-700) as I read
that the enclosure's voltage was inconsistent but 5 V or above, it's
[the quietest 120 mm fan available](https://www.quietpc.com/120mmfans), and I
figured the worst case scenario would be that the GPU fans would kick on or the
system would go into thermal shutdown. It worked and it was almost silent but I
think the system was too hot. It was almost always quite warm and, when in heavy
use, downright hot.

I replaced the fan with the quietest 5 V fan available but
it is significantly louder. The system is always quite cool now. Maybe a 140 mm
would fit. I wish the enclosure had a 12 V PWM (four pin). The GPU fans turn off
when they can.

The GPU fans blow outward so I had flipped the direction of the side fan to blow
inward but the GPU doesn't quite cover the air vent so I guessed that the
original design to have both the side fan and GPU fans blowing outwards is
probably best and have stuck with that.
[The airflow direction of the side fan is from the manufacturer's label outwards](https://noctua.at/en/how-can-i-find-out-the-direction-of-airflow-and-sense-of-rotation).
I imagine that the air could mostly flow in over the power supply where there's
a small vent but the case is really just an approximation for any card that fits
and having replaced the supply with such a different one, I suppose all bets are
off. In both directions, the fans seem to pull in a lot of particulates despite
resting high on my desk shelf.

### Peripherals

- [Dell UltraSharp 27 4K USB-C Monitor - U2720Q, 68.4cm (27") (210-AVJV)](https://www.dell.com/en-us/work/shop/ultrasharp-27-4k-usb-c-monitor-u2720q/apd/210-avjv/monitors-monitor-accessories)
- [Cintiq 16" DTK-1660 pen display](https://101.wacom.com/UserHelp/en/TOC/DTK-1660.html) with [ExpressKey™ remote](https://estore.wacom.com/en-US/expresskey-remote-accessory-us-ack411050.html)
- [CalDigit TS4 Dock](https://www.caldigit.com/thunderbolt-station-4)
- [8BitDo SN30 Pro+](https://www.8bitdo.com/sn30-pro-plus)
- [CalDigit Thunderbolt 3 cable](http://shop.caldigit.com/us/TBT3-A20B-540) (I have a second Belkin cable but I failed to note the model)
- Apple Wired Keyboard (A1243)
- Mostly [3M Wired Ergonomic Mouse, Small (EM500GPS)](https://www.3m.com/3M/en_US/p/d/cbgbjw011265); sometimes [MX Vertical Advanced Ergonomic Mouse](https://www.logitech.com/en-us/products/mice/mx-vertical-ergonomic-mouse.910-005447.html)
- LX3 Wireless Charging Stand

### Desk

- [Bamboo, curved, UPLIFT standing desk with black v2-commercial C-frame 72" x 30"](https://www.upliftdesk.com/uplift-v2-standing-desk-v2-or-v2-commercial)
- [Big Ultra-Thin Keyboard Tray System by UPLIFT Desk](https://www.upliftdesk.com/big-ultra-thin-keyboard-tray-system-uplift-desk)
  - KBT009-BLK Quick-Adjust Mechanism
  - UPLIFT Track Spacer
- [8-Outlet mountable surge protector](https://www.upliftdesk.com/8-outlet-mountable-surge-protector-uplift-desk)

## Back Up

1. Empty trash; consider dumping Steam and Wine games.
2. Export bookmarks and settings from Firefox and bookmarklets from Chromium.
3. Back up browser tabs, unsaved editor files (check Code, Sublime Text, and
   DeaDBeeF).
4. Back up packages and verify contents.
5. Check Grub options.
6. Take screenshot of launcher.
7. Copy files to two disks: `cd / && tarpipe /media/stephen/disk/home home`.
8. Backup with rsync too: `backup`. To-do: checksum tally as I go.
9. Download the latest [Debian stable release](https://cdimage.debian.org/images/unofficial/non-free/images-including-firmware/current-live/amd64/iso-hybrid/debian-live-11.2.0-amd64-gnome+nonfree.iso) and check `md5sum *.iso`.
10. Copy to USB thumbdrive (not SD Card): `time cat foo.iso > /dev/sdX`. I use `gnome-disks` to unmount any preexisting partitions.

## Install

### BIOS

I don't remember what else I changed but it was a number of things, including, I
think, enabling secure boot.

### Debian Installer

I wanted to increase the swap partition size but it didn't seem easily
customizable in itself. I haven't noticed any out-of-memory issues.

![Disk partition configuration](partman_choose_partition_0.png)

### Firmware

The following grabbed the latest upgrades listed on the manufacturer's website:

```bash
fwupdmgr refresh
fwupdmgr get-devices
sudo fwupdmgr update
```

I had trouble getting fwupd to recognize my 8BitDo SF30 Pro controller and I'm unsure
if changing the controller's boot mode (maybe L1 + R1 + Start) or using a custom
fwupdmgr install fixed it. I used [macOS for the SN30 Pro+](https://support.8bitdo.com/firmware-updater.html) ([see issue](https://github.com/fwupd/fwupd/issues/1681)).

### External GPU

I think radeon is for old graphics cards and amdgpu is the name of both the
open and closed-source newer drivers. In the past, I had quite a bit of trouble
getting the closed-source one running and given the naming collision, maybe I
didn't, and was actually using the open-source one in a most convoluted way.

The stock install will actually connect video but the framerate of `glxgears` is
only about 25 FPS at any resolution and window animations are noticeably choppy.
[all-ways-egpu](https://github.com/ewagner12/all-ways-egpu) (Wayland) and
[egpu-switcher](https://github.com/hertg/egpu-switcher) (Xorg) both work to fix
it. I had some trouble with pen pressure detection under Wayland so I switched
to Xorg temporarily. For whatever reason, the changes under all-ways-egpu seem
to be working there.

#### all-ways-egpu (Wayland)

```bash
git clone https://github.com/ewagner12/all-ways-egpu.git
cd all-ways-egpu
sudo make install
```

I tried method 1 but it ended up hanging my boot and I had to reenable the internal GPU from recovery mode.

I am currently using method 2. I identify external GPU as primary.

```
$ sudo all-ways-egpu setup
To force the eGPU as primary, we need to know which card is the eGPU to be used as primary.
00:02.0 VGA compatible controller: Intel Corporation TigerLake GT2 [Iris Xe Graphics] (rev 01)
Is this the eGPU to be used as primary? [y/N]

Not using 00:02.0
24:00.0 VGA compatible controller: Advanced Micro Devices, Inc. [AMD/ATI] Navi 21 [Radeon RX 6800/6800 XT / 6900 XT] (rev c0)
Is this the eGPU to be used as primary? [y/N]
y
Using as primary 24:00.0
00:1f.3 Audio device: Intel Corporation Tiger Lake-LP Smart Sound Technology Audio Controller (rev 20)
Is this the eGPU to be used as primary? [y/N]

Not using 00:1f.3
24:00.1 Audio device: Advanced Micro Devices, Inc. [AMD/ATI] Device ab28
Is this the eGPU to be used as primary? [y/N]

Not using 24:00.1
Identify all iGPU/dGPUs to be potentially disabled boot:
00:02.0 VGA compatible controller: Intel Corporation TigerLake GT2 [Iris Xe Graphics] (rev 01)
Would you like to disable this device during boot [y/N]

Not disabling 00:02.0
24:00.0 VGA compatible controller: Advanced Micro Devices, Inc. [AMD/ATI] Navi 21 [Radeon RX 6800/6800 XT / 6900 XT] (rev c0)
Would you like to disable this device during boot [y/N]

Not disabling 24:00.0
00:1f.3 Audio device: Intel Corporation Tiger Lake-LP Smart Sound Technology Audio Controller (rev 20)
Would you like to disable this device during boot [y/N]

Not disabling 00:1f.3
24:00.1 Audio device: Advanced Micro Devices, Inc. [AMD/ATI] Device ab28
Would you like to disable this device during boot [y/N]

Not disabling 24:00.1
Manual Setup: Enter Bus IDs and drivers in the following example format or enter 'n' to skip.
xx:xx.x driver
n
Recommended if using Method 1: Attempt to re-enable the iGPU/initially disabled devices after login? [y/N]

Recommended if using Method 2: Attempt to set boot_vga flag at startup? [y/N]
y
Created symlink /etc/systemd/system/graphical.target.wants/all-ways-egpu-boot-vga.service → /etc/systemd/system/all-ways-egpu-boot-vga.service.
Created symlink /etc/systemd/system/halt.target.wants/all-ways-egpu-shutdown.service → /etc/systemd/system/all-ways-egpu-shutdown.service.
Created symlink /etc/systemd/system/shutdown.target.wants/all-ways-egpu-shutdown.service → /etc/systemd/system/all-ways-egpu-shutdown.service.
Created symlink /etc/systemd/system/reboot.target.wants/all-ways-egpu-shutdown.service → /etc/systemd/system/all-ways-egpu-shutdown.service.
Configuration files sucessfully created. See help for usage information
```

#### egpu-switcher (X11)

This generates X11 display configs and then links the appropriate one (internal or egpu):

```xorg
# /etc/X11/xorg.conf.egpu
Section "Module"
    Load           "modesetting"
EndSection

Section "Device"
    Identifier     "Device0"
    Driver         "amdgpu"
    BusID          "36:0:0"
    Option         "AllowEmptyInitialConfiguration"
    Option         "AllowExternalGpus" "True"
EndSection
```

```xorg
# /etc/X11/xorg.conf.internal
Section "Module"
    Load           "modesetting"
EndSection

Section "Device"
    Identifier     "Device0"
    Driver         "intel"
    BusID          "0:2:0"
    Option         "AllowEmptyInitialConfiguration"
    Option         "AllowExternalGpus" "True"
EndSection
```

#### Diagnostics

With the AMD graphic driver loaded (amdgpu), radeontop (not to be confused with
the older driver, radeon) has very low usage during normal desktop usage and
high usage playing games:

```bash
sudo radeontop
```

`glxgears` at full screen stimulates it slightly with the graphics pipe bouncing
between 0.83% and 7.5%.

```
$ glxgears
Running synchronized to the vertical refresh.  The framerate should be
approximately the same as the monitor refresh rate.
304 frames in 5.0 seconds = 60.621 FPS
300 frames in 5.0 seconds = 59.998 FPS
301 frames in 5.0 seconds = 60.002 FPS
300 frames in 5.0 seconds = 59.999 FPS
301 frames in 5.0 seconds = 59.999 FPS
```

```lines=16
$ boltctl
 ● Razer Core X Chroma
   ├─ type:          peripheral
   ├─ name:          Core X Chroma
   ├─ vendor:        Razer
   ├─ generation:    Thunderbolt 3
   ├─ status:        authorized
   │  ├─ rx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  ├─ tx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  └─ authflags:  none
   ├─ authorized:    Sat 26 Feb 2022 04:13:45 AM UTC
   ├─ connected:     Sat 26 Feb 2022 04:13:45 AM UTC
   └─ stored:        Fri 25 Feb 2022 08:55:48 PM UTC
      ├─ policy:     iommu
      └─ key:        no

 ● Razer Core X Chroma #2
   ├─ type:          peripheral
   ├─ name:          Core X Chroma
   ├─ vendor:        Razer
   ├─ generation:    Thunderbolt 3
   ├─ status:        authorized
   │  ├─ rx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  ├─ tx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  └─ authflags:  none
   ├─ authorized:    Sat 26 Feb 2022 04:13:52 AM UTC
   ├─ connected:     Sat 26 Feb 2022 04:13:52 AM UTC
   └─ stored:        Fri 25 Feb 2022 08:55:48 PM UTC
      ├─ policy:     iommu
      └─ key:        no

 ● CalDigit, Inc. TS3 Plus
   ├─ type:          peripheral
   ├─ name:          TS3 Plus
   ├─ vendor:        CalDigit, Inc.
   ├─ generation:    Thunderbolt 3
   ├─ status:        authorized
   │  ├─ rx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  ├─ tx speed:   40 Gb/s = 2 lanes * 20 Gb/s
   │  └─ authflags:  none
   ├─ authorized:    Sat 26 Feb 2022 04:13:44 AM UTC
   ├─ connected:     Sat 26 Feb 2022 04:13:44 AM UTC
   └─ stored:        Fri 25 Feb 2022 08:55:48 PM UTC
      ├─ policy:     iommu
      └─ key:        no
```

For the closed source driver, I had to manually insert it with
`sudo modprobe -v amdgpu` which would either quit the current session or hang
it. In the latter case, I had to reset it with `alt-prtsc-k`.

`lspci -k | grep -EiA3 '3d|vga|video'` reports the drivers used:

```
lspci -k | grep -EA3 '3D|VGA|Video'
00:02.0 VGA compatible controller: Intel Corporation TigerLake GT2 [Iris Xe Graphics] (rev 01)
	Subsystem: Lenovo Iris Xe Graphics
	Kernel driver in use: i915
	Kernel modules: i915
--
24:00.0 VGA compatible controller: Advanced Micro Devices, Inc. [AMD/ATI] Navi 21 [Radeon RX 6800/6800 XT / 6900 XT] (rev c0)
	Subsystem: Advanced Micro Devices, Inc. [AMD/ATI] Navi 21 [Radeon RX 6800/6800 XT / 6900 XT]
	Kernel driver in use: amdgpu
	Kernel modules: amdgpu
```

System information as detected by Steam:

```
…
Video Card:
    Driver:  AMD AMD SIENNA_CICHLID (DRM 3.40.0, 5.10.0-11-amd64, LLVM 11.0.1)
    Driver Version:  4.6 (Compatibility Profile) Mesa 20.3.5
    OpenGL Version: 4.6
    Desktop Color Depth: 24 bits per pixel
    Monitor Refresh Rate: 59 Hz
    VendorID:  0x1002
    DeviceID:  0x73bf
    Revision Not Detected
    Number of Monitors:  2
    Number of Logical Video Cards:  2
    Primary Display Resolution:  3840 x 2160
    Desktop Resolution: 5760 x 2160
    Primary Display Size: 23.62" x 13.39" (27.13" diag)
                                            60.0cm x 34.0cm (68.9cm diag)
    Primary VRAM: 16384 MB
…
```

`glxinfo -B` / `DRI_PRIME=0 glxinfo -B` / `DRI_PRIME=2 glxinfo -B` / `DRI_PRIME=3 glxinfo -B` reports:

```
name of display: :0
display: :0  screen: 0
direct rendering: Yes
Extended renderer info (GLX_MESA_query_renderer):
    Vendor: AMD (0x1002)
    Device: AMD SIENNA_CICHLID (DRM 3.40.0, 5.10.0-11-amd64, LLVM 11.0.1) (0x73bf)
    Version: 20.3.5
    Accelerated: yes
    Video memory: 16384MB
    Unified memory: no
    Preferred profile: core (0x1)
    Max core profile version: 4.6
    Max compat profile version: 4.6
    Max GLES1 profile version: 1.1
    Max GLES[23] profile version: 3.2
Memory info (GL_ATI_meminfo):
    VBO free memory - total: 15326 MB, largest block: 15326 MB
    VBO free aux. memory - total: 16205 MB, largest block: 16205 MB
    Texture free memory - total: 15326 MB, largest block: 15326 MB
    Texture free aux. memory - total: 16205 MB, largest block: 16205 MB
    Renderbuffer free memory - total: 15326 MB, largest block: 15326 MB
    Renderbuffer free aux. memory - total: 16205 MB, largest block: 16205 MB
Memory info (GL_NVX_gpu_memory_info):
    Dedicated video memory: 16384 MB
    Total available memory: 32752 MB
    Currently available dedicated video memory: 15326 MB
OpenGL vendor string: AMD
OpenGL renderer string: AMD SIENNA_CICHLID (DRM 3.40.0, 5.10.0-11-amd64, LLVM 11.0.1)
OpenGL core profile version string: 4.6 (Core Profile) Mesa 20.3.5
OpenGL core profile shading language version string: 4.60
OpenGL core profile context flags: (none)
OpenGL core profile profile mask: core profile

OpenGL version string: 4.6 (Compatibility Profile) Mesa 20.3.5
OpenGL shading language version string: 4.60
OpenGL context flags: (none)
OpenGL profile mask: compatibility profile

OpenGL ES profile version string: OpenGL ES 3.2 Mesa 20.3.5
OpenGL ES profile shading language version string: OpenGL ES GLSL ES 3.20
```

`DRI_PRIME=1 glxinfo -B` reports the internal GPU:

```
name of display: :0
display: :0  screen: 0
direct rendering: Yes
Extended renderer info (GLX_MESA_query_renderer):
    Vendor: Intel (0x8086)
    Device: Mesa Intel(R) Xe Graphics (TGL GT2) (0x9a49)
    Version: 20.3.5
    Accelerated: yes
    Video memory: 3072MB
    Unified memory: yes
    Preferred profile: core (0x1)
    Max core profile version: 4.6
    Max compat profile version: 4.6
    Max GLES1 profile version: 1.1
    Max GLES[23] profile version: 3.2
OpenGL vendor string: Intel
OpenGL renderer string: Mesa Intel(R) Xe Graphics (TGL GT2)
OpenGL core profile version string: 4.6 (Core Profile) Mesa 20.3.5
OpenGL core profile shading language version string: 4.60
OpenGL core profile context flags: (none)
OpenGL core profile profile mask: core profile

OpenGL version string: 4.6 (Compatibility Profile) Mesa 20.3.5
OpenGL shading language version string: 4.60
OpenGL context flags: (none)
OpenGL profile mask: compatibility profile

OpenGL ES profile version string: OpenGL ES 3.2 Mesa 20.3.5
OpenGL ES profile shading language version string: OpenGL ES GLSL ES 3.20
```

Maximizing `glxgears` and turning off vsync enables higher frame rates:

```
$ vblank_mode=0 glxgears
ATTENTION: default value of option vblank_mode overridden by environment.
72519 frames in 5.0 seconds = 14503.789 FPS
61577 frames in 5.0 seconds = 12315.346 FPS
61253 frames in 5.0 seconds = 12250.429 FPS

$ DRI_PRIME=1 vblank_mode=0 glxgears
ATTENTION: default value of option vblank_mode overridden by environment.
1798 frames in 5.0 seconds = 358.478 FPS
333 frames in 5.0 seconds = 66.414 FPS
334 frames in 5.0 seconds = 66.627 FPS
```

#### What doesn't work so far

- Hotplugging. I have to power off to at/detach the cable. I need to fool with this more as the PCIE bus attaches at least the first time.

#### Observations

These are dated from the closed source driver:

- Max Payne 3 runs smoothly on nearly maxed out settings. Unfortunately, the internal GPU's memory is incorrectly identified as the limit and the game forbids exceeding it.
- Red Dead Redemption 2 runs smoothly with maxed out settings.
- Cyberpunk 2077 v1.3 was pretty choppy and at 1920x1080 and it didn't seem to matter what quality. Indoor battles were smooth though. v1.5 was a lot better but still lagged periodically, especially when driving.
- Portal runs smoothly on maxed out settings.
- NES runs smoothly.

Open-source driver:

- Nature Elsewhere runs between 55-57 FPS in Chromium. Probably an issue with the game :]

### Groups

The input group is need for joystick access.

```bash
su -c '/sbin/usermod -aG sudo stephen' && echo ok
su -c '/sbin/usermod -aG input stephen' && echo ok
```

This required a reboot not just a logout.

### Inputs

Xbox mode (2 LEDs) works well. I remapped axes and then calibrated with `jstest-gtk` then persisted the state with `sudo jscal-store /dev/input/js1`.

### Restore

1. Disable suspend on AC:

```bash
gsettings get org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type &&
gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-type nothing
```

2. `sudo apt install pigz vim`.
3. Decompress backup: `time tar --extract --file=/media/stephen/disk/home-2016-01-01-00-00-00-000000000.tar.gz --use-compress-program=pigz && echo ok`.

### APT

1. Open Software & Updates.
2. Enable DFSG-compatible Software with Non-Free Dependencies (contrib).

```bash
sudo apt update &&
sudo apt upgrade &&
sudo apt dist-upgrade &&
sudo apt install \
  bash-completion blender build-essential calibre chromium code colordiff \
  command-not-found csvtool curl diffpdf docker-compose entr fd-find flac flatpak flameshot fontforge \
  fonts-roboto fzf gimp git gpick gthumb htop imagemagick inkscape jstest-gtk \
  krita libimage-exiftool-perl lm-sensors lshw meld mednafen mesa-utils moreutils mpv nmap \
  obs-studio opus-tools peek picard pigz potrace powertop pv \
  radeontop rsync scrcpy sg3-utils sqlitebrowser sox tmux tree ttf-bitstream-vera \
  vim whois wmctrl xclip xdotool zoxide &&
sudo apt remove evolution rhythmbox thunderbolt unattended-upgrades &&
sudo apt autoremove
```

#### Compare Previously Installed Packages

```bash
alias strip='sed -r "1,5 d; s%^(ii|rc)\s+([^ ]+).*%\2%"' &&
meld <(strip dpkg.text|sort) <(dpkg --list|strip|sort)
```

### NPM

```bash
npm i -g create-react-app live-server npm-check-updates source-map-explorer wscat
```

### Wine

```bash
sudo apt install wine winetricks winbind &&
sudo dpkg --add-architecture i386 &&
sudo apt update && sudo apt install wine32
```

I am trying to use [Bottles](https://usebottles.com) (via Flatpak) and [CrossOver](https://www.codeweavers.com/crossover) more. 

#### Bottles

Fix permissions via `flatpak run com.github.tchx84.Flatseal`.

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

1. Set chrome://flags/#force-color-profile to sRGB.
2. Disable cache in the DevTools network tab.
3. Enable do not track.

##### about://settings

- Check On startup -> Continue where you left off.
- Make Chromium the default.
- Change search to DuckDuckGo.
- Enable classic theme otherwise incognito mode isn't different enough.

#### chrome://extensions

- [uBlacklist](https://chrome.google.com/webstore/detail/ublacklist/pncfbmialoiaghdehhbnbhkkgmjanfhe)
- [uBlock Origin](https://chrome.google.com/webstore/detail/ublock/cjpalhdlnbpafiamejdnhcphjbkeiagm)

### Firefox

I use Firefox as a bookmark manager presently. Debian stable is ancient so I had
to export my bookmarks as JSON and reimport them on the old version.

### Sublime Text 3

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
- [Chrome](https://www.google.com/chrome)
- [DeaDBeeF](http://deadbeef.sourceforge.net/download.html)
- [Delta](https://github.com/dandavison/delta/releases)
- [Disk Usage/Free Utility](https://github.com/muesli/duf)
- [Foliate](https://github.com/johnfactotum/foliate/releases)
- [Node.js](https://nodejs.org)
- [ripgrep](https://github.com/BurntSushi/ripgrep/releases)
- [Steam](https://store.steampowered.com/about)
- [TVPaint](https://www.tvpaint.com)
- [Visual Studio Code](https://code.visualstudio.com/download)

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

# Pin favorite programs to the dock.
gsettings get org.gnome.shell favorite-apps &&
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'chromium.desktop', 'deadbeef.desktop', 'sublime_text.desktop', 'codium.desktop', 'org.gnome.Terminal.desktop', 'aseprite.desktop', 'org.gnome.gThumb.desktop', 'com.github.johnfactotum.Foliate.desktop', 'gnome-system-monitor.desktop', 'mednaffe.desktop', 'steam.desktop', 'com.usebottles.bottles.desktop', 'cxmenu-cxoffice-0-29ra4ke-CrossOver.desktop', 'org.kde.krita.desktop', 'cxmenu-cxoffice-1b9804f8-f211-477e-a57d-05ab04a912c9-3ii0rqp-Adobe Photoshop CS2.desktop', 'gimp.desktop', 'tvp-animation-11.5-pro-demo.desktop', 'trimage.desktop', 'org.mapeditor.Tiled.desktop', 'artha.desktop', 'org.gnome.Characters.desktop', 'com.github.xournalpp.xournalpp.desktop', 'Write.desktop', 'com.github.tenderowl.frog.desktop', 'io.github.Qalculate.desktop', 'anki.desktop']"
```

# Limit window switcher carousel entries to current workspace.
gsettings get org.gnome.shell.app-switcher current-workspace-only &&
gsettings set org.gnome.shell.app-switcher current-workspace-only true

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

# Enable tap-to-click. The trackpad is too stiff.
gsettings get org.gnome.desktop.peripherals.touchpad tap-to-click && gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true

# Middle-clicking titlebar toggles vertical fill.
gsettings get org.gnome.desktop.wm.preferences action-middle-click-titlebar &&
gsettings set org.gnome.desktop.wm.preferences action-middle-click-titlebar action-middle-click-titlebar

# Set super-T to open terminal.
```

### Meld

```bash
gsettings get org.gnome.meld indent-width &&
gsettings set org.gnome.meld indent-width 2

gsettings get org.gnome.meld highlight-syntax &&
gsettings set org.gnome.meld highlight-syntax true
```

### MakeMKV

1. [Download the latest](https://www.makemkv.com/forum/viewtopic.php?f=3&t=224).
2. Install dependencies:

```
sudo apt install libbluray-dev libbluray2 libbluray-bdj libaacs0 libaacs-dev libbdplus0 libbdplus-dev
```

3. Install ffmpeg dependencies:

```
sudo apt install nasm yasm libfdk-aac2 libfdk-aac-dev
```

4. Follow the directions to install ffmpeg with libfdk-aac support to a temporary directory.
5. Follow the forum directions including installing the dependencies they specify (I couldn't get a prefix install to work).
6. Register via the GUI.

If it doesn't work, you probably have the disc number wrong.

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

AMDGPU SMU (i2c-12) caused a segfault in sensors-detect and scanning i2c-17 hung
sensors-detect so I skipped them.

#### Docker

```sh
sudo usermod -aG docker stephen
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
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-columns 96
gsettings get org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-rows &&
gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$profile/ default-size-rows 32
```

### Image Viewer (eog)

```bash
# Keep it pixelated.
gsettings get org.gnome.eog.view extrapolate &&
gsettings set org.gnome.eog.view extrapolate false
gsettings get org.gnome.eog.view interpolate &&
gsettings set org.gnome.eog.view interpolate false
```

### Grub

Set the sleep state to "Linux" / S3. I don't remember what else I changed but it
was a number of things, including, I think, enabling secure boot.

split_lock_detect=off https://github.com/ValveSoftware/steam-for-linux/issues/8003

Edit /etc/default/grub with the following
[temporary changes for keyboard lag](https://forums.lenovo.com/t5/ThinkPad-X-Series-Laptops/Lag-and-stuttering-on-X1-Carbon-Gen-9-while-running-Linux-untested-on-Windows/m-p/5082352):

```grub
GRUB_CMDLINE_LINUX_DEFAULT="i915.enable_psr=0"
```

### DeaDBeeF

- Enable GTK3 theme.
- Enable file browser plugin.

### Picard

### Calibre / Kindle

### Miscellaneous

- Add user icon.

```bash
# Enable location services.
gsettings get org.gnome.system.location enabled &&
gsettings set org.gnome.system.location enabled true

# Set lock screen delay to 10 min.
gsettings get org.gnome.desktop.session idle-delay &&
gsettings set org.gnome.desktop.session idle-delay 600

# Allow Bluetooth sleeping.

# Disable tracker-miner file indexer.
gsettings get org.freedesktop.Tracker.Miner.Files enable-monitors && gsettings set org.freedesktop.Tracker.Miner.Files enable-monitors false
gsettings get org.freedesktop.Tracker.Miner.Files crawling-interval && gsettings set org.freedesktop.Tracker.Miner.Files crawling-interval -2
echo y | LANG=en tracker reset --hard
```

## License (AGPLv3)

© Stephen Niedzielski.

### AGPL-3.0-only

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.
