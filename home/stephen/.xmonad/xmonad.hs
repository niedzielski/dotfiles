import XMonad
import XMonad.Actions.CycleWS(prevWS, nextWS, shiftToPrev, shiftToNext)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Hooks.ICCCMFocus(takeTopFocus)
import XMonad.Hooks.ManageHelpers(doFullFloat, isFullscreen)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Hooks.DynamicLog(dynamicLog, xmobar)

main = xmonad =<< xmobar cfg
cfg = defaultConfig {
  modMask = mod4Mask,
  manageHook = composeAll [
    manageHook defaultConfig,
    isFullscreen --> doFullFloat],
  workspaces = ["☎", "☕", "☂", "♥", "☠", "⚅", "♕", "♫", "✎"],
  normalBorderColor  = "#333",
  focusedBorderColor = "#00fbff",
  borderWidth = 2,
  layoutHook = smartBorders -- don't show window focus on maximized windows
             $ layoutHook defaultConfig,
  logHook = dynamicLog -- xmobar
          <+> takeTopFocus -- android studio
} `additionalKeysP` [
  ("M-g", withFocused toggleBorder),
  ("M-<Left>", prevWS),
  ("M-<Right>", nextWS),
  ("M-S-<Left>", shiftToPrev),
  ("M-S-<Right>", shiftToNext),
  ("M-l", spawn "xscreensaver-command --lock"),
  ("M-r", spawn "dmenu_run"),
  ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%- && paplay .vol.wav"),
  ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+ && paplay .vol.wav"),
  ("<XF86AudioMute>", spawn "amixer set Master toggle"),
  ("<XF86MonBrightnessUp>", spawn "brightness +"),
  ("<XF86MonBrightnessDown>", spawn "brightness -")]