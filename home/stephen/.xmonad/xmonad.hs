import Control.Monad(when)
import System.Exit(exitSuccess)
import System.IO(hPutStrLn)
import XMonad((-->), (<+>), borderWidth, defaultConfig,
  focusedBorderColor, handleEventHook, io, layoutHook, logHook, manageHook,
  mod4Mask, modMask, normalBorderColor, spawn, withFocused, workspaces, X,
  xmonad)
import XMonad.Actions.CycleWS(nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, ppCurrent, ppHidden,
  ppHiddenNoWindows, ppLayout, ppOutput, ppSep, ppTitle, ppUrgent, xmobar,
  xmobarColor, xmobarPP)
import XMonad.Hooks.ICCCMFocus(takeTopFocus)
import XMonad.Hooks.ManageDocks(avoidStruts, docksEventHook, manageDocks,
  ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers(doFullFloat, isFullscreen)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Operations(sendMessage)
import XMonad.Util.Dmenu(dmenu)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)

quitPrompt :: X()
quitPrompt = do
  let m = "quit?"
  s <- dmenu [m]
  when (m == s) (io exitSuccess)

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ defaultConfig {
    modMask = mod4Mask,
    manageHook = manageDocks -- xmobar
               <+> (isFullscreen --> doFullFloat)
               <+> manageHook defaultConfig,
    workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"],
    normalBorderColor  = "#aaa",
    focusedBorderColor = "#0ff",
    borderWidth = 2,
    layoutHook = avoidStruts -- xmobar
               . smartBorders -- don't show window focus on maximized windows
               $ layoutHook defaultConfig,
    handleEventHook = docksEventHook -- xmobar
                    <+> handleEventHook defaultConfig,
    logHook = dynamicLogWithPP xmobarPP {
      ppOutput = hPutStrLn xmproc,
      ppTitle = xmobarColor "#ddd" "",
      ppCurrent = xmobarColor "#0ff" "",
      ppHidden = xmobarColor "#aaa" "",
      ppHiddenNoWindows = xmobarColor "#666" "",
      ppLayout = (\layout -> ""),
      ppUrgent = xmobarColor "#f00" "",
      ppSep = xmobarColor "#000" "" " "
    } <+> takeTopFocus -- android studio
  } `additionalKeysP` [
    ("M-g", withFocused toggleBorder),
    ("M-<Left>", prevWS),
    ("M-<Right>", nextWS),
    ("M-S-<Left>", shiftToPrev),
    ("M-S-<Right>", shiftToNext),
    ("M-S-l", spawn "xscreensaver-command --lock"),
    ("M-S-q", quitPrompt),
    ("M-r", spawn "dmenu_run_history"),
    ("M-<F3>", spawn "snap"),
    ("M-b", sendMessage ToggleStruts),
    ("<XF86MonBrightnessDown>", spawn "brightness -"),
    ("<XF86MonBrightnessUp>", spawn "brightness +"),
    ("<XF86AudioPrev>", spawn "deadbeef --prev"),
    ("<XF86AudioPlay>", spawn "deadbeef --play-pause"),
    ("<XF86AudioNext>", spawn "deadbeef --next"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- && paplay .vol.wav"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ && paplay .vol.wav")]