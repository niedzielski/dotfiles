------------------------------------------------------------------------------
-- .xmonad.hs
-- Copyright 2012 Stephen Niedzielski. Licensed under GPLv3.

-- Imports
--import System.IO
import XMonad
import XMonad.Actions.CycleWS(prevWS, nextWS, shiftToPrev, shiftToNext)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Config.Gnome(gnomeConfig)
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops(ewmh) -- For touchegg.
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import XMonad.Layout.Gaps
--import XMonad.Layout.LayoutHints -- For gVim.
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Spacing(spacing)
import XMonad.Util.EZConfig(additionalKeys)
--import XMonad.Util.Loggers -- (logCmd)
--import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus ( takeTopFocus )
import qualified XMonad.Actions.ConstrainedResize as Sqr
import qualified Data.Map as M
--import Graphics.X11.Xinerama

-- Color Scheme
-- TODO: set in Xsession.
myFFGColor = "#d4d4d4"
myFBGColor = "#2c2c2c"

myDzenFgRgb = myFFGColor
myDzenBgRgb = myFBGColor

myVFGColor = myFBGColor
myVBGColor = myDzenBgRgb
myHFGColor = "#999999"
myUFGColor = "#780000" -- Urgent
myUBGColor = "#ffffff"
myIFGColor = "#00fa9a" -- Icon
myIBGColor = myDzenBgRgb
mySColor = "#171717" -- Seperator
myBorder = "#110E17"
myFocusedBorder = "#780000"
myIconDir = "/home/scott/.dzen/dzenIcons/"
mynormalBorderColor  = "#333333" -- "#6699cc"
myfocusedBorderColor = "#00fbff" -- myfocusedBorderColor = "#ff0000"

myborderwidth = 1
mymcspacing = 2
myDockHeight = 0 :: Int
myfont = "-*-bitstream vera sans-medium-r-normal-*-9-*-*-*-*-*-*-*"

-- Key Chords
modm = mod4Mask
myKeys =
    [ ((modm              , xK_g    ), withFocused toggleBorder)
    , ((modm              , xK_r    ), spawn "dmenu_run -i -nb \\#cccccc -nf \\#000000 -sb \\#0066ff -sf \\#ffffff") -- "dmenu_run -i -fn 'Bitstream Vera Sans Mono-8' -h 19 -nb \\#d4d4d4 -nf \\#000000 -sb \\#000000 -sf \\#00fbff"
    , ((modm              , xK_q    ), spawn "xmonad --recompile && xmonad --restart") -- killall dzen2; 
    , ((modm              , xK_Left ), prevWS)
    , ((modm              , xK_Right), nextWS)
    , ((modm .|. shiftMask, xK_Left ), shiftToPrev)
    , ((modm .|. shiftMask, xK_Right), shiftToNext)
    --, ((modm, xK_`), swapNextScreen)
    ]

myMouse x  = [ ((modm .|. shiftMask, button3), (\w -> focus w >> Sqr.mouseResizeWindow w True )) ]

newMouse x = M.union (mouseBindings defaultConfig x) (M.fromList (myMouse x))

-- Workspaces
myWorkspaces = ["www","dev","ref","4","vm","6","7","fm","jot"]


-- TODO: get from WM.
-- myScreenWidth=1440
-- myScreenHeight = 900

-- Dock Bar
--myDock = "dzen2 -x 0 -y 0 -ta l"
--     ++ " -w '"  ++ (show $ myScreenWidth) ++ "'"
--     ++ " -h '"  ++ (show $ myDockHeight)  ++ "'"
--     ++ " -bg '" ++ myFBGColor                   ++ "'"
--     ++ " -fg '" ++ myDzenFgRgb             ++ "'"
--     ++ " -fn '" ++ myfont                 ++ "'"

-- myDzenPP h = defaultPP
--      {  ppCurrent         = dzenColor myFFGColor myFBGColor . wrap ("^fg(" ++ myIFGColor ++ ")") ""
--       , ppVisible         = dzenColor myVFGColor myVBGColor . wrap ("^fg(" ++ myVFGColor ++ ")") ("^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/eye_r.xbm)")
--       , ppHidden          = dzenColor myDzenFgRgb myDzenBgRgb . wrap ("^i(" ++ myIconDir ++ "/dzen_bitmaps/has_win.xbm)") ""
--       , ppHiddenNoWindows = dzenColor myHFGColor myDzenBgRgb . wrap ("^i(" ++ myIconDir ++ "/dzen_bitmaps/has_win_nv.xbm)") ""
--       , ppUrgent          = dzenColor myUFGColor myUBGColor . wrap ("^i(" ++ myIconDir ++ "/info_03.xbm)") "" . dzenStrip
--       , ppTitle           = dzenColor myDzenFgRgb myDzenBgRgb . shorten 80 . wrap ("^p(_CENTER)" ++ "^p()") ""
--       , ppLayout          = (\x -> case x of
--                             otherwise -> ""
--                             _ -> x
--                             )
--       , ppSep             = "" -- " | "
--       , ppWsSep             =   " "
--       , ppExtras = [ date "^p(_RIGHT)%a, %b %d %I:%M^p()" ]
--       , ppOutput          = hPutStrLn h }

-- Fade inactive windows.
--myLogHook = fadeInactiveLogHook fadeAmount
--    where fadeAmount = 0.90

-- Main
main = do
    -- dzenizen <- spawnPipe myDock
    xmonad $ ewmh gnomeConfig
        { modMask            = modm
        , workspaces         = myWorkspaces
        , layoutHook         = smartBorders $ gaps [(U,myDockHeight)] $ layoutHook gnomeConfig -- $ spacing mymcspacing $ layoutHintsToCenter
        , normalBorderColor  = mynormalBorderColor
        , focusedBorderColor = myfocusedBorderColor
        , borderWidth        = myborderwidth
        --, logHook            = myLogHook -- >> (dynamicLogWithPP $ myDzenPP dzenizen)
        , manageHook         = composeAll
            [ manageHook gnomeConfig
            , isFullscreen --> doFullFloat]
  --      , handleEventHook    = hintsEventHook
        , startupHook = setWMName "LG3D"
, mouseBindings = newMouse
  , logHook = takeTopFocus

        } `additionalKeys` myKeys
