------------------------------------------------------------------------------
-- .xmonad.hs
-- Copyright 2012 Stephen Niedzielski. Licensed under GPLv3.

-- Imports
import System.IO
import XMonad
import XMonad.Actions.CycleWS(prevWS, nextWS, shiftToPrev, shiftToNext)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Config.Gnome(gnomeConfig)
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops(ewmh) -- For touchegg.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
--import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints -- For gVim.
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Spacing(spacing)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers -- (logCmd)
import XMonad.Util.Run(spawnPipe)


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
mynormalBorderColor  = "#6699cc"
myfocusedBorderColor = "#00fbff"

myborderwidth = 1
mymcspacing = 4
--myDockHeight = 14 :: Int
myfont = "-*-bitstream vera sans-medium-r-normal-*-9-*-*-*-*-*-*-*"

-- Key Chords
myModMask = mod4Mask
myKeys =
    [ ((myModMask              , xK_g    ), withFocused toggleBorder)
    , ((myModMask              , xK_p    ), spawn "dmenu_run -i -fn 'Bitstream Vera Sans Mono-8' -h 19 -nb \\#d4d4d4 -nf \\#000000 -sb \\#000000 -sf \\#00fbff")
    , ((myModMask              , xK_q    ), spawn "xmonad --recompile && xmonad --restart") -- killall dzen2; 
    , ((myModMask              , xK_Left ), prevWS)
    , ((myModMask              , xK_Right), nextWS)
    , ((myModMask .|. shiftMask, xK_Left ), shiftToPrev)
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext)
    ]

-- Workspaces
myWorkspaces = ["www","dev","ref","vm","5","6","7","fm","jot"]


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
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.95

-- Main
main = do
    -- dzenizen <- spawnPipe myDock
    xmonad $ ewmh gnomeConfig
        { modMask            = myModMask
        , workspaces         = myWorkspaces
        , layoutHook         = spacing mymcspacing $ smartBorders $ layoutHintsToCenter $ layoutHook gnomeConfig -- $ gaps [(U,myDockHeight)] 
        , normalBorderColor  = mynormalBorderColor
        , focusedBorderColor = myfocusedBorderColor
        , borderWidth        = myborderwidth
        , logHook            = myLogHook -- >> (dynamicLogWithPP $ myDzenPP dzenizen)
        , manageHook         = composeAll
            [ manageHook gnomeConfig
            , isFullscreen --> doFullFloat]
        , handleEventHook    = hintsEventHook
        } `additionalKeys` myKeys
