import XMonad
import XMonad.Actions.CycleWS(prevWS, nextWS, shiftToPrev, shiftToNext)
import XMonad.Actions.NoBorders(toggleBorder)
import XMonad.Config.Xfce(xfceConfig)
import XMonad.Hooks.ICCCMFocus(takeTopFocus)
import XMonad.Hooks.ManageHelpers(isFullscreen, doFullFloat)
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Spacing(spacing)
import XMonad.Util.EZConfig(additionalKeys)

myBorder = "#110E17"
myFocusedBorder = "#780000"
mynormalBorderColor  = "#333333"
myborderwidth = 1
myfocusedBorderColor = "#00fbff"

myWork = ["www","dev","3","4","5","6","7","8","jot"]

-- Key Chords
modKey = mod4Mask
keys =
  [ ((modKey              , xK_g    ), withFocused toggleBorder)
  , ((modKey              , xK_r    ), spawn "dmenu_run -i -fn DejaVuSansMono-9 -nb \\#d4d4d4 -nf \\#000000 -sb \\#000000 -sf \\#00fbff")
  , ((modKey              , xK_q    ), spawn "xmonad --recompile && xmonad --restart && killall xfce4-panel")
  , ((modKey              , xK_Left ), prevWS)
  , ((modKey              , xK_Right), nextWS)
  , ((modKey .|. shiftMask, xK_Left ), shiftToPrev)
  , ((modKey .|. shiftMask, xK_Right), shiftToNext)
  ]

main = do xmonad $ xfceConfig
            { modMask = modKey
            -- , terminal = "gnome-terminal"
                        , workspaces = myWork
                        , normalBorderColor  = mynormalBorderColor
                        , focusedBorderColor = myfocusedBorderColor
                        , borderWidth        = myborderwidth
            , layoutHook = smartBorders $ layoutHook xfceConfig -- $ spacing 4
            , logHook = takeTopFocus -- necessary for Android Studio
            , manageHook = composeAll
                [ manageHook xfceConfig
                , isFullscreen --> doFullFloat]
            } `additionalKeys` Main.keys
