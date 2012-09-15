import XMonad
import XMonad.Actions.NoBorders
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS

myModMask = mod4Mask
myKeys =
    [ ((myModMask, xK_g), withFocused toggleBorder) -- Toggle window focus border.
    , ((myModMask, xK_p), spawn "dmenu_run -i")
    , ((myModMask, xK_Left), prevWS)
    , ((myModMask, xK_Right), nextWS)
    , ((myModMask .|. shiftMask, xK_Left), shiftToPrev)
    , ((myModMask .|. shiftMask, xK_Right), shiftToNext)
    ]

main = xmonad $ gnomeConfig
    { modMask = myModMask -- Use super instead of alt.
    , layoutHook = smartBorders $ layoutHook gnomeConfig -- Auto border.
    , manageHook = composeAll
        [ manageHook gnomeConfig
        , isFullscreen --> doFullFloat -- When window is supposed to be
        ]                              -- full screen, make it so.
    } `additionalKeys` myKeys
