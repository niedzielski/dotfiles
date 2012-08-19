import XMonad
import XMonad.Actions.NoBorders
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS

myKeys =
    [ ((mod4Mask, xK_g), withFocused toggleBorder) -- Toggle window focus border.
    , ((mod4Mask, xK_p), spawn "dmenu_run")
    , ((mod4Mask, xK_Left), prevWS)
    , ((mod4Mask, xK_Right), nextWS)
    , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev)
    , ((mod4Mask .|. shiftMask, xK_Right), shiftToNext)
    ]

main = xmonad $ gnomeConfig
    { modMask = mod4Mask -- Use super instead of alt.
    , layoutHook = smartBorders $ layoutHook gnomeConfig -- Auto border.
    , manageHook = composeAll
        [ manageHook gnomeConfig
        , isFullscreen --> doFullFloat -- When window is supposed to be
        ]                              -- full screen, make it so.
    } `additionalKeys` myKeys
