import XMonad
import XMonad.Actions.NoBorders
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

myKeys =
    [ ((mod4Mask, xK_g), withFocused toggleBorder) -- Toggle window focus border.
    , ((mod4Mask, xK_p), spawn "dmenu_run")
    ]

main = xmonad $ gnomeConfig
    { modMask = mod4Mask -- Use super instead of alt.
    , layoutHook = smartBorders $ layoutHook gnomeConfig -- Auto border.
    , manageHook = composeAll
        [ manageHook gnomeConfig
        , isFullscreen --> doFullFloat -- When window is supposed to be
        ]                              -- full screen, make it so.
    } `additionalKeys` myKeys
