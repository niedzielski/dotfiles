import XMonad
import XMonad.Actions.NoBorders
import XMonad.Config.Gnome
import XMonad.Util.EZConfig(additionalKeys)

myKeys =
    [ ((mod4Mask, xK_g), withFocused toggleBorder) -- Toggle window focus border.
    ]

main = do
    xmonad $ gnomeConfig {
        modMask = mod4Mask -- Use super instead of alt.
    } `additionalKeys` myKeys
