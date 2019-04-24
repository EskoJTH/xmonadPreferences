import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
import System.IO


--

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> manageHook defaultConfig
    , startupHook = setWMName "LG3D" --for java based graphical things to work.
    , layoutHook = avoidStruts $ mouseResizableTile ||| (ResizableTall 1 (5/100) (1/3) []) --Rezise to up and down too.
        ||| (avoidStruts $ layoutHook defaultConfig)
    , handleEventHook = handleEventHook defaultConfig <+> docksEventHook --The order is crucial for the xmobar actually shwowing correctly.
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" ""
                } >> updatePointer (0.5, 0.5) (0, 0) --Pointer to the center of focused window.

    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    }`additionalKeys`
    [ ((mod4Mask, xK_f), spawn "ulauncher")
    , ((mod4Mask .|. shiftMask, xK_o), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage ExpandSlave) --MirrorShrink)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage ShrinkSlave) --MirrorExpand)
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
    ]


--import XMonad
--
--main = do
--    xmonad $ defaultConfig
--        { borderWidth        = 2,
--            terminal           = "urxvt",
--            normalBorderColor  = "#ffff00",
--            focusedBorderColor = "#ff00ff" }
--


{-
[ Run Weather "EGPF" ["-t"," <tempF>F","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
-}
