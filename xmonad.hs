{-# LANGUAGE ScopedTypeVariables #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
--import mouseResizeableTileClip
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.SetWMName
import Graphics.X11.ExtraTypes.XF86
import System.IO
import Data.Map as M hiding (member, keys)
import qualified XMonad.StackSet as W


--import XMonad
import XMonad.Util.XUtils (fi)
import Control.Arrow hiding ((|||), (<+>))
import Control.Monad
import XMonad.StackSet (member, peek, screenDetail, current)
import Data.Maybe
import Control.Exception

--See XMonad.Prompt for prompts

myManageHook = composeAll
    [className =? "zenity" --> doFloat] --These float by default

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = myManageHook <+> manageDocks <+> manageHook def
    , terminal = "kitty"
    , startupHook = setWMName "LG3D" --for java based graphical things to work.
    , layoutHook = avoidStruts $ mouseResizableTile ||| mouseResizableTileMirrored ||| Full
    , handleEventHook = handleEventHook def <+> docksEventHook --The order is crucial for the xmobar actually shwowing correctly.
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" ""
                } >> updatePointer (0.5, 0.5) (0, 0) --Pointer to the center of focused window.
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , keys          = \c -> mykeys c `M.union` keys def c
    }`additionalKeys`[
      ((mod4Mask, xK_f), spawn "rofi -combi-modi  run,window,drun,ssh -font \"hack 10\" -show combi -run-shell-command \'{terminal} -e \\\\\"{cmd}; read -n 1 -s\"\'")
    , ((mod4Mask .|. shiftMask, xK_o), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage ExpandSlave)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage ShrinkSlave)
--    , ((mod4Mask, xK_g), sendMessage MouseForceGrab)
    --, ((mod4Mask .|. shiftMask, xK_l), sendMessage ShrinkSlave)
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -3%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +3%")
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%") -- xrandr -q | grep " connected"
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%") -- xrandr --output eDP-1 --brightness 0.7
    ]--    ++
    --( [((mod4Mask, k), windows $ f i) -- Replace 'mod1Mask' with your mod key of choice.
     --     | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
     --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]))
    -- Don't swithc screens.
  where
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [
--         ((modm, xK_u), updatePointer' (-0.002, 0.5) (0, 1)),

        --((modm , xK_x), spawn "xlock")
        -- move focus up or down the window stack
        --, ((modm,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
        --, ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
         ((modm,               xK_n     ), windows W.focusDown) -- %! Move focus to the next window
        , ((modm,               xK_p     ), windows W.focusUp  ) -- %! Move focus to the previous window
        , ((modm,               xK_s     ), windows W.focusMaster  ) -- %! Move focus to the master window

        -- modifying the window order
        , ((modm .|. shiftMask, xK_s), windows W.swapMaster) -- %! Swap the focused window and the master window
        , ((modm .|. shiftMask, xK_n     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
        , ((modm .|. shiftMask, xK_p     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window
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
