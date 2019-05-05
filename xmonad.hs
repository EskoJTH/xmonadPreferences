{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Map as M hiding (member, keys)
import qualified XMonad.StackSet as W


--import XMonad
import XMonad.Util.XUtils (fi)
import Control.Arrow hiding ((|||), (<+>))
import Control.Monad
import XMonad.StackSet (member, peek, screenDetail, current)
import Data.Maybe
import Control.Exception

myManageHook = composeAll
    [className =? "zenity" --> doFloat] --These float by default

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = myManageHook <+> manageDocks <+> manageHook def
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
      ((mod4Mask, xK_f), spawn "rofi -combi-modi window,drun,ssh -theme solarized -font \"hack 10\" -show combi")
    , ((mod4Mask .|. shiftMask, xK_o), spawn "xscreensaver-command -lock")
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage ExpandSlave)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage ShrinkSlave)
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
    , ((mod4Mask, xK_a), updatePointer' (0.5, 0.5) (0, 0))
    ]
  where
    mykeys (XConfig {modMask = modm}) = M.fromList $
      [ --((modm , xK_x), spawn "xlock")
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





updatePointer' :: (Rational, Rational) -> (Rational, Rational) -> X ()
updatePointer' refPos ratio = do
  ws <- gets windowset
  dpy <- asks display
  let defaultRect = screenRect $ screenDetail $ current ws
  rect <- case peek ws of
        Nothing -> return defaultRect
        Just w  -> do tryAttributes <- io $ try $ getWindowAttributes dpy w
                      return $ case tryAttributes of
                        Left (_ :: SomeException) -> defaultRect
                        Right attributes          -> windowAttributesToRectangle attributes
  root <- asks theRoot
  mouseIsMoving <- asks mouseFocused
  (_sameRoot,_,currentWindow,rootX,rootY,_,_,_) <- io $ queryPointer dpy root
  drag <- gets dragging
  unless (pointWithin (fi rootX) (fi rootY) rect
          || mouseIsMoving
          || isJust drag
          || not (currentWindow `member` ws || currentWindow == none)) $ let
    -- focused rectangle
    (rectX, rectY) = (rect_x &&& rect_y) rect
    (rectW, rectH) = (fi . rect_width &&& fi . rect_height) rect
    -- reference position, with (0,0) and (1,1) being top-left and bottom-right
    refX = lerp (fst refPos) rectX (rectX + rectW)
    refY = lerp (snd refPos) rectY (rectY + rectH)
    -- final pointer bounds, lerped *outwards* from reference position
    boundsX = join (***) (lerp (fst ratio) refX) (rectX, rectX + rectW)
    boundsY = join (***) (lerp (snd ratio) refY) (rectY, rectY + rectH)
    -- ideally we ought to move the pointer in a straight line towards the
    -- reference point until it is within the above bounds, but…
    in io $ warpPointer dpy none root 0 0 0 0
        (round . clip boundsX $ fi rootX)
        (round . clip boundsY $ fi rootY)

windowAttributesToRectangle :: WindowAttributes -> Rectangle
windowAttributesToRectangle wa = Rectangle (fi (wa_x wa))
                                           (fi (wa_y wa))
                                           (fi (wa_width wa + 2 * wa_border_width wa))
                                           (fi (wa_height wa + 2 * wa_border_width wa))

lerp :: (RealFrac r, Real a, Real b) => r -> a -> b -> r
lerp r a b = (1 - r) * realToFrac a + r * realToFrac b

clip :: Ord a => (a, a) -> a -> a
clip (lower, upper) x = if x < lower then lower
    else if x > upper then upper else x
