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
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Util.Themes

--import Micro  
-- import  Control.Lens.Basic

import XMonad.Layout.DecorationMadness

--import XMonad
import XMonad.Util.XUtils (fi)
import Control.Arrow hiding ((|||), (<+>))
import Control.Monad
import XMonad.StackSet (member, peek, screenDetail, current)
import Data.Maybe
import Control.Exception
import XMonad.Layout.MultiColumns
import XMonad.Hooks.ManageHelpers

--See XMonad.Prompt for prompts
transTheme 
  (Theme
    activeColor
    inactiveColor
    urgentColor
    activeBorderColor
    inactiveBorderColor
    urgentBorderColor
    -- incorrectly here? -- activeBorderWidth
    -- incorrectly here? -- inactiveBorderWidth
    -- incorrectly here? -- urgentBorderWidth
    activeTextColor
    inactiveTextColor
    urgentTextColor
    fontName
    decoWidth
    decoHeight
    windowTitleAddons
    windowTitleIcons 
  )
     = (Theme
        "#000050" --activeColor
        "#400050" --inactiveColor
        "#ffffff" --urgentColor
        "#00ff00" --activeBorderColor
        "#000000" --inactiveBorderColor
        "#ff0000" --urgentBorderColor
        "#00ff00" --activeBorderColor
        "#00f000" --inactiveBorderColor
        "#ff0000" --urgentBorderColor
        -- incorrectly here? -- activeTextColor
        -- incorrectly here? -- inactiveTextColor
        -- incorrectly here? -- urgentTextColor
        fontName
        500        -- decoWidth
        decoHeight
        windowTitleAddons
        windowTitleIcons
       )
--transTheme = id

myManageHook = composeAll
    [className =? "zenity" --> doFloat,
    title =? "VLC media player" --> doFloat] --These float by default
    --doRectFloat

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]





main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ def
    { manageHook = myManageHook <+> manageDocks <+> manageHook def
    --, terminal = "kitty"
    , startupHook = setWMName "LG3D" --for java based graphical things to work.
    , layoutHook = avoidStruts ( mouseResizableTile ||| simpleDeco shrinkText (transTheme (theme kavonLakeTheme)) (multiCol [2] 4 0.01 0.4 )) ||| noBorders Full -- ||| mouseResizableTileMirrored
    , handleEventHook = handleEventHook def <+> docksEventHook --The order is crucial for the xmobar actually shwowing correctly.
    , logHook = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" ""
                } >> updatePointer (0.5, 0.5) (0, 0) --Pointer to the center of focused window.
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , keys          = \c -> mykeys c `M.union` keys def c
    , focusedBorderColor = "#00ff00"
    , normalBorderColor = "#000000"
    }`additionalKeys`[
      ((mod4Mask, xK_f), spawn "rofi -combi-modi  run,window,drun,ssh -font \"hack 10\" -show combi -run-shell-command \'{terminal} -e \\\\\"{cmd}; read -n 1 -s\"\'")
    , ((mod4Mask, xK_v), spawn clipmenu)
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
         ((modm, xK_u), updatePointer' (-0.002, 0.5) (0, 1)),

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




--Hahaa now it works everywhere!
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
  --These tests tell when not to center
  unless --(pointWithin (fi rootX) (fi rootY) rect
          (mouseIsMoving
          || isJust drag) $
  --        || not (currentWindow `member` ws || currentWindow == none)) $
   let
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









--- Well here comes the fucking Basic Lens I just cant get to be imported by stack for gods sake


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Basic lens type and functions.
--
-- The lens package should be a drop-in replacement for this.
--

-- module Basic
--   (-- * Lens type
--    Lens
--    -- * Functions
--   ,view
--   ,set
--   ,over
--   ,field)
--   where

import Control.Applicative
import Language.Haskell.TH

-- |
--
-- __Purpose__
--
-- A value of type @Lens s t a b@ provides the following:
--
-- * A reference into the structure @s@ to read and update the value @a@ inside it.
-- * The possibility to change the type of @s@ to @t@ and the type of @a@ to @b@.
--
-- __The @Functor@ constraint__
--
-- Operations may do something more interesting inside the `f`
-- functor. For the purpose of this module and package, all the
-- functions below ('view', 'over', 'set') use a no-op functor
-- and therefore the above type is equivalent to:
--
-- @type Lens s t a b = (a -> b) -> (s -> t)@
--
-- But it is left generic for forward compatibilty with the lens
-- package.
--
-- __Example__
--
-- @
-- λ> data Person = Person { personChar :: Char, personAge :: Int } deriving Show
-- λ> view $(field 'personChar) (Person 'a' 10)
-- 'a'
-- λ> over $(field 'personAge) (*2) (Person 'a' 10)
-- Person {personChar = 'a', personAge = 20}
-- λ>
-- @
--
-- __Laws__
--
-- 1) /Get-Put/: You get back what you put in.
--
-- @view l (set l v s) ≡ v@
--
-- 2) /Put-Get/: Putting back what you got doesn't change anything.
--
-- @set l (view l s) s ≡ s@
--
-- 3) /Put-Put/: Setting is idempotent.
--
-- @set l v (set l v s) ≡ set l v s@
--
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- Internal id functor.
newtype Id a = Id { runId :: a }

-- | Could use @DeriveFunctor@ here but that's not portable.
instance Functor Id where fmap f = Id . f . runId

-- | Get the @a@ inside the @s@.
view :: Lens s t a b -> s -> a
view l = getConst . l Const

-- | Modify the @a@ inside the @s@, optionally changing the types to
-- @b@ and @t@.
over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runId . l (Id . f)

-- | Set the @a@ inside the @s@, optionally changing the types to @b@
-- and @t@.
set :: Lens s t a b -> b -> s -> t
set l a = runId . l (Id . const a)

-- | Make a lens from a field name.
--
-- Example: @over $(field 'foo) (*2)@
field :: Name -> Q Exp
field name = do
  [|\f r ->
      fmap
        $(lamE
            [varP (mkName "a")]
            (recUpdE (varE (mkName "r")) [return (name, VarE (mkName "a"))]))
        (f ($(varE name) r))|]


