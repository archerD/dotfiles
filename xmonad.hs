-- xmonad imports
import XMonad
import qualified XMonad.StackSet as W

-- xmonad-contrib imports
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Paste
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys,removeKeys)
import XMonad.Config.Gnome
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders

-- xmonad-extra imports
import XMonad.Actions.Volume

-- other imports
import Control.Monad(void)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.IO

-- from https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Some_keys_not_working, to enable numpad keys as number keys
extraWorkspaces = ["0"]
myWorkspaces = workspaces def ++ extraWorkspaces
-- Non-numeric num pad keys, sorted by number 
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]                            -- 0

myModMask = modMask def -- defaults to the alt key, mod3Mask.
-- myModMask = mod4Mask -- set the mod key to the super/windows key

defaultLauncher = spawn "rofi -show run"
secondaryLauncher = spawn "$(yeganesh -x -- -p \"y:\")"
tertiaryLauncher = spawn "dmenu_run -p \"$\""

myKeys =
    [ ((mod4Mask, xK_l), spawn "xscreensaver-command -lock && xset dpms force off")
    , ((0, xF86XK_AudioLowerVolume), void (lowerVolume 4))
    , ((0, xF86XK_AudioRaiseVolume), void (raiseVolume 4))
    , ((0, xF86XK_AudioMute), void toggleMute)
    , ((0, xF86XK_AudioPlay), spawn "playerctl -p playerctld play-pause")
    , ((0, xF86XK_AudioPrev), spawn "playerctl -p playerctld previous")
    , ((0, xF86XK_AudioNext), spawn "playerctl -p playerctld next")
    , ((0, xF86XK_AudioStop), spawn "playerctl -p playerctld stop")
    , ((myModMask, xK_r), spawn "playerctld shift")
    , ((myModMask, xK_u), defaultLauncher)
    , ((myModMask .|. mod4Mask, xK_u), secondaryLauncher)
    , ((myModMask .|. shiftMask, xK_u), tertiaryLauncher)
    , ((myModMask, xK_f), sendMessage ToggleStruts)
    , ((0, xK_Print), spawn "gnome-screenshot --interactive")
    , ((myModMask .|. shiftMask, xK_m), windows W.focusMaster) -- Move focus to the master window, changing from the default mod-m
    , ((myModMask .|. shiftMask, xK_f), sendKey noModMask xF86XK_Forward)
    , ((myModMask .|. shiftMask, xK_h), prevWS)
    , ((myModMask .|. shiftMask, xK_l), nextWS)
    -- notification stuff
    , ((myModMask, xK_n), spawn "dunstctl history-pop")
    , ((myModMask .|. shiftMask, xK_n), spawn "dunstctl close")
    , ((myModMask .|. controlMask, xK_n), spawn "dunstctl context")
    ]
    -- TODO: add a bind (myModMask + '=' maybe) to open a terminal with some some repl (ipython or ghci probably) to do basic calculations.
    ++
    -- set the numpad to be usable for workspace management
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip extraWorkspaces [xK_0] ++ zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++
    -- add additional keybindings for moving between screens
    [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_v, xK_z] [1..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myMouse XConfig {XMonad.modMask = myModMask} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((myModMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Kill the window
    , ((myModMask, button2), \w -> focus w >> kill)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((myModMask, button3), \w -> focus w >> mouseResizeWindow w
                                        >> windows W.shiftMaster)

    -- mod-button4 (scroll up), Send the window back into the tiling
    , ((myModMask, button4), \w -> focus w >> withFocused (windows . W.sink))
    -- mod-button4 (scroll down), Send the window back into the tiling
    , ((myModMask, button5), \w -> focus w >> withFocused (windows . W.sink))

    -- mod-button8 (back button), send the forward key instead of the back key
    , ((myModMask, button8), sendKeyWindow noModMask xF86XK_Forward)
    ]
  where
    button6 = 6 -- scroll left
    button7 = 7 -- scroll right
    button8 = 8 -- back button
    button9 = 9 -- forward button (currently not sent, because logiops redirects first)

myLayoutHook = avoidStruts $ tiled ||| Mirror tiled ||| trifold ||| noBorders Full
    where
        -- base layouts
        tiled   = smartBorders $ Tall nmaster delta ratio
        trifold = smartBorders $ ThreeColMid nmaster delta ratio
        -- settings
        nmaster = 1
        ratio   = 1/2
        delta   = 2/100

-- should this be hooks instead?
configModifiers :: XConfig a -> XConfig a
configModifiers = docks . ewmh

main = do xmproc <- spawnPipe "xmobar"
          xmonad $ configModifiers def
            { layoutHook = myLayoutHook
            , logHook = dynamicLogWithPP xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor "green" "" . shorten 50
                            }
            , workspaces = myWorkspaces
            , modMask = myModMask
            , mouseBindings = myMouse
            -- , terminal = "x-terminal-emulator"
            , terminal = "kitty"
            }
            `removeKeys` [ (myModMask, xK_t)
                         , (myModMask, xK_m)
                         , (myModMask, xK_p)
                         ]
            `additionalKeys` myKeys


