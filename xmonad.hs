{-# LANGUAGE TupleSections #-}
-- xmonad imports
import XMonad
import qualified XMonad.StackSet as W

-- xmonad-contrib imports
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Dzen
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Paste
import XMonad.Util.EZConfig(additionalKeys,removeKeys)
import XMonad.Util.EZConfig(checkKeymap,mkNamedKeymap)
import XMonad.Util.NamedActions -- for future use
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare(getWsIndex)
-- import XMonad.Util.Run(runInTerm)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import XMonad.Layout.TwoPanePersistent
-- import XMonad.Layout.Tabbed
-- import XMonad.Layout.LayoutBuilder

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
numPadKeys = [ "<KP_1>", "<KP_2>", "<KP_3>"
             , "<KP_4>", "<KP_5>", "<KP_6>"
             , "<KP_7>", "<KP_8>", "<KP_9>"
                       , "<KP_0>"]
numPadKeys2 = [ "<KP_End>",  "<KP_Down>",  "<KP_Page_Down>" -- 1, 2, 3
              , "<KP_Left>", "<KP_Begin>", "<KP_Right>"     -- 4, 5, 6
              , "<KP_Home>", "<KP_Up>",    "<KP_Page_Up>"   -- 7, 8, 9
              , "<KP_Insert>"]                            -- 0

myModMask = modMask def -- defaults to the alt key, mod1/3Mask.
-- myModMask = mod4Mask -- set the mod key to the super/windows key

defaultLauncher = spawn "rofi -show run"
secondaryLauncher = spawn "$(yeganesh -x -- -p \"y:\")"
tertiaryLauncher = spawn "dmenu_run -p \"$\""
calculatorLauncher = spawn "rofi -modi \"calc:~/.dotfiles/rofi-scripts/rofi-calc.sh\" -show calc"

myKeysNamed :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeysNamed c =
    -- partially borrowed from https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
    let subKeys str ks = subtitle str : mkNamedKeymap c ks in
    subKeys "Custom Stuff"
    [ ("M4-l", addName "lock screen" $ spawn "xscreensaver-command -lock && sleep 2s ; xset dpms force off")
    , ("M4-S-l", addName "lock and suspend" $ spawn "xscreensaver-command -lock && systemctl suspend")
    ] ^++^
    subKeys "Media Keys"
    [ ("<XF86AudioLowerVolume>", addName "Volume down" $ void (lowerVolume 4))
    , ("<XF86AudioRaiseVolume>", addName "Volume up" $ void (raiseVolume 4))
    , ("<XF86AudioMute>", addName "Mute" $ void toggleMute)
    , ("<XF86AudioPlay>", addName "Play/Pause" $ spawn "playerctl -p playerctld play-pause")
    , ("<XF86AudioPrev>", addName "Previous" $ spawn "playerctl -p playerctld previous")
    , ("<XF86AudioNext>", addName "Next" $ spawn "playerctl -p playerctld next")
    , ("<XF86AudioStop>", addName "Stop" $ spawn "playerctl -p playerctld stop")
    , ("M-r", addName "Shift player" $ spawn "playerctld shift")
    ] ^++^
    subKeys "Launchers"
    [
      ("M-u", addName "Open Launcher" defaultLauncher)
    , ("M-=", addName "Open Mini Calculator" calculatorLauncher)
    , ("M-M4-u", addName "Open learning cmd line launcher" secondaryLauncher)
    , ("M-S-u", addName "Open cmd line launcher" tertiaryLauncher)
    , ("M-f", addName "Toggle status line" $ sendMessage ToggleStruts)
    , ("<Print>", addName "Screenshot" $ spawn "gnome-screenshot --interactive")
    , ("M-S-m", addName "Focus master window" $ windows W.focusMaster) -- Move focus to the master window, changing from the default mod-m
    , ("M-S-f", addName "Send the forward keystroke" $ sendKey noModMask xF86XK_Forward)
    , ("M-S-h", addName "Go to previous workspace" prevWS)
    , ("M-S-l", addName "Go to next workspace" nextWS)
    -- notification stuff
    , ("M-n", addName "Open notifications" $ spawn "dunstctl history-pop")
    , ("M-S-n", addName "Close notifications" $ spawn "dunstctl close")
    , ("M-C-n", addName "Open notification action menu" $ spawn "dunstctl context")
    -- random thing to try out.
    , ("M-C-<Space>", addName "Split screen" $ layoutSplitScreen 2 (TwoPane 0.5 0.5))
    , ("M-C-S-<Space>", addName "Reset screen splits" rescreen)
    ]
    ^++^ subKeys "Numpad workspace management"
    -- set the numpad to be usable for workspace management
    [("M-" ++ m ++ k, addName (d ++ " workspace " ++ i) $ windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys2
        , (f, m, d) <- [(W.greedyView, "", "Change to"), (W.shift, "S-", "Move window to")]
    ]
    ^++^ subKeys "Screen management"
    -- add additional keybindings for moving between screens
    [("M-" ++ m ++ key, addName (d ++ " screen "  ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["v", "z"] [1..]
        , (f, m, d) <- [(W.view, "", "Change to"), (W.shift, "S-", "Move window to")]
    ]
    -- TODO: add a bind (myModMask + '=' maybe) to open a terminal with some some repl (ipython or ghci probably) to do basic calculations.

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
    ||| TwoPanePersistent Nothing delta ratio
    where
        -- base layouts
        tiled   = smartBorders $ Tall nmaster delta ratio
        trifold = smartBorders $ ThreeColMid nmaster delta ratio
        -- someLayout = ( (layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) tiled ) $ layoutAll (relBox 0.5 0 1 1) $ simpleTabbed )
        -- settings
        nmaster = 1
        ratio   = 1/2
        delta   = 2/100

-- float the zoom popup windows
-- from https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
manageZoomHook =
  composeAll
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat
    -- , (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    -- shouldSink title = title `elem` tileTitles
    -- doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

newtype DunstUrgencyHook = DunstUrgencyHook { arguments :: [String] }
    deriving (Read, Show)

instance UrgencyHook DunstUrgencyHook where
    urgencyHook DunstUrgencyHook { arguments = a } w = do
        name <- getName w
        ws <- gets windowset
        wsIndexOf <- getWsIndex
        let workspace = W.findTag w ws >>= \n -> fmap (n,) (wsIndexOf n)
        whenJust workspace (flash name)
      where flash name (workspaceName, workspaceIndex) =
              -- consider some abstraction around dunstify (notify-send?) for sending notifications
              spawn $ "$(dunstify --appname=xmonad 'Urgency Notification' --action='xdotool set_desktop " ++ show workspaceIndex ++ ",goto workspace' " ++ unwords a ++ " '" ++ show name ++ " requests your attention on workspace " ++ workspaceName ++ "')"

-- enables kitty alerts to be caught by the urgency hook
-- from: https://github.com/kovidgoyal/kitty/issues/1016#issuecomment-480472827
fixSupportedAtoms :: X ()
fixSupportedAtoms = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom [ "_NET_WM_STATE"
                         , "_NET_WM_STATE_DEMANDS_ATTENTION"
                         ]
    io $ changeProperty32 dpy r a c propModeAppend (fmap fromIntegral supp)

myStartupHook :: X ()
myStartupHook = fixSupportedAtoms >> spawn "feh --randomize --bg-fill ~/.dotfiles/images/"

myXmobarPP :: X PP
myXmobarPP = clickablePP myBaseXmobarPP -- clickablePP requires xdotool is installed
-- myXmobarPP = pure myBaseXmobarPP

myBaseXmobarPP :: PP
myBaseXmobarPP = def
    { ppCurrent = yellow . wrap "[" "]"
    , ppTitle = green . shorten 60
    , ppVisible = wrap "(" ")"
    , ppUrgent  = xmobarColor "red" "yellow"
    , ppTitleSanitize = xmobarStrip
    , ppRename = \workspaceName _ -> xmobarRaw workspaceName -- What is a WindowSpace, and what is it good for (the _ argument)
    }
  where
    yellow = xmobarColor "yellow" ""
    green = xmobarColor "green" ""

myUrgencyHandler =
        DunstUrgencyHook { arguments = [ "-i", "~/.dotfiles/images/xmonad-logo.svg" ] }
        -- (dzenUrgencyHook { duration = seconds 5, args = ["-bg", "darkgreen", "-xs", "1"]})
        -- Not to be obvious or anything, but the dzenUrgencyHook needs dzen2 installed

-- should this be hooks instead?
configModifiers = docks . ewmh
    . withEasySB (statusBarProp "xmobar" myXmobarPP) defToggleStrutsKey
    . addDescrKeys ((myModMask, xK_r), xMessage) myKeysNamed
    . withUrgencyHookC myUrgencyHandler
        (urgencyConfig {suppressWhen = Focused}) -- may want to make "Focused" "OnScreen" instead... or remove the config entirely

myConfig = configModifiers def 
            { layoutHook = myLayoutHook
            , workspaces = myWorkspaces
            , modMask = myModMask
            , mouseBindings = myMouse
            , startupHook = myStartupHook
            -- I have no idea what isDialog works on...
            , manageHook = composeOne [ isDialog -?> doCenterFloat ] <+> manageZoomHook
            , handleEventHook = Hacks.windowedFullscreenFixEventHook <+> dynamicTitle manageZoomHook
            -- , terminal = "x-terminal-emulator"
            , terminal = "kitty"
            , focusFollowsMouse = True
            , borderWidth = 2
            -- , focusedBorderColor = "#ff0000"
            -- , normalBorderColor = "#dddddd"
            -- , focusedBorderColor = "#502c57"
            -- , normalBorderColor = "#33572c"
            -- , focusedBorderColor = "#680a7b"
            -- , normalBorderColor = "#1d7b0a"
            , focusedBorderColor = "#772388"
            , normalBorderColor = "#348823"
            }
            -- `removeKeys` [ (myModMask, xK_t)
            --              , (myModMask, xK_m)
            --              , (myModMask, xK_p)
            --              ]
            -- `additionalKeys` myKeys


main = do xmonad myConfig

