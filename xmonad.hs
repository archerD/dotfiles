{-# LANGUAGE TupleSections #-} -- for the numpad bindings
{-# LANGUAGE MultiWayIf #-} -- for the ewmh fix

-- xmonad imports
import           XMonad
import qualified XMonad.StackSet                as W
-- xmonad-contrib imports
import           XMonad.Actions.CycleWS
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.MessageFeedback
                    ( sendSomeMessages, sm )
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.OnPropertyChange
import           XMonad.Hooks.RefocusLast (refocusLastLogHook)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.UrgencyHook
import qualified XMonad.Prelude                 as Pre
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.Dzen
import qualified XMonad.Util.ExtensibleState    as XS
import           XMonad.Util.EZConfig
                    ( additionalKeys, removeKeys
                    , checkKeymap, mkNamedKeymap )
import qualified XMonad.Util.Hacks              as Hacks
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows (getName)
import           XMonad.Util.Paste
--import           XMonad.Util.Run (runInTerm)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WorkspaceCompare
                    ( getWsIndex, getSortByIndex )
import           XMonad.Util.XUtils (fi)
--import           XMonad.Layout.LayoutBuilder
import           XMonad.Layout.LayoutScreens
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Reflect
--import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane
import           XMonad.Layout.TwoPanePersistent
-- xmonad-extra imports
import           XMonad.Actions.Volume

-- other imports
import           Control.Monad (void)
import qualified Data.Map                       as M
import qualified Data.Monoid                    as DM
import           Graphics.X11.ExtraTypes.XF86
import           System.IO

-- from https://wiki.haskell.org/Xmonad/Frequently_asked_questions#Some_keys_not_working, to enable numpad keys as number keys
extraWorkspaces = ["NSP"]
myWorkspaces = workspaces def ++ extraWorkspaces
-- Non-numeric num pad keys, sorted by number 
numberKeys = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" ]
numPadKeys = [ "<KP_End>",  "<KP_Down>",  "<KP_Page_Down>" -- 1, 2, 3
             , "<KP_Left>", "<KP_Begin>", "<KP_Right>"     -- 4, 5, 6
             , "<KP_Home>", "<KP_Up>",    "<KP_Page_Up>"   -- 7, 8, 9
             , "<KP_Insert>"]                            -- 0

myModMask = modMask def -- defaults to the alt key, mod1/3Mask.
-- myModMask = mod4Mask -- set the mod key to the super/windows key

defaultLauncher = spawn "rofi -show run"
secondaryLauncher = spawn "$(yeganesh -x -- -p \"y:\")"
tertiaryLauncher = spawn "dmenu_run -p \"$\""
calculatorLauncher = spawn "rofi -show calc -calc-command \"echo -n '{result}' | xclip -selection c\""

-- scratchpad stuff
scratchpads =
    [ NS "repl ipython" "kitty --class nsp-repl-ipython ipython"
                (className =? "nsp-repl-ipython") defaultFloating
    , NS "repl ghci" "kitty --class nsp-repl-ghci ghci"
                (className =? "nsp-repl-ghci") defaultFloating
    , NS "neovide notes" "neovide --x11-wm-class nsp-neovide-notes"
                (className =? "nsp-neovide-notes") defaultFloating
    , NS "cmus window" "kitty --class nsp-cmus cmus"
                (className =? "nsp-cmus") defaultFloating
    , NS "popup terminal" "kitty --class nsp-kitty"
                (className =? "nsp-kitty") defaultFloating
    ]

myKeysNamed :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeysNamed c =
    -- partially borrowed from https://gitlab.com/dwt1/dotfiles/-/blob/master/.xmonad/xmonad.hs
    -- TODO: add the default keybinds to this explicitly.
    let subKeys str ks = subtitle str : mkNamedKeymap c ks in
    subKeys "Custom Stuff"
    -- [ ("M4-l", addName "lock screen" $ spawn "xscreensaver-command -lock && sleep 2s ; xset dpms force off")
    -- , ("M4-S-l", addName "lock and suspend" $ spawn "xscreensaver-command -lock && systemctl suspend")
    [ ("M4-l", addName "lock screen" $ spawn "xset s activate")
    , ("M4-S-l", addName "lock and suspend" $ spawn "xset s activate && systemctl suspend")
    ] ^++^
    subKeys "Media Keys"
    [ ("<XF86AudioLowerVolume>", addName "Volume down" $ void (lowerVolume 2))
    , ("<XF86AudioRaiseVolume>", addName "Volume up" $ void (raiseVolume 2))
    , ("<XF86AudioMute>", addName "Mute" $ void toggleMute)
    , ("<XF86AudioPlay>", addName "Play/Pause" $ spawn "playerctl -p playerctld play-pause")
    , ("<XF86AudioPrev>", addName "Previous" $ spawn "playerctl -p playerctld previous")
    , ("<XF86AudioNext>", addName "Next" $ spawn "playerctl -p playerctld next")
    , ("<XF86AudioStop>", addName "Stop" $ spawn "playerctl -p playerctld stop")
    , ("M-m M-m", addName "Change focused player" $ spawn "playerctld shift")
    , ("M-m S-m", addName "Change focused player (reverse)" $ spawn "playerctld unshift")
    , ("M-m <Space>", addName "Play/Pause focused player" $ spawn "playerctl -p playerctld play-pause")
    , ("M-m n", addName "Next track focused player" $ spawn "playerctl -p playerctld next")
    , ("M-m p", addName "Previous track focused player" $ spawn "playerctl -p playerctld previous")
    , ("<Print>", addName "Screenshot" $ spawn "gnome-screenshot --interactive")
    ] ^++^
    subKeys "Launchers"
    [ ("M-u", addName "Open Launcher" defaultLauncher)
    , ("M-= M-=", addName "Open Mini Calculator" calculatorLauncher)
    , ("M-= p", addName "Open python repl" $ namedScratchpadAction scratchpads "repl ipython")
    , ("M-= h", addName "Open ghci repl" $ namedScratchpadAction scratchpads "repl ghci")
    , ("M-= n", addName "Open neovide for notes" $ namedScratchpadAction scratchpads "neovide notes")
    , ("M-= c", addName "Open cmus window" $ namedScratchpadAction scratchpads "cmus window")
    , ("M-= t", addName "Open popup terminal" $ namedScratchpadAction scratchpads "popup terminal")
    , ("M-M4-u", addName "Open adaptive cmd line launcher" secondaryLauncher)
    , ("M-S-u", addName "Open cmd line launcher" tertiaryLauncher)
    ] ^++^
    subKeys "Layout Modifications"
    [ ("M-f", addName "Go to fullscreen mode" $ sendSomeMessages [sm ToggleStruts, sm (Toggle NBFULL)])
    , ("M-S-f", addName "Toggle status line" $ sendMessage ToggleStruts)
    , ("M-r m", addName "rotation 90 degrees (mirror)" $ sendMessage $ Toggle MIRROR)
    , ("M-r h", addName "reflect horizontally" $ sendMessage $ Toggle REFLECTY)
    , ("M-r v", addName "reflect vertically" $ sendMessage $ Toggle REFLECTX)
    -- , ("M-S-m", addName "Focus master window" $ windows W.focusMaster) -- Move focus to the master window, changing from the default mod-m
    -- , ("M-S-f", addName "Send the forward keystroke" $ sendKey noModMask xF86XK_Forward)
    , ("M-S-h", addName "Go to previous workspace" prevWS)
    , ("M-S-l", addName "Go to next workspace" nextWS)
    ] ^++^
    subKeys "notification stuff"
    [ ("M-S-n", addName "Open notifications" $ spawn "dunstctl history-pop")
    , ("M-n", addName "Close notifications" $ spawn "dunstctl close")
    , ("M-C-n", addName "Open notification action menu" $ spawn "dunstctl context")
    ] ^++^
    subKeys "Managing wide monitors"
    [ ("M-C-<Space> M-C-3", addName "tri split screen" $ rescreen >> layoutSplitScreen 3 (ThreeColMid 1 0.1 0.56))
    , ("M-C-<Space> M-C-2", addName "dual split screen" $ rescreen >> layoutSplitScreen 2 (TwoPane 0.5 0.5))
    , ("M-C-<Space> M-C-1", addName "dual asymetric split screen" $ rescreen >> layoutSplitScreen 2 (TwoPane 0.25 0.75))
    , ("M-C-<Space> M-C-0", addName "no screen splits" rescreen)
    ]
    ^++^ subKeys "Numpad workspace management"
    -- set the numpad to be usable for workspace management
    (let shiftAndView i = W.greedyView i . W.shift i in
    [("M-" ++ m ++ k, addName (d ++ " workspace " ++ i) $ windows $ f i)
        | (i, k) <- zip (myWorkspaces ++ myWorkspaces) (numPadKeys ++ numberKeys)
        , (f, m, d) <- [(W.greedyView, "", "Change to"), (W.shift, "S-", "Move window to"), (shiftAndView, "C-", "Follow window to")]
    ])
    ^++^ subKeys "Screen management"
    -- add additional keybindings for moving between screens
    [("M-" ++ m ++ key, addName (d ++ " screen "  ++ show sc) $ screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ["v", "z"] [1..]
        , (f, m, d) <- [(W.view, "", "Change to"), (W.shift, "S-", "Move window to")]
    ]

myMouse XConfig {XMonad.modMask = myModMask} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((myModMask, leftClick), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Kill the window
    , ((myModMask, middleClick), \w -> focus w >> kill)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((myModMask, rightClick), \w -> focus w >> mouseResizeWindow w
                                        >> windows W.shiftMaster)

    -- mod-button4 (scroll up), Send the window back into the tiling
    , ((myModMask, scrollUp), \w -> focus w >> withFocused (windows . W.sink))
    -- mod-button4 (scroll down), Send the window back into the tiling
    , ((myModMask, scrollDown), \w -> focus w >> withFocused (windows . W.sink))

    -- mod-button8 (back button), send the forward key instead of the back key
    , ((myModMask, backButton), sendKeyWindow noModMask xF86XK_Forward)
    ]
  where
    leftClick     = button1
    middleClick   = button2
    rightClick    = button3
    scrollUp      = button4
    scrollDown    = button5
    scrollLeft    = 6 -- scroll left
    scrollRight   = 7 -- scroll right
    backButton    = 8 -- back button
    forwardButton = 9 -- forward button (currently not sent, because logiops redirects first)

myLayoutHook = avoidStruts
        . mkToggle1 NBFULL . mkToggle1 MIRROR . mkToggle1 REFLECTX . mkToggle1 REFLECTY
        $ tiled ||| trifold ||| TwoPanePersistent Nothing delta ratio
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

manageKdeconnectPresenterHook =
    composeAll
        [ (className =? "kdeconnect.daemon") --> doFullFloat
          -- ((\title -> title == "KDE Connect Daemon") <$> title) --> doFullFloat
        ]

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

myStartupHook :: X () -- TODO: add a check that the keymap is good
myStartupHook = fixSupportedAtoms
                -- >> setWMName "LG3D" -- the wm name somehow helps java gui applications to show menus in the correct spot...
                -- >> spawnOnce "export _JAVA_AWT_WM_NONREPARENTING=1" -- the (prefered) alternative is to run this export (I don't think this line works...)
                >> spawnOnce "~/.dotfiles/xlogin_script"
                >> spawn "feh --randomize --bg-fill ~/.dotfiles/images/wallpapers/"

myXmobarPP :: X PP
myXmobarPP = clickablePP myBaseXmobarPP -- clickablePP requires xdotool is installed

myBaseXmobarPP :: PP
myBaseXmobarPP = def
    { ppCurrent = yellow . wrap "[" "]"
    , ppTitle = green . padTo 60 . shorten 60
    , ppVisible = wrap "(" ")"
    , ppUrgent  = xmobarColor "red" "yellow"
    , ppTitleSanitize = xmobarStrip
    , ppRename = \workspaceName _ -> -- What is a WindowSpace, and what is it good for (the _ argument)
        if workspaceName == scratchpadWorkspaceTag
        then xmobarColor "white" "" "=" -- change how the scratchpad workspace is displayed
        else xmobarRaw workspaceName
    }
  where
    yellow = xmobarColor "yellow" ""
    green = xmobarColor "green" ""
    padTo n s = s ++ replicate (60 - length s) ' '

myUrgencyHandler =
        DunstUrgencyHook { arguments = [ "-i", "~/.dotfiles/images/xmonad-logo.svg" ] } -- TODO: find this file...
        -- (dzenUrgencyHook { duration = seconds 5, args = ["-bg", "darkgreen", "-xs", "1"]})
        -- Not to be obvious or anything, but the dzenUrgencyHook needs dzen2 installed

-- to display messages in a nice gui window
displayMessage :: String -> X ()
displayMessage message = spawn ("gxmessage -font \"JetBrains Mono Nerd Font 12\" -name \"XMonad Message\" -buttons \"Close:0\" -default Close \"" ++ message ++ "\"") -- uses gxmessage for a nicer display.

-- This changes the ewmh to use a greedy desktop change instead of a desktop change, see https://github.com/xmonad/xmonad-contrib/issues/776
ewmhGreedyDesktopChangeEventHookConfig :: XConfig a -> XConfig a
ewmhGreedyDesktopChangeEventHookConfig c = c { handleEventHook = ewmhGreedyDesktopChangeEventHook <> handleEventHook c }

-- A very stripped down version of the default ewmhDesktopsEventHook:
--  keeps what is needed to do a greedy view on desktop change
ewmhGreedyDesktopChangeEventHook :: Event -> X DM.All
ewmhGreedyDesktopChangeEventHook
        ClientMessageEvent{ev_window = w, ev_message_type = mt, ev_data = d} =
    withWindowSet $ \s -> do
        sort' <- getSortByIndex -- this is the default for the EwmhDesktopsConfig
        let ws = sort' $ W.workspaces s
        a_cd <- getAtom "_NET_CURRENT_DESKTOP"

        if  | mt == a_cd, n : _ <- d, Just ww <- ws Pre.!? fi n ->
                if W.currentTag s == W.tag ww then mempty else windows $ W.greedyView (W.tag ww)
            | mt == a_cd -> trace $ "Bad _NET_CURRENT_DESKTOP with data=" ++ show d
            | otherwise -> mempty
        mempty
ewmhGreedyDesktopChangeEventHook _ = mempty

-- should this be hooks instead?
configModifiers = docks . ewmhGreedyDesktopChangeEventHookConfig . ewmh
    . withEasySB (statusBarProp "xmobar" myXmobarPP) defToggleStrutsKey
    . addDescrKeys ((myModMask .|. shiftMask, xK_slash), addName "Show Keybindings" . displayMessage . unlines . showKm) myKeysNamed
    . withUrgencyHookC myUrgencyHandler
        (def {suppressWhen = Focused}) -- may want to make "Focused" "OnScreen" instead... or remove the config entirely

myConfig = configModifiers def
            { layoutHook = myLayoutHook
            , workspaces = myWorkspaces
            , modMask = myModMask
            , mouseBindings = myMouse
            , startupHook = myStartupHook
            -- I have no idea what isDialog works on...
            , manageHook = composeOne [ isDialog -?> doCenterFloat ] <+> manageZoomHook
                <+> manageKdeconnectPresenterHook <+> namedScratchpadManageHook scratchpads
                <+> ((className =? "kitty-overlay") --> doCenterFloat)
            , handleEventHook = Hacks.windowedFullscreenFixEventHook <+> onTitleChange manageZoomHook
            , logHook = refocusLastLogHook >> nsHideOnFocusLoss scratchpads
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

