{-# LANGUAGE GHC2021 #-}
-- Cool language extensions \o/
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad (filterM)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import XMonad
import XMonad.Actions.EasyMotion
import XMonad.Actions.GridSelect
import XMonad.Actions.Prefix
import XMonad.Actions.Search
import XMonad.Actions.WindowBringer (windowMap)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Rescreen (RescreenConfig (afterRescreenHook), rescreenHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
-- import XMonad.Hooks.WallpaperSetter
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout
import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.StackSet qualified as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Font
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
myFont :: String
myFont = "xft:Iosevka:regular:size=12:antialias=true:hinting=true"

myScratchpads =
  [ NS "emacs" "emacsclient -c -a '' org/schizo.org" (role =? "notes") (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)),
    NS "SysResources" "st -e btop" (title =? "btop") defaultFloating
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      fromList
        [ ((0, xK_Escape), cancel),
          ((0, xK_Return), select),
          ((0, xK_slash), substringSearch myNavigation),
          ((0, xK_Left), move (-1, 0) >> myNavigation),
          ((0, xK_h), move (-1, 0) >> myNavigation),
          ((0, xK_Right), move (1, 0) >> myNavigation),
          ((0, xK_l), move (1, 0) >> myNavigation),
          ((0, xK_Down), move (0, 1) >> myNavigation),
          ((0, xK_j), move (0, 1) >> myNavigation),
          ((0, xK_Up), move (0, -1) >> myNavigation),
          ((0, xK_k), move (0, -1) >> myNavigation),
          ((0, xK_y), move (-1, -1) >> myNavigation),
          ((0, xK_i), move (1, -1) >> myNavigation),
          ((0, xK_n), move (-1, 1) >> myNavigation),
          ((0, xK_m), move (1, 1) >> myNavigation),
          ((0, xK_space), setPos (0, 0) >> myNavigation),
          ((0, xK_Tab), moveNext >> myNavigation),
          ((shiftMask, xK_Tab), movePrev >> myNavigation)
        ]
    navDefaultHandler = const myNavigation

myColorizer :: a -> Bool -> X (String, String)
myColorizer _ isSelected =
  if isSelected
    then pure ("#b4befe", "#11111b")
    else pure ("#181825", "#cdd6f4")

myGsConfig :: GSConfig String
myGsConfig =
  def
    { gs_cellheight = 40,
      gs_cellwidth = 180,
      gs_cellpadding = 6,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_navigate = myNavigation,
      gs_font = myFont,
      gs_colorizer = myColorizer
    }

myGsConfigWindow :: GSConfig Window
myGsConfigWindow =
  def
    { gs_cellheight = 40,
      gs_cellwidth = 180,
      gs_cellpadding = 6,
      gs_originFractX = 0.5,
      gs_originFractY = 0.5,
      gs_navigate = myNavigation,
      gs_font = myFont,
      gs_colorizer = myColorizer -- Use a default colorizer for Windows
    }

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = myFont,
      bgColor = "#11111b",
      fgColor = "#b4befe",
      historySize = 0,
      position = Top
    }

myEasyMotionConfig :: EasyMotionConfig
myEasyMotionConfig =
  def
    { txtCol = "#b4befe",
      bgCol = "#11111b",
      emFont = "xft:Iosevka:regular:size=52:antialias=true:hinting=true",
      cancelKey = xK_f
    }

myStartupHook = do
  spawnOnce "emacs --daemon"
  setDefaultCursor xC_coffee_mug
  spawnOnce "timeout 3 ~/.config/xmonad/wallpaper-changer"

myBrowserSelector :: PrefixArgument -> X ()
myBrowserSelector = \case
  Raw 1 -> promptSearch myXPConfig duckduckgo
  Numeric 1 -> promptSearch myXPConfig wikipedia
  Numeric 2 -> promptSearch myXPConfig phpsearch
  Numeric 3 -> promptSearch myXPConfig hoogle
  Numeric 4 -> promptSearch myXPConfig voidpgks_x86_64
  Numeric 5 -> promptSearch myXPConfig youtube
  Numeric 6 -> promptSearch myXPConfig images
  Numeric 7 -> promptSearch myXPConfig raesearch
  Raw _ -> promptSearch myXPConfig duckduckgo
  where
    phpsearch = searchEngine "php" "https://www.php.net/manual-lookup.php?pattern="
    raesearch = searchEngine "RAE" "https://dle.rae.es/"

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect myGsConfig lst >>= flip whenJust spawn

goToSelected' :: X ()
goToSelected' = goToSelected myGsConfigWindow

runSelectedAction' :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction' conf actions = do
  selectedActionM <- gridselect conf actions
  case selectedActionM of
    Just selectedAction -> selectedAction
    Nothing -> return ()

gsPrograms =
  [ ("Librewolf", "librewolf"),
    ("ST", "st"),
    ("BTOP", "st -e btop"),
    ("NVIM", "st -e nvim"),
    ("CuteBrowser", "qutebrowser"),
    ("Audio Manage", "pavucontrol"),
    ("Thunar", "thunar"),
    ("Meld", "meld"),
    ("Steam", "steam"),
    ("Xournal", "xournalpp"),
    ("VLC", "vlc"),
    ("qTorrent", "qbittorrent"),
    ("Emacs", "emacsclient -c ")
  ]

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . usePrefixArgument "M-u"
    . rescreenHook def {afterRescreenHook = onMonitorChange}
    . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
  where
    onMonitorChange :: X ()
    onMonitorChange = do
      spawn " wallpaper-changer" -- make wallpaper pretty

myWorkspaces =
  [ "1:Main",
    "2:Code",
    "3:Schizo",
    "4:logs",
    "5:",
    "6:",
    "7:",
    "8:",
    "9:VM"
  ]

myConfig =
  def
    { modMask = mod4Mask, -- Rebind Mod to the Super key
      startupHook = myStartupHook,
      layoutHook = myLayout, -- Use custom layouts
      manageHook = myManageHook, -- Match on certain windows
      normalBorderColor = "#1a1b26",
      workspaces = myWorkspaces,
      focusedBorderColor = "#c7a9ff",
      borderWidth = 0,
      handleEventHook = swallowEventHook (className =? "st-256color") (return True),
      terminal = "st"
    }
    `additionalKeysP` [
                        -- Modify keybindings:
                        ("M-<Return>", spawn "st"),
                        ("M-q", kill), -- Super+Shift+Q to kill a window
                        ("M-S-<Return>", spawn "rofi -show drun"),
                        ("M-S-c", spawn "xmonad --recompile; xmonad --restart"),
                        ("<F6>", spawn "xbacklight -inc 5"),
                        ("<F7>", spawn "xbacklight -dec 5"),
                        ("M-o", spawnSelected' gsPrograms),
                        ("M-y", unGrab >> spawn "maim -s |xclip -selection clipboard -t image/png"),
                        ("M-v", unGrab >> spawn "clipmenu"),
                        ("M-<Tab>", goToSelected'),
                        ("<XF86AudioMute>", spawn "pactl set-sink-volume 0 0%"),
                        ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%"),
                        ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%"),
                        ("M-f", selectWindow myEasyMotionConfig >>= (`whenJust` windows . W.focusWindow)),
                        ("M-g", spawn "emacsclient -e '(emacs-everywhere)'"),
                        ("M-C-x", spawn "xrandr --auto"),
                        ("M-s", withPrefixArgument myBrowserSelector)
                      ]

-- ("M-s", withPrefixArgument useACustomSearch)
-- ("M-S-b", manPrompt myXPConfig)
myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      isDialog --> doFloat
    ]

browser :: Browser
browser = "librewolf"

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " ⊲ ⊳ ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#f1fa8c" 2,
      ppHidden = white . wrap " " "",
      -- ppHiddenNoWindows = xmobarbgColor . pad "" "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . yellow . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . magenta . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30 . xmobarStrip

    blue, lowWhite, magenta, red, white, yellow, green :: String -> String
    magenta = xmobarColor "#c7a9ff" ""
    blue = xmobarColor "#8db0ff" ""
    white = xmobarColor "#c0caf5" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff899d" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    green = xmobarColor "#9fe044" ""
    xmobarbgColor = xmobarColor "#11111b" ""
