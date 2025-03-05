-- Imports
import           XMonad

-- Prompts
import           XMonad.Prompt

-- Hooks
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.EwmhDesktops

-- Utils
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Font

-- Layouts
import           XMonad.Layout.Magnifier
import           XMonad.Layout.ThreeColumns

-- Actions
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Search
import           XMonad.Actions.WindowBringer (windowMap)
import           XMonad.Actions.EasyMotion

-- Operations


-- System stuff
import           System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import           System.FilePath ((</>), takeExtension)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Control.Monad (filterM)
import qualified Data.Map as M
import XMonad.StackSet qualified as W

myFont :: String
myFont = "xft:Iosevka:regular:size=12:antialias=true:hinting=true"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap = M.fromList [
          ((0,xK_Escape)        , cancel)
         ,((0,xK_Return)        , select)
         ,((0,xK_slash)         , substringSearch myNavigation)
         ,((0,xK_Left)          , move (-1,0)  >> myNavigation)
         ,((0,xK_h)             , move (-1,0)  >> myNavigation)
         ,((0,xK_Right)         , move (1,0)   >> myNavigation)
         ,((0,xK_l)             , move (1,0)   >> myNavigation)
         ,((0,xK_Down)          , move (0,1)   >> myNavigation)
         ,((0,xK_j)             , move (0,1)   >> myNavigation)
         ,((0,xK_Up)            , move (0,-1)  >> myNavigation)
         ,((0,xK_k)             , move (0,-1)  >> myNavigation)
         ,((0,xK_y)             , move (-1,-1) >> myNavigation)
         ,((0,xK_i)             , move (1,-1)  >> myNavigation)
         ,((0,xK_n)             , move (-1,1)  >> myNavigation)
         ,((0,xK_m)             , move (1,1)   >> myNavigation)
         ,((0,xK_space)         , setPos (0,0) >> myNavigation)
         ,((0,xK_Tab)           , moveNext     >> myNavigation)
         ,((shiftMask, xK_Tab)  , movePrev     >> myNavigation)
         ]
       navDefaultHandler = const myNavigation



myColorizer :: a -> Bool -> X (String, String)
myColorizer _ isSelected = 
    if isSelected
       then pure ("#b4befe", "#11111b")
       else pure ("#181825", "#cdd6f4")


myGsConfig :: GSConfig String
myGsConfig = def
           { gs_cellheight   = 40
           , gs_cellwidth    = 180
           , gs_cellpadding  = 6
           , gs_originFractX = 0.5
           , gs_originFractY = 0.5
           , gs_navigate     = myNavigation
           , gs_font         = myFont
           , gs_colorizer    = myColorizer
           }

myGsConfigWindow :: GSConfig Window
myGsConfigWindow = def
    { gs_cellheight   = 40
    , gs_cellwidth    = 180
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_navigate     = myNavigation
    , gs_font         = myFont
    , gs_colorizer    = myColorizer-- Use a default colorizer for Windows
    }

myXPConfig :: XPConfig
myXPConfig =
  def
        {   font = myFont,
        bgColor = "#11111b",
        fgColor = "#b4befe",
        historySize = 5,
        position = Top        }

myEasyMotionConfig :: EasyMotionConfig
myEasyMotionConfig =
  def
    {
        txtCol="#b4befe"
        ,bgCol="#11111b"
        ,emFont="xft:Iosevka:regular:size=52:antialias=true:hinting=true"
        ,cancelKey = xK_f
    }

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
    [("Librewolf", "librewolf")
    ,("ST", "st")
    ,("BTOP", "st -e btop")
    ,("NVIM", "st -e nvim")
    ,("QuteBrowser", "qutebrowser")
    ,("Audio Manage", "pavucontrol")
    ,("Thunar", "thunar")
    ,("Wallpaper", "nitrogen")
    ,("Meld", "meld")
    ,("steam", "steam")
    ,("Xournal", "xournalpp")
    ,("VLC", "vlc")
    ,("qTorrent", "qbittorrent")
    ,("Chess", "pychess")
    ,("Emacs", "emacsclient -c ")
    ]


main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , handleEventHook    =  swallowEventHook ( className =? "st-256color") (return True) 
    , terminal = "st"
    }
  `additionalKeysP`
    [ -- Modify keybindings:
    ("M-<Return>", spawn "st")
    , ("M-q", kill)       -- Super+Shift+Q to kill a window
    , ("M-S-<Return>",spawn "rofi -show drun")
    , ("M-S-c", spawn "xmonad --recompile; xmonad --restart")
    , ("M-a", spawn "xbacklight -inc 5")
    , ("M-S-a", spawn "xbacklight -dec 5")
    , ("M-o", spawnSelected'  gsPrograms)
    , ("M-y", unGrab >> spawn "maim -s |xclip -selection clipboard -t image/png")
    , ("M-v", unGrab >> spawn "clipmenu")
    , ("M-<Tab>", goToSelected' )
    , ("<XF86AudioMute>",  spawn "pactl set-sink-volume 0 0%")
    , ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume 0 -5%")
    , ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume 0 +5%")
    , ("M-s", promptSearch myXPConfig wikipedia)
    , ("M-f", selectWindow myEasyMotionConfig >>= (`whenJust` windows . W.focusWindow))

    ]    

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]


myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

