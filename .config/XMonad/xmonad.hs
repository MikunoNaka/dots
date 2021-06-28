import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook

-- data
import Data.Tree
import qualified Data.Map as M

-- actions
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SwapWorkspaces
import qualified XMonad.Actions.TreeSelect as TS

-- layout modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.WindowNavigation as WN
import XMonad.Layout.Maximize
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders

-- Layouts
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed

-- hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh)

-- utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad

-- defaults
myModMask = mod4Mask 
altMask = mod1Mask
myLockscreen = "betterlockscreen -l -t 'Yo, Vidhu!'"
myScreenshot = "scrot /home/zt/Media/Screenshots/Screenshot-%m-%d-%Y-%T.png"
myColorPicker = "colorpicker --short --one-shot --preview | xsel -b"
myFont = "xft:Hack:style=Regular:size=14"

-- volume
myVolUp = "pamixer -i 2 && killall lemonblocks -5"
myVolDown = "pamixer -d 2 && killall lemonblocks -5"
myVolMute = "pamixer -m && killall lemonblocks -5"

myExtraWorkspaces = [(xK_0, "十")] -- , (xK_comma, "  十一  "), (xK_period, "  十二  "), (xK_slash, "  十三  ")]
myWorkspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九"] ++ (map snd myExtraWorkspaces)

myScratchpads = [
	 NS "Phone" "scrcpy" (title =? "Motorola One Power") defaultFloating,
	 NS "Terminal" "prime-run alacritty -t 'TerminalScratchpad'" (title =? "TerminalScratchpad") defaultFloating,
	 NS "Nitrogen" "nitrogen" (title =? "Nitrogen") defaultFloating
         ] where role = stringProperty "WM_WINDOW_ROLE"

myTabTheme = def { fontName            = myFont
                 , activeColor         = "#755999"
                 , inactiveColor       = "#282c35"
                 , activeBorderColor   = "#755999"
                 , inactiveBorderColor = "#313846"
                 , activeTextColor     = "#FFFFFF"
                 , inactiveTextColor   = "#d0d0d0"
                 , decoHeight          = 20
                 }

myTSConfig = TS.TSConfig { TS.ts_hidechildren = False
                           , TS.ts_background   = 0xdd282c34
                           , TS.ts_font         = myFont
                           , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                           , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                           , TS.ts_highlight    = (0xffffffff, 0xff755999)
                           , TS.ts_extra        = 0xffd0d0d0
                           , TS.ts_node_width   = 220
                           , TS.ts_node_height  = 34
                           , TS.ts_originX      = 10000
                           , TS.ts_originY      = 100
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = TS.defaultNavigation
                           }

myTreeMenu :: TS.TSConfig (X ()) -> X ()
myTreeMenu a = TS.treeselectAction a
    [ Node (TS.TSNode         "Section Screenshot" "" (spawn "scrot -s /home/zt/Media/Screenshots/Screenshot-%m-%d-%Y-%T.png")) []
    , Node (TS.TSNode         "Favourites"         "" (return()))
        [ Node (TS.TSNode     "VirtualBox"         "" (spawn "virtualbox")) []
        , Node (TS.TSNode     "Discord"            "" (spawn "discord")) []
        , Node (TS.TSNode     "pavucintrol"        "" (spawn "pavucontrol")) []
        ]
    , Node (TS.TSNode         "Accessories"        "" (return()))
        [ Node (TS.TSNode     "Scrcpy"             "" (spawn "scrcpy")) []
        , Node (TS.TSNode     "Pcmanfm"            "" (spawn "pcmanfm")) []
        , Node (TS.TSNode     "Thunderbird"        "" (spawn "thunderbird")) []
        , Node (TS.TSNode     "lf"                 "" (spawn "prime-run alacritty -e lf")) []
        , Node (TS.TSNode     "colorpicker"        "" (spawn "lxappearance")) []
        , Node (TS.TSNode     "networking"         "" (return()))
            [ Node (TS.TSNode "Konqueror"          "" (spawn "konqueror")) []
            , Node (TS.TSNode "Transmission"       "" (spawn "transmission-gtk")) []
            , Node (TS.TSNode "Blueman"            "" (spawn "blueman-manager")) []
            ]
        ]
    , Node (TS.TSNode         "Media"              "" (return()))
        [ Node (TS.TSNode     "VLC"                "" (spawn "prime-run vlc")) []
        , Node (TS.TSNode     "GIMP"               "" (spawn "prime-run gimp")) []
        , Node (TS.TSNode     "Kdenlive"           "" (spawn "prime-run kdenlive")) []
        ]
    , Node (TS.TSNode         "Utilities"          "" (return()))
        [ Node (TS.TSNode     "Nitrogen"           "" (spawn "nitrogen")) []
        , Node (TS.TSNode     "Pavucontrol"        "" (spawn "pavucontrol")) []
        , Node (TS.TSNode     "Blueman"            "" (spawn "blueman-manager")) []
        , Node (TS.TSNode     "Lxappearance"       "" (spawn "lxappearance")) []
        ]
    , Node (TS.TSNode         "Secreenshot"        "" (spawn "scrot /home/zt/Media/Screenshots/Screenshot-%m-%d-%Y-%T.png")) []
    , Node (TS.TSNode         "Power"              "" (return()))
        [ Node (TS.TSNode     "Shutdown"           "" (spawn "shutdown now")) []
        , Node (TS.TSNode     "Restart"            "" (spawn "restart")) []
        ]
    ]

myKeys = [
    ((myModMask, xK_Return), spawn ("prime-run alacritty"))
    , ((0, xK_Print), spawn (myScreenshot))
    , ((myModMask .|. shiftMask, xK_Return), spawn (myScreenshot))
    , ((myModMask, xK_q), spawn (myLockscreen))
    , ((myModMask, xK_n), spawn ("dunstctl close-all"))
    
    , ((altMask, xK_w), kill1)
    , ((myModMask, xK_a), withFocused $ windows . W.sink) -- unfloat windows
    
    -- launch apps/execute scripts
    , ((myModMask, xK_o), spawn ("pcmanfm"))
    , ((myModMask, xK_y), spawn ("gimp"))
    , ((myModMask, xK_e), spawn ("thunderbird"))
    , ((myModMask, xK_b), spawn ("konqueror"))
    , ((myModMask, xK_r), spawn ("prime-run alacritty -e lf"))
    	
    -- launch/copy apps
    , ((myModMask .|. shiftMask, xK_o), runOrCopy "pcmanfm" (className =? "Pcmanfm"))
    , ((myModMask, xK_m), runOrCopy "prime-run vlc" (className =? "vlc"))
    , ((myModMask .|. shiftMask, xK_m), spawn ("prime-run vlc"))
    
    -- scratchpad keybindings
    , ((myModMask, xK_u), namedScratchpadAction myScratchpads "Phone")
    , ((myModMask, xK_t), namedScratchpadAction myScratchpads "Terminal")
    , ((myModMask, xK_w), namedScratchpadAction myScratchpads "Nitrogen")
    
    -- modify gaps on runtime
    , ((myModMask, xK_equal), incWindowSpacing 1)
    , ((myModMask, xK_minus), decWindowSpacing 1)
    , ((myModMask .|. shiftMask, xK_equal), incScreenSpacing 1)  
    , ((myModMask .|. shiftMask, xK_minus), decScreenSpacing 1)         

    -- view prev/next workspaces
    , ((altMask, xK_h), prevWS)
    , ((altMask, xK_l), nextWS)
    
    -- move to prev/next workspaces
    , ((altMask .|. shiftMask, xK_h), shiftToPrev >> prevWS)
    , ((altMask .|. shiftMask, xK_l), shiftToNext >> nextWS)
    
    , ((myModMask, xK_backslash), sendMessage ToggleStruts) -- toggle both bars
    , ((myModMask, xK_f), withFocused (sendMessage . maximizeRestore)) -- toggle maximize

    -- cycle through windows 
    , ((altMask, xK_j), windows W.focusDown)
    , ((altMask, xK_k), windows W.focusUp)
    
    -- 2D navigation
    , ((myModMask,                 xK_l), sendMessage $ Go R)
    , ((myModMask,                 xK_h), sendMessage $ Go L)
    , ((myModMask,                 xK_k), sendMessage $ Go U)
    , ((myModMask,                 xK_j), sendMessage $ Go D)
    
    -- swap windows
    , ((myModMask .|. shiftMask, xK_l), sendMessage $ WN.Swap R)
    , ((myModMask .|. shiftMask, xK_h), sendMessage $ WN.Swap L)
    , ((myModMask .|. shiftMask, xK_k), sendMessage $ WN.Swap U)
    , ((myModMask .|. shiftMask, xK_j), sendMessage $ WN.Swap D)

    -- resize
    , ((myModMask .|. mod1Mask,                  xK_l  ), sendMessage $ ExpandTowards R)
    , ((myModMask .|. mod1Mask,                  xK_h  ), sendMessage $ ExpandTowards L)
    , ((myModMask .|. mod1Mask,                  xK_j  ), sendMessage $ ExpandTowards D)
    , ((myModMask .|. mod1Mask,                  xK_k  ), sendMessage $ ExpandTowards U)
    , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_l  ), sendMessage $ ShrinkFrom R)
    , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_h  ), sendMessage $ ShrinkFrom L)
    , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_j  ), sendMessage $ ShrinkFrom D)
    , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_k  ), sendMessage $ ShrinkFrom U)

    -- other bsp-only actions
    , ((myModMask,                              xK_d     ), sendMessage Rotate)
    , ((myModMask,                              xK_s     ), sendMessage BSP.Swap)
    ] ++ [ -- for extra workspace(s)
        ((myModMask, key), (windows $ W.greedyView ws))
        | (key,ws) <- myExtraWorkspaces
    ] ++ [ -- also for extra workspaces
        ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
        | (key,ws) <- myExtraWorkspaces
    ] ++ [ -- to swap workspaces
        ((mod1Mask .|. shiftMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip myWorkspaces [xK_1 ..]
    ] ++ [ -- copy
        ((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip (myWorkspaces) [xK_1 ..]
        , (f, m) <- [(copy, shiftMask .|. altMask)]
    ] ++ [ -- sticky-ing windows
        ((myModMask, xK_v), windows copyToAll)  -- make window visible on all screens
        , ((myModMask .|. shiftMask, xK_v), killAllOtherCopies)
    ]

sGap = 4 -- screen gap
wGap = 10 -- window gap

myGap = spacingRaw True (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True
myGap' = spacingRaw False (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True

main :: IO ()
main = do
  xmonad $ ewmh $ docks $ fullscreenSupport def {
    focusFollowsMouse  = True
    , borderWidth        = 2
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = "#130F23"
    , focusedBorderColor = "#BF00FF"
    , layoutHook = avoidStruts $ maximize $ windowNavigation $ smartBorders $ myGap $ (
        emptyBSP ||| tabbed shrinkText myTabTheme ||| emptyBSP ||| Grid)
    , manageHook         = namedScratchpadManageHook myScratchpads <+> manageDocks
    , startupHook        = do
        spawnOnce "polybar mybar"
        setWMName "LG3D"
        setDefaultCursor xC_left_ptr
} `additionalMouseBindings` [ 
      ((altMask, 2), \w -> kill1)
      , ((myModMask, 4), \w -> prevWS)
      , ((myModMask, 5), \w -> nextWS)
    ] `additionalKeys` myKeys `additionalKeysP` [
        ("M-i l", runOrCopy "librewolf" (className =? "Librewolf"))
        , ("M-i S-l", spawn "librewolf")
        , ("M-i b", spawn "brave")
        , ("M-i S-b", spawn "brave --incognito")
        , ("M-i d", spawn "discord")
        , ("M-i s", spawn "steam")
        , ("M-i t", spawn "transmission-gtk")
        , ("M-i k", spawn "konqueror")
        , ("M-i f", spawn "filezilla")
        , ("M-i b", spawn "blueman-manager")
        , ("M-i p", spawn "pavucontrol")
        -- this doesn't work when in myKeys
        , ("M-c", spawn myColorPicker)
        , ("M-x", spawn "notify_battery_status.sh")
        -- Launchers
        , ("M-p", myTreeMenu myTSConfig)
        , ("M-S-p", spawn "dmenu_run")
        -- GridSelect
        , ("M-g g", goToSelected defaultGSConfig)
        , ("M-g b", bringSelected defaultGSConfig)
        -- volume
        , ("<XF86AudioMute>",        spawn (myVolMute))
        , ("<XF86AudioLowerVolume>", spawn (myVolDown))
        , ("<XF86AudioRaiseVolume>", spawn (myVolUp))
    ]
