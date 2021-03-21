import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook

-- xmonad-log imports
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- data
import Data.Tree
import qualified Data.Map as M

-- actions
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.WindowBringer
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.TreeSelect as TS

-- layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowNavigation as WN
import XMonad.Layout.Fullscreen
import XMonad.Layout.Renamed as R (renamed, Rename(Replace))

-- Layouts
-- import XMonad.Layout.AvoidFloats
import XMonad.Layout.BinarySpacePartition as BSP
-- import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Maximize
import XMonad.Layout.Tabbed -- fix this it doesnt work
import XMonad.Layout.NoBorders

-- hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.FadeInactive

-- utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig
import XMonad.Util.Cursor
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad


myStartupHook :: X ()
myStartupHook = do
	  spawnOnce "start-lemonbar.sh"
          spawnOnce "firefox"
	  -- setWMName "AnimeThighsWM"
	  setWMName "LG3D"
          setDefaultCursor xC_left_ptr

-- defaults
-- myModMask :: KeyMask
myModMask = mod4Mask 

altMask :: KeyMask
altMask = mod1Mask

myTerminal :: String
myTerminal = "st"

myLauncher :: String
myLauncher = ".//home/zt/.config/scripts/run_dmenu"

myBrowser :: String
myBrowser = "brave"

myFileManager :: String
myFileManager = "pcmanfm"

myEmailClient :: String
myEmailClient = "thunderbird"

myEditor :: String
myEditor = "nvim"

myLockscreen :: String
myLockscreen = "betterlockscreen -l -t 'Yo, Vidhu!'"

myScreenshot :: String
myScreenshot = "scrot /zt/Screenshots/Screenshot-%Y-%d-%m--%T.png"

myColorPicker :: String
myColorPicker = ".//zt/Programs/colorpicker --short --one-shot --preview | xsel -b"

-- volume
myVolUp :: String
myVolUp = "pulseaudio-ctl up && killall lemonblocks -5"

myVolDown :: String
myVolDown = "pulseaudio-ctl down && killall lemonblocks -5"

myVolMute :: String
myVolMute = "pulseaudio-ctl mute && killall lemonblocks -5"


myFont :: String
myFont = "xft:Roboto:pixelsize=16:antialias=true"


-- wm variables

nBorder = "#130F23" -- "#3804f4" -- "#bf00ff"
fBorder = "#00ff85"

myBorderWidth = 1

sGap = 1 -- screen gap
wGap = 2 -- window gap

myExtraWorkspaces = [(xK_0, "  十  ")] -- , (xK_comma, "  十一  "), (xK_period, "  十二  "), (xK_slash, "  十三  ")]
myWorkspaces = ["  一  ","  二  ","  三  ","  四  ","  五  ","  六  ","  七  ","  八  ", "  九  "] ++ (map snd myExtraWorkspaces)

-- treeselect config
defaultNavigation = M.fromList
    [ ((0, xK_Escape), TS.cancel)
    , ((0, xK_Return), TS.select)
    , ((0, xK_space),  TS.select)
    , ((0, xK_Up),     TS.movePrev)
    , ((0, xK_Down),   TS.moveNext)
    , ((0, xK_Left),   TS.moveParent)
    , ((0, xK_Right),  TS.moveChild)
    , ((0, xK_k),      TS.movePrev)
    , ((0, xK_j),      TS.moveNext)
    , ((0, xK_h),      TS.moveParent)
    , ((0, xK_l),      TS.moveChild)
    , ((0, xK_o),      TS.moveHistBack)
    , ((0, xK_i),      TS.moveHistForward)
    ]

myTSConfig = TS.TSConfig { TS.ts_hidechildren = False
                           , TS.ts_background   = 0xdd282c34
                           , TS.ts_font         = myFont
                           , TS.ts_node         = (0xffd0d0d0, 0xff1c1f24)
                           , TS.ts_nodealt      = (0xffd0d0d0, 0xff282c34)
                           , TS.ts_highlight    = (0xffffffff, 0xff755999)
                           , TS.ts_extra        = 0xffd0d0d0
                           , TS.ts_node_width   = 200
                           , TS.ts_node_height  = 28
                           , TS.ts_originX      = 100
                           , TS.ts_originY      = 100
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = TS.defaultNavigation
                           }

myPowerMenu :: TS.TSConfig (X ()) -> X ()
myPowerMenu a = TS.treeselectAction a
    [Node (TS.TSNode "SHUTDOWN" "It's not like I'm gonna miss you, b- baka!" (spawn "shutdown now")) []
    , Node (TS.TSNode "REBOOT" "Be quick, okay?" (spawn "reboot")) []
    , Node (TS.TSNode "CANCEL" "Do nothing" (return ())) []
    ]

myTreeMenu :: TS.TSConfig (X ()) -> X ()
myTreeMenu a = TS.treeselectAction a
    [ Node (TS.TSNode "Section Screenshot" "Take screenshot of a section on the screen" (spawn "scrot -s /zt/Screenshots/Screenshot-%Y-%d-%m--%T.png")) []
    , Node (TS.TSNode "Utilities" "" (return()))
        [ Node (TS.TSNode "Pavucontrol" "" (spawn "pavucontrol")) []
        , Node (TS.TSNode "Color Picker" "" (spawn myColorPicker)) []
        , Node (TS.TSNode "bashtop" "" (spawn "st -e bashtop")) []
        , Node (TS.TSNode "htop" "" (spawn "st -e htop")) []
        , Node (TS.TSNode "lxappearance" "" (spawn "st -e lxappearance")) []
	]
    , Node (TS.TSNode "Apps" "" (return()))
        [ Node (TS.TSNode "Torrents" "" (spawn "qbittorrent")) []
        , Node (TS.TSNode "Discord" "" (spawn "discord-canary")) []
        , Node (TS.TSNode "LibreOffice" "" (spawn "libreoffice")) []
    	, Node (TS.TSNode "Browsers" "" (return()))
            [ Node (TS.TSNode "Vivaldi" "" (spawn "vivaldi-stable")) []
            , Node (TS.TSNode "Firefox" "" (spawn "firefox")) []
            , Node (TS.TSNode "Brave" "" (spawn "brave")) []
	    ]
	]
    , Node (TS.TSNode "Other" "" (return()))
        [ Node (TS.TSNode "Recompile XMonad" "Recompile and restart the window manager" (spawn "xmonad --recompile && xmonad --restart")) []
        , Node (TS.TSNode "Launch DMenu" "just in case the keybinding doesn't work" (spawn "run_dmenu")) []
        , Node (TS.TSNode "Restart server" "Restart startpage server" (spawn "killall startpage-server;startpage-server")) []
        , Node (TS.TSNode "notify" "send a notification" (spawn "notify-send 'This is a notification' 'Hello World!'")) []
	]
    , Node (TS.TSNode "Power" "" (return()))
        [ Node (TS.TSNode "Shutdown" "It's not like I'll miss you b- baka!" (spawn "shutdown now")) []
        , Node (TS.TSNode "Reboot" "What are you doing!" (spawn "reboot")) []
        , Node (TS.TSNode "Cancel" "Yamete kudasai" (return())) []
	]
    ]



-- scratchpad config
myScratchpads = [
	 NS "Phone" "scrcpy" (title =? "Motorola One Power") defaultFloating,
	 NS "Terminal" "st -t 'TerminalScratchpad'" (title =? "TerminalScratchpad") defaultFloating,
	 NS "Nitrogen" "nitrogen" (title =? "Nitrogen") defaultFloating
         ] where role = stringProperty "WM_WINDOW_ROLE"


-- keybindings
myKeys = [
         ((myModMask, xK_Return), spawn (myTerminal))
         , ((myModMask .|. shiftMask, xK_p), spawn (myLauncher)) -- Doesn't work
	 , ((myModMask .|. shiftMask, xK_Return), spawn (myScreenshot))
	 , ((myModMask, xK_q), spawn (myLockscreen))
	 , ((myModMask, xK_c), spawn (myColorPicker)) -- Doesn't work
	 , ((myModMask, xK_n), spawn ("dunstctl close-all"))

         , ((altMask, xK_w), kill1)
         , ((altMask .|. shiftMask, xK_k), kill1)
         , ((myModMask, xK_a), withFocused $ windows . W.sink) -- unfloat windows

         -- TreeSelect
	 , ((altMask, xK_F4), myPowerMenu myTSConfig)
	 , ((myModMask, xK_p), myTreeMenu myTSConfig)

         -- launch apps/execute scripts
         -- , ((myModMask, xK_i), spawn (myBrowser))
         , ((myModMask, xK_o), spawn (myFileManager))
         , ((myModMask, xK_y), spawn ("gimp"))
         , ((myModMask, xK_e), spawn (myEmailClient))
         -- , ((myModMask, xK_m), spawn ("vlc"))
         , ((myModMask, xK_b), spawn ("konqueror"))
	
	 -- launch/copy apps
         , ((myModMask .|. shiftMask, xK_o), runOrCopy "pcmanfm" (className =? "Pcmanfm"))
         , ((myModMask, xK_i), runOrCopy "firefox" (className =? "Firefox"))
         , ((myModMask, xK_m), runOrCopy "vlc" (className =? "vlc"))

         -- volume
         , ((altMask, xK_0), spawn (myVolMute))
         , ((altMask, xK_minus), spawn (myVolDown))
         , ((altMask, xK_equal), spawn (myVolUp))

	 -- scratchpad keybindings
	 , ((myModMask, xK_u), namedScratchpadAction myScratchpads "Phone")
	 , ((myModMask, xK_t), namedScratchpadAction myScratchpads "Terminal")
	 , ((myModMask, xK_w), namedScratchpadAction myScratchpads "Nitrogen")

         -- view prev/next workspaces
         , ((altMask, xK_h), prevWS)
         , ((altMask, xK_l), nextWS)

         -- move to prev/next workspaces
         , ((altMask .|. shiftMask, xK_h), shiftToPrev >> prevWS)
         , ((altMask .|. shiftMask, xK_l), shiftToNext >> nextWS)

         -- modify gaps on runtime
         , ((myModMask, xK_equal), incWindowSpacing 1)
         , ((myModMask, xK_minus), decWindowSpacing 1)
         , ((myModMask .|. shiftMask, xK_equal), incScreenSpacing 1)  
         , ((myModMask .|. shiftMask, xK_minus), decScreenSpacing 1)         

         -- Swap the focused window and the master window
         , ((mod1Mask .|. shiftMask, xK_Return), windows W.swapMaster)

         -- these keybindings are for WindowNavigation
         -- and they conflict with BSP layout
         -- directional navigation of windows
         , ((myModMask,                 xK_l), sendMessage $ Go R)
         , ((myModMask,                 xK_h), sendMessage $ Go L)
         , ((myModMask,                 xK_k), sendMessage $ Go U)
         , ((myModMask,                 xK_j), sendMessage $ Go D)

         -- swap windows
         , ((myModMask .|. shiftMask, xK_l), sendMessage $ WN.Swap R)
         , ((myModMask .|. shiftMask, xK_h), sendMessage $ WN.Swap L)
         , ((myModMask .|. shiftMask, xK_k), sendMessage $ WN.Swap U)
         , ((myModMask .|. shiftMask, xK_j), sendMessage $ WN.Swap D)
         
         -- cycle through windows 
         , ((altMask, xK_j), windows W.focusDown)
         , ((altMask, xK_k), windows W.focusUp)

         -- grid select
         , ((myModMask, xK_g), goToSelected defaultGSConfig)

         -- kinda bad horizontal resizing of windows in non-bsp layouts
         , ((myModMask, xK_Right), sendMessage Expand)
         , ((myModMask, xK_Left), sendMessage Shrink)

      
         -- windowbringer
         , ((myModMask, xK_b), bringMenu)
--         , ((myModMask, xK_g), gotoMenu)

         -- paste x selection
         , ((altMask, xK_v), pasteSelection)

         -- toggle bars
         , ((myModMask, xK_backslash), sendMessage ToggleStruts) -- toggle both bars
         , ((myModMask, xK_bracketleft), sendMessage $ ToggleStrut D) -- toggle bottom bar
         , ((myModMask, xK_bracketright), sendMessage $ ToggleStrut U) -- toggle top bar

         -- BSP layout keybindings
         -- resize
         , ((myModMask .|. mod1Mask,                  xK_l  ), sendMessage $ ExpandTowards R)
         , ((myModMask .|. mod1Mask,                  xK_h  ), sendMessage $ ExpandTowards L)
         , ((myModMask .|. mod1Mask,                  xK_j  ), sendMessage $ ExpandTowards D)
         , ((myModMask .|. mod1Mask,                  xK_k  ), sendMessage $ ExpandTowards U)
         , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_l  ), sendMessage $ ShrinkFrom R)
         , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_h  ), sendMessage $ ShrinkFrom L)
         , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_j  ), sendMessage $ ShrinkFrom D)
         , ((myModMask .|. mod1Mask .|. shiftMask ,   xK_k  ), sendMessage $ ShrinkFrom U)
         -- other
         , ((myModMask,                              xK_d     ), sendMessage Rotate)
         , ((myModMask,                              xK_s     ), sendMessage BSP.Swap)
--          , ((myModMask .|. shiftMask .|. controlMask , xK_j     ), sendMessage $ SplitShift Prev)
--          , ((myModMask .|. shiftMask .|. controlMask , xK_k     ), sendMessage $ SplitShift Next)
         , ((myModMask, xK_f), withFocused (sendMessage . maximizeRestore))
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
-- mouse keybindings
-- 1, 2, 3 = left, middle, right
myMouseBindings = [((altMask, 2), \w -> kill1)
         , ((altMask, 1), \w -> spawn "pcmanfm")
         , ((altMask, 3), \w -> spawn "konqueror")
         , ((altMask, 2), \w -> spawn myVolMute)
         , ((altMask, 4), \w -> spawn myVolDown)
         , ((altMask, 5), \w -> spawn myVolUp)
         , ((myModMask, 4), \w -> prevWS)
         , ((myModMask, 5), \w -> nextWS)
         ]

-- tabs config
myTabTheme = def { fontName            = "roboto"
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }



-- layouts
myGap = spacingRaw True (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True


myLayoutHook = avoidStruts (
	renamed [R.Replace "BSP"]                  (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ emptyBSP)
        ||| renamed [R.Replace "ThreeCol Mid (1)"] (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ ThreeColMid 1 (3/100) (1/2))
        ||| renamed [R.Replace "ThreeCol Mid (2)"] (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ ThreeColMid 2 (3/100) (1/2))
	||| renamed [R.Replace "Grid"]             (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ Grid)
	||| renamed [R.Replace "TwoPane"]          (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ TwoPane (3/100) (1/2))
	||| renamed [R.Replace "ThreeCol (1)"]     (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ ThreeCol 1 (3/100) (1/2))
	||| renamed [R.Replace "ThreeCol (2)"]     (maximize $ smartBorders $ fullscreenFocus $ windowNavigation $ myGap $ ThreeCol 2 (3/100) (1/2))
	||| tabbedRight shrinkText def
	)

-- myLemonbarPP :: D.Client -> PP
-- myLemonbarPP dbus = def { ppOutput = dbusOutput dbus
myLemonbarPP  = def {
                   ppCurrent = wrap "%{B#d33682}%{F-}" "%{B-}" 
                   , ppWsSep = ""
                   , ppHidden = wrap "%{F#02fc45}%{B#130F23}" "%{B-}%{f-}"
                   , ppHiddenNoWindows = wrap "%{F#268bd2}" "%{F-}"
                   , ppTitle = wrap " %{B#130F23}%{F#6c71c4}  " "  %{F-}%{B-}" . shorten 60
                   , ppUrgent = wrap "%{B#9cfc02}    " "    %{B-}%{F-}"
                   , ppLayout = wrap "%{B#130F23}%{F#cb31d6} " " %{F-}%{B-}"
                   , ppSep =  " "
                   , ppOrder  = \(ws:l:t:ex) -> [ws]++[l]++[t]++ex
                   }

-- dbusOutput :: D.Client -> String -> IO ()
-- dbusOutput dbus str = do
--     let signal = (D.signal objectPath interfaceName memberName) {
--             D.signalBody = [D.toVariant $ UTF8.decodeString str]
--         }
--     D.emit dbus signal
--   where
--     objectPath = D.objectPath_ "/org/xmonad/Log"
--     interfaceName = D.interfaceName_ "org.xmonad.Log"
--     memberName = D.memberName_ "Update"





main :: IO ()
main = do
  notXMobar <- spawnPipe "lemonbar -p -b -g 800x21+0+0  -B '#171520' -F '#ffffff' -o -3 -f 'Source Han Sans JP:size=10' -o 0 -f 'RobotoMono Nerd Font:style=Regular:size=15'"
  -- dbus <- D.connectSession
  -- Request access to the DBus name
  -- D.requestName dbus (D.busName_ "org.xmonad.Log")
  --     [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad $ docks def
    {
  terminal           = myTerminal,
  focusFollowsMouse  = True,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
  workspaces         = myWorkspaces,
  normalBorderColor  = nBorder,
  focusedBorderColor = fBorder,
  -- mouseBindings      = additionalMouseBindings myMouseBindings,
  layoutHook         = myLayoutHook,
  manageHook         = namedScratchpadManageHook myScratchpads <+> fullscreenManageHook, 
  handleEventHook    = fullscreenEventHook,
  -- logHook = dynamicLogWithPP (myLemonbarPP dbus),
  logHook            = dynamicLogWithPP myLemonbarPP { ppOutput = \x -> hPutStrLn notXMobar x}, 
  startupHook        = myStartupHook
} `additionalMouseBindings` myMouseBindings `additionalKeys` myKeys
