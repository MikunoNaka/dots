import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook

-- xmonad-log imports import qualified DBus as D
-- import qualified DBus.Client as D
-- import qualified Codec.Binary.UTF8.String as UTF8

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
import XMonad.Actions.SpawnOn
import qualified XMonad.Actions.TreeSelect as TS

-- layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowNavigation as WN
import XMonad.Layout.Renamed as R (renamed, Rename(Replace))
import XMonad.Layout.Maximize
-- import XMonad.Layout.PerWorkspace

-- Layouts
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed -- fix this it doesnt work
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.ZoomRow


-- hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

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
	  spawnOn   "  二  " "firefox"
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
myLauncher = "dmenu_run"

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
myColorPicker = "colorpicker --short --one-shot --preview | xsel -b"

-- volume
myVolUp :: String
myVolUp = "pulseaudio-ctl up && killall lemonblocks -5"

myVolDown :: String
myVolDown = "pulseaudio-ctl down && killall lemonblocks -5"

myVolMute :: String
myVolMute = "pulseaudio-ctl mute && killall lemonblocks -5"


myFont :: String
myFont = "xft:Hack:style=Regular:size=12"


-- wm variables

-- fBorder = "#bf00ff"
-- fBorder = "#3804f4"
fBorder = "#00ff85"
nBorder = "#130F23"

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
	 , ((myModMask .|. shiftMask, xK_Return), spawn (myScreenshot))
	 , ((myModMask, xK_q), spawn (myLockscreen))
	 , ((myModMask, xK_n), spawn ("dunstctl close-all"))

         , ((altMask, xK_w), kill1)
         , ((myModMask, xK_a), withFocused $ windows . W.sink) -- unfloat windows

         -- launch apps/execute scripts
         , ((myModMask, xK_o), spawn (myFileManager))
         , ((myModMask, xK_y), spawn ("gimp"))
         , ((myModMask, xK_e), spawn (myEmailClient))
         , ((myModMask, xK_b), spawn ("konqueror"))
         , ((myModMask, xK_r), spawn (myTerminal ++ " -e lf"))
	
	 -- launch/copy apps
         , ((myModMask .|. shiftMask, xK_o), runOrCopy "pcmanfm" (className =? "Pcmanfm"))
         , ((myModMask, xK_m), runOrCopy "vlc" (className =? "vlc"))
         , ((myModMask .|. shiftMask, xK_m), spawnOn "  十  " "vlc")

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

         -- kinda bad horizontal resizing of windows in non-bsp layouts
         , ((myModMask, xK_Right), sendMessage Expand)
         , ((myModMask, xK_Left), sendMessage Shrink)

         -- paste x selection
         , ((altMask, xK_v), pasteSelection)

         -- toggle bars
         , ((myModMask, xK_backslash), sendMessage ToggleStruts) -- toggle both bars

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
-- Emacs style keybindings
myKeys' :: [(String, X ())]
myKeys' =
         [-- Running or copying browsers 
	 ("M-i f", runOrCopy "firefox" (className =? "Firefox"))
	 , ("M-i S-f", spawn "firefox")
         , ("M-i b", spawn "brave")
         , ("M-i S-b", spawn "brave --incognito")
	 -- this doesn't work when in myKeys
	 , ("M-c", spawn myColorPicker)
	 -- Launchers
         , ("M-p", myTreeMenu myTSConfig)
	 , ("M-S-p", spawn myLauncher)
	 -- GridSelect
	 , ("M-g g", goToSelected defaultGSConfig)
	 , ("M-g b", bringSelected defaultGSConfig)
	 ]
-- mouse keybindings
-- 1, 2, 3 = left, middle, right
myMouseBindings = [((altMask, 2), \w -> kill1)
         -- , ((altMask, 1), \w -> spawn "pcmanfm")
         -- , ((altMask, 3), \w -> spawn "konqueror")
         , ((altMask, 2), \w -> spawn myVolMute)
         , ((altMask, 4), \w -> spawn myVolDown)
         , ((altMask, 5), \w -> spawn myVolUp)
         , ((myModMask, 4), \w -> prevWS)
         , ((myModMask, 5), \w -> nextWS)
         ]

-- tabs config
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#755999"
                 , inactiveColor       = "#282c35"
                 , activeBorderColor   = "#755999"
                 , inactiveBorderColor = "#313846"
                 , activeTextColor     = "#FFFFFF"
                 , inactiveTextColor   = "#d0d0d0"
                 , decoHeight          = 20
                 }

-- layouts
myGap = spacingRaw True (Border sGap sGap sGap sGap) True (Border wGap wGap wGap wGap) True


-- myLayoutHook = onWorkspaces ["  二  ","  九  ","  十  "] altLayouts $ onWorkspaces ["  一  ","  三  ","  四  ","  五  ","  六  ","  七  ","  八  "] defLayouts
-- myLayoutHook = onWorkspace "  二  " renamed [R.Replace "Tabbed"] (avoidstruts $ smartBorders $ windowNavigation $ myGap $ tabbed shrinkText myTabTheme) ||| defLayouts

myLayoutHook = avoidStruts ( -- layouts to be used in almost every workspace
        renamed [R.Replace "BSP"]                  (maximize $ smartBorders $ windowNavigation $ myGap $ emptyBSP)
        ||| renamed [R.Replace "Tabbed"]           (maximize $ smartBorders $ windowNavigation $ myGap $ tabbed shrinkText myTabTheme)
        ||| renamed [R.Replace "Accordion"]        (maximize $ smartBorders $ windowNavigation $ myGap $ Accordion)
        ||| renamed [R.Replace "ZoomRow"]          (maximize $ smartBorders $ windowNavigation $ myGap $ zoomRow)
        ||| renamed [R.Replace "TwoPane"]          (maximize $ smartBorders $ windowNavigation $ myGap $ TwoPane (3/100) (1/2))
        ||| renamed [R.Replace "ZoomRow Mirrored"] (maximize $ smartBorders $ windowNavigation $ myGap $ Mirror zoomRow)
        ||| renamed [R.Replace "ThreeCol Mid (1)"] (maximize $ smartBorders $ windowNavigation $ myGap $ ThreeColMid 1 (3/100) (1/2))
        ||| renamed [R.Replace "ThreeCol Mid (2)"] (maximize $ smartBorders $ windowNavigation $ myGap $ ThreeColMid 2 (3/100) (1/2))
        ||| renamed [R.Replace "Grid"]             (maximize $ smartBorders $ windowNavigation $ myGap $ Grid)
        ||| renamed [R.Replace "ThreeCol (1)"]     (maximize $ smartBorders $ windowNavigation $ myGap $ ThreeCol 1 (3/100) (1/2))
        ||| renamed [R.Replace "Tabbed"]           (maximize $ smartBorders $ windowNavigation $ myGap $ tabbed shrinkText myTabTheme)
        ||| renamed [R.Replace "ThreeCol (2)"]     (maximize $ smartBorders $ windowNavigation $ myGap $ ThreeCol 2 (3/100) (1/2))
	-- ||| onWorkspace "  二  " renamed [R.Replace "Tabbed"]           (smartBorders $ windowNavigation $ myGap $ tabbed shrinkText myTabTheme)
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
  layoutHook         = myLayoutHook,
  manageHook         = manageSpawn <+> namedScratchpadManageHook myScratchpads,
  handleEventHook    = handleEventHook def <+> fullscreenEventHook,

  -- logHook = dynamicLogWithPP (myLemonbarPP dbus),
  logHook            = dynamicLogWithPP myLemonbarPP { ppOutput = \x -> hPutStrLn notXMobar x}, 
  startupHook        = myStartupHook
} `additionalMouseBindings` myMouseBindings `additionalKeys` myKeys `additionalKeysP` myKeys'
