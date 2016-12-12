{- xmonad.hs
 -}
 
-- Import stuff
import XMonad
import qualified XMonad.StackSet as W 
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
 
 
-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.RotSlaves
 
-- utils
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt as P
import XMonad.Prompt.Shell
import XMonad.Prompt
 
 
-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
 
-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Util.WindowProperties
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.HintedGrid
import XMonad.Layout.BoringWindows
import XMonad.Layout.TwoPane
import XMonad.Layout.LimitWindows
import XMonad.Layout.SimpleFloat
import XMonad.Layout.FixedColumn
import XMonad.Layout.Magnifier
import XMonad.Layout.Circle
 
-- Data.Ratio for IM layout
import Data.Ratio ((%))

-- We will need liftM2
import Control.Monad (liftM2)

 
 
-- Main --
main = do
        xmproc <- spawnPipe "/usr/bin/xmobar"  -- start xmobar
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
            { manageHook = myManageHook
                , layoutHook = myLayoutHook  
                , borderWidth = myBorderWidth
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys = myKeys
                , logHook = myLogHook xmproc
                , modMask = myModMask  
                , terminal = myTerminal
                , workspaces = myWorkspaces
                , focusFollowsMouse = False
                }
 
 
 
-- hooks
-- automaticly switching app to workspace 
myManageHook :: ManageHook
myManageHook = composeAll . concat $
                [[isFullscreen                  --> doFullFloat
                , className =?  "Xmessage"      --> doCenterFloat 
                , className =? "feh"    --> doCenterFloat 
                , className =? "Gimp"           --> doShift "9:gimp"
                , className =? "Uzbl"           --> doShift "2:web"
                , className =? "Ekiga"          --> doFloat
                , className =? "Mozilla"        --> doShift "2:web"
                , className =? "Iceweasel"      --> doShift "2:web"
                , className =? "Navigator"      --> doShift "2:web"
                , className =? "Firefox"    --> doShift "2:web"
                , className =? "Thunderbird"    --> doShift "5:mail"
                , (className =? "Navigator" <&&> resource =? "Dialog") --> doFloat
                , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
                , (className =? "Firefox" <&&> title =? "Firefox Preferences") --> doFloat
                , className =? "Pidgin"           --> doShift "1:chat"
                , className =? "Skype"           --> doShift "1:chat"
                , className =? "MPlayer"        --> doShift "8:vid"
                , className =? "tvtime"         --> doShift "8:vid"
                , className =? "Options"        --> doFloat
                , className =? "frame"           --> doShift "4:pdf" --xchm
                , className =? "Acroread"         --> doShift "4:pdf"
                , className =? "Djview"         --> doShift "4:pdf"
                , className =? "Djview4"         --> doShift "4:pdf"
                , className =? "Evince"         --> doShift "4:pdf"
                , className =? "Gnome-terminal" --> doShift "3:code"
                , appName =? "VirtualBox" --> doShift "3:code"
                , resource =? "trayer" --> doIgnore
                , manageDocks
                 ]
                ]
                where
                       -- viewShift = doF . liftM2 (.) W.greedyView W.shift
                        myIgnores = ["trayer"]
                        myFloats  = []
                        myOtherFloats = []
 
 
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
 
 
 
---- Looks --
---- bar
customPP :: PP
customPP = defaultPP { 
                            ppHidden = xmobarColor "#00FF00" ""
                          , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
                          , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                          , ppLayout = xmobarColor "#FF0000" "" . shorten 10
                          , ppTitle = xmobarColor "#00FF00" "" . shorten 30
                          , ppSep = "<fc=#0033FF> | </fc>"
                     }
 
-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    
    { 
        font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u" 
        ,fgColor = "#00FFFF"
        , bgColor = "#000000"
        , bgHLight    = "#000000"
        , fgHLight    = "#FF0000"
        , position = Top
    }
 
--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }
 
--LayoutHook
myLayoutHook  =  onWorkspace "1:chat" chatL $  onWorkspace "2:web" webL $ onWorkspace "3:code" devL $ onWorkspace "4:pdf" webL $ onWorkspace "9:gimp" gimpL $ onWorkspace "6:code2" devL $ onWorkspace "7:code3" devL $ onWorkspace "0:code4" experimentL $ onWorkspace "-:code5" devL $ onWorkspace "=:code6" devL $ onWorkspace "8:vid" fullL $ standardLayouts
   where
        standardLayouts =   avoidStruts  $ (tiled |||  reflectTiled ||| Mirror tiled  ||| Full) 
 
        --Layouts
        tiled     =  smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbed shrinkText myTheme)
        full      = noBorders Full
        twoPane   = TwoPane (1/10) (1/2)
        mirror2Pane = (Mirror $ TwoPane (1/10) (1/2))
        myGrid = (limitWindows 4 $ GridRatio (5/4) False)
 
        --Chat Layout
        --im = ((withIM ratio pidginRoster) $ reflectHoriz $ (withIM ratio  skypeRoster))
        chatL = avoidStruts $ smartBorders $ (withIM ratio skypeRoster) $ reflectHoriz $ (withIM ratio pidginRoster) $ myGrid  
        ratio = 1%6
        --roster = And (ClassName "Pidgin" ) (Role "buddy_list")
        pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
        skypeRoster     = And (ClassName "Skype") (Not (Role "ConversationsWindow")) 
--chatL = avoidStruts $ smartBorders $ im tiled 
 
        --Gimp Layout
        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") simpleFloat 
 
        mathL = avoidStruts $ smartBorders $ withIM (0.11) mathInput $ tabLayout 
--        mathInput = And (StringProperty "WM_CLASS" "XMathematica") (StringProperty "WM_NAME" "Basic Math Input")
        mathInput = (Title "Basic Math Input")
        --Web Layout
        webL      = boringWindows $ avoidStruts $  tabLayout  ||| Mirror tiled |||  full 
        experimentL = (limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 60 10) ||| Circle ||| tabLayout
 
        --VirtualLayout
        fullL = avoidStruts $ full
 
        --DevLayout
        devL = avoidStruts $  (tabLayout ||| myGrid ||| twoPane ||| mirror2Pane)
 
 
 
 
 
-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"
 
-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod1Mask
 
 
 
-- borders
myBorderWidth :: Dimension
myBorderWidth = 1
--  
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#FF0000"
--
 
 
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:web", "3:code", "4:pdf", "5:mail", "6:code2" ,"7:code3", "8:vid", "9:gimp","0:code4", "-:code5","=:code6"] 
--
 
-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web")                           -- (0,0a)
--
 
--Search engines to be selected :  [google (g), wikipedia (w) , youtube (y) , maps (m), dictionary (d) , wikipedia (w), bbs (b) ,aur (r), wiki (a) , TPB (t), mininova (n), isohunt (i) ]
--keybinding: hit mod + s + <searchengine>
searchEngineMap method = M.fromList $
       [ ((0, xK_g), method S.google )
       , ((0, xK_y), method S.youtube )
       , ((0, xK_m), method S.maps )
       , ((0, xK_d), method S.dictionary )
       , ((0, xK_w), method S.wikipedia )
       , ((0, xK_h), method S.hoogle )
       , ((0, xK_b), method $ S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
       , ((0, xK_r), method $ S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
       , ((0, xK_a), method $ S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
       ]
 
 
 
-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)
 
    -- opening program launcher / search engine
    , ((modMask , xK_s ), SM.submap $ searchEngineMap $ S.promptSearch myXPConfig)
    , ((modMask .|. shiftMask , xK_s ), SM.submap $ searchEngineMap $ S.selectSearch) 
    ,((modMask , xK_p), shellPrompt myXPConfig)
 
    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)
 
    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)
 
    -- refresh'
    , ((modMask, xK_n ), refresh)
 
    -- focus
    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_Tab), rotSlavesUp)
 
 
    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )
 
    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))
 
    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)
 
    -- mpd controls
    , ((0                       , 0x1008ff16 ), spawn "ncmpcpp prev")
    , ((0                       , 0x1008ff17 ), spawn "ncmpcpp next")
    , ((0                       , 0x1008ff14 ), spawn "ncmpcpp play")
    , ((0                       , 0x1008ff15 ), spawn "ncmpcpp pause")
 
    -- Libnotify
    , ((modMask .|.  shiftMask, xK_b ), spawn "xscreensaver-command -lock")
 
    -- volume control
    , ((0                       , 0x1008ff13 ), spawn "amixer -q set Master 2dB+")
    , ((0                       , 0x1008ff11 ), spawn "amixer -q set Master 2dB-")
    , ((0                       , 0x1008ff12 ), spawn "amixer -q set Master toggle")
 
    -- printscreen 
    , ((modMask .|. shiftMask, xK_s ), spawn "/home/jelle/bin/scrotinput")
 
    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0,xK_minus,xK_equal])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0,xK_minus,xK_equal])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
