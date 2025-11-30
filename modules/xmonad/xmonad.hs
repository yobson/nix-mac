import XMonad
import System.Exit (exitSuccess)
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Reflect

import Data.Kind

import qualified DBus as D
import qualified DBus.Client as D

main :: IO ()
main = do
  dbus <- mkDbusClient
  xmonad $ docks $ def
    { terminal = "@wezterm@"
    , startupHook = startUp
    , layoutHook  = layout
    , logHook     = polybar dbus
    } `removeKeysP` (map fst keyBindings)
      `additionalKeysP` keyBindings

keyBindings :: [(String, X ())]
keyBindings = 
  [ ("M-<Space>" , spawn "@rofi@ -show drun")
  , ("M-<Return>", spawn "@wezterm@")
  , ("M-S-q"     , kill)
  , ("C-M-q"     , io exitSuccess)
  ]

startUp :: X ()
startUp = do
  spawnOnce "@feh@ --bg-fill @wallpaper@"

layout = avoidStruts
       $ gaps [(U, 5), (R, 5), (D,5), (L,5)]
       $ spacing 5
       $ reflectHoriz $ Tall 1 (3/100) (1/2)

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

polybar :: D.Client -> X ()
polybar dbus = dynamicLogWithPP $ def
  { ppOutput  = dbusOutput dbus
  , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
  , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
  , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
  , ppHidden  = wrap " " " "
  , ppWsSep   = ""
  , ppSep     = " : "
  , ppTitle   = shorten 40
  }
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#504945"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
