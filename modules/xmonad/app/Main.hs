{-# LANGUAGE LambdaCase #-}
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
import XMonad.Hooks.EwmhDesktops

import Data.Kind

import Rofi
import Monitor
import QS

main :: IO ()
main = do
  xmonad $ ewmh $ docks $ def
    { terminal = "@terminal@"
    , startupHook = startUp
    , layoutHook  = layout
    , logHook     = polybar
    } `removeKeysP` (map fst keyBindings)
      `additionalKeysP` keyBindings

keyBindings :: [(String, X ())]
keyBindings = 
  [ ("M-<Space>" , spawn "@rofi@ -show drun")
  , ("M-<Return>", spawn "@terminal@")
  , ("M-S-q"     , kill)
  , ("C-M-q"     , io exitSuccess)
  , ("C-M-r"     , selectResolution)
  ]

startUp :: X ()
startUp = do
  spawnOnce "@feh@ --bg-fill @wallpaper@"
  startQS

layout = avoidStruts
       $ gaps [(U, 5), (R, 5), (D,5), (L,5)]
       $ spacing 5
       $ reflectHoriz $ Tall 1 (3/100) (1/2)

polybar :: X ()
polybar = dynamicLogWithPP $ def
  { ppOutput  = sendIPC "bar" "setLog" . (:[])
  , ppCurrent = wrap "[" "]"
--  , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
--  , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
--  , ppHidden  = wrap " " " "
--  , ppWsSep   = ""
--  , ppSep     = " : "
  , ppTitle   = shorten 40
  }

selectResolution :: X ()
selectResolution = liftIO $ do
  mon <- getMonitors >>= rofi "Which Monitor?"
  case mon of
    Nothing -> return ()
    Just mon -> do
      res <- getResolutionOptions mon >>= rofi "Resolution"
      maybe (return ()) (setResolution mon) res
