import XMonad
import System.Exit (exitSuccess)
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad $ def
  { terminal = "@wezterm@"
  , startupHook = startUp
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
