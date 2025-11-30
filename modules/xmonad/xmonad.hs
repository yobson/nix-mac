import XMonad
import System.Exit (exitSuccess)
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ def
  { terminal = "@wezterm@"
  } `removeKeysP` (map fst keyBindings)
    `additionalKeysP` keyBindings

keyBindings :: [(String, X ())]
keyBindings = 
  [ ("M-<Space>" , spawn "@rofi@ -show drun")
  , ("M-<Return>", spawn "@wezterm@")
  , ("M-S-q"     , kill)
  , ("C-M-q"     , io exitSuccess)
  ]
