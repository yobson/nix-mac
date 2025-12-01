{-# LANGUAGE OverloadedStrings #-}
module Monitor where

import Data.List
import System.Process.Typed
import Data.ByteString.Lazy.Char8 (unpack, pack)

getMonitors :: IO [String]
getMonitors = do
  (ec, out) <- readProcessStdout "xrandr | grep ' connected' | awk '{print $1}'"
  case ec of
    ExitFailure _ -> return []
    ExitSuccess -> return $ lines $ unpack out

getResolutionOptions :: String -> IO [String]
getResolutionOptions mon = do
  (ec, out) <- readProcessStdout
                  $ shell $ "xrandr | grep -A15 '" <> mon <> "' | grep -oE '[0-9]+x[0-9]+'"
  case ec of
    ExitFailure _ -> return []
    ExitSuccess -> return $ nub $ lines $ unpack out

setResolution :: String -> String -> IO ()
setResolution mon res = do
  runProcess $ shell $ unwords ["xrandr --output", mon ,"--mode ", res]
  return ()
