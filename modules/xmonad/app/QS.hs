
module QS 
( sendIPC
, startQS
) where

import XMonad
import XMonad.Util.SpawnOnce

type Target   = String
type Function = String

sendIPC :: Show a => Target -> Function -> [a] -> IO ()
sendIPC target function args = spawn $ unwords $ 
  ["@quickshell@ ipc call", target, function] ++ map show args

startQS :: X ()
startQS = spawnOnce "@quickshell@"
