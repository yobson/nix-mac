{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}

module FileSystem 
( startFileSystem
, stopFileSystem
) where

import Network.NineP
import Control.Concurrent
import Control.Monad.Fix
import Control.Monad
import System.Directory

import System.Process.Typed

socketPath :: FilePath
socketPath = "/tmp/.xmonad.sock"

fsPath :: FilePath
fsPath = "/tmp/.xmonad-fs"

fileSystem :: FileSystem ()
fileSystem = 
  dir "/" $ do
    file "hello-from-xmonad"
    file "hello-from-9p"

startFileSystem :: IO ThreadId
startFileSystem = do
  forkIO $ serveFileSystem (defaultConf "tcp!127.0.0.1!8080" {- $ UnixDomain socketPath -}) fileSystem

mountFS :: IO ()
mountFS = fix $ \loop -> do
  test <- doesPathExist socketPath
  if test 
    then do
      runProcess $ shell $ unwords ["mount -t 9p -o trans=unix", socketPath, fsPath]
      return ()
    else threadDelay 1000000 >> loop

stopFileSystem :: IO ()
stopFileSystem = do
  runProcess $ shell $ unwords ["umount", fsPath]
  return ()
