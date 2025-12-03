
module Rofi (rofi) where

import System.Process.Typed
import Data.ByteString.Lazy.Char8 (unpack, pack)

rofi :: String -> [String] -> IO (Maybe String)
rofi prompt opts = do
  let rofiConf = setStdin (byteStringInput $ pack (unlines opts))
               $ proc "@rofi@" ["-dmenu", "-p", prompt]
  (c, out) <- readProcessStdout rofiConf
  case c of
    ExitFailure _ -> return Nothing
    ExitSuccess   -> return $ Just $ init $ unpack out
  
