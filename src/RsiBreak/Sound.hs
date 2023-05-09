module RsiBreak.Sound where

import Paths_rsi_break
import System.IO
import System.Process (ProcessHandle, runInteractiveProcess)

dingBell :: IO (Handle, Handle, Handle, ProcessHandle)
dingBell = do
    soundPath <- getDataFileName "assets/sound/bell.mp3"
    runInteractiveProcess "mpv" ["--no-video", soundPath] Nothing Nothing
