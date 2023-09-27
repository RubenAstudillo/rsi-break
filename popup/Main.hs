{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Data.Functor (void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import System.Environment (getArgs)
import System.IO (IOMode (..), openFile)
import System.Process
import System.Timeout (timeout)

main :: IO ()
main = do
    args <- getArgs
    let str = fromMaybe "Resting time!" (listToMaybe args)
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    _ <- forkIO dingBell
    void . timeout 5_000_000 $ startApp () (\_ _ _ _ -> []) (buildUI str) cfg
  where
    config icon' roboto =
        [ appWindowTitle "Rest now"
        , appWindowIcon icon'
        , appTheme darkTheme
        , appFontDef "Regular" roboto
        ]

buildUI :: String -> WidgetEnv () () -> () -> WidgetNode () ()
buildUI str _wenv _model =
    box_ [alignCenter]
        . animFadeIn_ [autoStart, duration 4_000]
        $ label (fromString str) `styleBasic` [textSize 80.0]

dingBell :: IO ()
dingBell = do
    outHandle <- openFile "/dev/null" WriteMode
    inHandle <- openFile "/dev/null" ReadMode
    soundPath <- getDataFileName "assets/sound/bell.mp3"
    let process =
            (shell (unwords ["mpv", "--no-video", soundPath]))
                { std_in = UseHandle inHandle
                , std_out = UseHandle outHandle
                , std_err = UseHandle outHandle
                }
    (_, _, _, phandle) <- createProcess process
    void (waitForProcess phandle)
