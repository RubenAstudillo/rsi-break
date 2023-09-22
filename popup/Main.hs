{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import System.Timeout (timeout)
import Data.Functor (void)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Process (runInteractiveProcess, waitForProcess)
import System.Exit (ExitCode)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    args <- getArgs
    let str = fromMaybe "Resting time!" (listToMaybe args)
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    _ <- forkIO $ void dingBell
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

dingBell :: IO ExitCode
dingBell = do
    soundPath <- getDataFileName "assets/sound/bell.mp3"
    (_, _, _, phandle) <- runInteractiveProcess "mpv" ["--no-video", soundPath] Nothing Nothing
    waitForProcess phandle
