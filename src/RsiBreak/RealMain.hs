{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.RealMain (realMain) where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import RsiBreak.Actions (getOrCreateConfigFile)
import RsiBreak.Widget.Clockdown qualified as Clockdown
import RsiBreak.Widget.Timer qualified as Timer

realMain :: IO ()
realMain = do
    timerSettings <- getOrCreateConfigFile
    let model = Clockdown.ClockModel "0:00" Timer.TimerNoWait timerSettings
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    startApp model Clockdown.handleEvent Clockdown.buildUI cfg
  where
    config icon' roboto =
        [ appWindowTitle "RSI Break"
        , appWindowIcon icon'
        , appTheme darkTheme
        , appFontDef "Regular" roboto
        ]
