{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.RealMain where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import qualified RsiBreak.Widget.Clockdown as Clockdown
import qualified RsiBreak.Widget.Settings as Settings
import qualified RsiBreak.Widget.Timer as Timer
-- import RsiBreak.Controller
-- import RsiBreak.Model
-- import RsiBreak.View
-- import qualified RsiBreak.Settings as Settings

realMain :: IO ()
realMain = do
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
    model = Clockdown.ClockModel "0:00" Timer.TimerNoWait (Settings.TimerSetting 20 10)

-- realMain :: IO ()
-- realMain = do
--     windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
--     robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
--     let cfg = config windowIconPath robotoRegularFont
--     startApp model handleEvent buildUI cfg
--   where
--     config icon' roboto =
--         [ appWindowTitle "RSI Break"
--         , appWindowIcon icon'
--         , appTheme darkTheme
--         , appFontDef "Regular" roboto
--         ]
--     model = AppModel (Settings.TimerSetting 20 10) "0:00" NoWait
