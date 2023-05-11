{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.RealMain where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import RsiBreak.Controller
import RsiBreak.Model
import RsiBreak.View

realMain :: IO ()
realMain = do
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    startApp model handleEvent buildUI cfg
  where
    config icon' roboto =
        [ appWindowTitle "Tutorial 01 - Basics"
        , appWindowIcon icon'
        , appTheme darkTheme
        , appFontDef "Regular" roboto
        ]
    model = AppModel 30 5 "0:00" NoWait
