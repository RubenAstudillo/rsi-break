{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.RealMain where

import Monomer
import RsiBreak.Controller
import RsiBreak.Model
import RsiBreak.View

realMain :: IO ()
realMain =
    startApp model handleEvent buildUI config
  where
    config =
        [ appWindowTitle "Tutorial 01 - Basics"
        , appWindowIcon "./assets/images/icon.png"
        , appTheme darkTheme
        , appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
        ]
    model = AppModel 5 1 "0:00" NoWait
