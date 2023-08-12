{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.View where

import Control.Lens (view)
import Monomer
import RsiBreak.Controller
import RsiBreak.Model
import qualified RsiBreak.Settings as Settings

greenBgStyle, blueBgStyle :: StyleState
greenBgStyle = mempty{_sstBgColor = Just (Color 0 128 0 1)}
blueBgStyle = mempty{_sstBgColor = Just (Color 0 0 128 1)}

buildUI ::
    WidgetEnv AppModel AppEvent ->
    AppModel ->
    WidgetNode AppModel AppEvent
buildUI _wenv model =
    vstack
        [ label "Rsi Break!"
        , textField_ currentCountdown [readOnly] `nodeKey` mainCounter `styleBasic` currentCountdownColor
        , spacer
        , composite "settings-param" timerSettings Settings.buildUI (Settings.handleEvent AppStopTimer)
        , spacer
        , button "Start" AppStartWorkTime
        , button "Stop" AppStopTimer
        ]
        `styleBasic` [padding 10]
  where
    currentCountdownColor =
        case view currentState model of
            RestWait _ -> [blueBgStyle]
            WorkWait _ -> [greenBgStyle]
            NoWait -> []
