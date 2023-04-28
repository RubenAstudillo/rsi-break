{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.View where

import Control.Lens (view)
import Monomer
import RsiBreak.Controller
import RsiBreak.Model

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
        , hstack
            [ label "Work time: "
            , numericField_ workInterval [minValue 0, maxValue 300, onChange AppNewWorkTime]
            ]
        , hstack
            [ label "Rest time: "
            , numericField_ restInterval [minValue 0, maxValue 300, onChange AppNewRestTime]
            ]
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
