{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Settings where

import Control.Lens (makeLenses, set)
import Monomer

type Minutes = Int

data TimerSetting = TimerSetting
    { _workInterval :: Minutes
    , _restInterval :: Minutes
    }
    deriving (Eq, Show)

$(makeLenses 'TimerSetting)

data TimerSettingEvent
    = TSENewWorkTime Minutes
    | TSENewRestTime Minutes
    deriving (Eq, Show)

handleEvent ::
    WidgetEnv TimerSetting TimerSettingEvent ->
    WidgetNode TimerSetting TimerSettingEvent ->
    TimerSetting ->
    TimerSettingEvent ->
    [EventResponse TimerSetting TimerSettingEvent sp ep]
handleEvent _wenv _node model evt =
    case evt of
        TSENewWorkTime newm -> [Model (set workInterval newm model)]
        TSENewRestTime newm -> [Model (set restInterval newm model)]

buildUI ::
    WidgetEnv TimerSetting TimerSettingEvent ->
    TimerSetting ->
    WidgetNode TimerSetting TimerSettingEvent
buildUI _wenv _model =
    vstack
        [ hstack
            [ label "Work time: "
            , numericField_ workInterval [minValue 0, maxValue 300, onChange TSENewWorkTime]
            ]
        , hstack
            [ label "Rest time: "
            , numericField_ restInterval [minValue 0, maxValue 300, onChange TSENewRestTime]
            ]
        ]
