{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Widget.Settings
  ( TimerSetting(..)
  , handleEvent
  , buildUI
  ) where

import Control.Lens (makeLenses, set)
import Monomer
import RsiBreak.Model.Minutes (Minutes)

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
    ep ->
    WidgetEnv TimerSetting TimerSettingEvent ->
    WidgetNode TimerSetting TimerSettingEvent ->
    TimerSetting ->
    TimerSettingEvent ->
    [EventResponse TimerSetting TimerSettingEvent sp ep]
handleEvent onChangeEvent _wenv _node model evt =
    let changeModel = case evt of
            TSENewWorkTime newm -> Model (set workInterval newm model)
            TSENewRestTime newm -> Model (set restInterval newm model)
     in [changeModel, Report onChangeEvent]

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
