{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Widget.Settings
  ( TimerSetting(..)
  , SettingsProps(..)
  , handleEvent
  , buildUI
  ) where

import Control.Lens (makeLenses, set)
import Data.Typeable (Typeable)
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

data SettingsProps where
    CancelTimersOn :: Typeable i => i -> WidgetKey -> SettingsProps

propToMessage :: SettingsProps -> EventResponse s e sp ep
propToMessage (CancelTimersOn ev key) = Message key ev

handleEvent ::
    SettingsProps ->
    WidgetEnv TimerSetting TimerSettingEvent ->
    WidgetNode TimerSetting TimerSettingEvent ->
    TimerSetting ->
    TimerSettingEvent ->
    [EventResponse TimerSetting TimerSettingEvent sp ep]
handleEvent prop _wenv _node model evt =
    let changeModel = case evt of
            TSENewWorkTime newm -> Model (set workInterval newm model)
            TSENewRestTime newm -> Model (set restInterval newm model)
     in [changeModel, propToMessage prop]

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
