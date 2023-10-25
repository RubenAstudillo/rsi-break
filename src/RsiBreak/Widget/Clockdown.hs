{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

{- |
Module      : RsiBreak.Widget.Clockdown
Copyright   : (c) Ruben Astudillo, 2023
License     : BSD-2
Maintainer  : ruben.astud@gmail.com

Main composite of the the application.
-}
module RsiBreak.Widget.Clockdown (
    ClockModel (ClockModel),
    handleEvent,
    buildUI,
) where

import Control.Lens (ALens', lens, makeLensesFor)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Monomer
import RsiBreak.Widget.Settings qualified as Settings
import RsiBreak.Model.Settings qualified as Settings
import RsiBreak.Widget.Timer qualified as Timer

greenBgStyle, blueBgStyle :: StyleState
greenBgStyle = mempty{_sstBgColor = Just (Color 0 128 0 1)}
blueBgStyle = mempty{_sstBgColor = Just (Color 0 0 128 1)}

data ClockModel = ClockModel
    { _cmClock :: Text
    , _cmTimer :: Timer.TimerState
    , _cmSettings :: Settings.TimerSetting
    }
    deriving (Eq)

$(makeLensesFor [("_cmClock", "cmClock"), ("_cmSettings", "cmSettings")] ''ClockModel)

data ClockEvent = ClockUpdate NominalDiffTime | ClockCancelTimer

mainCounter :: Text
mainCounter = "MainCounter"

mainCounterKey :: WidgetKey
mainCounterKey = WidgetKey mainCounter

buildUI :: UIBuilder ClockModel ClockEvent
buildUI _wenv (ClockModel _ timer _) =
    vstack
        [ label "Rsi break!"
        , textField_ cmClock [readOnly] `styleBasic` [countdownStyle timer]
        , spacer
        , composite "settings-parameters" cmSettings Settings.buildUI (Settings.handleEvent ClockCancelTimer)
        , spacer
        , composite "timer" toTimerModel Timer.buildUI (Timer.handleEvent ClockUpdate) `nodeKey` mainCounter
        ]
        `styleBasic` [padding 10]

handleEvent :: EventHandler ClockModel ClockEvent es ep
handleEvent _wenv _node model (ClockUpdate td) =
    let !tdText = fromString (formatTime defaultTimeLocale "%m:%02S" td)
     in [Model (model{_cmClock = tdText}), Request RenderOnce]
handleEvent _ _ _ ClockCancelTimer = [Message mainCounterKey Timer.TimerStop]

countdownStyle :: Timer.TimerState -> StyleState
countdownStyle settings = case settings of
    Timer.TimerWorkWait _ -> greenBgStyle
    Timer.TimerRestWait _ -> blueBgStyle
    _ -> mempty

toTimerModel :: ALens' ClockModel Timer.TimerModel
toTimerModel = lens getter setter
  where
    getter clock = Timer.TimerModel (_cmSettings clock) (_cmTimer clock)
    setter clock timer =
        clock
            { _cmTimer = Timer.tmState timer
            , _cmSettings = Timer.tmSettings timer
            }
