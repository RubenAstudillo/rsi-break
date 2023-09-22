{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Widget.Clockdown where

import Control.Lens (ALens', lens, makeLenses)
import Data.String (IsString (fromString))
import Data.Text
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Monomer
import RsiBreak.Widget.Settings (TimerSetting)
import qualified RsiBreak.Widget.Settings as Settings
import RsiBreak.Widget.Timer (TimerModel (..), TimerState (..))
import qualified RsiBreak.Widget.Timer as Timer

greenBgStyle, blueBgStyle :: StyleState
greenBgStyle = mempty{_sstBgColor = Just (Color 0 128 0 1)}
blueBgStyle = mempty{_sstBgColor = Just (Color 0 0 128 1)}

data ClockModel = ClockModel
    { _cmClock :: Text
    , _cmTimer :: TimerState
    , _cmSettings :: TimerSetting
    }
    deriving (Eq)

$(makeLenses ''ClockModel)

newtype ClockEvent = ClockUpdate NominalDiffTime

mainCounter :: Text
mainCounter = "MainCounter"

mainCounterKey :: WidgetKey
mainCounterKey = WidgetKey mainCounter

buildUI :: WidgetEnv ClockModel ClockEvent -> ClockModel -> WidgetNode ClockModel ClockEvent
buildUI _wenv (ClockModel _clock timer _) =
    let settingProp = Settings.CancelTimersOn Timer.TimerStop mainCounterKey
     in vstack
            [ label "Rsi break!"
            , textField_ cmClock [readOnly] `nodeKey` mainCounter `styleBasic` [countdownStyle timer]
            , spacer
            , composite "settings-parameters" cmSettings Settings.buildUI (Settings.handleEvent settingProp)
            , spacer
            , composite "timer" toTimerModel Timer.buildUI (Timer.handleEvent ClockUpdate)
            ]

handleEvent :: WidgetEnv ClockModel ClockEvent -> WidgetNode ClockModel ClockEvent -> ClockModel -> ClockEvent -> [AppEventResponse ClockModel ClockEvent]
handleEvent _wenv _node model (ClockUpdate td) =
    let tdText = fromString (formatTime defaultTimeLocale "%m:%02S" td)
     in [ Model (model{_cmClock = tdText})
        , Request RenderOnce
        ]

countdownStyle :: TimerState -> StyleState
countdownStyle settings = case settings of
    TimerWorkWait _ -> blueBgStyle
    TimerRestWait _ -> greenBgStyle
    _ -> mempty

toTimerModel :: ALens' ClockModel TimerModel
toTimerModel = lens getter setter
  where
    getter clock = TimerModel (_cmSettings clock) (_cmTimer clock)
    setter clock timer = clock{_cmTimer = tmState timer} -- no modify settings
