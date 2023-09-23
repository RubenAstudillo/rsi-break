{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Widget.Clockdown (
    ClockModel (ClockModel),
    handleEvent,
    buildUI,
) where

import Control.Lens (ALens', lens, makeLensesFor)
import Data.String (IsString (fromString))
import Data.Text
import Data.Time (NominalDiffTime, defaultTimeLocale, formatTime)
import Monomer
import qualified RsiBreak.Widget.Settings as Settings
import RsiBreak.Widget.Timer (TimerModel (..), TimerState (..))
import qualified RsiBreak.Widget.Timer as Timer

greenBgStyle, blueBgStyle :: StyleState
greenBgStyle = mempty{_sstBgColor = Just (Color 0 128 0 1)}
blueBgStyle = mempty{_sstBgColor = Just (Color 0 0 128 1)}

data ClockModel = ClockModel
    { _cmClock :: Text
    , _cmTimer :: TimerState
    , _cmSettings :: Settings.TimerSetting
    }
    deriving (Eq)

$(makeLensesFor [("_cmClock", "cmClock"), ("_cmSettings", "cmSettings")] ''ClockModel)

data ClockEvent = ClockUpdate NominalDiffTime | ClockCancelTimer

mainCounter :: Text
mainCounter = "MainCounter"

mainCounterKey :: WidgetKey
mainCounterKey = WidgetKey mainCounter

buildUI :: WidgetEnv ClockModel ClockEvent -> ClockModel -> WidgetNode ClockModel ClockEvent
buildUI _wenv (ClockModel _ timer _) =
    vstack
        [ label "Rsi break!"
        , textField_ cmClock [readOnly] `styleBasic` [countdownStyle timer]
        , spacer
        , composite "settings-parameters" cmSettings Settings.buildUI (Settings.handleEvent ClockCancelTimer)
        , spacer
        , composite "timer" toTimerModel Timer.buildUI (Timer.handleEvent ClockUpdate) `nodeKey` mainCounter
        ] `styleBasic` [padding 10]

handleEvent ::
    WidgetEnv ClockModel ClockEvent ->
    WidgetNode ClockModel ClockEvent ->
    ClockModel ->
    ClockEvent ->
    [AppEventResponse ClockModel ClockEvent]
handleEvent _wenv _node model (ClockUpdate td) =
    let tdText = fromString (formatTime defaultTimeLocale "%m:%02S" td)
     in [Model (model{_cmClock = tdText}), Request RenderOnce]
handleEvent _ _ _ ClockCancelTimer = [Message mainCounterKey Timer.TimerStop]

countdownStyle :: TimerState -> StyleState
countdownStyle settings = case settings of
    TimerWorkWait _ -> greenBgStyle
    TimerRestWait _ -> blueBgStyle
    _ -> mempty

toTimerModel :: ALens' ClockModel TimerModel
toTimerModel = lens getter setter
  where
    getter clock = TimerModel (_cmSettings clock) (_cmTimer clock)
    setter clock timer = clock{_cmTimer = tmState timer, _cmSettings = tmSettings timer}
