{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Widget.Clockdown where

import Control.Lens (ALens', lens, makeLenses)
import Data.Text
import RsiBreak.Widget.Settings (TimerSetting)
import RsiBreak.Widget.Timer (TimerModel (..), TimerState)

data ClockModel = ClockModel
    { _cmClock :: Text
    , _cmTimer :: TimerState
    , _cmSettings :: TimerSetting
    }
    deriving (Eq)

$(makeLenses ''ClockModel)

toTimerModel :: ALens' ClockModel TimerModel
toTimerModel = lens getter setter
  where
    getter clock = TimerModel (_cmSettings clock) (_cmTimer clock)
    setter clock timer = clock{_cmTimer = tmState timer} -- no modify settings
