{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Model.Settings where

import Control.Lens (makeLenses)
import Data.Ini.Config.Bidir
import RsiBreak.Model.Minutes (Minutes)

data TimerSetting = TimerSetting
    { _workInterval :: Minutes
    , _restInterval :: Minutes
    }
    deriving (Eq, Show)

$(makeLenses 'TimerSetting)

defSetting :: TimerSetting
defSetting = TimerSetting 20 10

timerSettingSpec :: IniSpec TimerSetting ()
timerSettingSpec =
    section "TimerSetting" $ do
        workInterval
            .= field "workInterval" number
            & comment ["The desired work interval in minutes"]
        restInterval
            .= field "restInterval" number
            & comment ["The desired rest interval in minutes"]
