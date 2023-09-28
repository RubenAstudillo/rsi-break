module RsiBreak.Widget.Settings (
    TimerSetting (..),
    handleEvent,
    buildUI,
) where

import Control.Applicative (Alternative (empty))
import Control.Lens (set)
import Monomer
import RsiBreak.Actions (storeSettingsOnConfigFile)
import RsiBreak.Model.Minutes (Minutes)
import RsiBreak.Model.Settings

data TimerSettingEvent = TSENewWorkTime Minutes | TSENewRestTime Minutes
    deriving (Eq, Show)

handleEvent :: ep -> EventHandler TimerSetting TimerSettingEvent sp ep
handleEvent onChangeEvent _wenv _node model evt =
    let newModel = case evt of
            TSENewWorkTime newm -> set workInterval newm model
            TSENewRestTime newm -> set restInterval newm model
     in [ Model newModel
        , Report onChangeEvent
        , Task (storeSettingsOnConfigFile newModel *> empty)
        ]

buildUI :: UIBuilder TimerSetting TimerSettingEvent
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
