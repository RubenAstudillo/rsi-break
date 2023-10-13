module RsiBreak.Widget.Settings (
    TimerSetting (..),
    handleEvent,
    buildUI,
) where

import Control.Lens (set)
import Monomer
import RsiBreak.Actions (storeSettingsOnConfigFile)
import RsiBreak.Model.Minutes (Minutes)
import RsiBreak.Model.Settings

data TimerChange = TSENewWorkTime Minutes | TSENewRestTime Minutes
    deriving (Eq, Show)

data TimerSettingEvent = TimerChangeEvent TimerChange | TSENoOp
    deriving (Eq, Show)

handleEvent :: ep -> EventHandler TimerSetting TimerSettingEvent sp ep
handleEvent _onChangeEvent _wenv _node _model TSENoOp = []
handleEvent onChangeEvent _wenv _node model (TimerChangeEvent evt) =
    let newModel = case evt of
            TSENewWorkTime newm -> set workInterval newm model
            TSENewRestTime newm -> set restInterval newm model
     in [ Model newModel
        , Report onChangeEvent
        , Task (TSENoOp <$ storeSettingsOnConfigFile newModel)
        ]

buildUI :: UIBuilder TimerSetting TimerSettingEvent
buildUI _wenv _model =
    vstack
        [ hstack
            [ label "Work time: "
            , numericField_ workInterval [minValue 0, maxValue 300, onChange (TimerChangeEvent . TSENewWorkTime)]
            ]
        , hstack
            [ label "Rest time: "
            , numericField_ restInterval [minValue 0, maxValue 300, onChange (TimerChangeEvent . TSENewRestTime)]
            ]
        ]
