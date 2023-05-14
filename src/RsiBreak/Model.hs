{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Model where

import Control.Concurrent.Async
import Control.Lens
import Data.Text (Text)
import qualified RsiBreak.Settings as Settings

type Minutes = Int

data WaitState
  = WorkWait (Async ())
  | RestWait (Async ())
  | NoWait
  deriving (Eq)

data AppModel = AppModel
    { _timerSettings :: Settings.TimerSetting
    , _currentCountdown :: Text
    , _currentState :: WaitState
    }
  deriving (Eq)

$(makeLenses 'AppModel)
