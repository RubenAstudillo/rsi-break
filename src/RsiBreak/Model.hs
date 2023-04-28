{-# LANGUAGE TemplateHaskell #-}

module RsiBreak.Model where

import Control.Concurrent.Async
import Control.Lens
import Data.Text (Text)

type Minutes = Int

data WaitState
  = WorkWait (Async ())
  | RestWait (Async ())
  | NoWait
  deriving (Eq)

data AppModel = AppModel
    { _workInterval :: Minutes
    , _restInterval :: Minutes
    , _currentCountdown :: Text
    , _currentState :: WaitState
    }
  deriving (Eq)

$(makeLenses 'AppModel)
