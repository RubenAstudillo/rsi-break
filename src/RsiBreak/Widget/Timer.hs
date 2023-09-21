{-# LANGUAGE StrictData #-}

module RsiBreak.Widget.Timer where

import Control.Concurrent.Async (Async, cancel)
import Data.Functor (void)
import Data.Maybe (maybeToList)
import Monomer
import RsiBreak.Widget.Settings (TimerSetting (..))
import System.Process (runInteractiveProcess, waitForProcess)

data TimerEvent
    = TimerStartWorkTime
    | TimerStartRestTime
    | TimerStop
    | TimerStateUpdate TimerState

data TimerState
    = TimerWorkWait (Async ())
    | TimerRestWait (Async ())
    | TimerNoWait
    deriving (Eq)

data TimerModel = TimerModel {tmSettings :: TimerSetting, tmState :: TimerState}

stopTimer :: TimerState -> IO ()
stopTimer (TimerWorkWait t) = cancel t
stopTimer (TimerRestWait t) = cancel t
stopTimer _ = return ()

handleEvent ::
    WidgetEnv TimerModel TimerEvent ->
    WidgetNode TimerModel TimerEvent ->
    TimerModel ->
    TimerEvent ->
    [AppEventResponse TimerModel TimerEvent]
handleEvent wenv _node model evt =
    case evt of
        TimerStateUpdate wstate -> [Model (model{tmState = wstate})]
        TimerStop -> [Task (TimerStateUpdate TimerNoWait <$ stopTimer (tmState model))]
        _ -> undefined

popWin :: Maybe String -> IO ()
popWin mstr = do
    (_, _, _, than) <- runInteractiveProcess "rsi-break-popup" (maybeToList mstr) Nothing Nothing
    void $ waitForProcess than
