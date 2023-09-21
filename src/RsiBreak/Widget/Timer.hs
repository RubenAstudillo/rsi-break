{-# LANGUAGE StrictData #-}

module RsiBreak.Widget.Timer where

import Control.Concurrent.Async (Async, cancel)
import Data.Functor (void)
import Data.Maybe (maybeToList)
import Data.Time (NominalDiffTime)
import Monomer
import RsiBreak.Widget.Settings (TimerSetting (..))
import System.Process (runInteractiveProcess, waitForProcess)

data TimerEvent
    = TimerStartWorkTime
    | TimerStartRestTime
    | TimerStop
    | TimerStateUpdate TimerState
    | TimerReport NominalDiffTime

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
    (NominalDiffTime -> ep) ->
    WidgetEnv TimerModel TimerEvent ->
    WidgetNode TimerModel TimerEvent ->
    TimerModel ->
    TimerEvent ->
    [EventResponse TimerModel TimerEvent sp ep]
handleEvent toEp wenv _node model evt =
    case evt of
        TimerStateUpdate wstate -> [Model (model{tmState = wstate})]
        TimerReport timediff -> [Report (toEp timediff)]
        TimerStop ->
            [ Task (TimerStateUpdate TimerNoWait <$ stopTimer (tmState model))
            , Event (TimerReport 0)
            ]
        _ -> undefined

popWin :: Maybe String -> IO ()
popWin mstr = do
    (_, _, _, than) <- runInteractiveProcess "rsi-break-popup" (maybeToList mstr) Nothing Nothing
    void $ waitForProcess than

-- waitTime :: NominalDiffTime ->
