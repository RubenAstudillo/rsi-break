{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RsiBreak.Widget.Timer where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Monad (when)
import Data.Either (isRight)
import Data.Functor (void)
import Data.Maybe (maybeToList)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Monomer
import RsiBreak.Model.Minutes (toTimeDiff)
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
  deriving (Eq)

stopTimer :: TimerState -> IO ()
stopTimer (TimerWorkWait t) = cancel t
stopTimer (TimerRestWait t) = cancel t
stopTimer _ = return ()

isWorkTime :: TimerState -> Bool
isWorkTime (TimerWorkWait _) = True
isWorkTime _ = False

handleEvent ::
    (NominalDiffTime -> ep) ->
    WidgetEnv TimerModel TimerEvent ->
    WidgetNode TimerModel TimerEvent ->
    TimerModel ->
    TimerEvent ->
    [EventResponse TimerModel TimerEvent sp ep]
handleEvent toEp _wenv _node model@(TimerModel settings timer) evt =
    case evt of
        TimerStateUpdate wstate -> [Model (model{tmState = wstate})]
        TimerReport timediff -> [Report (toEp timediff)]
        TimerStop ->
            [ Task (TimerStateUpdate TimerNoWait <$ stopTimer timer)
            , Event (TimerReport 0)
            ]
        TimerStartWorkTime ->
            fmap (responseIf . not . isWorkTime $ timer) [Event TimerStop, Producer (waitWork settings)]
        TimerStartRestTime ->
            [Producer (waitRest settings)]

buildUI :: WidgetEnv TimerModel TimerEvent -> TimerModel -> WidgetNode TimerModel TimerEvent
buildUI _wenv _model =
    vstack
        [ button "Start" TimerStartWorkTime
        , button "Stop" TimerStop
        ]

popWin :: Maybe String -> IO ()
popWin mstr = do
    (_, _, _, than) <- runInteractiveProcess "rsi-break-popup" (maybeToList mstr) Nothing Nothing
    void $ waitForProcess than

waitTime :: NominalDiffTime -> ProducerHandler TimerEvent
waitTime totalTime handle = getCurrentTime >>= go
  where
    go startTime = do
        currentTime <- getCurrentTime
        let timeDiff = diffUTCTime currentTime startTime
        if timeDiff <= totalTime
            then do
                handle (TimerReport (totalTime - timeDiff))
                threadDelay 500_000
                go startTime
            else handle (TimerReport 0)

waitWork :: TimerSetting -> ProducerHandler TimerEvent
waitWork ts handle = do
    let totalTime = toTimeDiff (_workInterval ts)
    waitThread <- async (waitTime totalTime handle)
    handle (TimerStateUpdate (TimerWorkWait waitThread))
    res <- waitCatch waitThread
    when (isRight res) (handle TimerStartRestTime)

waitRest :: TimerSetting -> ProducerHandler TimerEvent
waitRest ts handle = do
    let totalTime = toTimeDiff (_restInterval ts)
    waitThread <- async (waitTime totalTime handle)
    handle (TimerStateUpdate (TimerRestWait waitThread))
    res <- waitCatch waitThread
    when (isRight res) (handle TimerStartWorkTime)
