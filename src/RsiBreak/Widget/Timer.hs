{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.Widget.Timer (
    TimerModel(..),
    TimerState (..),
    TimerEvent (..),
    handleEvent,
    buildUI,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (when)
import Data.Either (isRight)
import Data.Functor (void)
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

handleEvent :: (NominalDiffTime -> ep) -> EventHandler TimerModel TimerEvent es ep
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

buildUI :: UIBuilder TimerModel TimerEvent
buildUI _wenv _model =
    vstack
        [ button "Start" TimerStartWorkTime
        , button "Stop" TimerStop
        ]

popWin :: String -> IO ()
popWin str = do
    (_, _, _, than) <- runInteractiveProcess "rsi-break-popup" (pure str) Nothing Nothing
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
waitWork ts handle =
    withAsync (waitTime totalTime handle) $ \waitThr ->
        withAsync (popWin "Working Time!") $ \_popThr -> do
            handle (TimerStateUpdate (TimerWorkWait waitThr))
            res <- waitCatch waitThr
            when (isRight res) (handle TimerStartRestTime)
  where
    totalTime = toTimeDiff (_workInterval ts)

waitRest :: TimerSetting -> ProducerHandler TimerEvent
waitRest ts handle =
    withAsync (waitTime totalTime handle) $ \waitThr ->
        withAsync (popWin "Resting Time!") $ \_popThr -> do
            handle (TimerStateUpdate (TimerRestWait waitThr))
            res <- waitCatch waitThr
            when (isRight res) (handle TimerStartWorkTime)
  where
    totalTime = toTimeDiff (_restInterval ts)
