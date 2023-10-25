{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : RsiBreak.Widget.Timer
Copyright   : (c) Ruben Astudillo, 2023
License     : BSD-2
Maintainer  : ruben.astud@gmail.com

Composite holding the threads with the time counter.
-}
module RsiBreak.Widget.Timer (
    TimerModel (..),
    TimerState (..),
    TimerEvent (TimerStop),
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
import RsiBreak.Model.Settings qualified as Settings (TimerSetting (..))
import System.Process (runInteractiveProcess, waitForProcess)

data TimerEvent
    = TimerStartWorkTime
    | TimerStartRestTime
    | TimerStop
    | TimerStateUpdate TimerState
    | TimerReport NominalDiffTime

-- | State data type that will be read and __written__ by this composite.
data TimerState
    = TimerWorkWait (Async ())
    | TimerRestWait (Async ())
    | TimerNoWait
    deriving (Eq)

{- | State data type that holds a read-only reference to
     @Settings.TimerSetting@. We will read such reference when launching a
     timer.
-}
data TimerModel = TimerModel {tmSettings :: Settings.TimerSetting, tmState :: TimerState}
    deriving (Eq)

stopTimer :: TimerState -> IO ()
stopTimer (TimerWorkWait t) = cancel t
stopTimer (TimerRestWait t) = cancel t
stopTimer _ = return ()

isWorkTime :: TimerState -> Bool
isWorkTime (TimerWorkWait _) = True
isWorkTime _ = False

handleEvent :: (NominalDiffTime -> ep) -- ^ Wrapper for event to report on parent composite
            -> EventHandler TimerModel TimerEvent es ep
handleEvent toEp _wenv _node model@(TimerModel settings timer) evt =
    case evt of
        TimerStateUpdate wstate -> [Model (model{tmState = wstate})]
        TimerReport timediff -> [Report (toEp timediff)]
        TimerStop ->
            [ Task (TimerStateUpdate TimerNoWait <$ stopTimer timer)
            , Event (TimerReport 0)
            ]
        TimerStartWorkTime
            | isWorkTime timer -> [] -- no relaunch
            | otherwise -> [Event TimerStop, Producer (waitWork settings)]
        TimerStartRestTime
            | isWorkTime timer -> [Event TimerStop, Producer (waitRest settings)]
            | otherwise -> [] -- Do not jump to rest if the timer is stopped.

buildUI :: UIBuilder TimerModel TimerEvent
buildUI _wenv _model =
    vstack
        [ button "Start" TimerStartWorkTime
        , button "Finish just this cycle" TimerStartRestTime
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
                handle $! TimerReport (totalTime - timeDiff)
                threadDelay 500_000
                go startTime
            else handle (TimerReport 0)

waitWork :: Settings.TimerSetting -> ProducerHandler TimerEvent
waitWork ts handle =
    withAsync (waitTime totalTime handle) $ \waitThr ->
        withAsync (popWin "Working Time!") $ \_popThr -> do
            handle (TimerStateUpdate (TimerWorkWait waitThr))
            res <- waitCatch waitThr
            when (isRight res) (handle TimerStartRestTime)
  where
    totalTime = toTimeDiff (Settings._workInterval ts)

waitRest :: Settings.TimerSetting -> ProducerHandler TimerEvent
waitRest ts handle =
    withAsync (waitTime totalTime handle) $ \waitThr ->
        withAsync (popWin "Resting Time!") $ \_popThr -> do
            handle (TimerStateUpdate (TimerRestWait waitThr))
            res <- waitCatch waitThr
            when (isRight res) (handle TimerStartWorkTime)
  where
    totalTime = toTimeDiff (Settings._restInterval ts)
