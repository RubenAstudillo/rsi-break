{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.Controller where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Lens
import Control.Monad (when)
import Data.Either (isRight)
import Data.Function (fix)
import Data.Maybe (maybeToList)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Monomer
import RsiBreak.Model

data AppEvent
    = AppNewWorkTime Minutes
    | AppNewRestTime Minutes
    | AppStartWorkTime
    | AppUpdateCountDown NominalDiffTime
    | AppStartRestTimer
    | AppUpdateWaitState WaitState
    | AppStopTimer

handleEvent ::
    WidgetEnv AppModel AppEvent ->
    WidgetNode AppModel AppEvent ->
    AppModel ->
    AppEvent ->
    [AppEventResponse AppModel AppEvent]
handleEvent wenv _node model evt =
    case evt of
        AppNewWorkTime newS -> [Model (set workInterval newS model)]
        AppNewRestTime newS -> [Model (set restInterval newS model)]
        AppUpdateWaitState wstate ->
            [Model (set currentState wstate model)]
        AppUpdateCountDown td ->
            let tdText = fromString (formatTime defaultTimeLocale "%m:%02S" td)
             in [Model (set currentCountdown tdText model)]
        AppStartWorkTime ->
            let ws = view workInterval model
                curWaitState = view currentState model
             in if curWaitState == NoWait
                    then Producer (waitSetup ws WorkWait AppStartRestTimer) : updateCounterReq wenv
                    else []
        AppStartRestTimer ->
            let ws = view restInterval model
             in Producer (waitSetup ws RestWait AppStartWorkTime) : stopCounterReq wenv
        AppStopTimer -> stopTimerSetup model

mainCounter :: Text
mainCounter = "MainCounter"

mainCounterUpdateRef :: WidgetEnv model event -> [WidgetId]
mainCounterUpdateRef wenv = maybeToList . widgetIdFromKey wenv $ WidgetKey mainCounter

updateCounterReq :: WidgetEnv model event -> [EventResponse s e sp ep]
updateCounterReq wenv = (\i -> Request (RenderEvery i 500 Nothing)) <$> mainCounterUpdateRef wenv

stopCounterReq :: WidgetEnv model event -> [EventResponse s e sp ep]
stopCounterReq wenv = Request . RenderStop <$> mainCounterUpdateRef wenv

waitSetup :: Minutes -> (Async () -> WaitState) -> AppEvent -> (AppEvent -> IO ()) -> IO ()
waitSetup totalTimeMin waitStateWrap thenEv handler = do
    let
        totalTimeDiff :: NominalDiffTime
        totalTimeDiff = fromIntegral (totalTimeMin * 60)
    startTime <- getCurrentTime
    waitThr <- async . fix $ \again -> do
        curTime <- getCurrentTime
        let timeDiff = diffUTCTime curTime startTime
        if timeDiff <= totalTimeDiff
            then do
                handler (AppUpdateCountDown (totalTimeDiff - timeDiff))
                threadDelay 500_000
                again
            else handler (AppUpdateCountDown 0)
    handler (AppUpdateWaitState (waitStateWrap waitThr))
    res <- waitCatch waitThr
    when (isRight res) (handler thenEv)

stopTimerSetup :: AppModel -> [AppEventResponse AppModel AppEvent]
stopTimerSetup model =
    let mthr = case view currentState model of
            WorkWait t -> [t]
            RestWait t -> [t]
            _otherwise -> []
        delTimer = map (\t -> Task (AppUpdateWaitState NoWait <$ cancel t)) mthr
     in delTimer ++ [Event (AppUpdateCountDown 0)]
