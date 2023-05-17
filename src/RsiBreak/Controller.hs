{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module RsiBreak.Controller where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Lens
import Control.Monad (void, when)
import Data.Either (isRight)
import Data.Function (fix)
import Data.Maybe (maybeToList)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Monomer
import RsiBreak.Model
import qualified RsiBreak.Settings as Settings
import RsiBreak.Sound (dingBell)
import System.Process

data AppEvent
    = AppStartWorkTime
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
        AppUpdateWaitState wstate ->
            [Model (set currentState wstate model)]
        AppUpdateCountDown td ->
            let tdText = fromString (formatTime defaultTimeLocale "%m:%02S" td)
             in [Model (set currentCountdown tdText model)]
        AppStartWorkTime ->
            let ws = view (timerSettings . Settings.workInterval) model
                effect =
                    stopCounterReq wenv
                        ++ updateCounterReq wenv
                        ++ [ (Producer . const . popWin . pure) "Working Time!"
                           , Producer (waitSetup ws WorkWait AppStartRestTimer)
                           ]
             in case view currentState model of
                    WorkWait _ -> []
                    RestWait _ -> effect
                    NoWait -> effect
        AppStartRestTimer ->
            let ws = view (timerSettings . Settings.restInterval) model
                effect =
                    stopCounterReq wenv
                        ++ updateCounterReq wenv
                        ++ [ Producer (const (popWin Nothing))
                           , Producer (waitSetup ws RestWait AppStartWorkTime)
                           ]
             in case view currentState model of
                    RestWait _ -> error "Should be impossible."
                    NoWait -> []
                    WorkWait _ -> effect
        AppStopTimer -> stopTimerSetup model ++ stopCounterReq wenv

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
            else do
                (_, _, _, playThr) <- dingBell
                _ <- waitForProcess playThr
                handler (AppUpdateCountDown 0)
    handler (AppUpdateWaitState (waitStateWrap waitThr))
    res <- waitCatch waitThr
    when (isRight res) (handler thenEv)

popWin :: Maybe String -> IO ()
popWin mstr = do
    (_, _, _, than) <- runInteractiveProcess "rsi-break-popup" (maybeToList mstr) Nothing Nothing
    void $ waitForProcess than

stopTimerSetup :: AppModel -> [AppEventResponse AppModel AppEvent]
stopTimerSetup model =
    let mthr = case view currentState model of
            WorkWait t -> [t]
            RestWait t -> [t]
            _otherwise -> []
        delTimer = map (\t -> Task (AppUpdateWaitState NoWait <$ cancel t)) mthr
     in delTimer ++ [Event (AppUpdateCountDown 0)]
