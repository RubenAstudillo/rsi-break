{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break
import System.Timeout (timeout)
import Data.Functor (void)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)

main :: IO ()
main = do
    args <- getArgs
    let str = fromMaybe "Resting time!" (listToMaybe args)
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    void . timeout 5_000_000 $ startApp () (\_ _ _ _ -> []) (buildUI str) cfg
  where
    config icon' roboto =
        [ appWindowTitle "Rest now"
        , appWindowIcon icon'
        , appTheme darkTheme
        , appFontDef "Regular" roboto
        ]

buildUI :: String -> WidgetEnv () () -> () -> WidgetNode () ()
buildUI str _wenv _model =
    box_ [alignCenter]
        . animFadeIn_ [autoStart, duration 4_000]
        $ label (fromString str) `styleBasic` [textSize 80.0]
