{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (..))
import Monomer
import Paths_rsi_break

main :: IO ()
main = do
    windowIconPath <- fromString <$> getDataFileName "assets/images/icon.png"
    robotoRegularFont <- fromString <$> getDataFileName "assets/fonts/Roboto-Regular.ttf"
    let cfg = config windowIconPath robotoRegularFont
    startApp () (\_ _ _ _ -> []) buildUI cfg
  where
    config icon' roboto =
        [ appWindowTitle "Tutorial 01 - Basics"
        , appWindowIcon icon'
        , appTheme darkTheme
        , appFontDef "Regular" roboto
        ]

buildUI :: WidgetEnv () () -> () -> WidgetNode () ()
buildUI _wenv _model = box_ [alignCenter] (label "Resting time!")
