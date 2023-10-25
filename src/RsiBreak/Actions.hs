{- |
Module      : RsiBreak.Actions
Copyright   : (c) Ruben Astudillo, 2023
License     : BSD-2
Maintainer  : ruben.astud@gmail.com

Actions to be run on change of values such as settings.
-}

module RsiBreak.Actions (getOrCreateConfigFile, storeSettingsOnConfigFile) where

import Control.Monad (unless)
import Data.Ini.Config.Bidir
import Data.Text.IO qualified as TIO (readFile, writeFile)
import RsiBreak.Model.Settings
import System.Directory
import System.FilePath ((</>))

getOrCreateConfigFile :: IO TimerSetting
getOrCreateConfigFile = do
    dir <- getXdgDirectory XdgConfig "rsi-break"
    let file = dir </> "settings.ini"
    settingsFileExist <- doesFileExist file
    unless settingsFileExist $
        createNewInitialSettings dir file
    settingFileContent <- TIO.readFile file
    let eIniSettings = parseIni settingFileContent defaultIni
    case eIniSettings of
        Left _err -> do
            removeDirectoryRecursive dir
            createNewInitialSettings dir file
            pure defSetting
        Right ini' -> pure (getIniValue ini')
  where
    createNewInitialSettings dir file = do
        createDirectory dir
        TIO.writeFile file (serializeIni defaultIni)

defaultIni :: Ini TimerSetting
defaultIni =
    setIniUpdatePolicy
        (defaultUpdatePolicy{updateGeneratedCommentPolicy = CommentPolicyAddFieldComment})
        (ini defSetting timerSettingSpec)

storeSettingsOnConfigFile :: TimerSetting -> IO ()
storeSettingsOnConfigFile updatedSettings = do
    let updatedIni = updateIni updatedSettings defaultIni
    dir <- getXdgDirectory XdgConfig "rsi-break"
    let file = dir </> "settings.ini"
    TIO.writeFile file (serializeIni updatedIni)
