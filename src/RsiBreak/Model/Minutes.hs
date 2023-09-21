module RsiBreak.Model.Minutes (Minutes, toTimeDiff) where

import Data.Time (NominalDiffTime)

type Minutes = Int

toTimeDiff :: Minutes -> NominalDiffTime
toTimeDiff = fromIntegral . (* 60)
