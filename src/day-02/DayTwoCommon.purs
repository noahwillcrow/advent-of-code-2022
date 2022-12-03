module DayTwoCommon
  ( determineScoreForRound
  )
  where

import Prelude

determineScoreForRound :: String -> Int
determineScoreForRound "A X" = 3 + 1
determineScoreForRound "A Y" = 6 + 2
determineScoreForRound "A Z" = 0 + 3
determineScoreForRound "B X" = 0 + 1
determineScoreForRound "B Y" = 3 + 2
determineScoreForRound "B Z" = 6 + 3
determineScoreForRound "C X" = 6 + 1
determineScoreForRound "C Y" = 0 + 2
determineScoreForRound "C Z" = 3 + 3
determineScoreForRound _ = 0
