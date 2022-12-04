module DayTwoPartTwo
  ( runDayTwoPartTwo
  )
  where

import Prelude

import Data.Array (head, tail)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

determineScoreForRound :: String -> Int
--                     "1 2" = pts for option played + pts for result
determineScoreForRound "A X" = 3 + 0
determineScoreForRound "A Y" = 1 + 3
determineScoreForRound "A Z" = 2 + 6
determineScoreForRound "B X" = 1 + 0
determineScoreForRound "B Y" = 2 + 3
determineScoreForRound "B Z" = 3 + 6
determineScoreForRound "C X" = 2 + 0
determineScoreForRound "C Y" = 3 + 3
determineScoreForRound "C Z" = 1 + 6
determineScoreForRound _ = 0

handleInputLines :: Array String -> Int -> Int
handleInputLines inputArray currentScore = case (head inputArray) of
  Nothing -> currentScore
  Just inputLine -> handleInputLines (fromMaybe [] (tail inputArray)) (currentScore + (determineScoreForRound inputLine))

runDayTwoPartTwo :: Effect Unit
runDayTwoPartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-02-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))


