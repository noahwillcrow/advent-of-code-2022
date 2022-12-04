module DayTwoPartOne
  ( runDayTwoPartOne
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

handleInputLines :: Array String -> Int -> Int
handleInputLines inputArray currentScore = case (head inputArray) of
  Nothing -> currentScore
  Just inputLine -> handleInputLines (fromMaybe [] (tail inputArray)) (currentScore + (determineScoreForRound inputLine))

runDayTwoPartOne :: Effect Unit
runDayTwoPartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-02-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))


