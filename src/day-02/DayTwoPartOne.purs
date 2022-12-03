module DayTwoPartTwo
  ( runDayTwoPartTwo
  )
  where

import Prelude

import Data.Array (head, tail)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines)
import DayTwoCommon (determineScoreForRound)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


handleInputLines :: Array String -> Int -> Int
handleInputLines inputArray currentScore = case (head inputArray) of
  Nothing -> currentScore
  Just inputLine -> handleInputLines (fromMaybe [] (tail inputArray)) (currentScore + (determineScoreForRound inputLine))

runDayTwoPartTwo :: Effect Unit
runDayTwoPartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-02-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))


