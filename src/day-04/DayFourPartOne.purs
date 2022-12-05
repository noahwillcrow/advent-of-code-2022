module DayFourPartOne
  ( runDayFourPartOne
  )
  where

import Prelude

import ArrayUtils (tailOrEmpty)
import Data.Array (head)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines)
import Data.Tuple (Tuple, fst, snd)
import DayFourCommon (parseLine)
import DebugUtils (traceReturnValue)
import Effect (Effect)
import Effect.Console (log)
import IntRange (IntRange, isOneRangeEntirelyInsideTheOther)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

shouldIncrementForRangeTuple :: Tuple IntRange IntRange -> Boolean
shouldIncrementForRangeTuple ranges = isOneRangeEntirelyInsideTheOther (fst ranges) (snd ranges)

handleInputLines :: Array String -> Int -> Int
handleInputLines inputArray result = case (head inputArray) of
  Nothing -> result
  Just inputLine -> 
    if
      shouldIncrementForRangeTuple (parseLine inputLine)
    then
      handleInputLines (tailOrEmpty inputArray) (result + 1)
    else
      handleInputLines (tailOrEmpty inputArray) result

runDayFourPartOne :: Effect Unit
runDayFourPartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-04-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))
