module DayOnePartOne
  ( runDayOnePartOne
  )
  where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import DayOneCommon (countCaloriesForNextElf)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

handleInputLines :: Array String -> Int-> Int
handleInputLines array maxCount = case (countCaloriesForNextElf array 0) of
  Tuple newValue [] -> max maxCount newValue
  Tuple newValue remainingArray -> handleInputLines remainingArray (max maxCount newValue)

runDayOnePartOne :: Effect Unit
runDayOnePartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-one-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))


