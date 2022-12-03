module DayOnePartTwo
  ( runDayOnePartTwo
  )
  where

import Prelude

import Data.Array (insert, take, reverse)
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import DayOneCommon (countCaloriesForNextElf)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

handleInputLines :: Array String -> Array Int-> Array Int
handleInputLines inputArray outputArray = case (countCaloriesForNextElf inputArray 0) of
  Tuple _ [] -> outputArray
  Tuple newValue remainingInputArray -> handleInputLines remainingInputArray (insert newValue outputArray)

runDayOnePartTwo :: Effect Unit
runDayOnePartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-one-input.txt"
  log (toStringAs decimal (sum (take 3 (reverse (handleInputLines (lines inputFileContents) [])))))


