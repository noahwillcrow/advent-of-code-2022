module DayOnePartTwo
  ( runDayOnePartTwo
  )
  where

import Prelude

import Data.Array (insertBy, take)
import Data.Foldable (sum)
import Data.Int (decimal, toStringAs)
import Data.Ordering (invert)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import DayOneCommon (countCaloriesForNextElf)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

invertCompare :: forall a. Ord a => a -> a -> Ordering
invertCompare a b = invert $ compare a b

handleInputLines :: Array String -> Array Int-> Array Int
handleInputLines inputArray outputArray = case (countCaloriesForNextElf inputArray 0) of
  Tuple _ [] -> outputArray
  Tuple newValue remainingInputArray -> handleInputLines remainingInputArray (insertBy invertCompare newValue outputArray)

runDayOnePartTwo :: Effect Unit
runDayOnePartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-one-input.txt"
  log (toStringAs decimal (sum (take 3 (handleInputLines (lines inputFileContents) []))))


