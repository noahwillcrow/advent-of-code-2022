module DayThreePartOne
  ( runDayThreePartOne
  )
  where

import Prelude

import Data.Array (head, length, tail)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, insert, member)
import Data.String as Str
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import DayThreeCommon (scoreItem)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

identifySharedItemInLine :: Array Char -> Int -> Set Char -> Char
identifySharedItemInLine inputArray originalInputLength charsInFirstHalf =
  if
    length inputArray > originalInputLength / 2
  then
    identifySharedItemInLine (fromMaybe [] (tail inputArray)) originalInputLength (insert (fromMaybe '0' (head inputArray)) charsInFirstHalf)
  else
    if
      member (fromMaybe '0' (head inputArray)) charsInFirstHalf
    then
      fromMaybe '0' (head inputArray)
    else
      identifySharedItemInLine (fromMaybe [] (tail inputArray)) originalInputLength charsInFirstHalf
evaluateLine :: String -> Int
evaluateLine line = scoreItem (identifySharedItemInLine (toCharArray line) (Str.length line) empty)

handleInputLines :: Array String -> Int -> Int
handleInputLines inputArray result = case (head inputArray) of
  Nothing -> result
  Just inputLine -> handleInputLines (fromMaybe [] (tail inputArray)) (result + (evaluateLine inputLine))

runDayThreePartOne :: Effect Unit
runDayThreePartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-03-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))
