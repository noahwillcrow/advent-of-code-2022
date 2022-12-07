module DaySevenPartTwo
  ( runDaySevenPartTwo
  )
  where

import Prelude

import Data.Foldable as Foldable
import Data.Int (decimal, toStringAs)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines)
import DaySevenCommon (SolutionState, deriveFinalSolutionStateFromInputLines)
import Effect (Effect)
import Effect.Console (log)
import MiscUtils (doAndIgnore)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

findSizeOfBestDirectoryToDelete :: SolutionState -> Int -> Int -> Maybe Int
findSizeOfBestDirectoryToDelete state totalStorageCapacity desiredMinAvailableStorage =
  (\value ->
    if
      value == (-1)
    then
      Nothing
    else
      Just value)
    ((\minRequiredToClear -> Foldable.foldl
      (\currentValue directoryPath ->
          (\directorySize ->
            if
              directorySize > minRequiredToClear && (directorySize < currentValue || currentValue == (-1))
            then
              doAndIgnore directoryPath directorySize
            else
              currentValue)
          (fromMaybe 0 (Map.lookup directoryPath state.directorySizes)))
        (-1)
        state.interestingDirectoryKeys)
      (desiredMinAvailableStorage - (totalStorageCapacity - (fromMaybe 0 (Map.lookup "" state.directorySizes)))))

runDaySevenPartTwo :: Effect Unit
runDaySevenPartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-07-input.txt"
  log (toStringAs decimal (fromMaybe (-1) (findSizeOfBestDirectoryToDelete (deriveFinalSolutionStateFromInputLines (List.fromFoldable (lines inputFileContents)) (\_ -> true)) 70000000 30000000)))
