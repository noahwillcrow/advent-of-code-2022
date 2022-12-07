module DaySevenPartOne
  ( runDaySevenPartOne
  )
  where

import Prelude

import Data.Foldable as Foldable
import Data.Int (decimal, toStringAs)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.Utils (lines)
import DaySevenCommon (SolutionState, deriveFinalSolutionStateFromInputLines)
import DebugUtils (traceReturnValue)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

sumOverInterestingDirectorySizes :: SolutionState -> Int
sumOverInterestingDirectorySizes state = Foldable.foldl
  (\currentValue directoryPath -> currentValue + (fromMaybe 0 $ traceReturnValue (Map.lookup (traceReturnValue directoryPath) state.directorySizes)))
  0
  state.interestingDirectoryKeys

runDaySevenPartOne :: Effect Unit
runDaySevenPartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-07-input.txt"
  log (toStringAs decimal (sumOverInterestingDirectorySizes (deriveFinalSolutionStateFromInputLines (List.fromFoldable (lines inputFileContents)) (\value -> value <= 100000))))
