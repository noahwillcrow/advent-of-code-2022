module DayFivePartOne
  ( runDayFivePartOne
  )
  where

import Prelude

import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (lines)
import DayFiveCommon (doMoveOperations, getOutputFromFinalState, parseInitialState)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

handleInputLines :: Array String -> String
handleInputLines inputArray = fromCharArray (getOutputFromFinalState (doMoveOperations true (parseInitialState inputArray)))

runDayFivePartOne :: Effect Unit
runDayFivePartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-05-input.txt"
  log (handleInputLines (lines inputFileContents))
