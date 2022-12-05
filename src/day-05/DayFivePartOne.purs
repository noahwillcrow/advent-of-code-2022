module DayFivePartOne
  ( runDayFivePartOne
  )
  where

import Prelude

import Data.Int (fromString)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

handleInputLines :: Array String -> String
handleInputLines _ = "done"

runDayFivePartOne :: Effect Unit
runDayFivePartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-05-input.test.txt"
  log (handleInputLines (lines inputFileContents))
