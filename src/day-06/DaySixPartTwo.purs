module DaySixPartTwo
  ( runDaySixPartTwo
  )
  where

import Prelude

import Data.List.Lazy (fromFoldable)
import Data.String.Utils (lines)
import DaySixCommon (handleInputLines)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

runDaySixPartTwo :: Effect Unit
runDaySixPartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-06-input.txt"
  handleInputLines (fromFoldable (lines inputFileContents)) 14
