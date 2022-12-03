module DayOnePartOne
  ( runDayOnePartOne
  )
  where

import Prelude

import Data.Array (head, tail)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String.Utils (lines)

import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

handleInputLines :: Array String -> Int -> Int -> Int
handleInputLines array currentCount maxCount = case (head array) of
  Nothing -> maxCount
  Just inputLine -> case (fromString inputLine) of
    Nothing -> handleInputLines (fromMaybe [] (tail array)) 0 maxCount
    Just value -> handleInputLines (fromMaybe [] (tail array)) (currentCount + value) (max maxCount (currentCount + value))

runDayOnePartOne :: Effect Unit
runDayOnePartOne = do
  inputFileContents <- readTextFile UTF8 "./.inputs/part-one-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0 0))


