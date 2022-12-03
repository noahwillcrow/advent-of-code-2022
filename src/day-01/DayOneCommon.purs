module DayOneCommon
  ( countCaloriesForNextElf
  )
  where

import Prelude

import Data.Array (head, tail)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))

countCaloriesForNextElf :: Array String -> Int -> Tuple Int (Array String)
countCaloriesForNextElf array currentCount = case (head array) of
  Nothing -> Tuple currentCount array
  Just inputLine -> case (fromString inputLine) of
    Nothing -> Tuple currentCount (fromMaybe [] (tail array))
    Just value -> countCaloriesForNextElf (fromMaybe [] (tail array)) (value + currentCount)
