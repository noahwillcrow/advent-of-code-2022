module DaySixCommon
  ( handleInputLines
  )
  where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List.Lazy (List, fromFoldable, head, length, nil)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import DebugUtils (traceReturnValue)
import Effect (Effect)
import Effect.Console (log)
import LazyListUtils (consWithMaxLength, isEveryElementUnique, tailOrNil)
import MiscUtils (doAndIgnore)

isValidMarker :: List Char -> Int -> Boolean
isValidMarker charsList markerLength = ((length charsList) == markerLength) && (isEveryElementUnique charsList)

_findIndexAfterMarker :: List Char -> Int -> List Char -> Int -> Maybe Int
_findIndexAfterMarker inputChars markerLength recentChars progressCounter = case (head inputChars) of
  Nothing -> Nothing
  Just inputChar -> do
    if
      isValidMarker (consWithMaxLength inputChar recentChars markerLength) markerLength
    then
      Just (progressCounter + 1)
    else
      _findIndexAfterMarker (tailOrNil inputChars) markerLength (consWithMaxLength inputChar recentChars markerLength) (progressCounter + 1)

findIndexAfterMarker :: String -> Int -> Maybe Int
findIndexAfterMarker inputString markerLength = _findIndexAfterMarker (fromFoldable (toCharArray inputString)) markerLength nil 0

handleInputLine :: String -> Int -> Effect Unit
handleInputLine inputLine markerLength = case (findIndexAfterMarker inputLine markerLength) of
  Nothing -> log (traceReturnValue $ "No start of packet marker found")
  Just afterMarkerIndex -> log (traceReturnValue $ joinWith " " ["Start of packet marker found at index", toStringAs decimal afterMarkerIndex])

handleInputLines :: List String -> Int -> Effect Unit
handleInputLines inputList markerLength = case (head inputList) of
  Nothing -> log "Done"
  Just inputLine -> doAndIgnore (handleInputLine inputLine markerLength) (handleInputLines (tailOrNil inputList) markerLength)
