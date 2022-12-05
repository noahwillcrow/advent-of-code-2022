module DayFourCommon
  ( parseLine
  )
  where

import Prelude

import ArrayUtils (tailOrEmpty, unsafeIndex)
import Data.Array (head, snoc)
import Data.Char (toCharCode)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import IntRange (IntRange, defineIntRange)

_getNumbersFromLine :: Array Char -> Int -> Array Int -> Array Int
_getNumbersFromLine charArray currentInt outputArray = case (head charArray) of
  Nothing -> 
    if 
      currentInt == -1
    then
      outputArray
    else
      snoc outputArray currentInt
  Just char ->
    if
      char >= '0' && char <= '9'
    then
      _getNumbersFromLine (tailOrEmpty charArray) (((max 0 currentInt) * 10) + ((toCharCode char) - (toCharCode '0'))) outputArray
    else
      if
        currentInt == -1
      then
        _getNumbersFromLine (tailOrEmpty charArray) (-1) outputArray
      else
        _getNumbersFromLine (tailOrEmpty charArray) (-1) (snoc outputArray currentInt)

getNumbersFromLine :: String -> Array Int
getNumbersFromLine inputString = _getNumbersFromLine (toCharArray inputString) (-1) []

lineNumbersToNestedTuple :: Array Int -> Tuple IntRange IntRange
lineNumbersToNestedTuple lineNumbers = Tuple (defineIntRange (unsafeIndex lineNumbers 0) (unsafeIndex lineNumbers 1)) (defineIntRange (unsafeIndex lineNumbers 2) (unsafeIndex lineNumbers 3))

parseLine :: String -> Tuple IntRange IntRange
parseLine inputString = lineNumbersToNestedTuple $ getNumbersFromLine inputString
