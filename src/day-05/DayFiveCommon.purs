module DayFiveCommon
  ( doMoveOperations
  , getOutputFromFinalState
  , parseInitialState
  )
  where

import Prelude

import ArrayUtils (createForLengthWithDefaultValue, map, tailOrEmpty, unsafeIndex)
import Data.Array (head, modifyAt)
import Data.Array as Data.Array
import Data.Char (toCharCode)
import Data.Int (ceil, floor, toNumber)
import Data.List.Lazy (List, cons, drop, nil, reverse, take)
import Data.List.Lazy as Data.List.Lazy
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (length, toCharArray)
import Data.Tuple (Tuple(..))
import MiscUtils (unsafeJust)

invertIndex :: forall a. Int -> Array a -> Int
invertIndex i a = ((Data.Array.length a) - 1) - i

computeFlooredIndex :: Int -> Int -> Int
computeFlooredIndex value divisor = floor ((toNumber value) / (toNumber divisor))

pushValueToColumnInState :: Array (List Char) -> Int -> Char -> Array (List Char)
pushValueToColumnInState currentState columnIndex value = unsafeJust (modifyAt columnIndex (cons value) currentState)

pushValuesToColumnInState :: Array (List Char) -> Int -> List Char -> Array (List Char)
pushValuesToColumnInState currentState columnIndex values = case (Data.List.Lazy.last values) of
  Nothing -> currentState
  Just value -> pushValuesToColumnInState (pushValueToColumnInState currentState (invertIndex columnIndex currentState) value) columnIndex (fromMaybe nil (Data.List.Lazy.init values))

pushValuesToColumnInStateReversed :: Array (List Char) -> Int -> List Char -> Array (List Char)
pushValuesToColumnInStateReversed currentState columnIndex values = case (Data.List.Lazy.head values) of
  Nothing -> currentState
  Just value -> pushValuesToColumnInStateReversed (pushValueToColumnInState currentState (invertIndex columnIndex currentState) value) columnIndex (fromMaybe nil (Data.List.Lazy.tail values))

pushValuesToColumnInStateWithDirectionOption :: Array (List Char) -> Int -> List Char -> Boolean -> Array (List Char)
pushValuesToColumnInStateWithDirectionOption currentState columnIndex values true = pushValuesToColumnInStateReversed currentState columnIndex values
pushValuesToColumnInStateWithDirectionOption currentState columnIndex values false = pushValuesToColumnInState currentState columnIndex values

dropValuesFromColumnInState :: Array (List Char) -> Int -> Int -> Array (List Char)
dropValuesFromColumnInState currentState columnIndex numberOfValues = unsafeJust (modifyAt (invertIndex columnIndex currentState) (drop numberOfValues) currentState)

takeValuesFromColumnInState :: Array (List Char) -> Int -> Int -> List Char
takeValuesFromColumnInState currentState columnIndex numberOfValues = take numberOfValues (unsafeIndex currentState (invertIndex columnIndex currentState))

reverseColumnsInState :: Array (List Char) -> Array (List Char)
reverseColumnsInState inputState = map reverse inputState

_parseAndUpdateInitialState :: Array Char -> Array (List Char) -> Int -> Tuple (Array (List Char)) (Boolean)
_parseAndUpdateInitialState inputLine currentState numberOfCharsProcessedInLineSoFar = case (head inputLine) of
  Nothing -> Tuple currentState false
  Just inputChar ->
    if
      inputChar >= '0' && inputChar <= '9'
    then
      Tuple currentState true
    else
      if
        inputChar >= 'A' && inputChar <= 'Z'
      then
        _parseAndUpdateInitialState (tailOrEmpty inputLine) (pushValueToColumnInState currentState (computeFlooredIndex numberOfCharsProcessedInLineSoFar 4) inputChar) (numberOfCharsProcessedInLineSoFar + 1)
      else
        _parseAndUpdateInitialState (tailOrEmpty inputLine) currentState (numberOfCharsProcessedInLineSoFar + 1)

parseAndUpdateInitialState :: Array Char -> Array (List Char) -> Tuple (Array (List Char)) (Boolean)
parseAndUpdateInitialState inputLine currentState = _parseAndUpdateInitialState inputLine currentState 0

createBlankInitialState :: Int -> Array (List Char)
createBlankInitialState lineLength = createForLengthWithDefaultValue (ceil ((toNumber lineLength) / 4.0)) (\_ -> nil)

_parseInitialState :: Array String -> Array (List Char) -> Tuple (Array (List Char)) (Array String)
_parseInitialState inputArray currentState = case (head inputArray) of
  Nothing -> Tuple (reverseColumnsInState currentState) inputArray
  Just inputLine -> case (parseAndUpdateInitialState (toCharArray inputLine) currentState) of
    Tuple newState true -> Tuple (reverseColumnsInState newState) (tailOrEmpty (tailOrEmpty inputArray))
    Tuple newState false -> _parseInitialState (tailOrEmpty inputArray) newState

parseInitialState :: Array String -> Tuple (Array (List Char)) (Array String)
parseInitialState inputArray = _parseInitialState inputArray (createBlankInitialState (length (fromMaybe "" (head inputArray))))

type MoveLine = { numberOfAffectedCrates :: Int, fromColumnIndex :: Int, toColumnIndex :: Int }

createInProgressMoveLine :: MoveLine
createInProgressMoveLine = { numberOfAffectedCrates: (-1), fromColumnIndex: (-1), toColumnIndex: (-1) }

applyParsedValueToMoveLine :: Int -> MoveLine -> MoveLine
applyParsedValueToMoveLine parsedValue inProgressMoveLine =
  if
    inProgressMoveLine.numberOfAffectedCrates == (-1)
  then
    { numberOfAffectedCrates: parsedValue, fromColumnIndex: (-1), toColumnIndex: (-1) }
  else
    if
      inProgressMoveLine.fromColumnIndex == (-1)
    then
      { numberOfAffectedCrates: inProgressMoveLine.numberOfAffectedCrates, fromColumnIndex: (parsedValue - 1), toColumnIndex: (-1) }
    else
      { numberOfAffectedCrates: inProgressMoveLine.numberOfAffectedCrates, fromColumnIndex: inProgressMoveLine.fromColumnIndex, toColumnIndex: (parsedValue - 1) }

_parseMoveLine :: Array Char -> MoveLine -> Int -> MoveLine
_parseMoveLine remainingChars result currentParsedValue = case (head remainingChars) of
  Nothing -> applyParsedValueToMoveLine currentParsedValue result
  Just currentChar ->
    if
      '0' <= currentChar && currentChar <= '9'
    then
      _parseMoveLine (tailOrEmpty remainingChars) result (((max 0 currentParsedValue) * 10) + ((toCharCode currentChar) - (toCharCode '0')))
    else
      if
        currentParsedValue > (-1)
      then
        _parseMoveLine (tailOrEmpty remainingChars) (applyParsedValueToMoveLine currentParsedValue result) (-1)
      else
        _parseMoveLine (tailOrEmpty remainingChars) result (-1)

parseMoveLine :: String -> MoveLine
parseMoveLine inputLine = _parseMoveLine (toCharArray inputLine) (createInProgressMoveLine) (-1)

applyMoveOperation :: Boolean -> MoveLine -> Array (List Char) -> Array (List Char)
applyMoveOperation shouldStackInReverse moveLine currentState = dropValuesFromColumnInState
  (pushValuesToColumnInStateWithDirectionOption
    currentState
    moveLine.toColumnIndex
    (takeValuesFromColumnInState currentState moveLine.fromColumnIndex moveLine.numberOfAffectedCrates)
    shouldStackInReverse)
  moveLine.fromColumnIndex
  moveLine.numberOfAffectedCrates

_doMoveOperations :: Boolean -> Array String -> Array (List Char) -> Array (List Char)
_doMoveOperations shouldStackInReverse inputArray currentState = case (head inputArray) of
  Nothing -> currentState
  Just inputLine -> _doMoveOperations shouldStackInReverse (tailOrEmpty inputArray) (applyMoveOperation shouldStackInReverse (parseMoveLine inputLine) currentState)

doMoveOperations :: Boolean -> Tuple (Array (List Char)) (Array String) -> Array (List Char)
doMoveOperations shouldStackInReverse tupleInput = case (tupleInput) of
  Tuple initialState inputArray -> _doMoveOperations shouldStackInReverse inputArray initialState

_getOutputFromFinalState :: Array (List Char) -> Array Char -> Array Char
_getOutputFromFinalState remainingState output = case (head remainingState) of
  Nothing -> output
  Just column -> _getOutputFromFinalState (tailOrEmpty remainingState) (Data.Array.cons (fromMaybe '-' (Data.List.Lazy.head column)) output)

getOutputFromFinalState :: Array (List Char) -> Array Char
getOutputFromFinalState finalState = _getOutputFromFinalState finalState []
