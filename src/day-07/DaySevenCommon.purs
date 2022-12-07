module DaySevenCommon
  ( SolutionState
  , deriveFinalSolutionStateFromInputLines
  )
  where

import Prelude

import ArrayUtils (tailOrEmpty)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String.CodeUnits (toCharArray)
import LazyListUtils (initOrNil, tailOrNil)

directoryChainToPathString :: List String -> String
directoryChainToPathString chain = joinWith "/" (Array.fromFoldable chain)

type SolutionState = { directoryChain :: List String, directorySizes :: Map String Int, interestingDirectoryKeys :: List String }

createInitialSolutionState :: SolutionState
createInitialSolutionState = { directoryChain: (List.fromFoldable [""]), directorySizes: Map.empty, interestingDirectoryKeys: List.nil }

_addToDirectorySize :: SolutionState -> String -> Int -> (Int -> Boolean) -> SolutionState
_addToDirectorySize currentState directoryPath newSize isDirectorySizeInteresting = {
  directoryChain: currentState.directoryChain,
  directorySizes: Map.insert directoryPath newSize currentState.directorySizes,
  interestingDirectoryKeys: 
    if
      isDirectorySizeInteresting newSize
    then
      case (List.find (eq directoryPath) currentState.interestingDirectoryKeys) of
        Nothing -> List.cons directoryPath currentState.interestingDirectoryKeys
        Just _ -> currentState.interestingDirectoryKeys
    else
      List.filter (\v -> (not (eq directoryPath v))) currentState.interestingDirectoryKeys
}

_addFileSizeToDirectoryChain :: SolutionState -> Int -> (Int -> Boolean) -> List String -> SolutionState
_addFileSizeToDirectoryChain currentState sizeAddend isDirectorySizeInteresting remainingChain =
  if
    List.null remainingChain
  then
    currentState
  else 
    _addFileSizeToDirectoryChain
      ((\directoryPath -> case (Map.lookup directoryPath currentState.directorySizes) of
        Nothing -> _addToDirectorySize currentState directoryPath sizeAddend isDirectorySizeInteresting
        Just existingValue -> _addToDirectorySize currentState directoryPath (existingValue + sizeAddend) isDirectorySizeInteresting)
      (directoryChainToPathString remainingChain))
      sizeAddend
      isDirectorySizeInteresting
      (initOrNil remainingChain)

addFileSizeToDirectoryChain :: SolutionState -> Int -> (Int -> Boolean) -> SolutionState
addFileSizeToDirectoryChain currentState sizeAddend isDirectorySizeInteresting = _addFileSizeToDirectoryChain currentState sizeAddend isDirectorySizeInteresting currentState.directoryChain

updateDirectoryChain :: SolutionState -> List String -> SolutionState
updateDirectoryChain currentState newDirectoryChain = { directoryChain: newDirectoryChain, directorySizes: currentState.directorySizes, interestingDirectoryKeys: currentState.interestingDirectoryKeys }

_parseIntAtStartOfString :: Array Char -> Int -> Int
_parseIntAtStartOfString remainingChars currentValue = case (Array.head remainingChars) of
  Nothing -> currentValue
  Just nextChar ->
    if
      '0' <= nextChar && nextChar <= '9'
    then
      _parseIntAtStartOfString (tailOrEmpty remainingChars) (((max 0 currentValue) * 10) + ((toCharCode nextChar) - (toCharCode '0')))
    else
      currentValue

parseIntAtStartOfString :: String -> Maybe Int
parseIntAtStartOfString str = 
  (\parsedValue ->
    if
      parsedValue == (-1)
    then
      Nothing
    else
      Just parsedValue)
  (_parseIntAtStartOfString (toCharArray str) (-1))

updateStateFromCommand :: SolutionState -> String -> SolutionState
updateStateFromCommand currentState commandString = case (stripPrefix (Pattern "cd ") commandString) of 
  Nothing -> currentState
  Just toPath ->
    if
      toPath == ".."
    then
      updateDirectoryChain currentState (initOrNil currentState.directoryChain)
    else if
      toPath == "/"
    then
      updateDirectoryChain currentState (List.fromFoldable [""])
    else
      updateDirectoryChain currentState (List.snoc currentState.directoryChain toPath)

updateStateFromChildDescription :: SolutionState -> String -> (Int -> Boolean) -> SolutionState
updateStateFromChildDescription currentState childDescription isDirectorySizeInteresting = case (stripPrefix (Pattern "dir ") childDescription) of
  Just _ -> currentState
  Nothing -> case (parseIntAtStartOfString childDescription) of
    Nothing -> currentState
    Just fileSize -> addFileSizeToDirectoryChain currentState fileSize isDirectorySizeInteresting

_deriveFinalSolutionStateFromInputLines :: List String -> (Int -> Boolean) -> SolutionState -> SolutionState
_deriveFinalSolutionStateFromInputLines inputLines isDirectorySizeInteresting currentState = case (List.head inputLines) of
  Nothing -> currentState
  Just inputLine -> case (stripPrefix (Pattern "$ ") inputLine) of
    Nothing -> _deriveFinalSolutionStateFromInputLines (tailOrNil inputLines) isDirectorySizeInteresting (updateStateFromChildDescription currentState inputLine isDirectorySizeInteresting)
    Just commandString -> _deriveFinalSolutionStateFromInputLines (tailOrNil inputLines) isDirectorySizeInteresting (updateStateFromCommand currentState commandString)

deriveFinalSolutionStateFromInputLines :: List String -> (Int -> Boolean) -> SolutionState
deriveFinalSolutionStateFromInputLines inputLines isDirectorySizeInteresting = _deriveFinalSolutionStateFromInputLines inputLines isDirectorySizeInteresting (createInitialSolutionState)