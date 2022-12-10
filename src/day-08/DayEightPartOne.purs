module DayEightPartOne
  ( runDayEightPartOne
  )
  where

import Prelude

import ArrayUtils (unsafeIndex, unsafeModifyAt)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable as Foldable
import Data.Int (decimal, toStringAs)
import Data.List.Lazy (List)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import DebugUtils (traceReturnValue)
import Effect (Effect)
import Effect.Console (log)
import LazyListUtils (tailOrNil)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type SolutionState = {
  numberOfColumns :: Int,
  numberOfRows :: Int,
  -- listed clockwise
  tallestVisibleIndexesFromTopPerspective :: Array Int,
  tallestVisibleIndexesFromRightPerspective :: Array Int,
  tallestVisibleIndexesFromBottomPerspective :: Array Int,
  tallestVisibleIndexesFromLeftPerspective :: Array Int,
  tallestVisibleValuesFromTopPerspective :: Array Int,
  tallestVisibleValuesFromRightPerspective :: Array Int,
  tallestVisibleValuesFromBottomPerspective :: Array Int,
  tallestVisibleValuesFromLeftPerspective :: Array Int
}

createInitialSolutionState :: Int -> Int -> SolutionState
createInitialSolutionState numberOfColumns numberOfRows = {
  numberOfColumns: numberOfColumns,
  numberOfRows: numberOfRows,
  tallestVisibleIndexesFromTopPerspective: Array.replicate numberOfColumns (-1),
  tallestVisibleIndexesFromRightPerspective: Array.replicate numberOfRows (-1),
  tallestVisibleIndexesFromBottomPerspective: Array.replicate numberOfColumns (-1),
  tallestVisibleIndexesFromLeftPerspective: Array.replicate numberOfRows (-1),
  tallestVisibleValuesFromTopPerspective: Array.replicate numberOfColumns (-1),
  tallestVisibleValuesFromRightPerspective: Array.replicate numberOfRows (-1),
  tallestVisibleValuesFromBottomPerspective: Array.replicate numberOfColumns (-1),
  tallestVisibleValuesFromLeftPerspective: Array.replicate numberOfRows (-1)
}

updateForPotentialTallestVisibleTreeFromTopPerspective :: Int -> Int -> Int -> SolutionState -> SolutionState
updateForPotentialTallestVisibleTreeFromTopPerspective x y value currentState =
  if
    value > (unsafeIndex currentState.tallestVisibleValuesFromTopPerspective x)
  then
    currentState {
      tallestVisibleIndexesFromTopPerspective = unsafeModifyAt currentState.tallestVisibleIndexesFromTopPerspective x y,
      tallestVisibleValuesFromTopPerspective = unsafeModifyAt currentState.tallestVisibleValuesFromTopPerspective x value
    }
  else
    currentState

updateForPotentialTallestVisibleTreeFromRightPerspective :: Int -> Int -> Int -> SolutionState -> SolutionState
updateForPotentialTallestVisibleTreeFromRightPerspective x y value currentState =
  if
    value > (unsafeIndex currentState.tallestVisibleValuesFromRightPerspective y)
  then
    currentState {
      tallestVisibleIndexesFromRightPerspective = unsafeModifyAt currentState.tallestVisibleIndexesFromRightPerspective y x,
      tallestVisibleValuesFromRightPerspective = unsafeModifyAt currentState.tallestVisibleValuesFromRightPerspective y value
    }
  else
    currentState

updateForPotentialTallestVisibleTreeFromBottomPerspective :: Int -> Int -> Int -> SolutionState -> SolutionState
updateForPotentialTallestVisibleTreeFromBottomPerspective x y value currentState =
  if
    value > (unsafeIndex currentState.tallestVisibleValuesFromBottomPerspective x)
  then
    currentState {
      tallestVisibleIndexesFromBottomPerspective = unsafeModifyAt currentState.tallestVisibleIndexesFromBottomPerspective x y,
      tallestVisibleValuesFromBottomPerspective = unsafeModifyAt currentState.tallestVisibleValuesFromBottomPerspective x value
    }
  else
    currentState

updateForPotentialTallestVisibleTreeFromLeftPerspective :: Int -> Int -> Int -> SolutionState -> SolutionState
updateForPotentialTallestVisibleTreeFromLeftPerspective x y value currentState =
  if
    value > (unsafeIndex currentState.tallestVisibleValuesFromLeftPerspective y)
  then
    currentState {
      tallestVisibleIndexesFromLeftPerspective = unsafeModifyAt currentState.tallestVisibleIndexesFromLeftPerspective y x,
      tallestVisibleValuesFromLeftPerspective = unsafeModifyAt currentState.tallestVisibleValuesFromLeftPerspective y value
    }
  else
    currentState

updateStateForInputLine :: SolutionState -> Int -> List Char -> Int -> SolutionState
updateStateForInputLine currentState numberOfLinesSoFar remainingChars numberOfCharsSoFar = case (List.head remainingChars) of
  Nothing -> currentState
  Just inputChar -> updateStateForInputLine
    ((\inputValue ->
      updateForPotentialTallestVisibleTreeFromTopPerspective numberOfCharsSoFar numberOfLinesSoFar inputValue
        (updateForPotentialTallestVisibleTreeFromRightPerspective numberOfCharsSoFar numberOfLinesSoFar inputValue
          (updateForPotentialTallestVisibleTreeFromBottomPerspective numberOfCharsSoFar numberOfLinesSoFar inputValue
            (updateForPotentialTallestVisibleTreeFromLeftPerspective numberOfCharsSoFar numberOfLinesSoFar inputValue currentState))))
      ((toCharCode inputChar) - (toCharCode '0')))
    numberOfLinesSoFar
    (tailOrNil remainingChars)
    (numberOfCharsSoFar + 1)

_determineTallestVisibleIndexesFromEachPerspectiveForInputLines :: List String -> Int -> SolutionState -> SolutionState
_determineTallestVisibleIndexesFromEachPerspectiveForInputLines inputLines numberOfLinesSoFar currentState = case (List.head inputLines) of
  Nothing -> currentState
  Just inputLine -> _determineTallestVisibleIndexesFromEachPerspectiveForInputLines
    (tailOrNil inputLines)
    (numberOfLinesSoFar + 1)
    (updateStateForInputLine currentState numberOfLinesSoFar (toCharArray inputLine) 0)


determineTallestVisibleIndexesFromEachPerspectiveForInputLines :: List String -> SolutionState
determineTallestVisibleIndexesFromEachPerspectiveForInputLines inputLines = _determineTallestVisibleIndexesFromEachPerspectiveForInputLines
  inputLines
  0
  (createInitialSolutionState (Array.length inputLines) (String.length (unsafeIndex inputLines 0)))

xyCoordinateToFlattenedIndex :: Int -> Int -> Int -> Int
xyCoordinateToFlattenedIndex x y width = x + (y * width)

countUniqueVisibleTrees :: SolutionState -> Int
countUniqueVisibleTrees solutionState = Set.size (Set.fromFoldable (Array.concat [
  -- all the edge trees
  (Array.mapWithIndex (\i _ -> xyCoordinateToFlattenedIndex i 0) (Array.range 0 (solutionState.numberOfColumns - 1))), -- top
  (Array.mapWithIndex (\i _ -> xyCoordinateToFlattenedIndex (solutionState.numberOfColumns - 1) i) (Array.range 0 (solutionState.numberOfRows - 1))), -- right
  (Array.mapWithIndex (\i _ -> xyCoordinateToFlattenedIndex i (solutionState.numberOfRows - 1)) (Array.range 0 (solutionState.numberOfColumns - 1))), -- bottom
  (Array.mapWithIndex (\i _ -> xyCoordinateToFlattenedIndex 0 i) (Array.range 0 (solutionState.numberOfRows - 1))), -- left
  -- all the identified tallest trees from each perspective
  (Array.mapWithIndex (\i v -> xyCoordinateToFlattenedIndex i v) solutionState.tallestVisibleIndexesFromTopPerspective),
  (Array.mapWithIndex (\i v -> xyCoordinateToFlattenedIndex v i) solutionState.tallestVisibleIndexesFromRightPerspective),
  (Array.mapWithIndex (\i v -> xyCoordinateToFlattenedIndex i v) solutionState.tallestVisibleIndexesFromBottomPerspective),
  (Array.mapWithIndex (\i v -> xyCoordinateToFlattenedIndex v i) solutionState.tallestVisibleIndexesFromLeftPerspective)
]))

runDayEightPartOne :: Effect Unit
runDayEightPartOne = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-07-input.txt"
  log (toStringAs decimal (countUniqueVisibleTrees (determineTallestVisibleIndexesFromEachPerspectiveForInputLines (List.fromFoldable (lines inputFileContents)))))
