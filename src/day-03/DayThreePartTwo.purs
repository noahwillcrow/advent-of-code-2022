module DayThreePartTwo
  ( runDayThreePartTwo
  )
  where

import Prelude

import Data.Array (head, length, tail)
import Data.Char (toCharCode)
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, findMin, fromFoldable, insert, intersection, member)
import Data.String as Str
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..), fst, snd)
import DayThreeCommon (scoreItem)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

identifySharedItemInGroup :: Array String -> Maybe (Set Char) -> Maybe (Set Char) -> Tuple Char (Array String)
identifySharedItemInGroup inputArray maybeFirstSet maybeSecondSet = case (Tuple maybeFirstSet maybeSecondSet) of
  Tuple Nothing Nothing -> identifySharedItemInGroup (fromMaybe [] (tail inputArray)) (Just (fromFoldable (toCharArray (fromMaybe "" (head inputArray))))) Nothing
  Tuple (Just firstSet) Nothing -> identifySharedItemInGroup (fromMaybe [] (tail inputArray)) (Just firstSet) (Just (fromFoldable (toCharArray (fromMaybe "" (head inputArray)))))
  Tuple Nothing (Just secondSet) -> identifySharedItemInGroup (fromMaybe [] (tail inputArray)) (Just (fromFoldable (toCharArray (fromMaybe "" (head inputArray))))) (Just secondSet)
  Tuple (Just firstSet) (Just secondSet) -> Tuple (fromMaybe '0' (findMin (intersection firstSet (intersection secondSet (fromFoldable (toCharArray (fromMaybe "" (head inputArray)))))))) (fromMaybe [] (tail inputArray))

evaluateGroupItemTuple :: Tuple Char (Array String) -> Tuple Int (Array String)
evaluateGroupItemTuple inputTuple = Tuple (scoreItem (fst inputTuple)) (snd inputTuple)

evaluateGroupItem :: Array String -> Tuple Int (Array String)
evaluateGroupItem inputArray = evaluateGroupItemTuple (identifySharedItemInGroup inputArray Nothing Nothing)

handleGroupEvaluation :: Tuple Int (Array String) -> Int -> Int
handleGroupEvaluation inputTuple sum = handleInputLines (snd inputTuple) (sum + (fst inputTuple))

handleInputLines :: Array String -> Int -> Int
handleInputLines [] result = result
handleInputLines inputArray result = handleGroupEvaluation (evaluateGroupItem inputArray) result

runDayThreePartTwo :: Effect Unit
runDayThreePartTwo = do
  inputFileContents <- readTextFile UTF8 "./inputs/day-03-input.txt"
  log (toStringAs decimal (handleInputLines (lines inputFileContents) 0))
