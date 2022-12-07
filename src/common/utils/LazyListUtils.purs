module LazyListUtils
  ( consWithMaxLength
  , initOrNil
  , isEveryElementUnique
  , listContainsElement
  , tailOrNil
  )
  where

import Prelude

import Data.List.Lazy (List, cons, find, head, init, length, nil, tail, take)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set, fromFoldable, insert, member)

consWithMaxLength :: forall a. a -> List a -> Int -> List a
consWithMaxLength value list maxLength =
  if
    (length list) >= maxLength
  then
    cons value (take (maxLength - 1) list)
  else
    cons value list

initOrNil :: forall a. List a -> List a
initOrNil l = fromMaybe nil (init l)

_isEveryElementUnique :: forall a. Ord a => List a -> Set a -> Boolean
_isEveryElementUnique inputList seenElements = case (head inputList) of
  Nothing -> true
  Just value ->
    if
      member value seenElements
    then
      false
    else
      _isEveryElementUnique (tailOrNil inputList) (insert value seenElements)

isEveryElementUnique :: forall a. Ord a => List a -> Boolean
isEveryElementUnique inputList = _isEveryElementUnique inputList (fromFoldable [])

listContainsElement :: forall a. Eq a => a -> List a -> Boolean
listContainsElement value list = isJust (find (eq value) list)

tailOrNil :: forall a. List a -> List a
tailOrNil l = fromMaybe nil (tail l)
