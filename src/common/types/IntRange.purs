module IntRange
  ( IntRange
  , defineIntRange
  , doRangesOverlap
  , isOneRangeEntirelyInsideTheOther
  , rangeIntersection
  )
  where

import Prelude

type IntRange = { min :: Int, max :: Int }

defineIntRange :: Int -> Int -> IntRange
defineIntRange a b = { min: min a b, max: max a b }

doRangesOverlap :: IntRange -> IntRange -> Boolean
doRangesOverlap a b =
  (min a.max b.max) >= (max a.min b.min)

_isOneRangeEntirelyInsideTheOther :: IntRange -> IntRange -> IntRange -> Boolean
_isOneRangeEntirelyInsideTheOther a b intersectionRange =
  (a.min >= intersectionRange.min && a.max <= intersectionRange.max) || (b.min >= intersectionRange.min && b.max <= intersectionRange.max)

isOneRangeEntirelyInsideTheOther :: IntRange -> IntRange -> Boolean
isOneRangeEntirelyInsideTheOther a b = _isOneRangeEntirelyInsideTheOther a b (rangeIntersection a b)

rangeIntersection :: IntRange -> IntRange -> IntRange
rangeIntersection a b = { min: max a.min b.min, max: min a.max b.max }