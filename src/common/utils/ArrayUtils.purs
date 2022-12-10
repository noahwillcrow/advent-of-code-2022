module ArrayUtils
  ( createForLengthWithDefaultValue
  , map
  , tailOrEmpty
  , unsafeIndex
  , unsafeModifyAt
  )
  where

import Prelude

import Data.Array (cons, head, length, modifyAt, tail, (!!))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Partial.Unsafe (unsafePartial)


_createForLengthWithDefaultValue :: forall a. Int -> (_ -> a) -> Array a -> Array a
_createForLengthWithDefaultValue desiredLength createValue resultArray =
  if
    (length resultArray) == desiredLength
  then
    resultArray
  else
    _createForLengthWithDefaultValue desiredLength createValue (cons (createValue (length resultArray)) resultArray)

createForLengthWithDefaultValue :: forall a. Int -> (_ -> a) -> Array a
createForLengthWithDefaultValue desiredLength createValue = _createForLengthWithDefaultValue desiredLength createValue []

_map :: forall a b. (a -> b) -> Array a -> Array b -> Array b
_map func inputArray outputArray = case (head inputArray) of
  Nothing -> outputArray
  Just inputValue -> _map func (fromMaybe [] (tail inputArray)) (cons (func inputValue) outputArray)

map :: forall a b. (a -> b) -> Array a -> Array b
map func inputArray = _map func inputArray []

tailOrEmpty :: forall a. Array a -> Array a
tailOrEmpty a = fromMaybe [] (tail a)

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex a i = unsafePartial (fromJust (a !! i))

unsafeModifyAt :: forall a. Array a -> Int -> a -> Array a
unsafeModifyAt a i v = unsafePartial (fromJust (modifyAt i (\_ -> v) a))
