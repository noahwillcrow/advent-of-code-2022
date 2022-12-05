module ArrayUtils
  ( map
  , tailOrEmpty
  , unsafeIndex
  )
  where

import Data.Array (cons, head, tail, (!!))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Partial.Unsafe (unsafePartial)

tailOrEmpty :: forall a. Array a -> Array a
tailOrEmpty a = fromMaybe [] (tail a)

_map :: forall a b. (a -> b) -> Array a -> Array b -> Array b
_map func inputArray outputArray = case (head inputArray) of
  Nothing -> outputArray
  Just inputValue -> _map func (fromMaybe [] (tail inputArray)) (cons (func inputValue) outputArray)

map :: forall a b. (a -> b) -> Array a -> Array b
map func inputArray = _map func inputArray []

unsafeIndex :: forall a. Array a -> Int -> a
unsafeIndex a i = unsafePartial (fromJust (a !! i))
